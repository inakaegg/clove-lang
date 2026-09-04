#!/usr/bin/env bash
# Sign and notarize a macOS `clove` executable for distribution.
#
# Usage: tools/release/sign-macos.sh <path-to-binary>
#
# Required environment; if any of them is empty the script prints why and exits
# 0 without touching the binary, so a release cut before the signing material
# is registered still publishes unsigned archives:
#   MACOS_CERT_P12        base64 of the Developer ID Application .p12
#   MACOS_CERT_PASSWORD   password of that .p12
#   APPLE_API_KEY_P8      contents of the App Store Connect API key (.p8)
#   APPLE_API_KEY_ID      key id of that key
#   APPLE_API_ISSUER_ID   issuer id of that key
# Optional:
#   MACOS_SIGN_IDENTITY   identity to sign with; by default the first
#                         "Developer ID Application" identity in the .p12
#
# Nothing here may print a secret, so `set -x` must stay off.

set -euo pipefail

die() { printf 'sign-macos.sh: %s\n' "$1" >&2; exit 1; }
skip() { printf 'sign-macos.sh: not signing (%s)\n' "$1" >&2; exit 0; }

[ $# -eq 1 ] || die "usage: sign-macos.sh <path-to-binary>"
binary="$1"
[ -f "$binary" ] || die "no such file: $binary"

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
entitlements="$script_dir/entitlements.plist"
[ -f "$entitlements" ] || die "missing $entitlements"

[ "$(uname -s)" = "Darwin" ] || skip "this is not macOS"

missing=""
for var in MACOS_CERT_P12 MACOS_CERT_PASSWORD APPLE_API_KEY_P8 \
           APPLE_API_KEY_ID APPLE_API_ISSUER_ID; do
  [ -n "${!var:-}" ] || missing="$missing $var"
done
[ -z "$missing" ] || skip "these variables are empty:$missing"

umask 077
work="$(mktemp -d)"
keychain="$work/clove-signing.keychain-db"
keychain_created=0
cleanup() {
  if [ "$keychain_created" -eq 1 ]; then
    security delete-keychain "$keychain" >/dev/null 2>&1 || true
  fi
  rm -rf "$work"
}
trap cleanup EXIT

# A throwaway keychain keeps the certificate out of the login keychain, and
# `codesign --keychain` keeps it out of the search list as well, so running
# this on a developer's own Mac leaves no trace once the trap fires.
keychain_password="$(uuidgen)"
security create-keychain -p "$keychain_password" "$keychain"
keychain_created=1
security set-keychain-settings -lut 21600 "$keychain"
security unlock-keychain -p "$keychain_password" "$keychain"

printf '%s' "$MACOS_CERT_P12" | base64 --decode > "$work/certificate.p12"
security import "$work/certificate.p12" -k "$keychain" -P "$MACOS_CERT_PASSWORD" \
  -T /usr/bin/codesign -f pkcs12 >/dev/null
# Without this, codesign stops for a GUI prompt that no CI runner can answer.
security set-key-partition-list -S apple-tool:,apple:,codesign: \
  -s -k "$keychain_password" "$keychain" >/dev/null
rm -f "$work/certificate.p12"

identity="${MACOS_SIGN_IDENTITY:-}"
if [ -z "$identity" ]; then
  identity="$(security find-identity -v -p codesigning "$keychain" \
    | awk '/Developer ID Application/ { print $2; exit }')"
fi
[ -n "$identity" ] || die "the certificate holds no Developer ID Application identity"

codesign --force --sign "$identity" --keychain "$keychain" \
  --options runtime --timestamp --entitlements "$entitlements" "$binary"

codesign --verify --strict --verbose=2 "$binary"

# The entitlement is the reason clove can still load a native plugin dylib the
# user built themselves -- from the default trusted directories as well as with
# --allow-native-plugins. A signature without it would break those paths in the
# downloaded binary only.
codesign -d --entitlements - "$binary" > "$work/entitlements-actual" 2>&1
if ! grep -q 'disable-library-validation' "$work/entitlements-actual"; then
  cat "$work/entitlements-actual" >&2
  die "the signature does not carry com.apple.security.cs.disable-library-validation"
fi

printf '%s' "$APPLE_API_KEY_P8" > "$work/AuthKey.p8"
ditto -c -k --norsrc --noextattr "$binary" "$work/notarize.zip"
xcrun notarytool submit "$work/notarize.zip" \
  --key "$work/AuthKey.p8" \
  --key-id "$APPLE_API_KEY_ID" \
  --issuer "$APPLE_API_ISSUER_ID" \
  --wait --timeout 30m

# A bare executable cannot be stapled -- `xcrun stapler` only handles bundles,
# disk images, and installer packages -- so Gatekeeper looks the notarization
# ticket up online the first time a quarantined copy runs. README records that.
#
# spctl's verdict is reported but not enforced: it assesses a standalone
# executable against policies written for app bundles, and its answer also
# depends on reaching Apple for the ticket. The binding check is on a real
# download after the secrets are registered.
spctl --assess --type execute -vv "$binary" 2>&1 || true

printf 'sign-macos.sh: signed and notarized %s\n' "$binary"

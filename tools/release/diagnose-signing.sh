#!/usr/bin/env bash
# Runner-side probe for the "specified item could not be found in the keychain"
# failure of sign-macos.sh. Prints statuses, counts and identity names only.
# Never prints secret values; `set -x` must stay off.
set -uo pipefail

say() { printf '== %s\n' "$*"; }
missing=""
for var in MACOS_CERT_P12 MACOS_CERT_PASSWORD; do
  [ -n "${!var:-}" ] || missing="$missing $var"
done
[ -z "$missing" ] || { say "empty variables:$missing"; exit 1; }

say "macOS $(sw_vers -productVersion), $(security 2>&1 | head -1 | cut -c1-40)"
work="$(mktemp -d)"; umask 077
trap 'rm -rf "$work"' EXIT
printf '%s' "$MACOS_CERT_P12" | base64 --decode > "$work/c.p12" || { say "base64 decode FAILED"; exit 1; }
say "p12 bytes: $(wc -c < "$work/c.p12" | tr -d ' ')"
say "p12 structure (bag types via openssl, counts only):"
openssl pkcs12 -in "$work/c.p12" -info -noout -passin env:MACOS_CERT_PASSWORD 2>&1 \
  | grep -o 'Shrouded Keybag\|Certificate bag\|Key bag\|MAC verified OK\|Mac verify error\|unsupported\|error' | sort | uniq -c

probe() {
  # $1 label, $2 import flags (word-split), $3 add-to-search-list yes/no, $4 partition flags
  label="$1"; import_flags="$2"; add_search="$3"; part_flags="$4"
  kc="$work/$label.keychain-db"; pw="$(uuidgen)"
  security create-keychain -p "$pw" "$kc" >/dev/null 2>&1
  security set-keychain-settings -lut 21600 "$kc"
  security unlock-keychain -p "$pw" "$kc"
  # shellcheck disable=SC2086
  security import "$work/c.p12" -k "$kc" -P "$MACOS_CERT_PASSWORD" $import_flags -f pkcs12 >"$work/imp" 2>&1
  say "[$label] import status=$? ($(grep -c 'imported' "$work/imp") 'imported' lines)"
  say "[$label] keys in keychain: $(security dump-keychain "$kc" 2>/dev/null | grep -c 'class: "keys"' || true)  certs: $(security dump-keychain "$kc" 2>/dev/null | grep -c '0x80001000' || true)"
  say "[$label] find-identity: $(security find-identity -v -p codesigning "$kc" 2>&1 | sed -E 's/^ *[0-9]+\) [0-9A-F]{40} //' | tr '\n' ' ')"
  if [ "$add_search" = "yes" ]; then
    orig="$(security list-keychains -d user | tr -d '"' | tr '\n' ' ')"
    # shellcheck disable=SC2086
    security list-keychains -d user -s "$kc" $orig
    say "[$label] added to user search list"
  fi
  # shellcheck disable=SC2086
  security set-key-partition-list $part_flags -k "$pw" "$kc" >"$work/part" 2>&1
  st=$?
  say "[$label] set-key-partition-list ($part_flags) status=$st $(head -1 "$work/part" | cut -c1-120)"
  if [ "$st" -eq 0 ]; then
    identity="$(security find-identity -v -p codesigning "$kc" | awk '/Developer ID Application/ { print $2; exit }')"
    cp /bin/ls "$work/ls-copy"
    codesign --force --sign "$identity" --keychain "$kc" --options runtime --timestamp "$work/ls-copy" >"$work/cs" 2>&1
    say "[$label] codesign status=$? $(head -1 "$work/cs" | cut -c1-120)"
  fi
  if [ "$add_search" = "yes" ]; then
    # shellcheck disable=SC2086
    security list-keychains -d user -s $orig
  fi
  security delete-keychain "$kc" >/dev/null 2>&1
}

probe current  "-T /usr/bin/codesign"           no  "-S apple-tool:,apple:,codesign: -s"
probe searchlist "-T /usr/bin/codesign"         yes "-S apple-tool:,apple:,codesign: -s"
probe github   "-A -t cert"                     no  "-S apple-tool:,apple:"
probe github-searchlist "-A -t cert"            yes "-S apple-tool:,apple:"
say "done"

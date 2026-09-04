#!/usr/bin/env bash
# Build the distribution binary of `clove` and pack it for a GitHub release.
#
# The distribution build leaves out the embedded Ruby and Python runtimes
# (`--no-default-features --features repl_guard`), so the binary links against
# system libraries only and runs on a machine that has no rbenv/pyenv and no
# Rust toolchain. See docs/design-notes/prebuilt-binaries.md for why.
#
# Output in the chosen directory (default `dist/`):
#   clove-<version>-<host-target-triple>.tar.gz
#   SHA256SUMS
#
# The published SHA256SUMS covers every platform and is written by the release
# workflow after all archives exist; the one written here is for checking a
# local build.

set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -- "$script_dir/../.." && pwd)"

die() { printf 'package.sh: %s\n' "$1" >&2; exit 1; }

usage() {
  cat <<'USAGE'
Usage: tools/release/package.sh [options]

  --out-dir DIR    where to write the archive (default: dist/)
  --version VER    version used in the file name (default: from Cargo.toml)
  --no-build       reuse target/release/clove instead of building it
  --no-sign        skip the macOS signing step even when its secrets are set
  -h, --help       show this message

On macOS the archived binary is signed and notarized by
tools/release/sign-macos.sh, which skips itself when the signing environment
is not set.
USAGE
}

sha256() {
  if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$@"
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$@"
  else
    die "neither shasum nor sha256sum is available"
  fi
}

out_dir="$repo_root/dist"
version=""
do_build=1
do_sign=1

while [ $# -gt 0 ]; do
  case "$1" in
    --out-dir) [ $# -ge 2 ] || die "--out-dir needs a value"; out_dir="$2"; shift 2 ;;
    --version) [ $# -ge 2 ] || die "--version needs a value"; version="$2"; shift 2 ;;
    --no-build) do_build=0; shift ;;
    --no-sign) do_sign=0; shift ;;
    -h|--help) usage; exit 0 ;;
    *) usage >&2; die "unknown option: $1" ;;
  esac
done

command -v rustc >/dev/null 2>&1 || die "rustc is not on PATH"
target="$(rustc -vV | sed -n 's/^host: //p')"
[ -n "$target" ] || die "could not read the host target triple from 'rustc -vV'"

if [ -z "$version" ]; then
  version="$(awk -F'"' '
    /^\[/ { in_package = ($0 == "[package]") }
    in_package && /^version[[:space:]]*=/ { print $2; exit }
  ' "$repo_root/crates/clove-lang/Cargo.toml")"
fi
[ -n "$version" ] || die "could not read the version from crates/clove-lang/Cargo.toml"

name="clove-$version-$target"
binary="$repo_root/target/release/clove"

if [ "$do_build" -eq 1 ]; then
  ( cd "$repo_root" \
    && cargo build --release --locked -p clove-lang \
         --no-default-features --features repl_guard )
fi
[ -x "$binary" ] || die "$binary does not exist; run without --no-build"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT

mkdir -p "$stage/$name"
cp "$binary" "$stage/$name/clove"
cp "$repo_root/LICENSE-MIT" "$repo_root/LICENSE-APACHE" "$stage/$name/"

if [ "$do_sign" -eq 1 ]; then
  "$script_dir/sign-macos.sh" "$stage/$name/clove"
fi

mkdir -p "$out_dir"
out_dir="$(cd -- "$out_dir" && pwd)"
tar -czf "$out_dir/$name.tar.gz" -C "$stage" "$name"
( cd "$out_dir" && sha256 "$name.tar.gz" > SHA256SUMS )

printf 'package.sh: wrote %s\n' "$out_dir/$name.tar.gz"
cat "$out_dir/SHA256SUMS"

#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
SIZE="${1:-100000}"
ITERS="${2:-5}"
GET_OPS="${3:-1000000}"
FEATURES="${BENCH_FEATURES:-}"

cd "$ROOT_DIR"

if [[ -n "$FEATURES" ]]; then
  cargo run -p clove-core --features "$FEATURES" --bin bench_collections -- \
    --size "$SIZE" --iters "$ITERS" --get-ops "$GET_OPS"
else
  cargo run -p clove-core --bin bench_collections -- \
    --size "$SIZE" --iters "$ITERS" --get-ops "$GET_OPS"
fi

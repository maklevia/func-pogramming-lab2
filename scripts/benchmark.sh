#!/usr/bin/env bash
set -euo pipefail

export LC_ALL=C

cd "$(dirname "$0")/.."

SIZES="${SIZES:-600 1200 2000}"
REPEAT="${REPEAT:-2}"

echo "Building parallel-gauss-solver..."
cabal build exe:parallel-gauss-solver >/dev/null
BIN="$(cabal list-bin exe:parallel-gauss-solver)"

CORES="$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)"
echo "Machine cores: $CORES"
echo "Sizes: $SIZES; repeats per mode: $REPEAT"
echo
printf '%-8s %14s %14s %12s\n' "n" "sequential,ms" "parallel,ms" "speedup"

for n in $SIZES; do
  if ! seq_out="$("$BIN" --bench --gen "$n" --repeat "$REPEAT" +RTS -N1 2>/dev/null)"; then
    echo "Sequential benchmark failed for n=$n" >&2
    exit 1
  fi
  if ! par_out="$("$BIN" --bench --gen "$n" --repeat "$REPEAT" +RTS -N"$CORES" 2>/dev/null)"; then
    echo "Parallel benchmark failed for n=$n" >&2
    exit 1
  fi

  seq_ms="$(awk '/^Послідовний/ {print $2}' <<<"$seq_out")"
  par_ms="$(awk '/^Паралельний/ {print $2}' <<<"$par_out")"
  in_process="$(awk '/^Паралельний/ {print $3}' <<<"$par_out")"

  speedup="$(awk -v s="$seq_ms" -v p="$par_ms" 'BEGIN { if (p > 0) printf "%.2f", s / p; else print "n/a" }')"
  printf '%-8s %14s %14s %11sx\n' "$n" "$seq_ms" "$par_ms" "$speedup"
  echo "  (in-process -N$CORES table: $in_process)"
done

echo
echo "Fair comparison: sequential time from -N1, parallel time from -N$CORES."

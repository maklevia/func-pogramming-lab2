#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building parallel-gauss-solver..."
cabal build exe:parallel-gauss-solver >/dev/null

BIN="$(cabal list-bin exe:parallel-gauss-solver)"
PASS=0

pass() {
  PASS=$((PASS + 1))
  echo "PASS $1"
}

fail() {
  echo "FAIL $1" >&2
  exit 1
}

run_ok() {
  local description="$1"
  shift
  local output
  if ! output="$("$@" 2>&1)"; then
    fail "$description (unexpected non-zero exit)"
  fi
  printf '%s' "$output"
}

run_fail() {
  local expected_code="$1"
  local description="$2"
  shift 2
  local output code
  set +e
  output="$("$@" 2>&1)"
  code=$?
  set -e
  if [[ "$code" -ne "$expected_code" ]]; then
    echo "$output" >&2
    fail "$description (expected exit $expected_code, got $code)"
  fi
  printf '%s' "$output"
}

# 1. File input, parallel mode.
out="$(run_ok "solve examples/simple3.txt" "$BIN" examples/simple3.txt +RTS -N4)"
grep -q "x1 = 2" <<<"$out" || fail "simple3 x1"
grep -q "x2 = 3" <<<"$out" || fail "simple3 x2"
grep -q "x3 = -1" <<<"$out" || fail "simple3 x3"
grep -q "Доступно capabilities: 4" <<<"$out" || fail "simple3 capabilities"
pass "file input and parallel mode"

# 2. Sequential mode.
out="$(run_ok "solve --seq examples/simple3.txt" "$BIN" --seq examples/simple3.txt)"
grep -q "x1 = 2" <<<"$out" || fail "seq x1"
pass "sequential mode"

# 3. stdin input.
out="$(printf '2\n1 1 3\n2 -1 0\n' | run_ok "solve stdin" "$BIN" --seq)"
grep -q "x1 = 1" <<<"$out" || fail "stdin x1"
grep -q "x2 = 2" <<<"$out" || fail "stdin x2"
pass "stdin input"

# 4. Generated system with a known solution.
out="$(run_ok "solve generated system" "$BIN" --gen 20 --seed 7)"
grep -q "Макс. відхилення від відомого розв'язку" <<<"$out" || fail "generated known solution"
pass "generated system"

# 5. Benchmark mode on a medium generated system.
out="$(run_ok "benchmark" "$BIN" --bench --gen 300 --repeat 1 +RTS -N4)"
grep -q "Послідовний" <<<"$out" || fail "benchmark sequential row"
grep -q "Паралельний" <<<"$out" || fail "benchmark parallel row"
grep -q "Макс. |x_par - x_seq|" <<<"$out" || fail "benchmark cross-check"
pass "benchmark mode and parallel/sequential cross-check"

# 6. Singular matrix exits with code 2.
out="$(run_fail 2 "singular matrix" "$BIN" examples/singular.txt)"
grep -q "вироджена" <<<"$out" || fail "singular message"
pass "singular matrix error"

# 7. Malformed matrix exits with code 1.
out="$(printf '2\n1 2 3\n' | run_fail 1 "malformed input" "$BIN")"
grep -q "очікується" <<<"$out" || fail "malformed message"
pass "malformed input error"

# 8. Invalid options exit with code 1.
run_fail 1 "invalid epsilon" "$BIN" --eps 0 >/dev/null
pass "invalid epsilon error"
run_fail 1 "invalid generator size" "$BIN" --gen 0 >/dev/null
pass "invalid generator size error"
run_fail 1 "conflicting modes" "$BIN" --seq --bench >/dev/null
pass "conflicting modes error"

# 9. Missing file exits with code 1.
out="$(run_fail 1 "missing file" "$BIN" /definitely/missing/file.txt)"
grep -q "не вдалося прочитати файл" <<<"$out" || fail "missing file message"
pass "missing file error"

# 10. Help exits with code 0.
out="$(run_ok "help" "$BIN" --help)"
grep -q "Використання: parallel-gauss-solver" <<<"$out" || fail "help text"
pass "help output"

echo "Smoke test passed: $PASS checks."

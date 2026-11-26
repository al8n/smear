#!/usr/bin/env bash
#
# Self-test for `ci/perf/compare.py` — the half of the perf lane that DECIDES — over synthetic
# readings, before anything is built.
#
# `icount.py` is watched by its own reading: a broken callgrind parse gives an absurd number or none
# at all, and a row whose line bends or whose repeat moves is refused. The decision logic has no
# such witness. A `compare.py` that returned 0 on everything would leave the count job green, the
# table printed, the numbers right, and the gate off; one that let a wall-clock trailer license a
# count, or dropped a row it could not read, would do the same to one row at a time. So every
# verdict and every refusal the count kind can reach is run here against readings whose answer is
# known, and each note is watched from both sides: present where it belongs, absent where it does
# not. It needs neither valgrind nor a build and runs in a few seconds.
#
#     ci/perf/compare_selftest.sh            # PYTHON=/usr/bin/python3 to run it under another one

set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
python="${PYTHON:-python3}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
failures=0

# `reading <file> [setting...]`: a count reading of five rows, three of them J1's, each at 100 000
# Ir an iteration at `lo` and 200 000 at `hi`, with Dr and Dw beside them. Settings:
#   n=<N> arch=<machine> data=0 kind=alloc drop=<row>
#   <row>=<factor>       both sizes' Ir, Dr and Dw times <factor>
#   <row>.hi=<factor>    the large size's Ir alone
#   <row>.dr=<factor>    both sizes' Dr alone
#   <row>!=<reason>      the row NOT MEASURED, for <reason>
reading() {
  "$python" - "$@" <<'PY'
import json, sys
path, settings = sys.argv[1], sys.argv[2:]
rows = {
    "schema_real": "schema",
    "parse_type_system": "parse",
    "lex_graphql_syntactic": "lex",
    "lex_graphqlx_syntactic": "lex",
    "parse_graphqlx_executable": "graphqlx",
}
options = {"n": "10", "arch": "x86_64", "data": "1", "kind": "icount", "drop": ""}
scale, scale_hi, scale_dr, broken = {}, {}, {}, {}
for setting in settings:
    key, value = setting.split("=", 1)
    if key in options:
        options[key] = value
    elif key.endswith("!"):
        broken[key[:-1]] = value
    elif key.endswith(".hi"):
        scale_hi[key[:-3]] = float(value)
    elif key.endswith(".dr"):
        scale_dr[key[:-3]] = float(value)
    else:
        scale[key] = float(value)
n, data = int(options["n"]), options["data"] == "1"
workloads = {}
for name, family in rows.items():
    if name == options["drop"]:
        continue
    k = scale.get(name, 1.0)
    row = {"family": family, "lo_size": 4, "hi_size": 8,
           "lo": 100000.0 * k, "hi": 200000.0 * k * scale_hi.get(name, 1.0), "not_measured": None}
    if options["kind"] == "icount":
        for size, ir in (("lo", 100000.0), ("hi", 200000.0)):
            row[f"{size}_dr"] = ir / 3 * k * scale_dr.get(name, 1.0) if data else None
            row[f"{size}_dw"] = ir / 5 * k if data else None
        if name in broken:
            row.update({"lo": None, "not_measured": broken[name]})
    workloads[name] = row
if options["kind"] == "icount":
    document = {"instrument": "icount", "unit": "Ir/iter", "iterations": n,
                "points": [n, 2 * n, 3 * n], "data": data, "linearity": 1e-4,
                "host": {"image": "ubuntu-26.04", "os": "Ubuntu 26.04.1 LTS",
                         "arch": options["arch"], "cpu": "AMD EPYC 7763", "valgrind": "valgrind-3.26.0"},
                "workloads": workloads}
else:
    document = {"instrument": options["kind"], "unit": "bytes", "rounds": 1, "workloads": workloads}
json.dump(document, open(path, "w"), indent=1)
PY
}

# The arguments every count comparison below is run with, J1's rows and allowance among them.
J1=(--j1-rows lex_graphqlx_syntactic,parse_graphqlx_executable --j1-threshold 0.5)

# `run <args...>`: `compare.py` with the summary and the outcome kept, its output in `$out` and its
# exit status in `$rc`.
run() {
  rm -f "$tmp/summary.md" "$tmp/outcome"
  set +e
  out="$("$python" "$here/compare.py" "$@" --summary "$tmp/summary.md" --outcome "$tmp/outcome" 2>&1)"
  rc=$?
  set -e
  if [ -f "$tmp/summary.md" ]; then out="$out"$'\n'"$(cat "$tmp/summary.md")"; fi
  if [ -f "$tmp/outcome" ]; then out="$out"$'\n'"outcome: $(cat "$tmp/outcome")"; fi
}

verdict() {
  local name="$1" want_exit="$2"
  if [ "$rc" -ne "$want_exit" ]; then
    echo "FAIL $name: exit $rc, expected $want_exit"
    printf '%s\n' "$out" | sed 's/^/    /'
    failures=$((failures + 1))
    return 1
  fi
}

# `check <name> <exit> <text> -- <compare.py args...>`: exits so, and says <text> somewhere in its
# output, its summary or its outcome.
check() {
  local name="$1" want_exit="$2" want_text="$3"
  shift 4
  run "$@"
  verdict "$name" "$want_exit" || return 0
  case "$out" in
    *"$want_text"*) echo "ok   $name" ;;
    *)
      echo "FAIL $name: does not say: $want_text"
      printf '%s\n' "$out" | sed 's/^/    /'
      failures=$((failures + 1))
      ;;
  esac
}

# `check_absent <name> <exit> <text> -- <compare.py args...>`: exits so, and never says <text>.
check_absent() {
  local name="$1" want_exit="$2" forbidden="$3"
  shift 4
  run "$@"
  verdict "$name" "$want_exit" || return 0
  case "$out" in
    *"$forbidden"*)
      echo "FAIL $name: says what it must not: $forbidden"
      printf '%s\n' "$out" | sed 's/^/    /'
      failures=$((failures + 1))
      ;;
    *) echo "ok   $name" ;;
  esac
}

count() {
  # `count <base> <head> [extra args...]`, the count kind with the lane's own values.
  local base="$1" head="$2"
  shift 2
  printf '%s\n' --kind icount --base "$tmp/$base.json" --head "$tmp/$head.json" \
    --threshold 1.0 --ratio-tolerance 0.15 "${J1[@]}" "$@"
}

reading "$tmp/base.json"
reading "$tmp/same.json"
reading "$tmp/down.json" schema_real=0.9
reading "$tmp/up2.json" schema_real=1.02
reading "$tmp/up08.json" schema_real=1.008
reading "$tmp/law.json" schema_real.hi=1.2
reading "$tmp/lex.json" lex_graphql_syntactic=1.05
reading "$tmp/j1_08.json" parse_graphqlx_executable=1.008
reading "$tmp/j1_03.json" parse_graphqlx_executable=1.003
reading "$tmp/j1_2.json" parse_graphqlx_executable=1.02
reading "$tmp/broken.json" "parse_type_system!=head side, lo: callgrind exited 101 at n=20"
reading "$tmp/n16.json" n=16
reading "$tmp/short.json" drop=schema_real
reading "$tmp/dr.json" parse_type_system.dr=1.05
reading "$tmp/ironly.json" data=0
reading "$tmp/arm.json" arch=aarch64
reading "$tmp/arm_base.json" arch=aarch64
reading "$tmp/alloc_base.json" kind=alloc
reading "$tmp/alloc_up2.json" kind=alloc schema_real=1.02

echo "Perf-accept: icount/schema_real +3% the extra instructions buy the fix" > "$tmp/icount_ok.txt"
echo "Perf-accept: icount/schema_real +1.5% a ceiling below the delta" > "$tmp/icount_low.txt"
echo "Perf-accept: schema_real +3% written for the clock" > "$tmp/bare.txt"
echo "Perf-accept: icount/schema_real +3%" > "$tmp/noreason.txt"
echo "Perf-accept: wall/schema_real +3% an instrument with no scope" > "$tmp/badscope.txt"

# The positive control first: a check that cannot pass is as useless as one that cannot fail, and
# every case below is read against this one.
check "identical counts pass" 0 "no workload over its threshold" -- \
  $(count base same)
check "an improvement passes and is reported" 0 "-10.000%" -- $(count base down)
check "a regression over the threshold fails" 1 "::error::schema_real (icount)" -- \
  $(count base up2)
check "the failure names the unit" 1 "Ir/iter" -- $(count base up2)
check "a regression under the threshold passes" 0 "no workload over its threshold" -- \
  $(count base up08)
check "a change of law fails" 1 "LAW CHANGED" -- $(count base law)

# Acceptances, scoped to the instrument.
check "an icount/ acceptance above the delta passes" 0 "accepted (<= +3%)" -- \
  $(count base up2) --accept-file "$tmp/icount_ok.txt"
check "it prints its reason" 0 "the extra instructions buy the fix" -- \
  $(count base up2) --accept-file "$tmp/icount_ok.txt"
check "a ceiling below the delta still fails" 1 "::error::schema_real (icount)" -- \
  $(count base up2) --accept-file "$tmp/icount_low.txt"
check "a bare trailer does not license a count" 1 "::error::schema_real (icount)" -- \
  $(count base up2) --accept-file "$tmp/bare.txt"
check "the trailer it offers is scoped" 1 "Perf-accept: icount/schema_real" -- \
  $(count base up2)
check "an icount/ trailer does not license the allocation gate" 1 "::error::schema_real (alloc)" -- \
  --kind alloc --base "$tmp/alloc_base.json" --head "$tmp/alloc_up2.json" --threshold 1.0 \
  --ratio-tolerance 0.15 --accept-file "$tmp/icount_ok.txt"
check "a bare trailer still licenses the allocation gate" 0 "accepted (<= +3%)" -- \
  --kind alloc --base "$tmp/alloc_base.json" --head "$tmp/alloc_up2.json" --threshold 1.0 \
  --ratio-tolerance 0.15 --accept-file "$tmp/bare.txt"
check "an acceptance with no reason is refused" 1 "cannot read this as an acceptance" -- \
  $(count base up2) --accept-file "$tmp/noreason.txt"
check "an acceptance with an unknown scope is refused" 1 "cannot read this as an acceptance" -- \
  $(count base up2) --accept-file "$tmp/badscope.txt"
check "an icount/ trailer does not license a change of law" 1 "LAW CHANGED" -- \
  $(count base law) --accept-file "$tmp/icount_ok.txt"

# NOT MEASURED: in the table, failing, with its side, and in the run's last line.
check "a row NOT MEASURED fails" 1 "::error::parse_type_system (icount): NOT MEASURED" -- \
  $(count base broken)
check "and names its side" 1 "head side, lo: callgrind exited 101" -- $(count base broken)
check "and stays in the table" 1 '| `parse_type_system` | parse | - |' -- $(count base broken)
check "and is in the outcome" 1 "outcome: 1 of 5 NOT MEASURED: parse_type_system" -- \
  $(count base broken)
check "two sides read at different n are not compared" 1 \
  "outcome: NOT MEASURED: the two sides were read at different iterations (10 vs 16)" -- \
  $(count base n16)
check "a workload set mismatch is refused" 1 "outcome: NOT MEASURED: the two sides measured" -- \
  $(count base short)
check "a count is one file a side" 1 "a count is read once on each side" -- \
  --kind icount --base "$tmp/base.json,$tmp/same.json" --head "$tmp/same.json" \
  --threshold 1.0 --ratio-tolerance 0.15

# J1's second verdict: on its rows alone, under the threshold, a warning and never `ok`.
check "a J1 row over J1 is warned about" 0 "::warning::parse_graphqlx_executable (icount)" -- \
  $(count base j1_08)
check "and its verdict says so" 0 "over J1 (+0.5%)" -- $(count base j1_08)
check_absent "and is not called ok" 0 '| +0.800% | +0.800% | 2.000 | 2.000 | ok |' -- \
  $(count base j1_08)
check "and the outcome says so" 0 "over J1's +0.5%, a warning: parse_graphqlx_executable" -- \
  $(count base j1_08)
check_absent "a row J1 does not read gets no J1 warning" 0 "::warning::" -- $(count base up08)
check_absent "a J1 row inside J1 gets none" 0 "::warning::" -- $(count base j1_03)
check "a J1 row over the threshold is a regression" 1 "::error::parse_graphqlx_executable" -- \
  $(count base j1_2)
check_absent "and is not also a J1 warning" 1 "::warning::" -- $(count base j1_2)
check "a J1 row the readings do not hold is refused" 1 "--j1-rows names ['nope']" -- \
  --kind icount --base "$tmp/base.json" --head "$tmp/same.json" --threshold 1.0 \
  --ratio-tolerance 0.15 --j1-rows nope --j1-threshold 0.5

# Where the cost is: the builder's note for a schema row, and no note for a lexer row.
check "a schema-only failure is placed in the builder" 1 "inside \`Schema::build\`" -- \
  $(count base up2)
check_absent "a lexer-only failure is not" 1 "Schema::build" -- $(count base lex)

# Reads and writes: shown, and never a verdict.
check "reads and writes are shown" 0 "#### Reads and writes, advisory" -- $(count base dr)
check "with the change" 0 '| `parse_type_system` | +5.000% | +5.000% | +0.000% | +0.000% |' -- \
  $(count base dr)
check "instructions only says so" 0 "**Instructions only**" -- $(count ironly ironly)
check_absent "and shows no table of them" 0 "Reads and writes, advisory" -- \
  $(count ironly ironly)

# The heading: what the counts were read on, and what the threshold rests on.
check "the heading names the image, valgrind and the CPU" 0 \
  "Ubuntu 26.04.1 LTS, x86_64, CPU AMD EPYC 7763, valgrind-3.26.0" -- $(count base same)
check "an x86 table says it counts AVX2 kernels" 0 "memspan's AVX2 kernels" -- \
  $(count base same)
check_absent "and claims no missing sample" 0 "no drift sample" -- $(count base same)
check "an ARM table says it has no drift sample" 0 "This image has no drift sample" -- \
  $(count arm_base arm)
check_absent "and says nothing of AVX2" 0 "AVX2" -- $(count arm_base arm)

if [ "$failures" -ne 0 ]; then
  echo "::error::compare.py self-test: $failures case(s) failed"
  exit 1
fi
echo "compare.py self-test: all cases behaved as required"

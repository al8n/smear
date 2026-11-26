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
# One part of `icount.py` is checked here too: its reader of callgrind's files, against files built
# from the lines real runs wrote. A reader stricter than callgrind refuses every row of every run,
# and one laxer than it reads a file that was cut short; both are watched.
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
#   <row>.spread=<pct>   the large size's Ir reading spread that far (every spread is 0 otherwise)
#   <row>.drspread=<pct> the large size's Dr reading spread that far
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
scale, scale_hi, scale_dr, broken, spread, drspread = {}, {}, {}, {}, {}, {}
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
    elif key.endswith(".spread"):
        spread[key[:-7]] = float(value)
    elif key.endswith(".drspread"):
        drspread[key[:-9]] = float(value)
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
        row.update({"lo_spread": 0.0, "hi_spread": spread.get(name, 0.0)})
        for size, ir in (("lo", 100000.0), ("hi", 200000.0)):
            row[f"{size}_dr"] = ir / 3 * k * scale_dr.get(name, 1.0) if data else None
            row[f"{size}_dw"] = ir / 5 * k if data else None
            row[f"{size}_dr_spread"] = drspread.get(name, 0.0) if size == "hi" else 0.0
            row[f"{size}_dw_spread"] = 0.0
        if name in broken:
            row.update({"lo": None, "lo_spread": None, "not_measured": broken[name]})
    workloads[name] = row
if options["kind"] == "icount":
    document = {"instrument": "icount", "unit": "Ir/iter", "iterations": n,
                "points": [n, 2 * n, 3 * n], "data": data,
                "host": {"image": "ubuntu-26.04", "os": "Ubuntu 26.04.1 LTS",
                         "arch": options["arch"], "cpu": "AMD EPYC 7763", "valgrind": "valgrind-3.26.0"},
                "workloads": workloads}
else:
    document = {"instrument": options["kind"], "unit": "bytes", "rounds": 1, "workloads": workloads}
json.dump(document, open(path, "w"), indent=1)
PY
}

# The arguments every count comparison below is run with, J1's rows and allowance and the spread a
# reading may carry among them.
J1=(--j1-rows lex_graphqlx_syntactic,parse_graphqlx_executable --j1-threshold 0.5 --spread-share 0.25)

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
# Spreads the first CI runs read: `schema_iface_mismatch` at lo on x86, the widest of them;
# `schema_valid` at lo on x86, under the bound; and the widest J1 row's, `parse_graphqlx_type_system`
# at hi on x86. 0.2 sits between J1's bound and the threshold's.
reading "$tmp/spread_wide.json" schema_real.spread=0.44958345
reading "$tmp/spread_ok.json" schema_real.spread=0.18051854
reading "$tmp/spread_j1_real.json" parse_graphqlx_executable.spread=0.04974175
reading "$tmp/spread_j1_wide.json" parse_graphqlx_executable.spread=0.2
reading "$tmp/spread_same_wide.json" schema_real.spread=0.2
reading "$tmp/drspread.json" parse_type_system.dr=1.05 parse_type_system.drspread=0.3

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
check "and its line is read where its verdict is" 0 \
  '| +0.800% | +0.800% | 2.000 | 2.000 | 0.000% | over J1 (+0.5%) |' -- $(count base j1_08)
check_absent "and is not called ok" 0 '| +0.800% | +0.800% | 2.000 | 2.000 | 0.000% | ok |' -- \
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

# A reading's spread: a quarter of the finest line it is read against, on either side and size.
check "a reading spread past a quarter of the line is NOT MEASURED" 1 \
  "::error::schema_real (icount): NOT MEASURED" -- $(count base spread_wide)
check "and says how far, against which line" 1 \
  "head side, hi: its halves read 0.450% apart, past the 0.25% a reading against the +1% line may spread" \
  -- $(count base spread_wide)
check "a spread under the bound is a reading" 0 "no workload over its threshold" -- \
  $(count base spread_ok)
check "and the table shows it" 0 '| 2.000 | 2.000 | 0.181% | ok |' -- $(count base spread_ok)
check "the widest J1 spread read so far is a reading" 0 "no workload over its threshold" -- \
  $(count base spread_j1_real)
check "a J1 row is held to a quarter of J1's line" 1 \
  "past the 0.125% a reading against the +0.5% line may spread" -- $(count base spread_j1_wide)
check "the same spread on a row J1 does not read is a reading" 0 "no workload over its threshold" \
  -- $(count base spread_same_wide)
check "a read or write reading spread past the bound is not shown" 0 \
  '| `parse_type_system` | +5.000% | - | +0.000% | +0.000% | +3.125% | - |' -- $(count base drspread)

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

# ── `icount.py`'s reader of callgrind's files ─────────────────────────────────────────────────
#
# `callgrind_file <file> <summary numbers> <totals numbers, or "none">`: the lines the reader looks
# at, in the order callgrind writes them, with a cost line between.
callgrind_file() {
  {
    printf '# callgrind format\nversion: 1\ncreator: callgrind-3.26.0\n'
    printf 'events: Ir Dr Dw I1mr D1mr D1mw ILmr DLmr DLmw\n'
    printf 'summary: %s\n\n\nfn=(1) main\n16 %s\n' "$2" "$2"
    if [ "$3" != none ]; then
      printf 'totals: %s\n' "$3"
    fi
  } > "$1"
}

# `read_check <name> <the Ir it reads, or "refused"> <file>`
read_check() {
  local name="$1" want="$2" got
  set +e
  got="$("$python" -I -B -c '
import sys
sys.path.insert(0, sys.argv[1])
import icount
try:
    print(icount.parse_callgrind(sys.argv[2])["Ir"])
except ValueError as error:
    print(f"refused: {error}")
' "$here" "$3" 2>&1)"
  set -e
  case "$want:$got" in
    "refused:refused: "* | "$got:$got") echo "ok   $name" ;;
    *)
      echo "FAIL $name: read $got, expected $want"
      failures=$((failures + 1))
      ;;
  esac
}

# Two real runs' lines, `schema_valid` at `lo` and n=10 on the head side: x86-64 under valgrind
# 3.26.0, whose `totals:` stands 2 Ir from its `summary:`, and aarch64, whose stands 10 Ir, 1 Dr,
# 2 Dw and three misses from it. The reading is `summary:`'s Ir.
x86_small="63499790 17351860 10981110 247175 477544 123896 5647 2729 25054"
x86_large="63499792 17351860 10981110 247175 477544 123896 5647 2729 25054"
arm_small="58698820 15700261 10322629 625841 605466 175858 21029 418552 109501"
arm_large="58698830 15700262 10322631 625843 605466 175858 21031 418552 109501"
callgrind_file "$tmp/x86.out" "$x86_small" "$x86_large"
callgrind_file "$tmp/arm.out" "$arm_small" "$arm_large"
# Which of each pair was `summary:` that run's log did not record, so both orders must read.
callgrind_file "$tmp/x86_reversed.out" "$x86_large" "$x86_small"
callgrind_file "$tmp/arm_reversed.out" "$arm_large" "$arm_small"
callgrind_file "$tmp/cut.out" "$x86_small" none
callgrind_file "$tmp/far.out" "$x86_small" \
  "63500790 17351860 10981110 247175 477544 123896 5647 2729 25054"
callgrind_file "$tmp/edge.out" "$x86_small" \
  "63499854 17351924 10981174 247239 477608 123960 5711 2793 25118"
callgrind_file "$tmp/garbled.out" "$x86_small" \
  "6349x792 17351860 10981110 247175 477544 123896 5647 2729 25054"
callgrind_file "$tmp/long.out" "$x86_small" "$x86_large 7"
callgrind_file "$tmp/short.out" "63499790 17351860" "63499792 17351860"
{ cat "$tmp/x86.out"; printf 'summary: %s\n' "$x86_small"; } > "$tmp/twice.out"
printf 'events: Ir\ntotals: 5\n' > "$tmp/nosummary.out"

read_check "x86's lines, 2 Ir apart, read as summary:" 63499790 "$tmp/x86.out"
read_check "aarch64's, apart on several events, read as summary:" 58698820 "$tmp/arm.out"
read_check "x86's the other way round read too" 63499792 "$tmp/x86_reversed.out"
read_check "and aarch64's" 58698830 "$tmp/arm_reversed.out"
read_check "a gap of the slack exactly reads" 63499790 "$tmp/edge.out"
read_check "trailing events left out read as zero" 63499790 "$tmp/short.out"
read_check "a file with no totals: line was cut short" refused "$tmp/cut.out"
read_check "a totals: line far from summary: is refused" refused "$tmp/far.out"
read_check "a number that is not one is refused" refused "$tmp/garbled.out"
read_check "more numbers than events are refused" refused "$tmp/long.out"
read_check "a second summary: line is refused" refused "$tmp/twice.out"
read_check "no summary: line is refused" refused "$tmp/nosummary.out"

# `iteration_check <name> <"cost spread", or "refused"> <Ir at n, 2n, 3n and n again>`: what
# `icount.py` reads out of one row's four runs at n = 10.
iteration_check() {
  local name="$1" want="$2" got
  shift 2
  set +e
  got="$("$python" -I -B -c '
import sys
sys.path.insert(0, sys.argv[1])
import icount
cost, spread, why = icount.per_iteration("Ir", [{"Ir": int(total)} for total in sys.argv[2:]], 10)
print(f"refused: {why}" if why else f"{cost:.2f} {spread:.4f}")
' "$here" "$@" 2>&1)"
  set -e
  case "$want:$got" in
    "refused:refused: "* | "$got:$got") echo "ok   $name" ;;
    *)
      echo "FAIL $name: read $got, expected $want"
      failures=$((failures + 1))
      ;;
  esac
}

# Real rows' totals, both sides alike, from the first run that read every file: the cost is over
# iterations 11 to 30 and the spread how far apart its halves read.
iteration_check "schema_valid at lo on x86 reads over the longest baseline" "5499546.05 0.1805" \
  63499811 118445633 173490732 63499811
iteration_check "schema_iface_mismatch at lo on x86, the widest spread" "19832224.60 0.4496" \
  201967883 399844317 598612375 201967883
iteration_check "parse_graphqlx_type_system at hi on x86, the widest J1 row" "13871043.25 0.0497" \
  139527839 278203773 416948704 139527839
iteration_check "lex_graphqlx_syntactic at hi on x86, a straight line" "3634726.00 0.0000" \
  40610022 76957282 113304542 40610022
iteration_check "schema_transitive at hi on aarch64, its repeat 4 Ir out, is refused" refused \
  263104785 520308970 777520635 263104789
iteration_check "a count that does not grow from 2n to 3n is refused" refused 100 200 200 100

if [ "$failures" -ne 0 ]; then
  echo "::error::compare.py self-test: $failures case(s) failed"
  exit 1
fi
echo "compare.py self-test: all cases behaved as required"

#!/usr/bin/env python3
"""Count the instructions every perf workload executes, on both sides of one comparison.

`ci/perf/run.sh` builds the head's `wall_clock` and the base's, and this runs each one in its count
mode, `--only <name> --size lo|hi --iterations <n>` (see `smear/benches/solo/wall_clock.rs`), under
callgrind. A reading is a DIFFERENCE of two totals of one binary at two iteration counts, each
total the `summary:` line of callgrind's file for that run, over the longest baseline the runs give:

    (Ir(3n) - Ir(n)) / 2n  =  the measured region's cost per iteration, over iterations n+1 to 3n

Nothing inside the binary is toggled, marked or timed. Every once-per-process cost — the loader,
`main`, `warm_up`, building and parsing the corpus, the allocator's first arena, whatever callgrind
itself adds to a total — is in both totals and cancels exactly, so what is left is the region and
nothing else. The first n iterations cancel too, which makes them the warm-up.

Each row is read at both of its sizes, in `compare.py`'s own schema, so the count's per-doubling
ratio is gated by the code that gates the other two instruments' ratios.

ITS SPREAD. The cost of one iteration is not a constant on every row: where an iteration builds and
frees many heap objects, it depends on where the allocator's free lists left them, and that moves
from iteration to iteration even after the warm-up. So each reading carries how far its two halves
disagree, `|(Ir(3n) - Ir(2n)) - (Ir(2n) - Ir(n))|` over half of `Ir(3n) - Ir(n)`, in per cent.
`compare.py` holds a reading to a bound for the lines it is read against, and `run.sh` says how the
bound was set. Given the same bound here (`--spread-share` and the lines), a row whose reading is
past it, on either side at either size, is read again on BOTH sides at `ESCALATION` times n, and
that reading replaces the first; each row's readings say the n it was read at.

WHAT A ROW HAS TO SHOW BEFORE IT IS A READING AT ALL. Each (side, row, size) is run four times: at
n, 2n and 3n, and at n again; a row read again is run at its longer n, 2n and 3n. Every run's
`--iterations` is zero-padded to one width, so argv is the same length in all of them. A row is NOT
MEASURED, with the side and the reason in the readings, when:

  * a run exits non-zero, or does not finish inside `--timeout`, or its last line is not the
    `<name> <lo|hi> <n>` it was asked for;
  * the file a run left cannot be read as a whole one: see `parse_callgrind`;
  * the two runs at n differ in any event: a count that moves between two runs of one binary is
    not a property of the binary, and no threshold over it means anything;
  * Ir does not grow from n to 2n and from 2n to 3n.

A row NOT MEASURED stays in the readings, so `compare.py` puts it in the table and fails the stage
rather than the row going missing from the gate.

Usage:
    icount.py --head <bin> --base <bin> --out <dir> --outcome <file>
              [--iterations N] [--timeout SECONDS] [--jobs J] [--data yes|no]
              [--threshold PCT --spread-share FRACTION [--j1-rows NAME,... --j1-threshold PCT]]

It writes `<dir>/head.json` and `<dir>/base.json`, unit Ir per iteration, and exits 0 once both are
written, whatever they say. It exits 3 when nothing could be read at all — no valgrind, a binary that
cannot list its rows, two sides that list different rows — with the reason in `--outcome`, which
`run.sh` puts in the run's last line; 2 is a usage error. Python 3.9, like the rest of `ci/perf/`.
"""

import argparse
import concurrent.futures
import json
import os
import platform
import shutil
import subprocess
import sys
import time

# `compare.py` holds the bound a reading's spread is held to; the row this reads again is the row
# that gate would refuse. Imported without leaving bytecode beside the scripts.
sys.dont_write_bytecode = True
from compare import spread_bound  # noqa: E402

NOT_MEASURED = 3

# The events read off each run. Ir is gated. Dr and Dw — data reads and writes — need callgrind's
# cache simulator (`--data yes`); they are counts of accesses, which do not depend on the simulated
# cache's geometry, only the miss counts do, and `compare.py` shows them with no verdict.
GATED = "Ir"
ADVISORY = ("Dr", "Dw")

# valgrind's options beyond the tool, for this host. Some atomics in these binaries run on aarch64
# as load-exclusive/store-exclusive loops, and valgrind runs that pair natively by default, on the
# host's exclusive monitor, which events on the host clear: now and then a store-exclusive fails and
# its loop goes round once more. That is 4 Ir, 1 Dr and 1 Dw, which is exactly every difference two
# runs of one binary have shown on aarch64, twice in 208 runs. `fallback-llsc` has valgrind emulate
# the pair instead, and drop the reservation only where it re-enters the program's code
# (`coregrind/m_scheduler/scheduler.c`), which happens at the same places in every run of one binary.
# x86-64 has no such pair, and its runs agree to the instruction.
HOST_OPTIONS = ["--sim-hints=fallback-llsc"] if platform.machine() in ("aarch64", "arm64") else []

# How far a file's `totals:` line may stand from its `summary:` line, on any one event, before the
# file is taken for a damaged one. The two lines sum different things (`callgrind/dump.c`):
# `summary:` is each thread's entry context at the dump, and `totals:` is the sum of every cost line
# the dump wrote, printed last, as the file is closed. On real runs they are a few apart and the same
# few at every row and size: 2 Ir on x86-64, and 10 Ir, 1 Dr, 2 Dw and a few misses on aarch64. The
# bound is on the size of that gap, in either direction. Whatever constant it is, the reading is
# `summary:` alone, and a constant in a total cancels in `Ir(2n) - Ir(n)`; what refuses a total that
# moves is the repeat at n, not this. This only tells a whole file from a damaged one.
TOTALS_SLACK = 64

# How much longer the second reading of a row is, taken when the first spreads past its bound. The
# spread is allocator placement (`run.sh`, at `PERF_ICOUNT_SPREAD`). If the offset placement puts on
# a half is bounded, as it is when the layout cycles, a half four times as long carries a quarter of
# the spread; if each iteration's placement is independent of the last, half of it. One length of
# half cannot tell those apart, so which a row follows is read off its second reading. That reading
# is 24n iterations a size and side, against the first's 7n, and only a row that misses pays it.
ESCALATION = 4


def fail(outcome, message):
  """Stop with nothing read: the reason goes to the log and, for the run's last line, to `outcome`."""
  print(f"::error::icount: {message}")
  with open(outcome, "w", encoding="utf-8") as handle:
    handle.write(f"NOT MEASURED: {message}\n")
  sys.exit(NOT_MEASURED)


def first_line(command, prefix):
  """The rest of the first line of `command`'s output that starts with `prefix`, or None.

  Lines are matched with their indentation stripped: `lscpu` nests `Model name:` under `Vendor ID:`.
  """
  try:
    done = subprocess.run(command, capture_output=True, text=True, timeout=60)
  except (OSError, subprocess.SubprocessError):
    return None
  for line in done.stdout.splitlines():
    line = line.strip()
    if line.startswith(prefix):
      return line[len(prefix):].strip()
  return None


def host_facts():
  """What the counts are a property of, besides the two binaries: printed and kept in the readings.

  The CPU model matters to an absolute count even under valgrind — the C library picks a copy
  routine by CPU feature — and not to a same-run difference, where both sides ran on this one.
  """
  cpu = first_line(["lscpu"], "Model name:")
  if not cpu:
    try:
      with open("/proc/cpuinfo", encoding="utf-8") as handle:
        for line in handle:
          if line.lower().startswith("model name"):
            cpu = line.split(":", 1)[1].strip()
            break
    except OSError:
      pass
  if not cpu:
    cpu = first_line(["sysctl", "machdep.cpu.brand_string"], "machdep.cpu.brand_string:")
  release = None
  try:
    with open("/etc/os-release", encoding="utf-8") as handle:
      for line in handle:
        if line.startswith("PRETTY_NAME="):
          release = line.split("=", 1)[1].strip().strip('"')
  except OSError:
    pass
  image = os.environ.get("PERF_RUNNER_IMAGE") or "local"
  if os.environ.get("ImageVersion"):
    image = f"{image} (image {os.environ['ImageVersion']})"
  return {
    "image": image,
    "os": release or platform.platform(),
    "arch": platform.machine(),
    "cpu": cpu or "unknown",
    "valgrind": " ".join(
      [first_line(["valgrind", "--version"], "") or "unknown"] + HOST_OPTIONS
    ),
  }


def list_rows(binary, side, outcome):
  """`{name: (family, lo, hi)}`, as the binary's own `--list` prints them."""
  try:
    done = subprocess.run([binary, "--list"], capture_output=True, text=True, timeout=120)
  except (OSError, subprocess.SubprocessError) as error:
    fail(outcome, f"the {side} side's wall_clock could not list its rows: {error}")
  if done.returncode != 0:
    tail = done.stderr.strip().splitlines()[-1:] or ["(no output)"]
    fail(
      outcome,
      f"the {side} side's wall_clock could not list its rows: exit {done.returncode}, {tail[0]}",
    )
  rows = {}
  for line in done.stdout.splitlines():
    fields = line.split()
    if len(fields) != 4 or not fields[2].isdigit() or not fields[3].isdigit():
      fail(outcome, f"the {side} side's wall_clock listed a row this cannot read: `{line}`")
    rows[fields[0]] = (fields[1], int(fields[2]), int(fields[3]))
  if not rows:
    fail(outcome, f"the {side} side's wall_clock listed no rows")
  return rows


def parse_callgrind(path):
  """`{event: total}` for one callgrind run: its `summary:` line, columns named by `events:`.

  Reading the columns off `events:` keeps this right whichever events the run collected. The file
  must be a whole one: one `events:`, one `summary:` and one `totals:` line, every number a number,
  no more of them than there are events (fewer are trailing zeros, as in callgrind's cost lines),
  and `totals:` within `TOTALS_SLACK` of `summary:` on every event. `totals:` is the last line
  callgrind writes, so a file that has none was cut short.
  """
  events, lines = None, {"summary": [], "totals": []}
  with open(path, encoding="utf-8", errors="replace") as handle:
    for line in handle:
      key = line.split(":", 1)[0]
      if key == "events":
        if events is not None:
          raise ValueError("more than one `events:` line")
        events = line.split(":", 1)[1].split()
      elif key in lines:
        lines[key].append([int(field) for field in line.split(":", 1)[1].split()])
  if events is None:
    raise ValueError("no `events:` line; callgrind's output format has changed")
  for key, found in lines.items():
    if len(found) != 1:
      cut = " — callgrind writes it last, so the file was cut short" if key == "totals" else ""
      raise ValueError(f"{len(found)} `{key}:` lines, not one{cut}")
    if len(found[0]) > len(events):
      raise ValueError(f"`{key}:` has {len(found[0])} numbers for {len(events)} events")
  def by_event(numbers):
    return dict(zip(events, numbers + [0] * (len(events) - len(numbers))))

  summary, totals = by_event(lines["summary"][0]), by_event(lines["totals"][0])
  apart = {
    event: totals[event] - summary[event]
    for event in events
    if abs(totals[event] - summary[event]) > TOTALS_SLACK
  }
  if apart:
    raise ValueError(
      f"`totals:` stands from `summary:` by {apart}, past {TOTALS_SLACK} an event: summary "
      f"{[summary[event] for event in events]}, totals {[totals[event] for event in events]}"
    )
  if GATED not in summary:
    raise ValueError(f"`events: {' '.join(events)}` has no {GATED}")
  return summary


def run_once(task, scratch, timeout, data, width):
  """One callgrind run; `{"totals": {...}}` or `{"error": "..."}`, and how long it took."""
  side, binary, name, size, n, take = task
  # Every output path of one (row, size) has the same length, on both sides and for the repeat,
  # so nothing valgrind lays out from its own arguments differs between the runs compared.
  iterations = f"{n:0{width}d}"
  out = os.path.join(scratch, f"{side}-{size}-{iterations}-{take}.{name}.out")
  command = [
    "valgrind",
    "--tool=callgrind",
    f"--callgrind-out-file={out}",
    f"--cache-sim={'yes' if data else 'no'}",
    "--branch-sim=no",
    "--collect-systime=no",
    *HOST_OPTIONS,
    binary,
    "--only", name,
    "--size", size,
    "--iterations", iterations,
  ]
  started = time.monotonic()
  try:
    done = subprocess.run(command, capture_output=True, text=True, timeout=timeout)
  except subprocess.TimeoutExpired:
    return {"error": f"callgrind did not finish inside {timeout:g} s at n={n}"}, timeout
  except OSError as error:
    return {"error": f"callgrind could not start at n={n}: {error}"}, 0.0
  elapsed = time.monotonic() - started
  try:
    if done.returncode != 0:
      tail = done.stderr.strip().splitlines()[-3:] or ["(no output)"]
      return {
        "error": f"callgrind exited {done.returncode} at n={n}: {' | '.join(tail)}"
      }, elapsed
    lines = done.stdout.strip().splitlines()
    said = lines[-1] if lines else ""
    if said != f"{name} {size} {n}":
      return {"error": f"the run at n={n} said `{said}`, not `{name} {size} {n}`"}, elapsed
    try:
      return {"totals": parse_callgrind(out)}, elapsed
    except (OSError, ValueError) as error:
      return {"error": f"callgrind's output at n={n} could not be read: {error}"}, elapsed
  finally:
    if os.path.exists(out):
      os.remove(out)


def per_iteration(event, runs, n):
  """`(cost, spread, None)` for one iteration in `event`, or `(None, None, why)` when there is none.

  `runs` holds the totals at n, 2n and 3n, and the repeat at n after them when there is one. The
  cost is read over the longest baseline, iterations n+1 to 3n. The spread is how far apart its two
  halves read, as a per cent of the cost of one half.
  """
  first, double, triple = (run[event] for run in runs[:3])
  again = runs[3][event] if len(runs) > 3 else first
  if first != again:
    return None, None, (
      f"two runs at n={n} read {first:,} and {again:,} {event}: the count is not deterministic"
    )
  early, late = double - first, triple - double
  if early <= 0 or late <= 0:
    return None, None, (
      f"{event} did not grow with n: {first:,}/{double:,}/{triple:,} at n={n}/{2 * n}/{3 * n}"
    )
  half = (triple - first) / 2
  return (triple - first) / (2 * n), abs(late - early) / half * 100.0, None


def run_all(tasks, scratch, timeout, data, width, jobs, label):
  """Every task's result, keyed by the task, the longest runs started first."""
  tasks = sorted(tasks, key=lambda task: (-task[4], task[3] != "hi"))
  results = {}
  started = time.monotonic()
  with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
    futures = {
      pool.submit(run_once, task, scratch, timeout, data, width): task for task in tasks
    }
    for done, future in enumerate(concurrent.futures.as_completed(futures), 1):
      task = futures[future]
      result, elapsed = future.result()
      results[task] = result
      side, _, name, size, point, _ = task
      state = "failed" if "error" in result else "ok"
      print(
        f"icount: {label}[{done}/{len(tasks)}] {side} {name} {size} n={point}: {state} in "
        f"{elapsed:.1f} s"
      )
  minutes, seconds = divmod(int(time.monotonic() - started), 60)
  print(f"icount: {label}{len(tasks)} callgrind runs in {minutes}m{seconds:02d}s, {jobs} at a time")
  print()
  return results


def read_row(results, side, binary, name, row, points, data, label):
  """`row` with its readings at both sizes, from the runs at `points`, printed as it goes.

  `points` is n, 2n and 3n, with the repeat at n after them on the first reading.
  """
  n = points[0]
  problems = []
  for size in ("lo", "hi"):
    runs = [results[(side, binary, name, size, point, take)] for take, point in enumerate(points)]
    broken = [run["error"] for run in runs if "error" in run]
    reading, spread, problem = (None, None, broken[0]) if broken else per_iteration(
      GATED, [run["totals"] for run in runs], n
    )
    row[size] = reading
    row[f"{size}_spread"] = spread
    if problem:
      problems.append(f"{side} side, {size}: {problem}")
    shown = "NOT MEASURED" if reading is None else f"{reading:,.1f} Ir/iter ({spread:.4f}%)"
    for event in ADVISORY:
      key = f"{size}_{event.lower()}"
      value, apart, note = None, None, None
      if not broken and data:
        if all(event in run["totals"] for run in runs):
          value, apart, note = per_iteration(event, [run["totals"] for run in runs], n)
        else:
          note = f"callgrind recorded no {event}"
      row[key] = value
      row[f"{key}_spread"] = apart
      row.pop(f"{key}_note", None)
      if note:
        row[f"{key}_note"] = note
        # Said once for the row when the gated reading failed too; then this adds nothing.
        if problem is None:
          print(f"::warning::icount {side} side: {name} at {size}, {event} not shown: {note}")
      elif value is not None:
        shown += f"  {event} {value:,.1f} ({apart:.4f}%)"
    row.pop(f"{size}_totals", None)
    if not broken:
      # The raw totals at each point, for the events this reads. The miss counts callgrind also
      # records depend on the cache it simulates and are not kept.
      row[f"{size}_totals"] = {
        event: [run["totals"].get(event) for run in runs]
        for event in (GATED,) + ADVISORY
        if event in runs[0]["totals"]
      }
    print(f"icount {side}: {label}{name:<28} {size} n={n}  {shown}")
    if problem:
      print(f"::error::icount {side} side: {name} at {size} is NOT MEASURED: {problem}")
  row["n"] = n
  row["not_measured"] = "; ".join(problems) or None
  return row


def escalations(workloads, bound_of):
  """The workloads to read again at a longer baseline: a reading on both sides whose spread, on
  either side at either size, is past `bound_of(name)`."""
  again = []
  for name in workloads["head"]:
    sides = [workloads[side][name] for side in ("head", "base")]
    if any(row["not_measured"] for row in sides):
      continue
    if any(
      (row.get(f"{size}_spread") or 0.0) > bound_of(name) for row in sides for size in ("lo", "hi")
    ):
      again.append(name)
  return again


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("--head", required=True, help="the head side's wall_clock")
  parser.add_argument("--base", required=True, help="the base side's wall_clock")
  parser.add_argument("--out", required=True, help="directory for head.json and base.json")
  parser.add_argument("--outcome", required=True, help="where a reason to stop is written")
  parser.add_argument("--iterations", type=int, default=10)
  parser.add_argument("--timeout", type=float, default=600.0, help="seconds, per callgrind run")
  parser.add_argument("--jobs", type=int, default=0, help="parallel runs; 0 is one per CPU")
  parser.add_argument("--data", choices=("yes", "no"), default="yes")
  # The bound a reading's spread is held to, `compare.py`'s own: a row past it is read again.
  parser.add_argument("--threshold", type=float, help="the gate's line, in per cent")
  parser.add_argument("--j1-rows", default="", help="comma-separated workloads with J1's verdict")
  parser.add_argument("--j1-threshold", type=float, help="J1's allowance, in per cent")
  parser.add_argument("--spread-share", type=float, help="the share of its finest line a reading may spread")
  args = parser.parse_args()

  n = args.iterations
  # n is past the warm-up: the allocator's bins and `Scratch` grow over the first iterations and
  # are reused after, and a difference that straddles that is not on the line it assumes.
  if n < 8:
    parser.error(f"--iterations {n}: n must be at least 8")
  if args.timeout <= 0:
    parser.error("--timeout must be positive")
  j1_rows = [name for name in args.j1_rows.split(",") if name]
  if args.spread_share is not None and (
    args.threshold is None or (j1_rows and args.j1_threshold is None)
  ):
    parser.error("--spread-share needs --threshold, and --j1-threshold with --j1-rows")
  data = args.data == "yes"
  jobs = args.jobs if args.jobs > 0 else (os.cpu_count() or 1)
  # Every run's `--iterations` is written at one width, zero-padded, the longest reading's 3n
  # included, so argv — and the stack laid out from it — is the same length in every run.
  width = len(str(3 * n * ESCALATION))

  if shutil.which("valgrind") is None:
    fail(args.outcome, "valgrind is not installed on this host")
  resolved = {
    side: os.path.realpath(path) for side, path in (("head", args.head), ("base", args.base))
  }
  print(f"icount: head wall_clock {resolved['head']}")
  print(f"icount: base wall_clock {resolved['base']}")
  # One file handed to both sides would read +0.000% on every row, whatever the change, and look
  # like a run of two identical builds. Two builds of an unchanged library may well be byte-equal,
  # so only the FILE is compared, never its contents.
  same = resolved["head"] == resolved["base"]
  if not same:
    try:
      same = os.path.samefile(args.head, args.base)
    except OSError:
      pass
  if same:
    fail(
      args.outcome,
      f"the head's and the base's wall_clock are one file, {resolved['head']}: a count of it "
      f"against itself reads no difference",
    )
  # The binary's path is its argv[0], and argv and the environment are laid out at the top of the
  # initial stack. Paths of one length — `run.sh` builds into `target-head` and `target-base` — put
  # every stack buffer at the same address on both sides; another length shifts them all, and a
  # copy routine that branches on alignment would then count differently for no change in the code.
  lengths = [len(os.fsencode(path)) for path in (args.head, args.base)]
  if lengths[0] != lengths[1]:
    fail(
      args.outcome,
      f"the two sides' wall_clock are at paths of different lengths ({lengths[0]} and "
      f"{lengths[1]} bytes), so their stacks would be laid out apart",
    )

  host = host_facts()
  print(
    f"icount: {host['image']}: {host['os']}, {host['arch']}, CPU {host['cpu']}, "
    f"{host['valgrind']}"
  )
  print(
    f"icount: each row and size on each side at n = {n}, {2 * n} and {3 * n}, and at {n} again; "
    + ("Ir gated, Dr and Dw read beside it" if data else "instructions only")
    + f"; {jobs} runs at a time"
  )

  binaries = {"head": args.head, "base": args.base}
  rows = {side: list_rows(binary, side, args.outcome) for side, binary in binaries.items()}
  if rows["head"] != rows["base"]:
    only = sorted(set(rows["head"].items()) ^ set(rows["base"].items()))
    fail(args.outcome, f"the two sides' wall_clock list different rows: {only}")
  rows = rows["head"]

  os.makedirs(args.out, exist_ok=True)
  scratch = os.path.join(args.out, "runs")
  os.makedirs(scratch, exist_ok=True)

  points = (n, 2 * n, 3 * n, n)
  results = run_all(
    [
      (side, binaries[side], name, size, point, take)
      for side in binaries
      for name in rows
      for size in ("lo", "hi")
      for take, point in enumerate(points)
    ],
    scratch, args.timeout, data, width, jobs, "",
  )
  workloads = {
    side: {
      name: read_row(
        results, side, binary, name,
        {"family": family, "lo_size": lo_size, "hi_size": hi_size}, points, data, "",
      )
      for name, (family, lo_size, hi_size) in rows.items()
    }
    for side, binary in binaries.items()
  }

  # The rows whose reading spreads past its bound, read again on both sides at a longer baseline.
  # The repeat at n has already shown each of them deterministic, so the longer reading takes none.
  again = []
  if args.spread_share is not None:
    again = escalations(
      workloads,
      lambda name: spread_bound(
        name, args.threshold, j1_rows, args.j1_threshold, args.spread_share
      )[0],
    )
  if again:
    longer = ESCALATION * n
    print(
      f"icount: read again at n = {longer}, {2 * longer} and {3 * longer}, on both sides, for the "
      f"spread past their bound: {', '.join(again)}"
    )
    results = run_all(
      [
        (side, binaries[side], name, size, point, take)
        for side in binaries
        for name in again
        for size in ("lo", "hi")
        for take, point in enumerate((longer, 2 * longer, 3 * longer))
      ],
      scratch, args.timeout * ESCALATION, data, width, jobs, "again ",
    )
    for side, binary in binaries.items():
      for name in again:
        first = workloads[side][name]
        row = read_row(
          results, side, binary, name, dict(first), (longer, 2 * longer, 3 * longer), data,
          "again ",
        )
        row["first"] = {
          "n": n,
          "spread": max(first["lo_spread"], first["hi_spread"]),
          "lo_spread": first["lo_spread"],
          "hi_spread": first["hi_spread"],
          "lo_totals": first.get("lo_totals"),
          "hi_totals": first.get("hi_totals"),
        }
        workloads[side][name] = row
  shutil.rmtree(scratch, ignore_errors=True)

  for side in binaries:
    document = {
      "instrument": "icount",
      "unit": "Ir/iter",
      "iterations": n,
      "points": [n, 2 * n, 3 * n],
      "escalation": ESCALATION,
      "data": data,
      "host": host,
      "workloads": workloads[side],
    }
    # Unsorted, so the rows keep the order `--list` gave them and the table groups them by family
    # as the other two instruments' tables do.
    with open(os.path.join(args.out, f"{side}.json"), "w", encoding="utf-8") as handle:
      json.dump(document, handle, indent=2)
      handle.write("\n")
  return 0


if __name__ == "__main__":
  sys.exit(main())

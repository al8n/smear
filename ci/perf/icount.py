#!/usr/bin/env python3
"""Count the instructions every perf workload executes, on both sides of one comparison.

`ci/perf/run.sh` builds the head's `wall_clock` and the base's, and this runs each one in its count
mode, `--only <name> --size lo|hi --iterations <n>` (see `smear/benches/solo/wall_clock.rs`), under
callgrind. A reading is a DIFFERENCE of two totals of one binary at two iteration counts:

    Ir(2n) - Ir(n)  =  n x the measured region's cost per iteration

Nothing inside the binary is toggled, marked or timed. Every once-per-process cost — the loader,
`main`, `warm_up`, building and parsing the corpus, the allocator's first arena — is in both totals
and cancels exactly, so what is left is the region and nothing else.

Each row is read at both of its sizes, in `compare.py`'s own schema, so the count's per-doubling
ratio is gated by the code that gates the other two instruments' ratios.

WHAT A ROW HAS TO SHOW BEFORE IT IS A READING. Each (side, row, size) is run four times: at n, 2n and
3n, and at n again. A row is NOT MEASURED, with the side and the reason in the readings, when:

  * a run exits non-zero, or does not finish inside `--timeout`, or its last line is not the
    `<name> <lo|hi> <n>` it was asked for;
  * the two runs at n differ in any event: a count that moves between two runs of one binary is
    not a property of the binary, and no threshold over it means anything;
  * Ir does not grow from n to 2n;
  * the second difference, `(Ir(3n) - Ir(2n)) - (Ir(2n) - Ir(n))`, is over `LINEARITY` of the
    reading: the differencing assumes a straight line, and this is the check that it is one.

A row NOT MEASURED stays in the readings, so `compare.py` puts it in the table and fails the stage
rather than the row going missing from the gate.

Usage:
    icount.py --head <bin> --base <bin> --out <dir> --outcome <file>
              [--iterations N] [--timeout SECONDS] [--jobs J] [--data yes|no]

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

NOT_MEASURED = 3

# The second difference a row may show, as a fraction of its reading. The three runs' argv are the
# same length (see `--iterations`) and nothing else differs between them, so the residue should be
# zero; this is room for an allocator that settles a few instructions differently after a few more
# iterations, two orders of magnitude under the 1% threshold. A cost that grows with n is far
# larger, and a per-process difference is caught exactly by the repeat at n.
LINEARITY = 1e-4

# The events read off each run. Ir is gated. Dr and Dw — data reads and writes — need callgrind's
# cache simulator (`--data yes`); they are counts of accesses, which do not depend on the simulated
# cache's geometry, only the miss counts do, and `compare.py` shows them with no verdict.
GATED = "Ir"
ADVISORY = ("Dr", "Dw")


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
    "valgrind": first_line(["valgrind", "--version"], "") or "unknown",
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
  """`{event: total}` for every event a callgrind output file records.

  The `events:` line names the columns and `summary:` and `totals:` carry one number for each.
  Reading the columns off `events:` keeps this right whichever events the run collected, and the
  two lines must agree: they disagree only for a run that was not one thread.
  """
  events = None
  rows = []
  with open(path, encoding="utf-8", errors="replace") as handle:
    for line in handle:
      if line.startswith("events:"):
        events = line[len("events:"):].split()
      elif line.startswith(("summary:", "totals:")):
        rows.append(tuple(int(field) for field in line.split(":", 1)[1].split()))
  if events is None or not rows:
    raise ValueError("no `events:` or no `summary:` line; callgrind's output format has changed")
  if len(set(rows)) != 1:
    raise ValueError(f"`summary:` and `totals:` disagree: {sorted(set(rows))}")
  totals = dict(zip(events, rows[0]))
  if GATED not in totals:
    raise ValueError(f"`events: {' '.join(events)}` has no {GATED}")
  return totals


def run_once(task, scratch, timeout, data):
  """One callgrind run; `{"totals": {...}}` or `{"error": "..."}`, and how long it took."""
  side, binary, name, size, n, take = task
  # Every output path of one (row, size) has the same length, on both sides and for the repeat,
  # so nothing valgrind lays out from its own arguments differs between the runs compared.
  out = os.path.join(scratch, f"{side}-{size}-{n}-{take}.{name}.out")
  command = [
    "valgrind",
    "--tool=callgrind",
    f"--callgrind-out-file={out}",
    f"--cache-sim={'yes' if data else 'no'}",
    "--branch-sim=no",
    "--collect-systime=no",
    binary,
    "--only", name,
    "--size", size,
    "--iterations", str(n),
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
  """The cost of one iteration in `event`, from four runs, and why it is not one when it is not.

  `runs` holds the totals at n, 2n and 3n and the repeat at n, in that order.
  """
  first, double, triple, again = (run[event] for run in runs)
  if first != again:
    return None, (
      f"two runs at n={n} read {first:,} and {again:,} {event}: the count is not deterministic"
    )
  delta = double - first
  if delta <= 0:
    return None, f"{event} did not grow from n={n} to n={2 * n} ({first:,} to {double:,})"
  second = abs((triple - double) - delta)
  limit = max(1, int(delta * LINEARITY))
  if second > limit:
    return None, (
      f"the cost per iteration is not constant: {event} at n={n}/{2 * n}/{3 * n} is "
      f"{first:,}/{double:,}/{triple:,}, a second difference of {second:,} against a tolerance "
      f"of {limit:,}"
    )
  return delta / n, None


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
  args = parser.parse_args()

  n = args.iterations
  # n is past the warm-up — the allocator's bins and `Scratch` grow over the first iterations and
  # are reused after, and a difference that straddles that is not on the line it assumes — and n,
  # 2n and 3n have one digit count, so the three runs' argv are the same length.
  if n < 8 or not len(str(n)) == len(str(2 * n)) == len(str(3 * n)):
    parser.error(f"--iterations {n}: n must be at least 8, and n, 2n and 3n one digit count")
  if args.timeout <= 0:
    parser.error("--timeout must be positive")
  data = args.data == "yes"
  jobs = args.jobs if args.jobs > 0 else (os.cpu_count() or 1)

  if shutil.which("valgrind") is None:
    fail(args.outcome, "valgrind is not installed on this host")

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
  tasks = [
    (side, binaries[side], name, size, point, take)
    for side in binaries
    for name in rows
    for size in ("lo", "hi")
    for take, point in enumerate(points)
  ]
  # The longest runs first, so the last few to finish are the short ones.
  tasks.sort(key=lambda task: (-task[4], task[3] != "hi"))

  results = {}
  started = time.monotonic()
  with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
    futures = {pool.submit(run_once, task, scratch, args.timeout, data): task for task in tasks}
    for done, future in enumerate(concurrent.futures.as_completed(futures), 1):
      task = futures[future]
      result, elapsed = future.result()
      results[task] = result
      side, _, name, size, point, _ = task
      state = "failed" if "error" in result else "ok"
      print(
        f"icount: [{done}/{len(tasks)}] {side} {name} {size} n={point}: {state} in {elapsed:.1f} s"
      )
  shutil.rmtree(scratch, ignore_errors=True)
  minutes, seconds = divmod(int(time.monotonic() - started), 60)
  print(f"icount: {len(tasks)} callgrind runs in {minutes}m{seconds:02d}s, {jobs} at a time")
  print()

  for side, binary in binaries.items():
    workloads = {}
    for name, (family, lo_size, hi_size) in rows.items():
      row = {"family": family, "lo_size": lo_size, "hi_size": hi_size}
      problems = []
      for size in ("lo", "hi"):
        runs = [results[(side, binary, name, size, point, take)] for take, point in enumerate(points)]
        broken = [run["error"] for run in runs if "error" in run]
        reading, problem = (None, broken[0]) if broken else per_iteration(
          GATED, [run["totals"] for run in runs], n
        )
        row[size] = reading
        if problem:
          problems.append(f"{side} side, {size}: {problem}")
        shown = "NOT MEASURED" if reading is None else f"{reading:,.1f} Ir/iter"
        for event in ADVISORY:
          key = f"{size}_{event.lower()}"
          value, note = None, None
          if not broken and data:
            if all(event in run["totals"] for run in runs):
              value, note = per_iteration(event, [run["totals"] for run in runs], n)
            else:
              note = f"callgrind recorded no {event}"
          row[key] = value
          if note:
            row[f"{key}_note"] = note
            # Said once for the row when the gated reading failed too; then this adds nothing.
            if problem is None:
              print(f"::warning::icount {side} side: {name} at {size}, {event} not shown: {note}")
          elif value is not None:
            shown += f"  {event} {value:,.1f}"
        if not broken:
          # The raw totals at n, 2n, 3n and n again, for the events this reads. The miss counts
          # callgrind also records depend on the cache it simulates and are not kept.
          row[f"{size}_totals"] = {
            event: [run["totals"].get(event) for run in runs]
            for event in (GATED,) + ADVISORY
            if event in runs[0]["totals"]
          }
        print(f"icount {side}: {name:<28} {size}  {shown}")
        if problem:
          print(f"::error::icount {side} side: {name} at {size} is NOT MEASURED: {problem}")
      row["not_measured"] = "; ".join(problems) or None
      workloads[name] = row

    document = {
      "instrument": "icount",
      "unit": "Ir/iter",
      "iterations": n,
      "points": [n, 2 * n, 3 * n],
      "data": data,
      "linearity": LINEARITY,
      "host": host,
      "workloads": workloads,
    }
    with open(os.path.join(args.out, f"{side}.json"), "w", encoding="utf-8") as handle:
      json.dump(document, handle, indent=2, sort_keys=True)
      handle.write("\n")
  return 0


if __name__ == "__main__":
  sys.exit(main())

#!/usr/bin/env python3
"""Compare two perf readings and decide whether the head side regressed.

Both readings come from the SAME CI run, the SAME runner and the SAME toolchain — see
`ci/perf/run.sh` for why that is the whole design and not an implementation detail.

Two things are gated, per workload, and they catch different failures:

  * **the absolute**, at each of the workload's two sizes, which catches a constant-factor
    regression — a copy put back on a path that runs per item — with the law unchanged;
  * **the ratio** `hi / lo`, which catches a change of LAW with the absolute still small. A
    workload that read 1.97 per doubling and now reads 3.9 has gone from linear to quadratic, and
    at the sizes this corpus uses that can happen while the absolute is a few hundred kilobytes.

Neither subsumes the other, so a workload fails on either.

Three instruments are read: `alloc`, peak bytes; `wall`, nanoseconds per iteration; and `icount`,
instructions per iteration as `ci/perf/icount.py` counts them. The count brings five things the
other two do not have:

  * a workload `icount.py` could not read is in its readings with the reason, and reads NOT
    MEASURED here: it fails the stage, in the table, and is never dropped from it;
  * with `--spread-share`, so does a workload whose reading spreads, on either side at either
    size, past that share of the finest line it is read against — the threshold, or J1's for a J1
    row — because a reading that uncertain cannot say which side of the line a change fell;
  * the two sides must have been read at the same iteration counts, or nothing is compared;
  * `--j1-rows` names workloads that carry a second verdict at `--j1-threshold`, under the
    threshold, printed in the table and raised as a warning, and never shown as `ok`;
  * reads and writes, when `icount.py` collected them, in a table of their own with no verdict.

Usage:
    compare.py --kind alloc|wall|icount --base A[,A2...] --head B[,B2...] --threshold PCT
               --ratio-tolerance DELTA [--accept-file FILE] [--summary FILE] [--outcome FILE]
               [--j1-rows NAME[,NAME...] --j1-threshold PCT] [--spread-share FRACTION]

Several files per side are reduced with MIN per reading. That is meaningful for the wall-clock
instrument, where `run.sh` interleaves whole invocations and noise is one-directional; for the
allocation instrument there is only ever one file per side, because there is nothing to reduce,
and for the count there must be one.

`--outcome` receives one line, this stage's part of the run's last line: what it found, or why it
compared nothing. Exit 0 is no workload over its threshold; 1 is a workload over it, a workload
NOT MEASURED, or a refusal to compare at all. Python 3.9, like the rest of `ci/perf/`.
"""

import argparse
import json
import os
import re
import sys

# `Perf-accept: [icount/]<workload> +<pct>% <reason>` — `tokora`'s spelling, deliberately shared
# rather than reinvented, so one convention covers both repositories.
#
# An acceptance is SCOPED to an instrument. A bare name licenses the allocation and wall-clock gates,
# which read the same workloads, as it always has; `icount/<name>` licenses the count and nothing
# else. Unscoped, a trailer written to clear the clock's 12% would license a count twelve times over
# its own 1%, and print the clock's reason beside it as if the count had been argued for.
ACCEPT_RE = re.compile(
  r"^\s*Perf-accept:\s*(?:(?P<scope>icount)/)?(?P<workload>[A-Za-z0-9_]+)\s*"
  r"\+?(?P<pct>[0-9]+(?:\.[0-9]+)?)\s*%\s*(?P<reason>\S.*?)\s*$"
)

UNITS = {"alloc": "bytes", "wall": "ns/iter", "icount": "Ir/iter"}

# The reads-and-writes table: each event, and the two together, which is how J1 bounds them.
DATA_COLUMNS = (("Dr", ("dr",)), ("Dw", ("dw",)), ("Dr+Dw", ("dr", "dw")))

# Where `refuse` writes this stage's part of the run's last line; set from `--outcome`.
OUTCOME = None


def write_outcome(text):
  if OUTCOME:
    with open(OUTCOME, "w", encoding="utf-8") as handle:
      handle.write(text + "\n")


def refuse(message):
  """Stop without a verdict: the reason is the error, and the first line of it is the outcome."""
  write_outcome(f"NOT MEASURED: {message.splitlines()[0]}")
  sys.exit(f"::error::{message}")


def load_min(paths):
  """Read one side, reducing several invocations to the smallest reading of each row."""
  merged = None
  for path in paths:
    with open(path, encoding="utf-8") as handle:
      document = json.load(handle)
    if merged is None:
      merged = document
      continue
    if set(merged["workloads"]) != set(document["workloads"]):
      refuse(f"{path} measured a different workload set from the first file on its side")
    for name, row in document["workloads"].items():
      into = merged["workloads"][name]
      for field in ("lo", "hi"):
        into[field] = min(into[field], row[field])
  return merged


def parse_acceptances(path, kind):
  """Read `Perf-accept:` trailers, one per line, as harvested from the branch's commits, and keep
  those scoped to `kind`'s instrument.

  An entry with no reason is REFUSED rather than ignored, whichever instrument it names. An
  acceptance is a statement that a trade was worth making, and an acceptance with nothing said
  about the trade is the shape that turns this gate into a rubber stamp.
  """
  scope = "icount" if kind == "icount" else None
  accepted = {}
  if not path or not os.path.exists(path):
    return accepted
  with open(path, encoding="utf-8") as handle:
    for lineno, line in enumerate(handle, 1):
      if not line.strip():
        continue
      match = ACCEPT_RE.match(line)
      if not match:
        refuse(
          f"{path}:{lineno}: cannot read this as an acceptance:\n"
          f"    {line.strip()}\n"
          "  expected: Perf-accept: [icount/]<workload> +<pct>% <why it is worth it>"
        )
      if match["scope"] != scope:
        continue
      name, pct, reason = match["workload"], float(match["pct"]), match["reason"]
      # The widest ceiling wins when a branch carries more than one for a workload: the later
      # commit is the one that knew what the earlier ones cost.
      if name not in accepted or pct > accepted[name][0]:
        accepted[name] = (pct, reason)
  return accepted


def percent(base, head):
  if base == 0:
    return 0.0 if head == 0 else float("inf")
  return (head - base) / base * 100.0


def ratio(row):
  return row["hi"] / row["lo"] if row["lo"] else None


def data_delta(base, head, size, keys, bound):
  """The head's change in reads, writes or both at `size`, in per cent, or None if not read.

  A reading whose spread is past `bound` on either side is not shown either: it has no verdict to
  lose, but a number that uncertain would be read as one.
  """
  values = []
  for row in (base, head):
    parts = [row.get(f"{size}_{key}") for key in keys]
    if any(part is None for part in parts):
      return None
    if bound is not None and any(
      (row.get(f"{size}_{key}_spread") or 0.0) > bound for key in keys
    ):
      return None
    values.append(sum(parts))
  return percent(values[0], values[1])


def cell(value, spec):
  return "-" if value is None else format(value, spec)


def signed(value):
  return "-" if value is None else f"{value:+.3f}%"


def share(value):
  return "-" if value is None else f"{value:.3f}%"


def finest_line(name, args, j1_rows):
  """The smallest change a workload is read against: J1's allowance for a J1 row, else the gate's."""
  return min(args.threshold, args.j1_threshold) if name in j1_rows else args.threshold


def count_heading(head, args, j1_rows):
  """The count table's heading: what it was read on, how, and what its threshold rests on."""
  host = head.get("host", {})
  n = head["iterations"]
  collected = (
    "Ir is gated, and reads and writes are shown below it with no verdict."
    if head.get("data")
    else "**Instructions only**: reads and writes were not collected."
  )
  lines = [
    f"### instruction counts, {host.get('image', 'unknown image')}",
    "",
    f"{host.get('os', '?')}, {host.get('arch', '?')}, CPU {host.get('cpu', '?')}, "
    f"{host.get('valgrind', '?')}. Every workload and size was read at n = {n}, {2 * n} and "
    f"{3 * n} iterations on both sides, and at {n} again, and each reading is the cost over "
    f"iterations {n + 1} to {3 * n}. {collected}",
    "",
  ]
  if args.spread_share is not None:
    finest = [args.threshold * args.spread_share]
    if j1_rows:
      finest.append(min(args.threshold, args.j1_threshold) * args.spread_share)
    lines.append(
      "A reading's **spread** is how far apart the two halves of those iterations read. It may be "
      f"at most {args.spread_share:.0%} of the finest line its workload is read against — "
      + f"{finest[0]:g}% for the +{args.threshold:g}% line"
      + (f", {finest[1]:g}% for J1's rows" if j1_rows else "")
      + " — and a workload past that on either side, at either size, is NOT MEASURED."
    )
    lines.append("")
  if host.get("arch") in ("x86_64", "amd64", "AMD64"):
    lines.append(
      f"The +{args.threshold:g}% line rests on how far a row moves between two builds whose "
      "source it did not change, measured on GitHub's x86 runners (`ci/perf/run.sh` gives the "
      "sample). Under valgrind the lexing in every row runs memspan's AVX2 kernels: valgrind "
      "offers no AVX-512, which the clock's runs may take."
    )
  else:
    lines.append(
      f"**This image has no drift sample.** The +{args.threshold:g}% line rests on how far a "
      "row moves between two builds whose source it did not change, and that was measured on "
      "x86 alone (`ci/perf/run.sh` gives the sample)."
    )
  if j1_rows:
    named = ", ".join(f"`{name}`" for name in j1_rows)
    lines.append("")
    lines.append(
      f"J1's +{args.j1_threshold:g}% is a second verdict on {named}: shown in the table and "
      "raised as a warning, never as `ok`, and it fails nothing."
    )
  return "\n".join(lines) + "\n\n"


def main():
  global OUTCOME
  parser = argparse.ArgumentParser()
  parser.add_argument("--kind", choices=sorted(UNITS), required=True)
  parser.add_argument("--base", required=True, help="comma-separated JSON readings")
  parser.add_argument("--head", required=True, help="comma-separated JSON readings")
  parser.add_argument("--threshold", type=float, required=True)
  parser.add_argument("--ratio-tolerance", type=float, required=True)
  parser.add_argument("--accept-file")
  parser.add_argument("--summary", help="append a markdown table here (GITHUB_STEP_SUMMARY)")
  parser.add_argument("--outcome", help="write this stage's part of the run's last line here")
  parser.add_argument("--j1-rows", default="", help="comma-separated workloads with J1's verdict")
  parser.add_argument("--j1-threshold", type=float, help="J1's allowance, in per cent")
  parser.add_argument(
    "--spread-share", type=float, help="the share of its finest line a reading may spread"
  )
  args = parser.parse_args()
  OUTCOME = args.outcome
  counting = args.kind == "icount"
  j1_rows = [name for name in args.j1_rows.split(",") if name]
  if j1_rows and args.j1_threshold is None:
    parser.error("--j1-rows needs --j1-threshold")
  if args.spread_share is not None and not (counting and 0 < args.spread_share <= 1):
    parser.error("--spread-share is a fraction in (0, 1], and a count's alone")
  # The bound the reads-and-writes table holds its cells to: the gate's, since it carries no J1.
  data_bound = args.threshold * args.spread_share if args.spread_share is not None else None

  if counting and ("," in args.base or "," in args.head):
    refuse("a count is read once on each side, and there is nothing to reduce")
  base = load_min(args.base.split(","))
  head = load_min(args.head.split(","))
  unit = UNITS[args.kind]

  # The two sides must have been read the same way, or the difference is not a difference.
  if base["instrument"] != head["instrument"]:
    refuse(
      f"the two sides were produced by different instruments "
      f"({base['instrument']} vs {head['instrument']})"
    )
  if counting:
    for field in ("iterations", "points"):
      if base.get(field) != head.get(field):
        refuse(
          f"the two sides were read at different {field} ({base.get(field)} vs "
          f"{head.get(field)}), so their counts are not one difference apart"
        )
  if set(base["workloads"]) != set(head["workloads"]):
    only_base = sorted(set(base["workloads"]) - set(head["workloads"]))
    only_head = sorted(set(head["workloads"]) - set(base["workloads"]))
    refuse(
      "the two sides measured different workloads — the instrument is not the same on "
      f"both.\n  only in base: {only_base}\n  only in head: {only_head}"
    )
  for name in head["workloads"]:
    for field in ("lo_size", "hi_size"):
      if base["workloads"][name][field] != head["workloads"][name][field]:
        refuse(
          f"`{name}` was read at different {field} on the two sides "
          f"({base['workloads'][name][field]} vs {head['workloads'][name][field]}) — the ratio "
          "is not a comparison"
        )
  # A name here that the readings do not hold would take J1's verdict away in silence the day the
  # row is renamed: a warning that cannot fire looks exactly like one with nothing to say.
  missing = [name for name in j1_rows if name not in head["workloads"]]
  if missing:
    refuse(f"--j1-rows names {missing}, which the readings do not hold")
  data = counting and bool(base.get("data")) and bool(head.get("data"))

  accepted = parse_acceptances(args.accept_file, args.kind)

  rows = []
  for name, h in head["workloads"].items():
    b = base["workloads"][name]
    unmeasured = "; ".join(why for why in (b.get("not_measured"), h.get("not_measured")) if why)
    if not unmeasured and None in (b["lo"], b["hi"], h["lo"], h["hi"]):
      unmeasured = "a side's readings hold no number for it"
    spreads = [
      (side, size, row[f"{size}_spread"])
      for side, row in (("base", b), ("head", h))
      for size in ("lo", "hi")
      if row.get(f"{size}_spread") is not None
    ]
    spread = max((value for _, _, value in spreads), default=None)
    # A reading whose two halves disagree by more than its share of the line cannot say which side
    # of that line a change fell, so it is no reading for this gate, on either side.
    if not unmeasured and args.spread_share is not None:
      line = finest_line(name, args, j1_rows)
      bound = line * args.spread_share
      unmeasured = "; ".join(
        f"{side} side, {size}: its halves read {value:.3f}% apart, past the {bound:g}% a reading "
        f"against the +{line:g}% line may spread"
        for side, size, value in spreads
        if value > bound
      )
    if unmeasured:
      rows.append(
        {
          "name": name, "family": h["family"], "measured": False,
          "base_lo": None, "head_lo": None, "lo_pct": None,
          "base_hi": None, "head_hi": None, "hi_pct": None,
          "worst": None, "base_ratio": None, "head_ratio": None, "ratio_delta": None,
          "verdict": "NOT MEASURED", "reason": unmeasured, "j1": False, "data": {},
          "spread": spread,
        }
      )
      continue
    lo_pct, hi_pct = percent(b["lo"], h["lo"]), percent(b["hi"], h["hi"])
    worst = max(lo_pct, hi_pct)
    base_ratio, head_ratio = ratio(b), ratio(h)
    ratio_delta = (
      head_ratio - base_ratio if base_ratio is not None and head_ratio is not None else None
    )

    ceiling, reason = accepted.get(name, (None, None))
    over_abs = worst > args.threshold
    over_ratio = ratio_delta is not None and ratio_delta > args.ratio_tolerance

    if not over_abs and not over_ratio:
      verdict = "ok"
    elif ceiling is not None and worst <= ceiling and not over_ratio:
      verdict = f"accepted (<= +{ceiling:g}%)"
    elif over_ratio:
      verdict = "LAW CHANGED"
    else:
      verdict = "REGRESSION"

    # J1's verdict, beside the gate's and never instead of it: a row the threshold passes and J1
    # does not reads as over J1, not as `ok`. A row the threshold fails says so already.
    j1 = (
      name in j1_rows and verdict not in ("REGRESSION", "LAW CHANGED")
      and worst > args.j1_threshold
    )
    if j1:
      over = f"over J1 (+{args.j1_threshold:g}%)"
      verdict = over if verdict == "ok" else f"{verdict}; {over}"

    rows.append(
      {
        "name": name,
        "family": h["family"],
        "measured": True,
        "base_lo": b["lo"], "head_lo": h["lo"], "lo_pct": lo_pct,
        "base_hi": b["hi"], "head_hi": h["hi"], "hi_pct": hi_pct,
        "worst": worst,
        "base_ratio": base_ratio, "head_ratio": head_ratio, "ratio_delta": ratio_delta,
        "verdict": verdict, "reason": reason, "j1": j1,
        "data": {
          (label, size): data_delta(b, h, size, keys, data_bound)
          for label, keys in DATA_COLUMNS
          for size in ("lo", "hi")
        } if data else {},
        "spread": spread,
      }
    )

  # A workload NOT MEASURED first, since it is a failure with no number to rank it by.
  rows.sort(key=lambda row: (row["measured"], -row["worst"] if row["measured"] else 0.0))

  width = max(len(row["name"]) for row in rows)
  # The count's tables carry each workload's widest spread, so a reading near its bound shows it.
  spread_title = f"  {'spread':>8}" if counting else ""
  print()
  print(
    f"{'workload':<{width}}  {'lo delta':>9}  {'hi delta':>9}  "
    f"{'base hi/lo':>10}  {'head hi/lo':>10}{spread_title}  verdict"
  )
  print("-" * (width + 60 + len(spread_title)))
  for row in rows:
    spread_cell = f"  {share(row['spread']):>8}" if counting else ""
    print(
      f"{row['name']:<{width}}  {signed(row['lo_pct']):>9}  {signed(row['hi_pct']):>9}  "
      f"{cell(row['base_ratio'], '.3f'):>10}  {cell(row['head_ratio'], '.3f'):>10}{spread_cell}  "
      f"{row['verdict']}"
    )
    if row["reason"] and (row["verdict"].startswith("accepted") or not row["measured"]):
      print(f"{'':<{width}}  reason: {row['reason']}")
  print()
  print(
    f"unit {unit}; a workload fails above +{args.threshold:g}% at either size, or on a per-doubling "
    f"ratio more than {args.ratio_tolerance:g} above the base's. Both sides were built and measured "
    "in this run, on this runner, with this toolchain."
  )
  columns = [(label, size) for label, _ in DATA_COLUMNS for size in ("lo", "hi")]
  if data:
    wide = max(width, len("reads and writes"))
    print()
    print(
      f"{'reads and writes':<{wide}}  "
      + "  ".join(f"{label + ' ' + size:>10}" for label, size in columns)
      + "  (advisory, no verdict)"
    )
    for row in rows:
      values = [row["data"].get(column) for column in columns]
      print(f"{row['name']:<{wide}}  " + "  ".join(f"{signed(value):>10}" for value in values))

  failed = [row for row in rows if row["verdict"] in ("REGRESSION", "LAW CHANGED")]
  unmeasured = [row for row in rows if not row["measured"]]
  over_j1 = [row for row in rows if row["j1"]]

  # WHERE the cost is, said by the harness rather than left for the reader to work out. The parse
  # family is the control: `parse_type_system` and `schema_real` prepare the same fixtures and
  # differ only in whether `Schema::build` is inside the measured region. It places a schema or a
  # validate row and nothing else, so the note speaks of those two families alone.
  if failed:
    families = {row["family"] for row in failed} & {"schema", "validate"}
    control_moved = any(
      row["family"] == "parse" and row["measured"] and row["worst"] > args.threshold
      for row in rows
    )
    if control_moved and families - {"parse"}:
      print(
        "\nnote: the `parse` control family moved too. A change in the parser, the lexer or the "
        "input layer fits the evidence better than one in the builder or the rule engine — every "
        "schema and validate workload parses its corpus before the region opens, but they all "
        "link the same parser."
      )
    elif not control_moved and families:
      print(
        f"\nnote: the `parse` control family did not move, and {sorted(families)} did. Whatever "
        "changed is inside `Schema::build` or the rule engine behind it, not in the parser "
        "underneath them — `parse_type_system` and `schema_real` read the same fixtures and "
        "differ only in whether the builder is in the region."
      )

  for row in rows:
    if row["verdict"] == "LAW CHANGED":
      print(
        f"\nnote: `{row['name']}` changed its per-doubling ratio from {row['base_ratio']:.3f} to "
        f"{row['head_ratio']:.3f}. ~2 is linear and ~4 is quadratic, so this is a complexity "
        "change and not a constant-factor one — doubling the size again would roughly "
        f"{'square' if row['head_ratio'] > 3 else 'compound'} the difference. An acceptance "
        "trailer does not license this and is not offered for it: a `Perf-accept:` ceiling is a "
        "number, and a law that has changed has no number that stays true at the next size."
      )

  if args.summary:
    with open(args.summary, "a", encoding="utf-8") as handle:
      if counting:
        handle.write(count_heading(head, args, j1_rows))
      else:
        handle.write(f"### {head['instrument']} vs merge-base\n\n")
      handle.write(
        f"| workload | family | base lo | head lo | base hi | head hi | "
        f"lo delta | hi delta | base hi/lo | head hi/lo |{' spread |' if counting else ''} verdict |\n"
      )
      handle.write(
        "| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"
        + (" ---: |" if counting else "") + " --- |\n"
      )
      for row in rows:
        note = ""
        if row["reason"] and (row["verdict"].startswith("accepted") or not row["measured"]):
          note = f" — {row['reason']}"
        spread_cell = f"{share(row['spread'])} | " if counting else ""
        handle.write(
          f"| `{row['name']}` | {row['family']} | {cell(row['base_lo'], ',.0f')} | "
          f"{cell(row['head_lo'], ',.0f')} | {cell(row['base_hi'], ',.0f')} | "
          f"{cell(row['head_hi'], ',.0f')} | {signed(row['lo_pct'])} | {signed(row['hi_pct'])} | "
          f"{cell(row['base_ratio'], '.3f')} | {cell(row['head_ratio'], '.3f')} | "
          f"{spread_cell}{row['verdict']}{note} |\n"
        )
      handle.write(
        f"\nUnit **{unit}**. Threshold **+{args.threshold:g}%** at either size, ratio tolerance "
        f"**{args.ratio_tolerance:g}**; both sides built and measured in this run on this runner.\n\n"
      )
      if data:
        handle.write(
          "#### Reads and writes, advisory\n\nPer iteration, head against base: Dr is data "
          "reads, Dw data writes. No verdict and no threshold reads this table.\n\n"
        )
        handle.write(
          "| workload | " + " | ".join(f"{label} {size}" for label, size in columns) + " |\n"
        )
        handle.write("| --- |" + " ---: |" * len(columns) + "\n")
        for row in rows:
          values = [signed(row["data"].get(column)) for column in columns]
          handle.write(f"| `{row['name']}` | " + " | ".join(values) + " |\n")
        handle.write("\n")

  for row in over_j1:
    print(
      f"::warning::{row['name']} ({args.kind}): {row['lo_pct']:+.3f}% at n={row_size(head, row, 'lo')} "
      f"and {row['hi_pct']:+.3f}% at n={row_size(head, row, 'hi')} {unit}, over J1's "
      f"+{args.j1_threshold:g}%. The +{args.threshold:g}% line passes it and J1 does not, so it "
      "is not called ok."
    )

  if not failed and not unmeasured:
    outcome = "no workload over its threshold"
  else:
    pieces = []
    if failed:
      named = ", ".join(f"{row['name']} ({row['verdict']})" for row in failed)
      pieces.append(f"{len(failed)} over the threshold: {named}")
    if unmeasured:
      named = ", ".join(f"{row['name']} ({clip(row['reason'])})" for row in unmeasured)
      pieces.append(f"{len(unmeasured)} of {len(rows)} NOT MEASURED: {named}")
    outcome = "; ".join(pieces)
    if len(unmeasured) == len(rows):
      outcome = f"NOT MEASURED: no workload could be read; {outcome}"
  if over_j1:
    outcome += f"; over J1's +{args.j1_threshold:g}%, a warning: " + ", ".join(
      row["name"] for row in over_j1
    )
  write_outcome(outcome)

  if not failed and not unmeasured:
    print(f"perf: no workload regressed ({args.kind}).")
    return 0

  print()
  for row in unmeasured:
    print(f"::error::{row['name']} ({args.kind}): NOT MEASURED, so not compared: {row['reason']}")
  for row in failed:
    # A workload whose small size reads zero has no ratio, so the line says so rather than
    # formatting a `None`. Nothing in the current corpus does; a future one might, and a gate that
    # tracebacks while reporting a real regression reports nothing.
    if row["base_ratio"] is None or row["head_ratio"] is None:
      shape = "ratio unavailable (the small size read zero)"
    else:
      shape = f"ratio {row['base_ratio']:.3f} -> {row['head_ratio']:.3f}"
    print(
      f"::error::{row['name']} ({args.kind}): {row['base_lo']:,.0f} -> {row['head_lo']:,.0f} at "
      f"n={row_size(head, row, 'lo')} ({row['lo_pct']:+.3f}%), "
      f"{row['base_hi']:,.0f} -> {row['head_hi']:,.0f} at "
      f"n={row_size(head, row, 'hi')} ({row['hi_pct']:+.3f}%) {unit}; {shape}."
    )

  print(
    "\nTo see it locally:\n"
    + ("    PERF_ONLY=icount ci/perf/run.sh <the merge-base sha>    # Linux, with valgrind\n"
       if counting else "    ci/perf/run.sh <the merge-base sha>\n")
  )
  constant_only = [row for row in failed if row["verdict"] == "REGRESSION"]
  if constant_only:
    print(
      "If the extra cost buys something worth having, record the trade in a commit message on this "
      "branch — one trailer per workload:\n"
    )
    scope = "icount/" if counting else ""
    for row in constant_only:
      ceiling = max(1.0, round(row["worst"] + 1.0))
      print(f"    Perf-accept: {scope}{row['name']} +{ceiling:g}% <why the extra cost is worth it>")
    print(
      "\nThe trailer is read from this branch's own commits, so it expires when they are no longer "
      "this branch's: it licenses one change and does not become a standing allowance for the "
      "workload. `git commit --allow-empty` is enough to add one."
    )
    if counting:
      print(
        "It licenses the count alone: an `icount/` trailer does not reach the allocation or "
        "wall-clock gates, and theirs does not reach this one."
      )
  return 1


def row_size(document, row, size):
  return document["workloads"][row["name"]][f"{size}_size"]


def clip(text, limit=120):
  return text if len(text) <= limit else text[: limit - 1] + "…"


if __name__ == "__main__":
  sys.exit(main())

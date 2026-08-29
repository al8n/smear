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

Usage:
    compare.py --kind alloc|wall --base A[,A2...] --head B[,B2...] --threshold PCT
               --ratio-tolerance DELTA [--accept-file FILE] [--summary FILE]

Several files per side are reduced with MIN per reading. That is meaningful for the wall-clock
instrument, where `run.sh` interleaves whole invocations and noise is one-directional; for the
allocation instrument there is only ever one file per side, because there is nothing to reduce.
"""

import argparse
import json
import os
import re
import sys

# `Perf-accept: <workload> +<pct>% <reason>` — `tokora`'s spelling, deliberately shared rather than
# reinvented, so one convention covers both repositories.
ACCEPT_RE = re.compile(
  r"^\s*Perf-accept:\s*(?P<workload>[A-Za-z0-9_]+)\s*\+?(?P<pct>[0-9]+(?:\.[0-9]+)?)\s*%"
  r"\s*(?P<reason>\S.*?)\s*$"
)

UNITS = {"alloc": "bytes", "wall": "ns/iter"}


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
      sys.exit(f"::error::{path} measured a different workload set from the first file on its side")
    for name, row in document["workloads"].items():
      into = merged["workloads"][name]
      for field in ("lo", "hi"):
        into[field] = min(into[field], row[field])
  return merged


def parse_acceptances(path):
  """Read `Perf-accept:` trailers, one per line, as harvested from the branch's commits.

  An entry with no reason is REFUSED rather than ignored. An acceptance is a statement that a
  trade was worth making, and an acceptance with nothing said about the trade is the shape that
  turns this gate into a rubber stamp.
  """
  accepted = {}
  if not path or not os.path.exists(path):
    return accepted
  with open(path, encoding="utf-8") as handle:
    for lineno, line in enumerate(handle, 1):
      if not line.strip():
        continue
      match = ACCEPT_RE.match(line)
      if not match:
        sys.exit(
          f"::error::{path}:{lineno}: cannot read this as an acceptance:\n"
          f"    {line.strip()}\n"
          "  expected: Perf-accept: <workload> +<pct>% <why it is worth it>"
        )
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


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("--kind", choices=sorted(UNITS), required=True)
  parser.add_argument("--base", required=True, help="comma-separated JSON readings")
  parser.add_argument("--head", required=True, help="comma-separated JSON readings")
  parser.add_argument("--threshold", type=float, required=True)
  parser.add_argument("--ratio-tolerance", type=float, required=True)
  parser.add_argument("--accept-file")
  parser.add_argument("--summary", help="append a markdown table here (GITHUB_STEP_SUMMARY)")
  args = parser.parse_args()

  base = load_min(args.base.split(","))
  head = load_min(args.head.split(","))
  unit = UNITS[args.kind]

  # The two sides must have been read the same way, or the difference is not a difference.
  if base["instrument"] != head["instrument"]:
    sys.exit(
      f"::error::the two sides were produced by different instruments "
      f"({base['instrument']} vs {head['instrument']})"
    )
  if set(base["workloads"]) != set(head["workloads"]):
    only_base = sorted(set(base["workloads"]) - set(head["workloads"]))
    only_head = sorted(set(head["workloads"]) - set(base["workloads"]))
    sys.exit(
      "::error::the two sides measured different workloads — the instrument is not the same on "
      f"both.\n  only in base: {only_base}\n  only in head: {only_head}"
    )
  for name in head["workloads"]:
    for field in ("lo_size", "hi_size"):
      if base["workloads"][name][field] != head["workloads"][name][field]:
        sys.exit(
          f"::error::`{name}` was read at different {field} on the two sides "
          f"({base['workloads'][name][field]} vs {head['workloads'][name][field]}) — the ratio "
          "is not a comparison"
        )

  accepted = parse_acceptances(args.accept_file)

  rows = []
  for name, h in head["workloads"].items():
    b = base["workloads"][name]
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

    rows.append(
      {
        "name": name,
        "family": h["family"],
        "base_lo": b["lo"], "head_lo": h["lo"], "lo_pct": lo_pct,
        "base_hi": b["hi"], "head_hi": h["hi"], "hi_pct": hi_pct,
        "worst": worst,
        "base_ratio": base_ratio, "head_ratio": head_ratio, "ratio_delta": ratio_delta,
        "verdict": verdict, "reason": reason,
      }
    )

  rows.sort(key=lambda row: -row["worst"])

  width = max(len(row["name"]) for row in rows)
  print()
  print(
    f"{'workload':<{width}}  {'lo delta':>9}  {'hi delta':>9}  "
    f"{'base hi/lo':>10}  {'head hi/lo':>10}  verdict"
  )
  print("-" * (width + 60))
  for row in rows:
    base_ratio = "-" if row["base_ratio"] is None else f"{row['base_ratio']:.3f}"
    head_ratio = "-" if row["head_ratio"] is None else f"{row['head_ratio']:.3f}"
    print(
      f"{row['name']:<{width}}  {row['lo_pct']:>+8.3f}%  {row['hi_pct']:>+8.3f}%  "
      f"{base_ratio:>10}  {head_ratio:>10}  {row['verdict']}"
    )
    if row["reason"] and row["verdict"].startswith("accepted"):
      print(f"{'':<{width}}  reason: {row['reason']}")
  print()
  print(
    f"unit {unit}; a workload fails above +{args.threshold:g}% at either size, or on a per-doubling "
    f"ratio more than {args.ratio_tolerance:g} above the base's. Both sides were built and measured "
    "in this run, on this runner, with this toolchain."
  )

  failed = [row for row in rows if row["verdict"] in ("REGRESSION", "LAW CHANGED")]

  # WHERE the cost is, said by the harness rather than left for the reader to work out. The parse
  # family is the control: `parse_type_system` and `schema_real` prepare the same fixtures and
  # differ only in whether `Schema::build` is inside the measured region.
  if failed:
    families = {row["family"] for row in failed}
    control_moved = any(
      row["family"] == "parse" and row["worst"] > args.threshold for row in rows
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
      handle.write(f"### {head['instrument']} vs merge-base\n\n")
      handle.write(
        f"| workload | family | base lo | head lo | base hi | head hi | "
        f"lo delta | hi delta | base hi/lo | head hi/lo | verdict |\n"
      )
      handle.write("| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |\n")
      for row in rows:
        base_ratio = "-" if row["base_ratio"] is None else f"{row['base_ratio']:.3f}"
        head_ratio = "-" if row["head_ratio"] is None else f"{row['head_ratio']:.3f}"
        note = f" — {row['reason']}" if row["reason"] and row["verdict"].startswith("accepted") else ""
        handle.write(
          f"| `{row['name']}` | {row['family']} | {row['base_lo']:,.0f} | {row['head_lo']:,.0f} | "
          f"{row['base_hi']:,.0f} | {row['head_hi']:,.0f} | {row['lo_pct']:+.3f}% | "
          f"{row['hi_pct']:+.3f}% | {base_ratio} | {head_ratio} | {row['verdict']}{note} |\n"
        )
      handle.write(
        f"\nUnit **{unit}**. Threshold **+{args.threshold:g}%** at either size, ratio tolerance "
        f"**{args.ratio_tolerance:g}**; both sides built and measured in this run on this runner.\n\n"
      )

  if not failed:
    print(f"perf: no workload regressed ({args.kind}).")
    return 0

  print()
  for row in failed:
    print(
      f"::error::{row['name']} ({args.kind}): {row['base_lo']:,.0f} -> {row['head_lo']:,.0f} at "
      f"n={head['workloads'][row['name']]['lo_size']} ({row['lo_pct']:+.3f}%), "
      f"{row['base_hi']:,.0f} -> {row['head_hi']:,.0f} at "
      f"n={head['workloads'][row['name']]['hi_size']} ({row['hi_pct']:+.3f}%) {unit}; "
      f"ratio {row['base_ratio']:.3f} -> {row['head_ratio']:.3f}."
    )

  print(
    "\nTo see it locally:\n"
    "    ci/perf/run.sh <the merge-base sha>\n"
  )
  constant_only = [row for row in failed if row["verdict"] == "REGRESSION"]
  if constant_only:
    print(
      "If the extra cost buys something worth having, record the trade in a commit message on this "
      "branch — one trailer per workload:\n"
    )
    for row in constant_only:
      ceiling = max(1.0, round(row["worst"] + 1.0))
      print(f"    Perf-accept: {row['name']} +{ceiling:g}% <why the extra cost is worth it>")
    print(
      "\nThe trailer is read from this branch's own commits, so it expires when they are no longer "
      "this branch's: it licenses one change and does not become a standing allowance for the "
      "workload. `git commit --allow-empty` is enough to add one."
    )
  return 1


if __name__ == "__main__":
  sys.exit(main())

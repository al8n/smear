#!/usr/bin/env python3
"""Fails a build when a member crate declares a feature the umbrella cannot reach.

al8n/smear#84's strongest evidence for collapsing the crates was that the forwarding layer had
SILENTLY FAILED: `rowan` — the gate on the entire lossless CST tower — was not reachable through
the crate named `smear`, along with three more features, and nothing said so. The full-stack split
rebuilds that layer over more manifests than it had then, so the defect gets a gate before the
layer gets rebuilt.

THE DEFECT'S SIGNATURE IS ABSENCE, and that decides the direction of the walk. A check derived
from the umbrella's own forwarding table is structurally blind to it: the missing entry is missing
from the derivation too, so the table agrees with itself and reports nothing. The enumeration has
to run FROM THE MEMBERS, which are the superset, and check against the umbrella.

    for each workspace member M, excluding the umbrella:
      for each feature F declared in M's [features]:
        assert `smear` declares a feature whose transitive closure reaches M/F,
        or F is in EXEMPT with a written reason

TRANSITIVE, not direct. `smear`'s `lossless-coverage = ["rowan", …]` reaches
`smear-lexer/std` through `rowan -> std -> smear-lexer/std`, and a check that only read the
umbrella's direct `member/feature` entries would call that unreachable. What the gate asks is
whether SOME feature of `smear` turns M/F on, which is the question a dependent asks.

BOTH SIDES COME OUT OF `cargo metadata`. Nothing here is hardcoded except the umbrella's name and
the exemption table, and no count is written down anywhere: a census whose expected number is a
literal goes stale the first time a PR merges in between, and this repository has already shipped
that exact failure.

Run from the repository root:

    python3 ci/feature_reachability.py              # exit 1 on an unforwarded member feature
    python3 ci/feature_reachability.py --verbose    # also print every forwarded feature
    python3 ci/feature_reachability.py --selftest   # plant three defects and require each to fail

WHAT MAKES THIS GATE NON-VACUOUS. `--selftest` plants three DIFFERENT shapes of the defect against
synthetic metadata and requires the verdict to fire on each, because one plant proves the plant and
not the property. The three are named in `PLANTS` so `check()` and `selftest()` cannot drift:

  (a) a member feature the umbrella never names at all;
  (b) a member feature the umbrella names but forwards to the WRONG dependency;
  (c) a member feature forwarded under a RENAMED umbrella feature — which must PASS, because the
      umbrella is allowed to rename, and a gate that rejected it would be read as noise and
      switched off. What (c) proves is that the walk is over the transitive closure and not over
      spelling.

An exemption that matches nothing is also a failure, for the reason `ci/source_census` records
about its own tables: a stale exemption is either a feature that no longer exists or a reader that
has stopped seeing it, and both are the gate quietly not working.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys

UMBRELLA = "smear"

# Members with no public API of their own to forward. Each is a workspace member so the
# repository's own gates reach it, not because a dependent ever names it.
#
# This is deliberately a list of PACKAGES and not of features: a package here is skipped whole, so
# adding one is a visible decision, whereas a per-feature exemption that grew a package's worth of
# entries would not be.
EXEMPT_MEMBERS = {
  "smear-noatomic": "compiles the schema representation for a core with no atomics by "
  "`#[path]`-including `smear`'s own files; it has an empty `[dependencies]` table, which CI "
  "asserts, so there is no edge for a feature to travel",
  "smear-smoke": "consumes `smear` the way a dependent does and is `publish = false`; it is the "
  "far side of this gate, not a member with a surface to forward",
  "source-census": "reads `smear`'s source as text and links nothing in this workspace",
}

# Member features the umbrella deliberately does not forward, each with the argument for it.
# Empty today. An entry that matches nothing fails the run.
EXEMPT: dict[tuple[str, str], str] = {}

# The three shapes `--selftest` plants, named once so the verdict and the selftest cannot drift.
PLANTS = {
  "unnamed": "a member feature the umbrella never names — #84's actual defect",
  "wrong-dep": "a member feature the umbrella names but forwards to the wrong dependency",
  "renamed": "a member feature forwarded under a renamed umbrella feature, which must PASS",
}


def metadata() -> dict:
  out = subprocess.run(
    ["cargo", "metadata", "--no-deps", "--format-version", "1"],
    check=True,
    capture_output=True,
    text=True,
  )
  return json.loads(out.stdout)


def members(meta: dict) -> dict[str, dict]:
  """Workspace members by name, each mapped to its `[features]` table."""
  ids = set(meta["workspace_members"])
  return {p["name"]: p["features"] for p in meta["packages"] if p["id"] in ids}


def reachable(umbrella_features: dict[str, list[str]]) -> set[str]:
  """Every `dep/feature` some feature of the umbrella turns on, transitively.

  A feature list entry is one of `other-feature`, `dep/feature`, `dep?/feature` or `dep:name`.
  Only the middle two are edges to a member; the first is followed, and `dep:` is an optional
  dependency being activated and carries no feature with it.
  """
  seen_features: set[str] = set()
  out: set[str] = set()

  def walk(name: str) -> None:
    if name in seen_features:
      return
    seen_features.add(name)
    for entry in umbrella_features.get(name, ()):
      if entry.startswith("dep:"):
        continue
      if "/" in entry:
        dep, _, feature = entry.partition("/")
        out.add(f"{dep.removesuffix('?')}/{feature}")
        continue
      walk(entry)

  for name in umbrella_features:
    walk(name)
  return out


def check(feature_tables: dict[str, dict[str, list[str]]], verbose: bool = False) -> list[str]:
  """Every finding, as a list of sentences. Empty means the forwarding layer is complete."""
  findings: list[str] = []

  if UMBRELLA not in feature_tables:
    return [f"no workspace member named `{UMBRELLA}` — this gate has nothing to check against"]

  forwarded = reachable(feature_tables[UMBRELLA])
  used_exemptions: set[tuple[str, str]] = set()
  checked = 0

  for member in sorted(feature_tables):
    if member == UMBRELLA or member in EXEMPT_MEMBERS:
      continue
    for feature in sorted(feature_tables[member]):
      # `default` is not a capability a dependent asks the umbrella to forward: `smear` names
      # every member feature explicitly with `default-features = false`, which is what makes its
      # own default reproducible. Forwarding `member/default` would UNDO that.
      if feature == "default":
        continue
      checked += 1
      edge = f"{member}/{feature}"
      if edge in forwarded:
        if verbose:
          print(f"  ok        {edge}")
        continue
      if (member, feature) in EXEMPT:
        used_exemptions.add((member, feature))
        if verbose:
          print(f"  exempt    {edge} — {EXEMPT[(member, feature)]}")
        continue
      findings.append(
        f"`{member}` declares `{feature}` and no feature of `{UMBRELLA}` reaches "
        f"`{member}/{feature}`: a dependent of the umbrella cannot switch it on"
      )

  if checked == 0:
    findings.append(
      "zero member features were checked — either every member is exempt or the metadata was not "
      "read, and a gate that checks nothing is the failure it exists to catch"
    )

  for key, why in sorted(EXEMPT.items()):
    if key not in used_exemptions:
      findings.append(
        f"the exemption for `{key[0]}/{key[1]}` matches nothing ({why}): the feature is gone, or "
        f"this gate has stopped seeing it"
      )

  if verbose and not findings:
    print(f"  ({checked} member features checked against `{UMBRELLA}`)")
  return findings


# ── the selftest ────────────────────────────────────────────────────────────────────────────

_BASE = {
  UMBRELLA: {
    "default": ["std", "graphql"],
    "std": ["smear-lexer/std"],
    "graphql": ["smear-lexer/graphql"],
    "parser": ["dep:smear-parser", "smear-lexer/graphql"],
  },
  "smear-lexer": {"default": ["std"], "std": [], "graphql": []},
}


def _case(name: str, tables: dict, want_findings: bool) -> str | None:
  findings = check({k: dict(v) for k, v in tables.items()})
  fired = bool(findings)
  if fired != want_findings:
    verdict = "fired" if fired else "did not fire"
    wanted = "should have" if want_findings else "should not have"
    return f"{name}: the verdict {verdict} and {wanted} — {findings}"
  return None


def selftest() -> int:
  problems: list[str] = []

  problems.append(_case("the honest tree", _BASE, want_findings=False))

  # (a) a member feature the umbrella never names. #84's actual defect.
  planted = {k: dict(v) for k, v in _BASE.items()}
  planted["smear-lexer"]["rowan"] = []
  problems.append(_case(f"plant (a) {PLANTS['unnamed']}", planted, want_findings=True))

  # (b) named, but forwarded to the wrong dependency. The umbrella HAS a `rowan` feature and it
  # looks complete from the umbrella's side; it just does not reach the member that declares it.
  planted = {k: dict(v) for k, v in _BASE.items()}
  planted["smear-lexer"]["rowan"] = []
  planted[UMBRELLA]["rowan"] = ["smear-parser/rowan"]
  problems.append(_case(f"plant (b) {PLANTS['wrong-dep']}", planted, want_findings=True))

  # (c) forwarded under a different umbrella name, and reached only transitively. This must PASS:
  # the umbrella may rename, and what the plant proves is that the walk follows the closure rather
  # than matching spellings.
  planted = {k: dict(v) for k, v in _BASE.items()}
  planted["smear-lexer"]["rowan"] = []
  planted[UMBRELLA]["lossless"] = ["cst"]
  planted[UMBRELLA]["cst"] = ["smear-lexer/rowan"]
  problems.append(_case(f"plant (c) {PLANTS['renamed']}", planted, want_findings=False))

  # a stale exemption is a finding in its own right
  EXEMPT[("smear-lexer", "no-such-feature")] = "planted"
  problems.append(_case("plant (d) a stale exemption", _BASE, want_findings=True))
  del EXEMPT[("smear-lexer", "no-such-feature")]

  # and an exemption that DOES match suppresses the finding it is written for
  planted = {k: dict(v) for k, v in _BASE.items()}
  planted["smear-lexer"]["rowan"] = []
  EXEMPT[("smear-lexer", "rowan")] = "planted"
  problems.append(_case("plant (e) a live exemption suppresses its finding", planted, False))
  del EXEMPT[("smear-lexer", "rowan")]

  problems = [p for p in problems if p]
  if problems:
    print("::error::feature_reachability selftest: the gate does not implement its sentence")
    for p in problems:
      print(f"  - {p}")
    return 1
  print("feature_reachability selftest OK: 6 cases, 3 planted defect shapes")
  return 0


def main() -> int:
  ap = argparse.ArgumentParser(description=__doc__)
  ap.add_argument("--selftest", action="store_true", help="plant the defects and require failure")
  ap.add_argument("--verbose", action="store_true", help="print every forwarded member feature")
  args = ap.parse_args()

  if args.selftest:
    return selftest()

  tables = members(metadata())
  findings = check(tables, args.verbose)
  if findings:
    print("::error::feature_reachability: the umbrella cannot reach every member feature")
    for f in findings:
      print(f"  - {f}")
    print(
      "  This is al8n/smear#84's defect: a capability declared by a member and unreachable "
      "through the crate named `smear`. Add the forward, or add an entry to EXEMPT with the "
      "argument for leaving it unreachable."
    )
    return 1

  counted = sum(
    1
    for m, t in tables.items()
    if m != UMBRELLA and m not in EXEMPT_MEMBERS
    for f in t
    if f != "default"
  )
  print(f"feature_reachability OK: {counted} member features, all reachable through `{UMBRELLA}`")
  return 0


if __name__ == "__main__":
  sys.exit(main())

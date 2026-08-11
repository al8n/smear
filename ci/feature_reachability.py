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

# The second check: every publishable member is inside every selection that must cover it

The split has now produced the same defect twice by GROWTH rather than by edit. `-p smear --lib`
stopped covering the lexer's 130 unit tests and all four of this project's `unsafe` sites the day
`smear-lexer` moved out, and `ci/source_census` stopped reading the same crate's surface in the
same commit. Both gates went on passing; one was caught because its exemption table went stale in
the same instant, the other only by reading the scripts' own prose against the tree.

So the selections are read out of the files that hold them and checked against `cargo metadata`:

  for each publishable workspace member M:
    for each selection S that must cover every member:
      assert M is in S, or (S, M) is in EXEMPT_SELECTION with a written reason

A selection whose table cannot be FOUND in its file is a failure too, not an empty set — that is
the reader-stopped-seeing-it direction, and reporting "nothing is missing" from a table nobody
located is exactly how this gate would become the thing it exists to catch.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import pathlib
import re
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

# Selections that must contain every publishable workspace member, each read out of the file that
# owns it so the two cannot drift. The mount NAME a selection uses is not derivable — `smear-lexer`
# is published by the umbrella as `lexer` — so what is derived is completeness, not content.
#
# Each reader returns the set of package names the selection covers, or raises with why it could
# not read it. `None` is not an allowed answer: a table that cannot be located is a finding.
SELECTIONS = ("miri", "census", "cross")

# (selection, member) pairs deliberately outside a selection, each with the argument. Empty today.
# An entry that matches nothing — because the member is gone, or is in the selection after all —
# is a failure, exactly like a stale feature exemption.
EXEMPT_SELECTION: dict[tuple[str, str], str] = {}

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


# ── the third check: every member feature has an enforced equivalence ───────────────────────
#
# `smear` re-exports whole member crates, so a feature of `smear` gates what it advertises only
# where `smear/src/lib.rs` writes the `#[cfg]`. Where it forwards to a member feature, the `#[cfg]`
# is inside the member, and cargo unifies that member's features across the whole graph — so a
# second dependency naming the member directly switches the capability on behind the consumer.
# Measured before the repair: ten of ten forwarded pairs leaked; the four smear `#[cfg]`s itself
# did not.
#
# The repair is an equivalence smear asserts at compile time: each member publishes its resolved
# features as `__features` constants, and smear refuses to build when one disagrees with its own.
# This check is what keeps that total — a feature added to a member with no constant, or with no
# assertion in smear, is a hole in the gate and not a smaller gate.
#
# The exemption table is READ OUT OF `ci/downstream_pairs.py` rather than restated, so the two
# gates cannot come to disagree about which pairs owe an equivalence.

def _eq_twin() -> dict[tuple[str, str], str]:
  """`EQ_TWIN` out of `ci/downstream_pairs.py`, by importing it rather than by restating it."""
  path = REPO_ROOT / "ci" / "downstream_pairs.py"
  previous = sys.dont_write_bytecode
  sys.dont_write_bytecode = True
  try:
    spec = importlib.util.spec_from_file_location("_downstream_pairs_for_reachability", path)
    if spec is None or spec.loader is None:
      raise RuntimeError(f"{path} could not be loaded as a module")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
  finally:
    sys.dont_write_bytecode = previous
  table = getattr(module, "EQ_TWIN", None)
  if table is None:
    raise RuntimeError(f"{path} declares no EQ_TWIN")
  return table


def member_roots(meta: dict) -> dict[str, pathlib.Path]:
  """Every workspace member with features, other than the umbrella, and its crate root.

  DERIVED, and it did not used to be. A hand-written table is a second place a member can be
  missing from, and this gate exists because a pair went missing from one.
  """
  ids = set(meta["workspace_members"])
  out = {}
  for p in meta["packages"]:
    if p["id"] not in ids or p["name"] == UMBRELLA:
      continue
    if not [f for f in p["features"] if f != "default"]:
      continue  # `smear-smoke`, `smear-noatomic`, `source-census`: nothing to forward
    out[p["name"]] = pathlib.Path(p["manifest_path"]).parent / "src" / "lib.rs"
  return out


def check_equivalence(meta: dict, feature_tables: dict[str, dict[str, list[str]]]) -> list[str]:
  """Every member feature is witnessed by a constant AND pinned by an assertion in the umbrella.

  There is no skip. A pair whose umbrella twin is not its namesake declares the twin in
  `EQ_TWIN`; a pair with neither is a finding. The previous revision had an `EQ_EXEMPT` that
  removed a pair from the walk entirely, and the one entry in it was the pair that leaked.
  """
  findings: list[str] = []
  try:
    twins = _eq_twin()
  except Exception as err:  # noqa: BLE001 — the message is the finding
    return [f"the EQ_TWIN table could not be read ({err}), so this check knows nothing"]

  try:
    umbrella_src = (REPO_ROOT / "smear" / "src" / "lib.rs").read_text()
  except OSError as err:
    return [f"smear/src/lib.rs is unreadable ({err})"]
  umbrella_features = set(feature_tables.get(UMBRELLA, {}))

  roots = member_roots(meta)
  if not roots:
    return ["cargo metadata reported no member with features, so this check walked nothing"]

  checked = 0
  used_twin: set[tuple[str, str]] = set()
  for member, root in sorted(roots.items()):
    try:
      member_src = root.read_text()
    except OSError as err:
      findings.append(f"{root} is unreadable ({err}), so `{member}`'s witnesses are unknown")
      continue
    ident = member.replace("-", "_")
    for feature in sorted(f for f in feature_tables[member] if f != "default"):
      twin = twins.get((member, feature))
      if twin is not None:
        used_twin.add((member, feature))
      elif feature in umbrella_features:
        twin = feature
      else:
        findings.append(
          f"`{member}/{feature}` has no `{UMBRELLA}/{feature}` and no EQ_TWIN entry, so nothing "
          f"says what it should be equivalent to — and a pair this walk cannot name is a pair "
          f"outside the fence"
        )
        continue
      if twin not in umbrella_features:
        findings.append(f"EQ_TWIN sends `{member}/{feature}` to `{UMBRELLA}/{twin}`, "
                        f"which does not exist")
        continue
      checked += 1
      const = feature.upper().replace("-", "_")
      if f'pub const {const}: bool = cfg!(feature = "{feature}");' not in member_src:
        findings.append(
          f"`{member}` declares `{feature}` and publishes no `__features::{const}` constant, so "
          f"`smear` cannot see whether the graph turned it on"
        )
      # BOTH halves, and the second is the one that matters: a constant that exists and is never
      # read is a witness nobody consults. The assertion is matched WITH its right-hand side, so a
      # pair asserted against the wrong umbrella feature is a finding too.
      wanted = f'{ident}::__features::{const} == cfg!(feature = "{twin}")'
      if wanted not in umbrella_src:
        findings.append(
          f"`{member}/{feature}` is not asserted equal to `{UMBRELLA}/{twin}` in smear/src/lib.rs "
          f"(looked for `{wanted}`): a second dependency can switch it on behind a `smear` "
          f"consumer, which is the leak the equivalence exists to make unrepresentable"
        )
  for pair in sorted(set(twins) - used_twin):
    findings.append(f"the EQ_TWIN entry for `{pair[0]}/{pair[1]}` matches nothing")
  if checked == 0:
    findings.append("zero member features were checked for an equivalence")
  return findings


# ── the second check: selection completeness ────────────────────────────────────────────────

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

# Anchored on the declaration itself rather than on a line number or a neighbouring comment, and
# each raises if it matches nothing: a rename must be a hard error here, because a reader that
# silently finds an empty table would report every member as missing — or, worse, report nothing
# missing from a set it never read.
def _read_miri_packages() -> set[str]:
  """`MIRI_PACKAGES` out of `ci/miri_scope.py`, by importing it rather than by scraping."""
  path = REPO_ROOT / "ci" / "miri_scope.py"
  # Importing a file writes a `ci/__pycache__/` beside it, which nothing in this repository
  # ignores; running that same file as a script does not. Suppressed rather than gitignored, so a
  # gate leaves no trace in the tree it is reading.
  previous = sys.dont_write_bytecode
  sys.dont_write_bytecode = True
  try:
    spec = importlib.util.spec_from_file_location("_miri_scope_for_reachability", path)
    return _load_miri_packages(spec, path)
  finally:
    sys.dont_write_bytecode = previous


def _load_miri_packages(spec, path: pathlib.Path) -> set[str]:
  if spec is None or spec.loader is None:
    raise RuntimeError(f"{path} could not be loaded as a module")
  module = importlib.util.module_from_spec(spec)
  spec.loader.exec_module(module)
  packages = getattr(module, "MIRI_PACKAGES", None)
  if not packages:
    raise RuntimeError(f"{path} declares no non-empty MIRI_PACKAGES")
  return set(packages)


def _read_census_roots() -> set[str]:
  """The package names behind `DEFAULT_ROOTS` in `ci/source_census/src/main.rs`.

  The const holds crate-root PATHS, not package names — `("smear-lexer/src/lib.rs", …)` — so the
  package is the first path segment. That is exact for this workspace's layout, where a member's
  directory is its package name, and it is checked: a segment that is not a member is reported
  rather than silently ignored.
  """
  path = REPO_ROOT / "ci" / "source_census" / "src" / "main.rs"
  text = path.read_text()
  block = re.search(r"const DEFAULT_ROOTS:[^=]*=\s*&\[(.*?)\];", text, re.S)
  if block is None:
    raise RuntimeError(f"{path} has no `const DEFAULT_ROOTS` block this reader can find")
  roots = re.findall(r'\(\s*"([^"]+)"\s*,\s*"[^"]*"\s*\)', block.group(1))
  if not roots:
    raise RuntimeError(f"{path}'s DEFAULT_ROOTS block held no (path, name) pairs")
  return {root.split("/", 1)[0] for root in roots}


def _read_cross_packages() -> set[str]:
  """The publishable-member literal the `cross` job compares against, out of `ci.yml`.

  That job's tripwire is a frozen literal on purpose — see its own comment — so this reads the
  literal, which is the thing that has to grow, and not the `-p` list beside it. The two are
  required to agree by the job itself: the literal is what fails the run.
  """
  path = REPO_ROOT / ".github" / "workflows" / "ci.yml"
  text = path.read_text()
  found = re.search(r'\[ "\$PUBLISHABLE" != "([^"]+)" \]', text)
  if found is None:
    raise RuntimeError(f"{path} has no $PUBLISHABLE comparison this reader can find")
  return set(found.group(1).split(","))


READERS = {
  "miri": ("ci/miri_scope.py MIRI_PACKAGES", _read_miri_packages),
  "census": ("ci/source_census DEFAULT_ROOTS", _read_census_roots),
  "cross": (".github/workflows/ci.yml cross-job publishable literal", _read_cross_packages),
}


def check_selections(publishable: set[str]) -> list[str]:
  """Every publishable member is inside every selection, or exempt with a reason."""
  findings: list[str] = []
  used: set[tuple[str, str]] = set()

  if not publishable:
    return [
      "cargo metadata reported no publishable workspace member, so this check compared nothing"
    ]

  for key in SELECTIONS:
    label, reader = READERS[key]
    try:
      covered = reader()
    except Exception as err:  # noqa: BLE001 — the message is the finding
      findings.append(f"{label} could not be read ({err}), so its coverage is unknown, not empty")
      continue
    for member in sorted(publishable):
      if member in covered:
        continue
      if (key, member) in EXEMPT_SELECTION:
        used.add((key, member))
        continue
      findings.append(
        f"`{member}` is a publishable workspace member and is not in {label}: that selection has "
        f"stopped covering it, and every gate built on it will go on passing over less"
      )

  for pair, why in sorted(EXEMPT_SELECTION.items()):
    if pair not in used:
      findings.append(
        f"the selection exemption for `{pair[1]}` in `{pair[0]}` matches nothing ({why})"
      )
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

  # ── the selection check, planted the same way ─────────────────────────────────────────────
  #
  # (f) a publishable member outside a selection is the defect this half exists for, and (g) a
  # table the reader cannot find must fail rather than read as an empty set — the second is the
  # direction that would otherwise let a rename turn this gate off in silence.
  real = check_selections({"smear", "smear-lexer", "smear-parser"})
  if real:
    problems.append(f"the real tree should have no selection finding and had: {real}")

  planted_member = check_selections({"smear", "zzz-planted-member"})
  if len(planted_member) != len(SELECTIONS):
    problems.append(
      f"plant (f) a publishable member outside every selection: expected one finding per "
      f"selection ({len(SELECTIONS)}) and got {len(planted_member)}: {planted_member}"
    )

  saved = READERS["miri"]
  def _unreadable() -> set[str]:
    raise RuntimeError("planted: the table was renamed")
  READERS["miri"] = (saved[0], _unreadable)
  planted_reader = check_selections({"smear"})
  READERS["miri"] = saved
  if not any("could not be read" in f for f in planted_reader):
    problems.append(
      f"plant (g) an unreadable selection table: the reader failing must be a finding, got "
      f"{planted_reader}"
    )

  EXEMPT_SELECTION[("miri", "no-such-member")] = "planted"
  stale = check_selections({"smear", "smear-lexer", "smear-parser"})
  del EXEMPT_SELECTION[("miri", "no-such-member")]
  if not any("matches nothing" in f for f in stale):
    problems.append(f"plant (h) a stale selection exemption must fail, got {stale}")

  problems = [p for p in problems if p]
  if problems:
    print("::error::feature_reachability selftest: the gate does not implement its sentence")
    for p in problems:
      print(f"  - {p}")
    return 1
  print(
    "feature_reachability selftest OK: 10 cases, 5 planted defect shapes across both checks"
  )
  return 0


def main() -> int:
  ap = argparse.ArgumentParser(description=__doc__)
  ap.add_argument("--selftest", action="store_true", help="plant the defects and require failure")
  ap.add_argument("--verbose", action="store_true", help="print every forwarded member feature")
  args = ap.parse_args()

  if args.selftest:
    return selftest()

  meta = metadata()
  tables = members(meta)
  ids = set(meta["workspace_members"])
  publishable = {
    p["name"] for p in meta["packages"] if p["id"] in ids and p.get("publish") != []
  }

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

  if args.verbose:
    for key in SELECTIONS:
      print(f"  ok        {READERS[key][0]}")
  equivalence_findings = check_equivalence(meta, tables)
  if equivalence_findings:
    print("::error::feature_reachability: a member feature has no enforced equivalence")
    for f in equivalence_findings:
      print(f"  - {f}")
    print(
      "  Without it the umbrella's feature is advertising a gate it cannot hold: cargo unifies a "
      "member's features across the whole graph, so a second dependency naming that member turns "
      "the capability on behind the consumer. `ci/downstream.sh` is the experiment that shows it."
    )
    return 1
  if args.verbose:
    print("  ok        every member feature is asserted equal to its `smear` twin")

  selection_findings = check_selections(publishable)
  if selection_findings:
    print("::error::feature_reachability: a gate's selection has stopped covering every member")
    for f in selection_findings:
      print(f"  - {f}")
    print(
      "  A gate that narrows as the workspace grows keeps passing over less, which is how Miri "
      "lost the lexer and how the source census went blind. Add the member to the selection, or "
      "add an entry to EXEMPT_SELECTION with the argument for leaving it out."
    )
    return 1

  counted = sum(
    1
    for m, t in tables.items()
    if m != UMBRELLA and m not in EXEMPT_MEMBERS
    for f in t
    if f != "default"
  )
  print(
    f"feature_reachability OK: {counted} member features, all reachable through `{UMBRELLA}` and "
    f"all asserted equal to it; {len(publishable)} publishable members, all inside "
    f"{len(SELECTIONS)} gate selections"
  )
  return 0


if __name__ == "__main__":
  sys.exit(main())

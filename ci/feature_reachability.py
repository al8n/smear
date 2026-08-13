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

# Interpreter

Stdlib only, and deliberately runs on Python 3.9 — verified on `/usr/bin/python3` 3.9.6, the macOS
system interpreter. That is not incidental: this is a gate a person is TOLD to run locally, and a
local gate that needs an interpreter the machine does not have is a local gate nobody runs.

It used to need 3.11 and did not say so. Two readers reached a sibling gate's constant by
`importlib`-executing it, and `ci/miri_scope.py` has a top-level `import tomllib` — so this file
inherited that floor and exited 1 on 3.9 with `No module named 'tomllib'` before reading the tree.
An import inherits the imported module's dependencies; a read does not. `read_constant` parses the
assignment with `ast` and evaluates the literal, which is exact and drags in nothing, and the
fourth check below makes "read, never run" a property of `ci/` rather than a habit.

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
import ast
import json
import pathlib
import re
import subprocess
import sys

UMBRELLA = "smear"

# ── EVERY TABLE IN THIS DIRECTORY, AND WHAT RE-CHECKS IT ────────────────────────────────────
#
# Asked because it has now happened twice. `EQ_EXEMPT` was a skip table whose one entry carried a
# reason nobody re-ran; it was replaced by `EQ_TWIN`, which has no skip path. One round later
# `MIRI_NOT_SELECTED` arrived — a new exemption table whose entries were justified by a measurement
# nobody re-ran, guarded by nothing stronger than "the reason is non-empty", which is the same
# guarantee `EQ_EXEMPT` offered: that somebody once thought about it.
#
# So every table that asserts something about the tree is listed here with its checker. A table
# with no checker is either fixed or recorded as a bound — never left implicit.
#
# WHAT THIS TABLE IS NOT, stated because a row of it was cited as clearing something it never
# looked at. Every row answers ONE question: *is this table's claim re-checked, or can it go stale?*
# It is a staleness audit. It is not a safety audit, and a row saying "re-checked" says nothing
# about whether the code around that table is safe to call from the selftest, cheap enough to run,
# or correct in any other respect.
#
# The `SELECTIONS / READERS` row is the case in point. Its verdict — a reader that cannot locate
# its table reports "coverage is unknown, not empty" — was true when written and is still true.
# What it did not say, because the question was never asked here, is that `check_selections` used
# to CALL those readers from inside the selftest, so real files decided planted cases. That
# property belongs to `audit_containment`, and that is where it was missing. Reading each row's
# checker, which is what produced the two one-way downgrades below, could not have found it.
#
#   TABLE                          CLAIMS                          RE-CHECKED BY
#   ---------------------------------------------------------------------------------------------
#   EXEMPT_MEMBERS                 the member has no feature to    audit_exempt_members: it must be
#     (this file)                  forward                         a member and declare none
#   EXEMPT (features)              this feature need not be        an entry matching nothing is a
#     (this file)                  forwarded                       finding; empty today
#   EXEMPT_SELECTION               this member need not be in      an entry matching nothing is a
#     (this file)                  that selection                  finding; empty today
#   FLOOR_BEARING                  only this file may import a     two-sided: others may not, and
#     (this file)                  floor-bearing stdlib module     the owner must
#   SELECTIONS / READERS           these are the selections that   a reader that cannot find its
#     (this file)                  must cover every member         table is a finding; the readers
#                                                                  now run only in audit_selections,
#                                                                  never under the selftest
#   EQ_TWIN                        this pair's umbrella twin is    two-sided: a stale entry, and a
#     (downstream_pairs)           not its namesake                pair with no twin, both fail
#   MEMBERS                        these are the crates the        C4: a workspace member with
#     (downstream_pairs)           umbrella re-exports             features and no row fails
#   PRESENCE                       features a member's edge needs  self-verifying ONE WAY: too
#     (downstream_pairs)                                           narrow fails its POS leg; too
#                                                                  wide still compiles and only
#                                                                  makes the leg less isolated
#   REASONS                        how a negative leg may fail     self-verifying ONE WAY: an
#     (downstream_pairs)                                           unlisted reason is a finding;
#                                                                  an over-broad entry admits a
#                                                                  failure it should not
#   MIRI_PACKAGES                  what the Miri cells run         it IS the source: the scripts
#     (miri_scope)                                                 build `-p` from it, and the
#                                                                  post-run binary count checks it
#   MIRI_NOT_SELECTED              this member's lib harness is    --verify-exclusions re-runs the
#     (miri_scope)                 empty at these cargo flags      measurement the reason names
#   MIRI_DECLARED_IGNORES          WHICH sources carry a per-test  counted out of BOTH roots a cell
#     (miri_scope)                 ignore, and how many in each    compiles, held against every
#                                                                  binary's `ignored` digit, and its
#                                                                  total restated in miri.yml
#   WORKFLOW_BUDGET_SITES          where miri.yml restates the     matched against miri.yml
#     (miri_scope)                 declared total
#   CELL_FLAGS                     value of each bare cfg in a     self-verifying: an unmodelled
#     (miri_scope)                 Miri cell                       cfg raises rather than guesses
#   EXEMPTIONS (40 entries)        this error type is outside      an entry matching nothing is a
#     (source_census, Rust)        the diagnostic contract         hard error
#
# THE TWO ONE-WAY ROWS ABOVE ARE NOT "FINE". They are checked in the direction that has bitten and
# unchecked in the other, and saying so is the point of this table: eleven unexplained "sound"s is
# the shape both exemption tables had. Neither residual is silent in practice — a too-wide
# `PRESENCE` shows up as a LEAK leg that stops isolating one pair, which the judge's
# name-the-pair rule catches, and an over-broad `REASONS` entry would have to be written
# deliberately — but neither is closed by construction.
#
# TWO BOUNDS, recorded because they cannot be closed rather than because nobody tried:
#
#   PROBES (downstream_pairs) — which public path a feature gates is a JUDGEMENT, and cargo does
#   not carry it. Each probe is checked to name a real pair with a real `uses-` feature, and the EQ
#   family is total over every member feature, so a missing probe costs an observable path and not
#   a whole pair. Completeness of the judgement is not derivable.
#
#   EXECUTION_IMPORTS / EXECUTION_CALLS / SPAWN_CALLS (this file) — a denylist of spellings cannot
#   be complete in principle. What IS derived is the set of forbidden targets: `ci/*.py` basenames
#   and the Miri scripts, so a gate added tomorrow is covered without anyone remembering.
#
# Both bounds are ALSO written at the tables they describe, not only here. A bound recorded where
# nobody meets it is the shape that produced two exemption tables whose reasons nobody re-ran.

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
  """`cargo metadata`, reported as a finding rather than raised as a traceback.

  A gate that dies with a stack trace when the manifests do not parse tells the reader about
  Python. Found by a plant that happened to write invalid TOML: the run ended in
  `CalledProcessError` from `subprocess`, forty lines of it, with cargo's actual complaint nowhere
  in sight.
  """
  out = subprocess.run(
    ["cargo", "metadata", "--no-deps", "--format-version", "1"],
    capture_output=True,
    text=True,
  )
  if out.returncode != 0:
    print("::error::feature_reachability: `cargo metadata` failed, so this gate read nothing",
          file=sys.stderr)
    print(out.stderr.strip()[:1200], file=sys.stderr)
    raise SystemExit(1)
  try:
    return json.loads(out.stdout)
  except json.JSONDecodeError as err:
    print(f"::error::feature_reachability: `cargo metadata` printed no usable JSON ({err})",
          file=sys.stderr)
    raise SystemExit(1) from err


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


# ── the fourth check: no gate executes another gate ─────────────────────────────────────────
#
# The defect this exists for was in this file. Two readers reached a sibling gate's constant by
# `importlib`-executing it, and one of those siblings has a top-level `import tomllib` — so this
# gate silently required Python >=3.11 while the workflow and the local instructions both said
# `python3`. On the macOS system interpreter (3.9.6) it exited 1 before reading the tree.
#
# It failed CLOSED, which is the one thing that went right: the reader-failure branch reported
# "coverage is unknown, not empty". But it blamed the SELECTION, so the message sent the reader
# looking at `MIRI_PACKAGES` for a fault that was in the interpreter.
#
# The rule is one line: a gate may READ another gate's source, never RUN it. A read costs nothing
# but `ast`; an execution inherits every dependency the other file has now or acquires later. This
# check is what makes that a property of the directory rather than a habit — and it is derived, so
# a third gate added tomorrow is covered without anyone remembering.
# THE PROPERTY, and the previous revision was written against the mechanism instead: no `ci/*.py`
# obtains another gate's contents BY RUNNING IT, in any spelling. That revision matched
# `importlib`/`runpy` and four call names — every one of them the mechanism this gate had just
# stopped using — so the plainest spelling of the forbidden thing, `import miri_scope`, was the one
# form it could not see. A check that only recognises the instance it replaced is not a check on
# the property.
#
# Three spellings, and the first is derived from the directory so a gate added tomorrow is covered:
#
#   1. importing a sibling by name          `import miri_scope`, `from miri_scope import X`
#   2. importing the machinery to do it     `importlib`, `runpy`
#   3. shelling out to one                  `subprocess.run([sys.executable, "ci/miri_scope.py"])`
#
# Matched over the syntax tree, not the text: an earlier revision grepped for its own table and
# reported itself. A string literal is not a call, and `ast` knows the difference.
# ── BOUND: THIS LIST CANNOT BE COMPLETE, AND HERE IS WHAT IS DERIVED INSTEAD ────────────────
#
# A denylist of spellings is not a closure. There is no finite set of ways to run another file in
# Python, and anyone determined to can reach one past these names. Stated at the table rather than
# only in a report, because a bound recorded where nobody meets it is exactly the shape that
# produced this file's history: `EQ_EXEMPT`'s reason, `MIRI_NOT_SELECTED`'s measurement and this
# check's first revision all said the right thing somewhere nobody read.
#
# What IS closed is the other half — the set of forbidden TARGETS. `siblings` below is every
# `ci/*.py` basename and the Miri scripts are found by their content, so a gate added tomorrow is
# covered without anyone remembering. The residual risk is a novel spelling of "run it", not a new
# file going unwatched, and the first revision's actual defect was in the target half: it could not
# see a plain `import miri_scope`.
EXECUTION_IMPORTS = ("importlib", "runpy")
EXECUTION_CALLS = ("exec", "eval", "exec_module", "spec_from_file_location")
# Callables that hand a command line to the operating system. A `.py` of ours in one of their
# arguments, or `sys.executable`, is spelling 3.
SPAWN_CALLS = ("run", "call", "check_call", "check_output", "Popen", "system", "execv", "execvp",
               "spawnv", "spawnvp")

# Stdlib modules that carry an interpreter floor, and the one file allowed to have one.
#
# The floor's SCOPE is checked rather than stated. `tomllib` is 3.11+, `ci/miri_scope.py` genuinely
# needs it to parse `smear/Cargo.toml`, and every other gate here is 3.9-safe — verified by running
# them on `/usr/bin/python3` 3.9.6, the macOS system interpreter the defect was reported from. The
# entry is two-sided: a second file importing `tomllib` is a finding, and `miri_scope.py` NOT
# importing it is a finding too, so the exception cannot go stale after the need disappears.
FLOOR_BEARING = {"tomllib": ("miri_scope.py", "parses smear/Cargo.toml; stdlib since 3.11")}


def audit_no_gate_executes_another(verbose: bool = False) -> list[str]:
  """PROPERTY: no `ci/*.py` obtains another gate's contents by running it, in any spelling."""
  findings: list[str] = []
  scripts = sorted((REPO_ROOT / "ci").glob("*.py"))
  if not scripts:
    return ["no `ci/*.py` was found, so this check walked nothing"]
  # DERIVED from the directory, so a gate added tomorrow is a forbidden import target without
  # anyone remembering to add it.
  siblings = {s.stem for s in scripts}
  for script in scripts:
    try:
      tree = ast.parse(script.read_text(), filename=str(script))
    except (OSError, SyntaxError) as err:
      findings.append(f"ci/{script.name} could not be parsed ({err}), so it is unchecked")
      continue
    hits: set[str] = set()
    for node in ast.walk(tree):
      # 1 + 2 — importing a sibling gate by name, or the machinery for loading one. Anywhere in the
      # file, including inside a function: a deferred import is still an import.
      if isinstance(node, ast.Import):
        for alias in node.names:
          root = alias.name.split(".")[0]
          if root in EXECUTION_IMPORTS:
            hits.add(root)
          elif root in siblings:
            hits.add(f"import {root}")
      elif isinstance(node, ast.ImportFrom) and node.module:
        root = node.module.split(".")[0]
        if root in EXECUTION_IMPORTS:
          hits.add(root)
        elif root in siblings:
          hits.add(f"from {root} import …")
      elif isinstance(node, ast.Call):
        func = node.func
        name = func.id if isinstance(func, ast.Name) else (
          func.attr if isinstance(func, ast.Attribute) else None
        )
        if name in EXECUTION_CALLS:
          hits.add(f"{name}()")
        # 3 — a command line that names one of our scripts, or the interpreter running us.
        elif name in SPAWN_CALLS:
          for inner in ast.walk(node):
            if isinstance(inner, ast.Constant) and isinstance(inner.value, str):
              stem = pathlib.PurePosixPath(inner.value).name
              if stem.endswith(".py") and stem[:-3] in siblings:
                hits.add(f"{name}() on {stem}")
            elif (isinstance(inner, ast.Attribute) and inner.attr == "executable"
                  and isinstance(inner.value, ast.Name) and inner.value.id == "sys"):
              hits.add(f"{name}() with sys.executable")
    if hits:
      findings.append(
        f"`ci/{script.name}` can run another gate ({', '.join(sorted(hits))}). A gate may READ a "
        f"sibling's source — `read_constant` does, with `ast` — but running it inherits that "
        f"file's imports, which is how this gate acquired an unstated Python >=3.11 floor."
      )
    elif verbose:
      print(f"  ok        ci/{script.name} reads, does not execute")

    imported = set()
    for node in ast.walk(tree):
      if isinstance(node, ast.Import):
        imported |= {a.name.split(".")[0] for a in node.names}
      elif isinstance(node, ast.ImportFrom) and node.module:
        imported.add(node.module.split(".")[0])
    for module, (owner, why) in FLOOR_BEARING.items():
      if module in imported and script.name != owner:
        findings.append(
          f"`ci/{script.name}` imports `{module}`, which carries an interpreter floor. Only "
          f"`ci/{owner}` is allowed one ({why}); every other gate here is run locally and is "
          f"verified 3.9-safe."
        )
      if module not in imported and script.name == owner:
        findings.append(
          f"`ci/{owner}` no longer imports `{module}`, so the recorded floor is stale — either "
          f"delete the FLOOR_BEARING entry and the guards that cite it, or restore the import."
        )
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

def read_constant(path: pathlib.Path, name: str):
  """One module-level constant out of another gate's source, by READING it rather than running it.

  PROPERTY: the value returned is the value the module would hold at import time.

  Every clause below is that sentence: refuse a second write, because two writes mean the literal
  is not the final value; refuse an augmented assignment, for the same reason; refuse a `global`
  rebind, because then import time is not decidable by reading; and refuse a non-literal, because
  only the interpreter could evaluate it. Checking the implementation against the SENTENCE rather
  than against the case that prompted it is what turned up the second-write hole — the first
  revision returned the first match and stopped.

  THE DIFFERENCE IS THE WHOLE POINT, and it cost a review round. These readers used to
  `importlib`-execute the sibling gate, on the argument that importing beats scraping — which is
  right about *scraping* and wrong about *executing*: an import inherits the imported module's
  dependencies, and a read does not. `ci/miri_scope.py` has a top-level `import tomllib`, so
  importing it put a silent Python >=3.11 floor on this gate. Reproduced on `/usr/bin/python3`
  3.9.6, the macOS system interpreter: `feature_reachability.py --selftest` exited 1 with
  `No module named 'tomllib'` before it read a line of the tree.

  This is not the scraping the earlier repair rejected. `ast.literal_eval` over the assignment node
  is exact — it accepts the value the interpreter would build and rejects anything that is not a
  literal — so there is still one source of truth and still no second copy to drift. What it drops
  is the execution, and with it every dependency the other file happens to have.

  `ast` has been in the standard library since Python 2.6.
  """
  try:
    tree = ast.parse(path.read_text(), filename=str(path))
  except OSError as err:
    raise RuntimeError(f"{path} is unreadable ({err})") from err
  except SyntaxError as err:
    raise RuntimeError(f"{path} does not parse ({err})") from err

  # EVERY write that runs at import time, not the first one. Taking the first match and stopping
  # was this reader's own version of the defect it was written to fix: `NAME = (…)` followed by
  # `NAME += (…)`, or a later reassignment, returns the initial literal while EXECUTING the module
  # would use the final one — so replacing the import with a read would have created a fresh way
  # for the two gates to disagree, silently.
  #
  # `If`/`Try`/`For`/`While`/`With` bodies are descended into because they run on import;
  # `FunctionDef` and `ClassDef` bodies are not, because a name bound there is local — unless it is
  # declared `global`, which is why that is a finding on its own.
  writes = []
  def scan(body):
    for node in body:
      if isinstance(node, ast.Assign):
        for target in node.targets:
          if isinstance(target, ast.Name) and target.id == name:
            writes.append(node)
      elif isinstance(node, (ast.AnnAssign, ast.AugAssign)):
        target = node.target
        if isinstance(target, ast.Name) and target.id == name and getattr(node, "value", None):
          writes.append(node)
      elif isinstance(node, (ast.If, ast.For, ast.While, ast.With, ast.Try)):
        for attr in ("body", "orelse", "finalbody", "handlers"):
          inner = getattr(node, attr, None) or []
          scan([h for h in inner] if attr != "handlers" else
               [stmt for handler in inner for stmt in handler.body])
      elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        for inner in ast.walk(node):
          if isinstance(inner, ast.Global) and name in inner.names:
            raise RuntimeError(
              f"{path} declares `global {name}` inside `{node.name}`, so its value at import time "
              f"is not decidable by reading. Keep the constant a single module-level literal."
            )
  scan(tree.body)

  if not writes:
    raise RuntimeError(f"{path} declares no module-level `{name}`")
  if len(writes) > 1:
    lines = ", ".join(str(w.lineno) for w in writes)
    raise RuntimeError(
      f"{path} writes `{name}` {len(writes)} times (lines {lines}). This reader returns what the "
      f"module would hold at import time, and with more than one write that is not what a single "
      f"literal says — refuse rather than pick one."
    )
  node = writes[0]
  if isinstance(node, ast.AugAssign):
    raise RuntimeError(f"{path}'s `{name}` is built by augmented assignment, which has no literal")
  try:
    return ast.literal_eval(node.value)
  except ValueError as err:
    raise RuntimeError(
      f"{path}'s `{name}` is not a literal this reader can evaluate ({err}). It must stay a "
      f"plain literal: reading it is what keeps this gate free of that file's dependencies."
    ) from err


def _eq_twin() -> dict[tuple[str, str], str]:
  """`EQ_TWIN` out of `ci/downstream_pairs.py`."""
  table = read_constant(REPO_ROOT / "ci" / "downstream_pairs.py", "EQ_TWIN")
  if not isinstance(table, dict):
    raise RuntimeError("EQ_TWIN is not a dict")
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


def audit_equivalence(meta: dict, feature_tables: dict[str, dict[str, list[str]]]) -> list[str]:
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
  """Every publishable member `ci/miri_scope.py` ACCOUNTS FOR — selected, or excluded with a reason.

  Two tables, because the property is "accounted for", not "selected". `MIRI_PACKAGES` is what the
  Miri scripts execute — they build their `-p` list from it, so it is not a declaration any more —
  and `MIRI_NOT_SELECTED` names the publishable members with no lib unit tests to interpret at that
  feature set, each with the measurement. Selecting one of those would produce an empty harness,
  which `miri_scope`'s own check fails on purpose.

  A member in NEITHER table is the finding this check exists for. A member in BOTH is a
  contradiction and is also a finding, and an excluded member that no longer exists makes the
  reason stale — so the account cannot rot in either direction.

  Read and not imported, and not relocated either. Moving the constant into a shared
  dependency-light module was the other option; it would separate the tuple from the twenty lines
  above it that argue why that package list is what it is and what adding to it costs, and that
  argument is the thing which stops the list being edited carelessly. A read keeps the constant
  where its reasoning is, and `ci/miri_scope.py` keeps its `tomllib` — which it genuinely needs.

  Reading also leaves no `ci/__pycache__/` behind, which importing did and which nothing in this
  repository ignores.
  """
  path = REPO_ROOT / "ci" / "miri_scope.py"
  selected = read_constant(path, "MIRI_PACKAGES")
  if not selected:
    raise RuntimeError("ci/miri_scope.py declares no non-empty MIRI_PACKAGES")
  excluded = read_constant(path, "MIRI_NOT_SELECTED")
  if not isinstance(excluded, dict):
    raise RuntimeError("ci/miri_scope.py's MIRI_NOT_SELECTED is not a dict of member -> entry")
  overlap = sorted(set(selected) & set(excluded))
  if overlap:
    raise RuntimeError(
      f"ci/miri_scope.py both selects and excludes {overlap}; the two tables must partition"
    )
  # An entry has to be MEASURABLE, not merely worded. `features` is the cargo configuration its
  # claim is asserted at and `miri_scope.py --verify-exclusions` runs exactly that; a `why` with no
  # `features` beside it is the shape this table had when it was an unchecked exemption.
  for member, entry in sorted(excluded.items()):
    if (not isinstance(entry, dict)
        or entry.get("outcome") not in ("empty", "forbidden")
        or not str(entry.get("why", "")).strip()):
      raise RuntimeError(
        f"ci/miri_scope.py's exclusion of `{member}` is not "
        f"`{{'outcome': 'empty'|'forbidden', 'why': '...'}}`. The outcome is what "
        f"`--verify-exclusions` re-runs UNDER THE CELLS' RESOLVE; a reason with no outcome is a "
        f"sentence nobody can execute, which is what this table was the second time."
      )
  return set(selected) | set(excluded)


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


# The two scripts that must BUILD their selection from `MIRI_PACKAGES` rather than restate it.
#
# Deriving removed the duplicate; this stops it coming back. Planted: re-hardcoding
# `cargo miri test -p smear -p smear-lexer` in `ci/miri_sb.sh` left every other gate green, because
# nothing else reads a shell script's argument list.
def miri_scripts() -> list[pathlib.Path]:
  """The Miri cell scripts, DERIVED from the directory rather than listed.

  A hand-written pair is one more table resting on an unverified fact — "these are all of them" —
  and a third aliasing model added as `ci/miri_xx.sh` would be unchecked. Anything under `ci/` that
  runs `cargo miri test` is one of these, which is the property rather than the naming convention.
  """
  found = []
  for script in sorted((REPO_ROOT / "ci").glob("*.sh")):
    try:
      if "cargo miri test" in script.read_text():
        found.append(script)
    except OSError:
      continue
  return found


def audit_miri_scripts_derive() -> list[str]:
  """PROPERTY: what the Miri scripts RUN is what `MIRI_PACKAGES` says, because it is its source.

  Not "the two lists agree" — that would leave two lists. The scripts ask for the constant, and
  this refuses both ways of stopping: not asking, and passing a literal `-p` beside it.
  """
  findings: list[str] = []
  scripts = miri_scripts()
  if not scripts:
    return ["no script under `ci/` runs `cargo miri test`, so this check walked nothing"]
  for path in scripts:
    name = f"ci/{path.name}"
    try:
      text = path.read_text()
    except OSError as err:
      findings.append(f"{name} is unreadable ({err}), so its selection is unknown")
      continue
    if "miri_scope.py --print-packages" not in text:
      findings.append(
        f"{name} does not build its selection from `miri_scope.py --print-packages`, so "
        f"`MIRI_PACKAGES` is a declaration again and the guard would be reading a list the "
        f"workflow does not execute"
      )
    if "miri_scope.py --verify-exclusions" not in text:
      findings.append(
        f"{name} does not run `miri_scope.py --verify-exclusions`, so `MIRI_NOT_SELECTED`'s "
        f"reasons are back to being writable rather than executable — a member that gains a lib "
        f"unit test while excluded has one nothing interprets, and this fast gate cannot see it"
      )
    for line in text.splitlines():
      stripped = line.strip()
      if stripped.startswith("#") or "cargo miri test" not in stripped:
        continue
      if " -p " in stripped:
        findings.append(
          f"{name} passes a literal `-p` to `cargo miri test` ({stripped[:70]}…). The selection "
          f"must come from the constant; a second hard-coded list is what drifted last time."
        )
  return findings


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


def audit_selections(publishable: set[str]) -> list[str]:
  """Read every selection off disk, then hand the results to the pure checker.

  THE SPLIT IS THE POINT. `check_selections` used to do both, and it is called by the selftest with
  planted inputs — but it reached three real files through `READERS[key]`, a call the containment
  walk could not follow because `reader` is a local bound from a dict rather than a module-level
  name. So a `check_*` function was not argument-only, real workspace state could reach a planted
  case, and the gate that exists to stop exactly that saw nothing.
  """
  covered: dict[str, "set[str] | str"] = {}
  for key in SELECTIONS:
    label, reader = READERS[key]
    try:
      covered[key] = reader()
    except Exception as err:  # noqa: BLE001 — the message is the finding
      covered[key] = f"could not be read ({err})"
  return check_selections(publishable, covered)


def check_selections(publishable: set[str], covered: dict) -> list[str]:
  """PROPERTY: every publishable member is inside every selection, or exempt with a reason.

  ARGUMENT-ONLY BY CONSTRUCTION. `covered` maps each selection key to the set of members it covers,
  or to a string saying why it could not be read. Nothing here touches the filesystem, so the
  selftest can plant every input and no real state can leak into a synthetic case.
  """
  findings: list[str] = []
  used: set[tuple[str, str]] = set()

  if not publishable:
    return [
      "cargo metadata reported no publishable workspace member, so this check compared nothing"
    ]

  missing_keys = [key for key in SELECTIONS if key not in covered]
  if missing_keys:
    return [f"no coverage was supplied for {missing_keys}, so this check compared nothing"]

  for key in SELECTIONS:
    label = READERS[key][0]
    result = covered[key]
    if isinstance(result, str):
      findings.append(f"{label} {result}, so its coverage is unknown, not empty")
      continue
    for member in sorted(publishable):
      if member in result:
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


# ── THE TWO FAMILIES, AND WHY THE LINE BETWEEN THEM IS CHECKED AND NOT JUST FOLLOWED ────────
#
# `check_*` is called BY THE SELFTEST with planted inputs, so it must derive every finding from its
# arguments. `audit_*` compares a table against the real workspace and is reachable only from
# `main()`.
#
# This was got wrong twice, both times by putting a world-comparing check in a selftest-reachable
# function: it then fires on every synthetic case and reports the gate as broken. The second time
# was four minutes after writing the rule down as a comment — which is the whole argument for
# `audit_containment()` below. A constraint that has been restated and then violated is a
# constraint that needed to be structural.


def audit_exempt_members(feature_tables: dict[str, dict[str, list[str]]]) -> list[str]:
  """PROPERTY: a member exempted as having nothing to forward really has nothing to forward.

  `EXEMPT_MEMBERS` asserts a fact about the tree, so it is re-derived rather than trusted — a
  member that has grown a feature is being skipped on a claim that stopped being true, and one
  that is not a member at all describes nothing. Cheap: the same `cargo metadata` main() already
  read.
  """
  findings: list[str] = []
  for member, why in sorted(EXEMPT_MEMBERS.items()):
    if member not in feature_tables:
      findings.append(
        f"`{member}` is exempted as having nothing to forward and is not a workspace member ({why})"
      )
      continue
    declared = sorted(f for f in feature_tables[member] if f != "default")
    if declared:
      findings.append(
        f"`{member}` is exempted as having nothing to forward and now declares {declared}. The "
        f"exemption's claim is no longer true, so those features are unchecked."
      )
  return findings


def audit_miri_exclusions(publishable: set[str]) -> list[str]:
  """No member is excluded from the Miri selection for a reason that describes nothing.

  `audit_`, not `check_`: it reads the real tree, so it must never be reachable from the selftest,
  which calls the `check_*` family with synthetic package sets. A world-reading check among those
  fails on every plant and reports it as the gate's fault — done twice now, which is why the
  distinction is in the names.
  """
  findings: list[str] = []
  try:
    excluded = read_constant(REPO_ROOT / "ci" / "miri_scope.py", "MIRI_NOT_SELECTED")
  except Exception as err:  # noqa: BLE001 — the message is the finding
    return [f"ci/miri_scope.py's MIRI_NOT_SELECTED could not be read ({err})"]
  for member in sorted(set(excluded) - publishable):
    findings.append(
      f"ci/miri_scope.py excludes `{member}` from the Miri selection, and it is not a publishable "
      f"workspace member — the recorded reason describes nothing"
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


# Calls that touch the world, matched as METHOD calls — `path.read_text()`, `subprocess.run()` —
# plus the one builtin that does it bare.
#
# METHODS AND NOT BARE NAMES, because the first revision of this list matched any call to a name in
# it and `walk` is also what two local recursion helpers in this file are called. It reported
# `reachable`, then `check`, then `_case`, then `selftest` itself, through a chain of false
# positives — a detector wrong in the noisy direction, found by tracing the taint rather than by
# reading its verdict.
WORLD_METHODS = ("read_text", "read_bytes", "read", "write_text", "run", "check_output", "glob",
                 "rglob", "iterdir", "listdir", "walk", "exists", "is_file", "is_dir")
WORLD_BUILTINS = ("open",)
# `audit_containment` reads THIS FILE's own source, which is not workspace state and cannot differ
# between a planted case and a real one, so it is the one world-reader the selftest may reach.
CONTAINMENT_EXEMPT = ("audit_containment",)


def audit_containment() -> list[str]:
  """PROPERTY: nothing the selftest reaches can read the world, and every audit runs.

  THE NAME IS DOCUMENTATION; THIS IS THE CHECK. The previous revision asked whether a function
  called something *named* `audit_*`, which is a property of the call site — so
  `label, reader = READERS[key]` then `reader()` walked straight past it, and `check_selections`
  read three real files from inside the selftest while this reported nothing. The third check in a
  row written against the call shape I happened to have used.

  So world-reading is derived from what a function DOES — it calls `read_text`, `open`,
  `subprocess.run`, `glob` — and propagated along every edge this walk can resolve. And an edge it
  cannot resolve is itself a finding: a call through a value taken out of a subscript is exactly
  the shape that hid the last one, and a walk that silently drops an edge is a walk whose
  conclusion is about the edges it happened to follow.

  Three findings, then:

    * a world-reading function reachable from `selftest()`;
    * a call the walk cannot follow, inside anything `selftest()` reaches;
    * an `audit_*` no path from `main()` reaches — a check that does not run.
  """
  findings: list[str] = []
  try:
    tree = ast.parse(pathlib.Path(__file__).read_text(), filename=__file__)
  except (OSError, SyntaxError) as err:
    return [f"this file could not be parsed to check its own call graph ({err})"]
  funcs = {n.name: n for n in tree.body if isinstance(n, ast.FunctionDef)}

  def called_names(fn: ast.FunctionDef) -> set[str]:
    out = set()
    for node in ast.walk(fn):
      if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
        out.add(node.func.id)
    return out

  def reads_world_directly(fn: ast.FunctionDef) -> bool:
    for node in ast.walk(fn):
      if not isinstance(node, ast.Call):
        continue
      target = node.func
      if isinstance(target, ast.Attribute) and target.attr in WORLD_METHODS:
        return True
      if isinstance(target, ast.Name) and target.id in WORLD_BUILTINS:
        return True
    return False

  # Propagate world-reading along resolvable edges until it stops growing — but NOT through the
  # exemption. `audit_containment` reads this file's own source, which is the same in a planted
  # case and a real one; tainting everything that calls it would make `selftest` its own finding,
  # which is what the first attempt at this did.
  world = {n for n, fn in funcs.items() if reads_world_directly(fn)}
  changed = True
  while changed:
    changed = False
    for name, fn in funcs.items():
      if name in world:
        continue
      if (called_names(fn) - set(CONTAINMENT_EXEMPT)) & world:
        world.add(name)
        changed = True

  def reachable(root: str) -> set[str]:
    seen, stack = set(), [root]
    while stack:
      name = stack.pop()
      if name in seen or name not in funcs:
        continue
      seen.add(name)
      stack.extend(called_names(funcs[name]))
    return seen

  from_selftest = reachable("selftest")
  if "selftest" not in funcs:
    return ["there is no `selftest` in this file, so containment compared nothing"]

  for name in sorted((world & from_selftest) - set(CONTAINMENT_EXEMPT)):
    findings.append(
      f"`{name}` reads the world and is reachable from `selftest()`, which supplies PLANTED "
      f"inputs — so real workspace state can decide a synthetic case, and a change to an unrelated "
      f"file can redden it. Split it: an `audit_` wrapper does the reading, a `check_` takes the "
      f"result as an argument."
    )

  # ── IF YOU ARE WRITING A GRAPH WALK IN THIS FILE, THIS IS THE RULE ────────────────────────
  #
  #     An edge you cannot resolve is a FINDING, not an edge you drop.
  #
  # A walk that silently skips what it cannot follow reports a conclusion about the edges it
  # happened to manage, and states it in the voice of a conclusion about the graph. That is the
  # same failure as a reader returning an empty set when it means "I could not look", which is why
  # `check_selections` says *coverage is unknown, not empty* — and it is the failure that hid the
  # defect this function was rewritten for: `reader()`, bound from `READERS[key]`, was an edge the
  # previous walk dropped without a word, so `check_selections` read three real files from inside
  # the selftest and containment reported nothing.
  #
  # Every variation of tonight's work has been a tool reporting ABSENCE when it meant
  # I COULD NOT LOOK. If your walk meets something it cannot name — a subscript call, a closure, a
  # value from a table — say so and fail. Do not continue quietly.
  for name in sorted(from_selftest):
    fn = funcs[name]
    from_subscript = {
      target.id
      for node in ast.walk(fn)
      if isinstance(node, ast.Assign) and isinstance(node.value, ast.Subscript)
      for target in ast.walk(node.targets[0])
      if isinstance(target, ast.Name)
    }
    for node in ast.walk(fn):
      if not isinstance(node, ast.Call):
        continue
      target = node.func
      if isinstance(target, ast.Subscript):
        findings.append(
          f"`{name}` (reachable from `selftest()`) calls through a subscript at line "
          f"{node.lineno}, an edge this walk cannot follow — and an unfollowable edge is how the "
          f"last world-read hid"
        )
      elif isinstance(target, ast.Name) and target.id in from_subscript:
        findings.append(
          f"`{name}` (reachable from `selftest()`) calls `{target.id}()` at line {node.lineno}, "
          f"which was bound from a dispatch table — the walk cannot follow it, and that is exactly "
          f"how `READERS[key]` smuggled three file reads into the selftest"
        )

  audits = {n for n in funcs if n.startswith("audit_")} - set(CONTAINMENT_EXEMPT)
  if not audits:
    findings.append("no `audit_*` function exists, so this check compared nothing")
  from_main = reachable("main")
  for name in sorted(audits - from_main):
    findings.append(f"`{name}` is never reached from `main()`, so it is a check that does not run")
  return findings


def selftest() -> int:
  problems: list[str] = []

  # Before any case: the selftest's own containment. A planted case that fails because an `audit_`
  # is in the call graph looks exactly like the gate being wrong, and that misreading has cost two
  # rounds.
  problems.extend(audit_containment())

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
  # EVERY INPUT IS PLANTED NOW, coverage included. These cases used to pass only `publishable` and
  # let `check_selections` read three real files through `READERS`; the case called "the honest
  # tree" was therefore half real, and a change to `ci/miri_scope.py` could redden a synthetic
  # case. `covered` is supplied here, so the cases say exactly what they are about.
  members = {"smear", "smear-lexer", "smear-parser"}
  honest = {key: set(members) for key in SELECTIONS}

  real = check_selections(members, honest)
  if real:
    problems.append(f"a fully covered tree should have no selection finding and had: {real}")

  planted_member = check_selections(members | {"zzz-planted-member"}, honest)
  if len(planted_member) != len(SELECTIONS):
    problems.append(
      f"plant (f) a publishable member outside every selection: expected one finding per "
      f"selection ({len(SELECTIONS)}) and got {len(planted_member)}: {planted_member}"
    )

  unreadable = dict(honest)
  unreadable["miri"] = "could not be read (planted: the table was renamed)"
  planted_reader = check_selections(members, unreadable)
  if not any("coverage is unknown" in f for f in planted_reader):
    problems.append(
      f"plant (g) an unreadable selection table: the reader failing must be a finding, got "
      f"{planted_reader}"
    )

  missing = {key: set(members) for key in SELECTIONS if key != "miri"}
  if not check_selections(members, missing):
    problems.append(
      "plant (h) a selection with no coverage supplied at all must be a finding, and was not"
    )

  EXEMPT_SELECTION[("miri", "no-such-member")] = "planted"
  stale = check_selections(members, honest)
  del EXEMPT_SELECTION[("miri", "no-such-member")]
  if not any("matches nothing" in f for f in stale):
    problems.append(f"plant (i) a stale selection exemption must fail, got {stale}")

  problems = [p for p in problems if p]
  if problems:
    print("::error::feature_reachability selftest: the gate does not implement its sentence")
    for p in problems:
      print(f"  - {p}")
    return 1
  print(
    "feature_reachability selftest OK: 11 cases, 6 planted defect shapes across both checks"
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
    for path in miri_scripts():
      print(f"  ok        ci/{path.name} derives its selection and re-measures its exclusions")
  execution_findings = audit_no_gate_executes_another(args.verbose)
  if execution_findings:
    print("::error::feature_reachability: a gate executes another gate")
    for f in execution_findings:
      print(f"  - {f}")
    print(
      "  Read the constant instead — `read_constant` parses it with `ast` and evaluates the "
      "literal, which is exact and drags in nothing."
    )
    return 1

  equivalence_findings = audit_equivalence(meta, tables)
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

  selection_findings = (audit_selections(publishable)
                        + audit_miri_scripts_derive()
                        + audit_miri_exclusions(publishable)
                        + audit_exempt_members(tables))
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

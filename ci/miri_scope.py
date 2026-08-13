#!/usr/bin/env python3
"""Assert that a Miri run interpreted exactly the targets it was supposed to.

Run from `ci/miri_sb.sh` and `ci/miri_tb.sh` against the log those scripts tee, immediately
after `cargo miri test`. It exists because of two defects that both cost this repository real
time and both presented as a GREEN cell:

  #73  Miri built 39 test binaries and meaningfully ran five. Every lossless integration test
       opens with `#![cfg(feature = "rowan")]` and the scripts passed no `--features`, so 33 of
       them compiled to EMPTY harnesses: the targets were built, they linked, they ran, they
       printed `running 0 tests`, and they passed. Nothing warned, because from cargo's point of
       view nothing was wrong. A silent bound reads as full coverage.

  #77  The inverse. `smear` does not default to `rowan`, but three of the four workspace members
       enable it, and cargo unifies features across the SELECTED packages — so the moment #70
       added a member the bare workspace-wide command in those scripts started resolving `smear`
       with `rowan` ON. The lossless tower reached Miri for the first time by arithmetic, and
       reported undefined behaviour inside `rowan` itself. Nobody chose the widening and nobody
       could see it had happened.

So the two failure directions are symmetric and this checks both:

  * a target this cell's build DOES compile, that ran zero tests  -> #73's silence, and
  * a target this cell's build does NOT compile, that ran anyway  -> #77's unchosen widening.

WHICH SIDE A TARGET FALLS ON IS DERIVED, never written down here. Every file in `smear/tests/`
declares its own applicability in its crate-level `#![cfg(...)]`, and that expression is parsed
and evaluated against two things the guard also derives:

  * the `smear` features a Miri cell resolves — the closure of `default` in `smear/Cargo.toml`
    plus the closure of whatever the self dev-dependency asks for, because that edge is enabled
    for every dev target. `ci/miri_sb.sh` and `ci/miri_tb.sh` pass no `--features` and no
    `--no-default-features`, so that closure IS the cell's feature set; `--cargo-features` below
    exists so a script that starts passing some can say so.
  * `cfg(miri)`, which is true here by construction and false under `cargo test`.

A hand-maintained list would be a third thing that agrees with the source on the day it is
written, which is the shape of both defects above. It is also why an unmodelled predicate — a
`target_os`, a bare `cfg` name this file does not know — is a hard ERROR rather than a guess:
a guard that silently assumes is worse than one that stops.

EXCEPT FOR ONE TABLE, AND THE EXCEPTION IS THE POINT. `MIRI_DECLARED_IGNORES` — which sources carry
a per-test `#[ignore]`, and how many each — is written down rather than derived. Deriving it would
make it unfalsifiable: a per-test `#[cfg_attr(miri, ignore)]` is invisible in the partition above,
because the target still compiles, still runs and still passes, and a derived per-target count
would simply agree with whatever the sources now say. Adding one more would move the source, move
the log, and stay green. The whole class of defect this file is about is a coverage cut that nobody
chose, and this one's signature is ADDITION, which a derivation from the same sources erases. So
the set is declared, required in BOTH directions, and its total is required to be restated in
`.github/workflows/miri.yml` — the derived check and the declared one are kept together, not
traded off.

WHY A TABLE AND NOT A TOTAL, WHICH IS WHAT THIS WAS. A frozen count answers "how many" and the
question worth asking is "WHICH", and the two came apart in the direction that mattered: the
derivation read `smear/tests/` and nothing else, so an ignore inside a selected package's LIBRARY
unit tests was outside the derived count, outside the frozen total, and outside the per-target
cross-check below — and the lib binaries' own `ignored` digits were parsed and then never compared
to anything. A guard whose answer to "may this test be skipped?" was "no ignores anywhere" was, for
the lib half of the very selection it defines, actually answering "any number of them, silently".
The declared set closes both: it is keyed by repository-relative path over BOTH roots a cell
compiles, an entry that no longer matches its file is a finding, and a file with an ignore and no
entry is the finding this file exists for.

Exit 0 on agreement, 1 on any disagreement. The excluded set is printed on every run, pass or
fail, with the reason each target is on it, because "what this matrix does not cover" belongs in
the log rather than in a reader's assumption.
"""

from __future__ import annotations

import argparse
import contextlib
import io
import json
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile

# ── THIS SCRIPT NEEDS PYTHON >= 3.11, AND SAYS SO RATHER THAN TRACEBACKING ──────────────────
#
# `tomllib` entered the standard library in 3.11. This script parses `smear/Cargo.toml` to resolve
# which features a Miri cell compiles with, and a hand-rolled TOML reader is exactly the shape this
# repository has already been burned by — a checker written against one exemplar that was wrong on
# five of six valid forms. So the floor is real and stays.
#
# What was NOT acceptable was the floor being silent. `ci/feature_reachability.py` used to reach
# this file's `MIRI_PACKAGES` by importing it, which inherited this line: on the macOS system
# interpreter (`/usr/bin/python3`, 3.9.6) that gate exited 1 with `No module named 'tomllib'`
# before reading anything, while the workflow and its own usage line both said `python3`. That
# reader now parses the constant instead of executing this module, and this guard makes the
# remaining floor a sentence instead of a traceback.
#
# `ci/miri_sb.sh` and `ci/miri_tb.sh` — the only callers — check the same floor before they spend
# minutes on a Miri run. The other Python gates (`feature_reachability.py`, `downstream_pairs.py`)
# are deliberately stdlib-3.9-safe and are verified on 3.9.6, because they are the ones a person is
# told to run locally.
try:
  import tomllib
except ModuleNotFoundError as err:  # pragma: no cover - depends on the interpreter, not the tree
  raise SystemExit(
    f"::error::miri_scope needs Python >= 3.11 for `tomllib` and this is "
    f"{sys.version_info.major}.{sys.version_info.minor} ({err}). It parses `smear/Cargo.toml` to "
    f"resolve a Miri cell's feature set; a hand-rolled TOML reader is not an acceptable "
    f"substitute. On macOS, `/usr/bin/python3` is 3.9 — use a newer one."
  ) from err

# `.github/workflows/miri.yml` sets `CARGO_TERM_COLOR: always`, so cargo emits SGR escapes even
# though nothing here is a tty: `\x1b[1m\x1b[32m     Running\x1b[0m tests/oracle.rs (...)`. Every
# line is stripped before it is matched. Without this the two patterns below match nothing at all,
# and the guard would report a perfectly good run as having interpreted no targets — a check
# defeated by the format of its own input.
ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")

# `     Running tests/oracle.rs (/path/to/binary)` and
# `     Running unittests src/lib.rs (/path/to/binary)`.
RUNNING = re.compile(r"^\s+Running (?:unittests )?(\S+)")
COUNT = re.compile(r"^running (\d+) tests?$")
# `test result: ok. 4 passed; 0 failed; 2 ignored; 0 measured; 0 filtered out; finished in 1.2s`
IGNORED = re.compile(r"^test result:.*?(\d+) ignored")
# A real `#[cfg_attr(miri, ignore ...)]` — anchored at column 0 modulo indentation so the several
# mentions of the construct in these files' module docs, which begin `//!`, cannot be counted. The
# attribute is written across four lines, so `\s` has to be allowed to cross them.
MIRI_IGNORE = re.compile(r"^\s*#\[cfg_attr\(\s*miri\s*,\s*ignore", re.M)
# Any other `#[ignore]`, which is NOT a Miri decision and must still be accounted for, or the
# cross-check below would read one as the other.
PLAIN_IGNORE = re.compile(r"^\s*#\[ignore", re.M)
# A test file's crate-level `#![cfg(...)]`. Anchored to column 0 with `^`, which is what makes
# this a CRATE-level attribute rather than any inner attribute: a `mod x { #![cfg(...)] }` is
# indented and must not be read as gating the whole target. Non-greedy up to the closing `)]` so
# a nested `all(...)` / `any(...)` is captured whole, and `re.S` so it may span lines.
INNER_CFG = re.compile(r"^#!\[cfg\((.*?)\)\]", re.S | re.M)

# Bare `cfg` names whose value is fixed inside a `cargo miri test` cell. `miri` is what makes the
# two wall-clock exclusions below expressible in the source file rather than in a list here, and
# `test` is set by the `--test` harness every one of these targets is built with. Anything absent
# from this table is unmodelled and stops the guard; see the module header.
CELL_FLAGS = {"miri": True, "test": True, "docsrs": False, "tarpaulin": False}

# Why a target that this cell does not compile is not covered. Keyed by the feature whose absence
# excludes it; the fallback names the features and says nothing it cannot support.
ROWAN_REASON = (
    "gated on `rowan`, which cannot be interpreted: rowan 0.17.0 has undefined behaviour under "
    "Stacked Borrows in `src/arc.rs` and under Tree Borrows in `src/cursor.rs`, live in every "
    "published version through 0.17.0 (al8n/smear#77, rust-analyzer/rowan#108 and #192)"
)
MIRI_REASON = (
    "the file excludes itself from Miri with `not(miri)`, on wall clock: measured on CI run "
    "31318425279, `tests/syntactic_span_extent.rs` had not finished after 5h40m and the cell was "
    "killed there. The file's own header carries the decision and what it costs (al8n/smear#141)"
)

# Resolved from this file, not from the process's cwd. `ci/miri_sb.sh` and `ci/miri_tb.sh` run
# from the repository root, but the budget check below reads a file that no argument to this
# script names, and a check that silently reads nothing when it is invoked from somewhere else
# would pass forever — which is the shape of every defect in the header above.
REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW = REPO_ROOT / ".github" / "workflows" / "miri.yml"

# WHICH SOURCES A MIRI CELL MAY SKIP TESTS IN, AND HOW MANY IN EACH.
#
# A FROZEN TABLE, and deliberately not derived. `ignore_sources()` below counts the `#[ignore]`
# attributes in the files a cell compiles and `check()` requires each binary's run to report the
# same number, which catches a RUN that disagrees with the tree. What that cannot catch is the tree
# moving: add one more `#[cfg_attr(miri, ignore)]` and the source and the log agree on the new
# number, every per-binary check passes, and a coverage cut has ratified itself. An expectation
# derived from the sources it constrains erases precisely the case whose signature is addition.
#
# So the set is written down instead. Every per-test ignore is a deliberate coverage cut whose cost
# is recorded in its file's header, and CHANGING THIS TABLE IS THE DECISION BEING RECORDED: the
# diff to these lines is the thing a reviewer is meant to see. Move it in the same commit that adds
# or removes the ignore, and restate the total in `.github/workflows/miri.yml`, which is required
# below so the header cannot drift away from the guard.
#
# KEYED BY FILE RATHER THAN SUMMED, because a total cannot say where. It also cannot say where it
# is not looking: the previous revision was a bare `4`, counted out of `smear/tests/` alone, so an
# ignore in a selected package's `src/` was invisible to the count AND to the frozen number AND to
# the run cross-check, which never looked at a lib binary's `ignored` digit at all. The keys below
# span both roots and `ignore_sources()` derives from both, so "no undeclared ignore" is now a
# statement about everything a cell builds rather than about half of it.
#
# A plain `#[ignore]` counts here too. It is not a Miri decision, but it is equally a test that
# does not run in a Miri cell, and leaving it out would give the next coverage cut a spelling that
# this table does not see.
#
#   smear/tests/syntactic_span_extent.rs     the two corpus sweeps; `trivia_injection_leaves_
#   smear/tests/syntactic_x_span_extent.rs   every_span_on_its_own_tokens` spent 5h40m inside
#                                            itself on CI run 31318425279 and hit the six-hour job
#                                            ceiling. Each file's header carries the measurement.
#   graphql-proto/src/response/tests.rs      `a_deep_path_costs_its_depth_in_links_and_not_its_
#                                            square`, a COST gate: it asserts slot traversals and
#                                            allocator calls at depth 1 024, which is a claim about
#                                            the shape of the work rather than about its soundness,
#                                            and an interpreter answers the other question out of
#                                            MIR the file's shallow legs already cover. Neither it
#                                            nor its neighbour `execute::tests::a_flat_fragment_
#                                            chain_is_linear` returned inside ten minutes under
#                                            `cargo miri test`. The attribute's own `ignore = "..."`
#                                            reason carries the argument.
MIRI_DECLARED_IGNORES = {
    "smear/tests/syntactic_span_extent.rs": 2,
    "smear/tests/syntactic_x_span_extent.rs": 2,
    "graphql-proto/src/response/tests.rs": 1,
}

# The same decision as one number, because `.github/workflows/miri.yml` states a total and every
# run prints one. DERIVED from the table above and never written twice, so the sentence a reader is
# given and the set the guard enforces cannot say different things.
MIRI_IGNORE_BUDGET = sum(MIRI_DECLARED_IGNORES.values())

# PROPERTY: every lib unit test in a publishable member is interpreted by both Miri cells, or its
# member is recorded here as carrying none at this feature set.
#
# THE PACKAGES THE MIRI CELLS RUN. Not a description of them — the source of them: `ci/miri_sb.sh`
# and `ci/miri_tb.sh` build their `-p` list by asking `--print-packages` for this tuple.
#
# THAT IS THE REPAIR, and it is worth stating what it replaced. This tuple and the scripts' `-p`
# arguments were two hand-maintained lists. The split grew this one to six members as it extracted
# them and never touched the other, so the scripts ran two while the guard below expected six — and
# the guard reads THIS list, so it could pass against a declaration the workflow did not execute,
# leaving the cell to fail late, after the expensive run. Deriving deletes the second list instead
# of adding a check that compares them, and `ci/feature_reachability.py` fails if either script
# stops asking or starts passing a literal `-p`.
#
# It was one package until the crate split, and the check below was "exactly one lib unit-test
# binary ran" — a shape that would have stayed GREEN while `smear-lexer` carried 130 lib unit
# tests, and all four of this project's own `unsafe` sites, out of the selection. Both scripts'
# headers claim the covered half is "where this project's own `unsafe` lives"; these names are what
# makes that claim falsifiable rather than decorative.
#
# ADDING A PACKAGE IS A DECISION WITH A COST, because feature unification is over the SELECTED
# packages and that is the mechanism #77 turned on: `smear-smoke` enabling `rowan` is how the
# entire lossless tower entered a Miri cell that nobody widened. Checked for each of the four:
# `smear-lexer` declares no `rowan` feature at all; `smear-parser` and `smear-compiler` do declare
# one but it is not in their `default` and these scripts pass no `--features`, so the resolve
# leaves it off; `graphql-proto` declares only `std`. This file's own feature-set guard prints what
# actually resolved, so none of that is taken on trust.
#
# `ci/feature_reachability.py` requires every publishable member to be in this tuple or in
# `MIRI_NOT_SELECTED` below, so the account grows with the workspace instead of being remembered.
MIRI_PACKAGES = ("smear-lexer", "smear-parser", "smear-schema", "smear-compiler", "graphql-proto")

# The publishable member deliberately NOT selected, and the reason is now a BEHAVIOUR rather than a
# count.
#
# `smear-schema` used to be here, excused by "0 lib unit tests at the feature set these scripts
# build" — measured with an isolated `cargo test -p smear-schema --lib`, which is not that feature
# set. Under the resolve the cells actually build, `smear-compiler` takes `smear-schema` WITH
# `build`, and behind `build` it has three lib unit tests. Measured: the four-member selection lists
# 505 tests and the five-member one 508. Those three were interpreted by nothing, and an isolated
# re-measurement said zero every time. Feature unification decides the answer, and the measurement
# was being taken somewhere unification does not apply — the third time on this branch.
#
# `smear` cannot be selected at all, and that is a stronger fact than a test count. Co-selecting it
# with `smear-compiler` does not COMPILE: the compiler forces `smear-schema/build` on, `smear` at
# default features has `validator` off, and the equivalence assertion in `smear/src/lib.rs` refuses
# the graph. Measured: `cargo test -p smear -p smear-compiler --lib` exits 101 on
# `smear-schema/build` and `smear/validator` disagree. It also has no lib unit tests at any feature
# set, but that is now the second reason rather than the only one.
MIRI_NOT_SELECTED = {
    "smear": {
        "outcome": "forbidden",
        "why": "co-selecting the umbrella with `smear-compiler` does not compile — the compiler "
               "forces `smear-schema/build`, the umbrella at default features has `validator` off, "
               "and `smear/src/lib.rs`'s equivalence assertion refuses that graph. It carries no "
               "lib unit tests at any feature set either",
    },
}


def verify_exclusions(cargo: str = "cargo") -> int:
    """Re-run every `MIRI_NOT_SELECTED` reason UNDER THE RESOLVE THE CELLS BUILD.

    PROPERTY: a member is excluded only while, in the graph the Miri cells actually resolve, it
    either carries no lib unit tests or cannot be co-selected at all.

    THE FIRST VERSION MEASURED IN ISOLATION AND THAT WAS THE DEFECT. It ran
    `cargo test -p <member> --lib` with the entry's own flags, which is a different graph from the
    one the cells build: `smear-compiler` is selected there and takes `smear-schema` WITH `build`,
    so `smear-schema` had three lib unit tests that no selected root interpreted while an isolated
    re-measurement returned zero, every time. Feature unification decides this, and the measurement
    was taken where unification does not apply.

    So each excluded member is now added to the REAL selection and built there:

      outcome "empty"      it must co-select and produce a harness with 0 tests
      outcome "forbidden"  it must fail to build, and fail on the equivalence assertion — a member
                           that starts building again has an exclusion whose reason has gone

    Counting is per binary rather than by total, so a member's own harness is what is read and not
    a difference between two runs that unification could move underneath.
    """
    selection = [arg for package in MIRI_PACKAGES for arg in ("-p", package)]
    failures = []
    for member, entry in sorted(MIRI_NOT_SELECTED.items()):
        outcome = entry.get("outcome")
        command = [cargo, "test", *selection, "-p", member, "--lib", "--no-run",
                   "--message-format=json"]
        printed = " ".join(command)
        result = subprocess.run(command, capture_output=True, text=True)

        if outcome == "forbidden":
            if result.returncode == 0:
                failures.append(
                    f"{member}: `{printed}` now BUILDS, and the exclusion says it cannot. The "
                    f"reason has gone stale — either it is selectable now and belongs in "
                    f"MIRI_PACKAGES, or the recorded reason describes something else.\n"
                    f"      recorded reason: {entry['why']}"
                )
            # BOTH STREAMS. With `--message-format=json` the rendered diagnostic is a field of a
            # JSON object on STDOUT and only cargo's summary reaches stderr, so checking stderr
            # alone reported "failed for some other reason" about the very assertion it was
            # looking for.
            elif "disagree" not in (result.stdout + result.stderr):
                failures.append(
                    f"{member}: `{printed}` failed, but not on the equivalence assertion the "
                    f"reason names — so it is excluded for some other reason nobody has "
                    f"written down.\n{result.stderr.strip()[-500:]}"
                )
            else:
                print(f"  {member:16} cannot be co-selected, on the equivalence assertion — as recorded")
            continue

        if outcome != "empty":
            failures.append(
                f"{member}: `outcome` is {outcome!r}, which this check cannot measure. Use "
                f"'empty' (must co-select with a 0-test harness) or 'forbidden' (must not build)."
            )
            continue

        if result.returncode != 0:
            failures.append(
                f"{member}: `{printed}` exited {result.returncode}, so the claim could not be "
                f"measured at all\n{result.stderr.strip()[-500:]}"
            )
            continue
        executable = None
        for line in result.stdout.splitlines():
            try:
                message = json.loads(line)
            except json.JSONDecodeError:
                continue
            if message.get("reason") != "compiler-artifact" or not message.get("executable"):
                continue
            if f"/{member}-" in message["executable"] or f"/{member}/" in message["executable"]:
                executable = message["executable"]
        if executable is None:
            failures.append(
                f"{member}: the build produced no test executable for it, so nothing was measured"
            )
            continue
        listed = subprocess.run([executable, "--list"], capture_output=True, text=True)
        count = len(re.findall(r"^\S+: test$", listed.stdout, re.M))
        print(f"  {member:16} {count:>4} lib unit tests under the cells' resolve")
        if count:
            failures.append(
                f"{member}: {count} lib unit test(s) UNDER THE RESOLVE THE CELLS BUILD, and "
                f"MIRI_NOT_SELECTED excludes it on the claim that it has none. Those tests are "
                f"interpreted by nothing. Add it to MIRI_PACKAGES.\n"
                f"      recorded reason: {entry['why']}"
            )
    if failures:
        print("::error::miri_scope: an exclusion from the Miri selection is no longer true",
              file=sys.stderr)
        for failure in failures:
            print(f"  - {failure}", file=sys.stderr)
        return 1
    print(f"miri_scope: {len(MIRI_NOT_SELECTED)} exclusion(s) re-measured under the cells' "
          f"resolve, all still true")
    return 0


# The three ways the budget can be wrong, named once so `check()` and `selftest()` cannot drift
# apart on what they are calling them.
BUDGET_OVER = "no per-test `#[ignore]` was added without declaring it"
BUDGET_UNDER = "no per-test `#[ignore]` was removed without undeclaring it"
BUDGET_STATED = "`.github/workflows/miri.yml` restates the declared total"

# Where `miri.yml`'s header states that same number. Each entry is (what the site is, a pattern
# with `{n}` standing in for the budget).
#
# The count-list entry reads `#    4  individual tests`: the number is one row of a `37 / 9 / 4`
# breakdown and carries that table's column spacing, so a pattern spelling the literal string
# `4 individual tests` matches nothing in the file and passes forever — the exact defect this
# check exists to close, rebuilt inside the check. The spacing is therefore matched as spacing,
# and `--selftest` proves both patterns fire rather than trusting that they do.
WORKFLOW_BUDGET_SITES = (
    ("the summary sentence above the list", r"\band {n} of the tests inside the\b"),
    ("the count-list entry", r"^#[ \t]+{n}[ \t]+individual tests\b"),
)


class CfgError(Exception):
    """A `#![cfg(...)]` this guard cannot parse, or a predicate it does not model."""


def _tokens(text: str) -> list[tuple[str, str]]:
    """`all(feature = "std", not(miri))` -> a flat token list, or raise."""
    out: list[tuple[str, str]] = []
    pos, end = 0, len(text)
    while pos < end:
        ch = text[pos]
        if ch.isspace():
            pos += 1
            continue
        if ch in "(),=":
            out.append(("punct", ch))
            pos += 1
            continue
        if ch == '"':
            close = text.find('"', pos + 1)
            if close < 0:
                raise CfgError(f"unterminated string in cfg: {text!r}")
            out.append(("str", text[pos + 1 : close]))
            pos = close + 1
            continue
        match = re.match(r"[A-Za-z_][A-Za-z0-9_-]*", text[pos:])
        if not match:
            raise CfgError(f"cannot tokenize {text[pos:]!r} in cfg: {text!r}")
        out.append(("ident", match.group(0)))
        pos += match.end()
    return out


def _parse(tokens: list[tuple[str, str]], i: int, text: str) -> tuple[tuple, int]:
    if i >= len(tokens) or tokens[i][0] != "ident":
        raise CfgError(f"expected a cfg predicate at position {i} in {text!r}")
    name = tokens[i][1]
    i += 1
    if i < len(tokens) and tokens[i] == ("punct", "("):
        if name not in ("all", "any", "not"):
            raise CfgError(f"unmodelled cfg combinator `{name}(...)` in {text!r}")
        i += 1
        args: list[tuple] = []
        while True:
            if i < len(tokens) and tokens[i] == ("punct", ")"):
                i += 1
                break
            node, i = _parse(tokens, i, text)
            args.append(node)
            if i < len(tokens) and tokens[i] == ("punct", ","):
                i += 1
                continue
            if i < len(tokens) and tokens[i] == ("punct", ")"):
                i += 1
                break
            raise CfgError(f"expected `,` or `)` at position {i} in {text!r}")
        if name == "not" and len(args) != 1:
            raise CfgError(f"`not` takes exactly one predicate in {text!r}")
        if name in ("all", "any") and not args:
            raise CfgError(f"empty `{name}()` in {text!r}")
        return (name, args), i
    if i < len(tokens) and tokens[i] == ("punct", "="):
        i += 1
        if i >= len(tokens) or tokens[i][0] != "str":
            raise CfgError(f"expected a string after `{name} =` in {text!r}")
        value = tokens[i][1]
        i += 1
        return ("kv", name, value), i
    return ("flag", name), i


def parse_cfg(text: str) -> tuple:
    tokens = _tokens(text)
    node, i = _parse(tokens, 0, text)
    if i != len(tokens):
        raise CfgError(f"trailing tokens after the cfg expression in {text!r}")
    return node


def eval_cfg(node: tuple, features: set[str]) -> bool:
    """Evaluate a parsed `cfg` under this cell's feature set. Unmodelled predicates raise."""
    tag = node[0]
    if tag == "all":
        return all(eval_cfg(arg, features) for arg in node[1])
    if tag == "any":
        return any(eval_cfg(arg, features) for arg in node[1])
    if tag == "not":
        return not eval_cfg(node[1][0], features)
    if tag == "kv":
        key, value = node[1], node[2]
        if key == "feature":
            return value in features
        raise CfgError(
            f'unmodelled cfg key `{key} = "{value}"`. This guard decides what a Miri cell '
            "compiles; a predicate it cannot evaluate is a blind spot, so add it to `eval_cfg` "
            "rather than letting the classification be a guess"
        )
    if tag == "flag":
        name = node[1]
        if name in CELL_FLAGS:
            return CELL_FLAGS[name]
        raise CfgError(
            f"unmodelled bare cfg `{name}`. Add it to `CELL_FLAGS` with the value it has inside "
            "a `cargo miri test` cell, or this guard is guessing"
        )
    raise CfgError(f"unreachable cfg node: {node!r}")


def render(node: tuple) -> str:
    """A parsed cfg back as source, so a reason can name the predicate that refused."""
    tag = node[0]
    if tag in ("all", "any", "not"):
        return f"{tag}({', '.join(render(arg) for arg in node[1])})"
    if tag == "kv":
        return f'{node[1]} = "{node[2]}"'
    return node[1]


def unsatisfied(node: tuple, features: set[str]) -> list[str]:
    """The atoms that are false under this cell, for the printed exclusion reason.

    Recurses only where recursion narrows the answer. Under `any(...)` every branch is false or
    the node would not be here, so all of them are named; under `not(...)` the SUBTREE is true and
    naming its atoms would say the opposite of what happened, so the `not` is named whole.
    """
    if eval_cfg(node, features):
        return []
    tag = node[0]
    if tag in ("all", "any"):
        return [atom for arg in node[1] for atom in unsatisfied(arg, features)]
    return [render(node)]


def resolved_features(manifest: pathlib.Path, extra: list[str], defaults: bool) -> set[str]:
    """The `smear` features a Miri cell resolves, read out of `smear/Cargo.toml`.

    Two seeds, and both are load-bearing. `default`, because the scripts pass no
    `--no-default-features`; and whatever the crate's dev-dependency on ITSELF asks for, because
    that edge exists precisely so `test-support` is on for every dev target and off for every
    consumer — so a test file gated on it compiles here and the guard must know that.

    The closure is over this package's own feature table only: `dep:x` and `x/y` entries turn on
    something in another package and cannot satisfy a `feature = "..."` in `smear/tests/`.
    """
    data = tomllib.loads(manifest.read_text(encoding="utf-8"))
    table: dict[str, list[str]] = data.get("features", {})
    if not table:
        raise CfgError(f"{manifest} declares no [features] table")
    name = data["package"]["name"]
    seeds = list(extra)
    if defaults and "default" in table:
        seeds.append("default")
    self_dev = data.get("dev-dependencies", {}).get(name)
    if isinstance(self_dev, dict):
        seeds.extend(self_dev.get("features", []))

    out: set[str] = set()
    stack = list(seeds)
    while stack:
        feature = stack.pop()
        if feature in out:
            continue
        out.add(feature)
        for entry in table.get(feature, []):
            if entry.startswith("dep:") or "/" in entry:
                continue
            stack.append(entry)
    return out


def ignores_in(path: pathlib.Path) -> int:
    """How many of one file's tests a Miri cell will skip, by both spellings."""
    text = path.read_text(encoding="utf-8")
    return len(MIRI_IGNORE.findall(text)) + len(PLAIN_IGNORE.findall(text))


def repo_relative(path: pathlib.Path) -> str:
    """A path as `MIRI_DECLARED_IGNORES` spells it, whatever the caller passed.

    The keys are repository-relative and the scripts run from the repository root, so this is
    usually the identity — but `--tests-dir` accepts an absolute path, and a table compared against
    absolute keys would match nothing and report the whole tree as undeclared.
    """
    try:
        return path.resolve().relative_to(REPO_ROOT).as_posix()
    except ValueError:
        return path.as_posix()


def declared_ignores(tests_dir: pathlib.Path) -> dict[str, int]:
    """Per integration target, how many of its tests the SOURCE says a Miri cell will skip.

    A whole target excluded by its `#![cfg(...)]` is loud — it vanishes from the covered list and
    `check` prints it with a reason. A single test carrying `#[cfg_attr(miri, ignore)]` is not: it
    becomes one digit in `2 ignored` that nobody reads, which is #73's mechanism one level down.
    So the count is derived from the files and required to match what the run reports.

    Keyed by target stem, because that is what a `Running tests/<stem>.rs` line gives.
    [`declared_lib_ignores`] is the same question for the other half of what a cell builds, keyed
    by package, and [`ignore_sources`] is both of them keyed by file for the declared-set check.
    """
    return {path.stem: ignores_in(path) for path in sorted(tests_dir.glob("*.rs"))}


def declared_lib_ignores() -> dict[str, int]:
    """Per selected package, how many of its LIB unit tests the source says a cell will skip.

    The half that was not being asked. Cargo spells every package's unit-test binary the same way,
    so the `ignored` digit each one prints was parsed and then compared to nothing at all — a
    `#[cfg_attr(miri, ignore)]` on a lib test moved no number this guard read. The whole `src/` tree
    is walked rather than a list of files, because the point is to see one that nobody declared.
    """
    return {
        package: sum(
            ignores_in(path)
            for path in sorted((REPO_ROOT / package / "src").rglob("*.rs"))
        )
        for package in MIRI_PACKAGES
    }


def ignore_sources(tests_dir: pathlib.Path, compiled: list[str]) -> dict[str, int]:
    """Every source a Miri cell compiles that carries a per-test `#[ignore]`, and how many.

    Keyed the way `MIRI_DECLARED_IGNORES` is keyed, because the two are compared entry by entry.

    TWO ROOTS, and both are what a cell builds:

      * the integration targets this cell's feature set COMPILES — an ignore inside a target the
        cell does not build changes no coverage here, and a target leaving the compiled set takes
        its ignores with it, which is itself a coverage change and shows up as the table
        disagreeing;
      * every selected package's `src/`, whose lib unit tests are the largest thing a cell
        interprets and were entirely outside this accounting until the table replaced the total.

    Benches and examples are in neither, because no Miri cell builds them.
    """
    out: dict[str, int] = {}
    for stem in compiled:
        path = tests_dir / f"{stem}.rs"
        count = ignores_in(path)
        if count:
            out[repo_relative(path)] = count
    for package in MIRI_PACKAGES:
        for path in sorted((REPO_ROOT / package / "src").rglob("*.rs")):
            count = ignores_in(path)
            if count:
                out[repo_relative(path)] = count
    return out


def package_of(binary: str) -> str | None:
    """Which selected package a `Running unittests src/lib.rs (<path>)` line belongs to.

    Cargo names a lib test binary `<package with `-` spelled `_`>-<hash>`, and that file name is
    the ONLY thing distinguishing one package's unit-test binary from another's in the log — the
    text before the parentheses is identical for all of them. `None` for a binary that names no
    selected package, which is a finding rather than something to skip past.
    """
    stem = pathlib.Path(binary).name.rsplit("-", 1)[0]
    for package in MIRI_PACKAGES:
        if stem == package.replace("-", "_"):
            return package
    return None


def budget_findings(sources: dict[str, int]) -> dict[str, str]:
    """The declared-set checks, as {case name: finding} for the cases that FAILED.

    Keyed rather than listed because `--selftest` has to show each one failing on its own, and a
    case that cannot be named separately cannot be proven separately. The counterpart to the
    per-binary cross-check in `check()`: that asks whether the RUN matches the tree, and this asks
    whether the tree still matches the decision, which is the half nothing constrained.

    `sources` is what `ignore_sources()` derived — every file a cell compiles that carries an
    ignore. The finding names the FILE in both directions, because "the total moved" sent a reader
    looking for a number and what they need is the line.
    """
    out: dict[str, str] = {}
    over = sorted(
        f"{path} carries {count} and the table declares {MIRI_DECLARED_IGNORES.get(path, 0)}"
        for path, count in sources.items()
        if count > MIRI_DECLARED_IGNORES.get(path, 0)
    )
    under = sorted(
        f"{path} carries {sources.get(path, 0)} and the table declares {count}"
        for path, count in MIRI_DECLARED_IGNORES.items()
        if sources.get(path, 0) < count
    )
    if over:
        out[BUDGET_OVER] = (
            "a source a Miri cell compiles carries more `#[ignore]`s than "
            "`ci/miri_scope.py`'s `MIRI_DECLARED_IGNORES` declares for it: "
            + "; ".join(over)
            + ". Skipping a test under Miri is a coverage cut, so adding one is a decision and not "
            "bookkeeping: if it is intended, move the entry in the same commit, restate the new "
            f"total in `{WORKFLOW.name}`, and say in the test's own `ignore = \"...\"` reason what "
            "the cut costs."
        )
    if under:
        out[BUDGET_UNDER] = (
            "`ci/miri_scope.py`'s `MIRI_DECLARED_IGNORES` declares more `#[ignore]`s than the "
            "sources carry: "
            + "; ".join(under)
            + ". Either an ignore was removed and this table is stale — drop the entry and restate "
            f"the new total in `{WORKFLOW.name}` — or the file moved out of what a cell compiles, "
            "which is itself a coverage change and has to be recorded as one."
        )

    try:
        header = WORKFLOW.read_text(encoding="utf-8")
    except OSError as err:
        out[BUDGET_STATED] = (
            f"{WORKFLOW} could not be read ({err}), so this guard cannot tell whether the "
            "workflow header still states the budget"
        )
        return out
    absent: list[tuple[str, str]] = []
    for site, pattern in WORKFLOW_BUDGET_SITES:
        expected = pattern.replace("{n}", str(MIRI_IGNORE_BUDGET))
        if not re.search(expected, header, re.M):
            absent.append((site, expected))
    if absent:
        out[BUDGET_STATED] = (
            f"`{WORKFLOW.name}` no longer states a budget of {MIRI_IGNORE_BUDGET} at "
            + "; ".join(f"{site} (`{pattern}`)" for site, pattern in absent)
            + ". That header is where a reader is told what the matrix does not cover, so it may "
            "not disagree with the number the guard enforces. Update the prose, or the budget, "
            "until they say the same thing"
        )
    return out


def partition(
    tests_dir: pathlib.Path, features: set[str]
) -> tuple[dict[str, str], list[str]]:
    """Split `smear/tests/*.rs` into ({excluded target: why}, [compiled targets])."""
    excluded: dict[str, str] = {}
    compiled: list[str] = []
    for path in sorted(tests_dir.glob("*.rs")):
        # Whole file, not a prefix window: several of these open with a module doc comment long
        # enough that a fixed window would miss the attribute below it, and misclassifying a
        # gated target as un-gated would fail the cell with the wrong explanation.
        match = INNER_CFG.search(path.read_text(encoding="utf-8"))
        if match is None:
            compiled.append(path.stem)
            continue
        node = parse_cfg(match.group(1))
        if eval_cfg(node, features):
            compiled.append(path.stem)
            continue
        missing = sorted(set(unsatisfied(node, features)))
        if not missing:
            raise CfgError(
                f"{path.name}: `{match.group(1)}` evaluates false but names no unsatisfied "
                "predicate, so this guard cannot say why the target is excluded"
            )
        if any('"rowan"' in atom for atom in missing):
            excluded[path.stem] = ROWAN_REASON
        elif "not(miri)" in missing:
            excluded[path.stem] = MIRI_REASON
        else:
            excluded[path.stem] = (
                f"{', '.join(missing)} — this cell resolves `smear`'s default features and "
                "nothing else, so the target compiles to an empty harness here"
            )
    return excluded, compiled


def ran_counts(log: pathlib.Path):
    """Read the log into (targets -> tests, lib binaries in order, target -> ignored,
    lib binary -> ignored).

    Integration targets and lib binaries are kept apart because cargo spells every package's
    unit-test binary `Running unittests src/lib.rs (<path>)` — the same text for all of them,
    distinguished only by the path in parentheses. Collapsing them onto one key silently keeps
    whichever ran first, and on a workspace-wide selection that is a coin toss between packages:
    the CI log of run 31073592771 has two, `smear`'s and the since-dissolved
    `smear-apollo-bench`'s, both empty. So they are returned as a LIST and the caller requires one
    per selected package.

    The SAME collapse used to swallow the lib binaries' `ignored` digits — one key, first writer
    wins, and no reader — so those are returned keyed by binary path, which is the only thing that
    tells one of them from another.
    """
    counts: dict[str, int] = {}
    libs: list[tuple[str, int]] = []
    ignored: dict[str, int] = {}
    lib_ignored: dict[str, int] = {}
    current: str | None = None
    binary: str | None = None
    current_path: str = ""
    for raw in log.read_text(encoding="utf-8", errors="replace").splitlines():
        line = ANSI.sub("", raw)
        running = RUNNING.match(line)
        if running:
            target = running.group(1)
            binary = "<lib>" if target.endswith("lib.rs") else pathlib.Path(target).stem
            current = binary
            tail = line.split("(", 1)
            current_path = tail[1].rstrip(") ") if len(tail) == 2 else "<unknown path>"
            continue
        count = COUNT.match(line.strip())
        if count and current is not None:
            n = int(count.group(1))
            if current == "<lib>":
                libs.append((current_path, n))
            else:
                # A binary reports `running N tests` once; keep the first, so a `--no-capture`
                # re-run appended to the same log cannot overwrite it.
                counts.setdefault(current, n)
            current = None
            continue
        skipped = IGNORED.match(line.strip())
        if skipped and binary is not None:
            if binary == "<lib>":
                lib_ignored.setdefault(current_path, int(skipped.group(1)))
            else:
                ignored.setdefault(binary, int(skipped.group(1)))
    return counts, libs, ignored, lib_ignored


def selftest(tests_dir: pathlib.Path, manifest: pathlib.Path) -> int:
    """Prove every branch of `check` can fail, against the REAL test tree.

    A guard that cannot fail is the defect this guard exists to catch, so this runs first in
    `ci/miri_sb.sh` and `ci/miri_tb.sh` — before `rustup`, before `cargo miri setup`, before
    anything that costs minutes. The logs below are synthesised, but the partition is read from
    `smear/tests/` and `smear/Cargo.toml` exactly as a real run reads it: a case is stated in
    terms of "some excluded target" and "some compiled target", so it keeps meaning when a test
    file is added.
    """
    try:
        features = resolved_features(manifest, [], True)
        excluded, compiled = partition(tests_dir, features)
    except CfgError as err:
        print(f"::error::miri_scope selftest: {err}")
        return 1
    if not excluded or not compiled:
        print("::error::miri_scope selftest: the test tree has no excluded targets, or no "
              f"compiled ones ({len(excluded)} / {len(compiled)}), so the cases below would not "
              "exercise what they name")
        return 1
    if "rowan" in features:
        print("::error::miri_scope selftest: `rowan` resolves ON from `smear/Cargo.toml`'s own "
              "defaults, so this guard would let the lossless tower into a Miri cell — the "
              "condition #77 was about, seen before a single test runs")
        return 1
    a_excluded, a_compiled = sorted(excluded)[0], sorted(compiled)[0]

    declared = declared_ignores(tests_dir)
    lib_declared = declared_lib_ignores()

    # A `lib:<package>` entry is a lib unit-test binary and anything else is an integration target.
    # A PREFIX and not a bare package name, because a test target is free to be called
    # `graphql-proto` and the two would then be the same key — the collapse this guard has already
    # been bitten by once, rebuilt inside its own selftest.
    def log(entries: list[tuple[str, int | None]],
            ignored_override: dict[str, int] | None = None) -> str:
        out = []
        for name, count in entries:
            package = name[4:] if name.startswith("lib:") else None
            if package is None:
                where, binary = f"tests/{name}.rs", name
            else:
                where, binary = "unittests src/lib.rs", package.replace("-", "_")
            out.append(f"     Running {where} (/tmp/build/deps/{binary}-deadbeef)")
            if count is not None:
                if ignored_override and name in ignored_override:
                    skip = ignored_override[name]
                elif package is None:
                    skip = declared.get(name, 0)
                else:
                    skip = lib_declared.get(package, 0)
                out.append(f"running {count} tests")
                out.append(
                    f"test result: ok. {count - skip} passed; 0 failed; {skip} ignored; "
                    "0 measured; 0 filtered out; finished in 0.1s"
                )
        return "\n".join(out) + "\n"

    # Every compiled target's synthetic count has to leave at least one test running after its
    # declared `#[ignore]`s, or the "all of them are ignored" branch fires on the clean case.
    floor = max(declared.get(n, 0) for n in compiled) + 1
    # A compiled target that actually declares a Miri skip, so the two per-test cases below name a
    # real one rather than a hypothetical. If none does, they still work: `a_declared` is 0 and the
    # first case moves a target from 0 ignored to 1, which is the same finding.
    a_skipping = next((n for n in sorted(compiled) if declared.get(n, 0)), a_compiled)
    a_declared = declared.get(a_skipping, 0)
    # And the same on the library side, which is the half the declared set was introduced for.
    a_lib_skipping = next((p for p in MIRI_PACKAGES if lib_declared.get(p)), MIRI_PACKAGES[0])
    a_lib_declared = lib_declared.get(a_lib_skipping, 0)
    # One lib binary per selected package, which is what a correct run produces.
    libs_clean = [(f"lib:{package}", 400 - 10 * i)
                  for i, package in enumerate(MIRI_PACKAGES)]
    clean = (libs_clean
             + [(n, 0) for n in sorted(excluded)]
             + [(n, floor) for n in sorted(compiled)])
    cases: list[tuple[str, str, bool, int, int]] = [
        # (name, log text, tests_selected, miri_status, expected exit)
        ("a correct run passes", log(clean), True, 0, 0),
        ("a compiled target that ran zero tests fails (#73)",
         log([(n, 0 if n == a_compiled else c) for n, c in clean]), True, 0, 1),
        ("an excluded target that ran any test fails (#77)",
         log([(n, 3 if n == a_excluded else c) for n, c in clean]), True, 0, 1),
        ("a compiled target absent from a run that SUCCEEDED fails",
         log([e for e in clean if e[0] != a_compiled]), True, 0, 1),
        ("a compiled target absent from a run that ABORTED is truncation, not a finding",
         log([e for e in clean if e[0] != a_compiled]), True, 1, 0),
        ("a lib binary reporting zero tests fails",
         log([(libs_clean[0][0], 0)] + clean[1:]), True, 0, 1),
        ("a lib binary never running fails",
         log(clean[1:]), True, 0, 1),
        ("`--lib` alone does not require the integration targets to appear",
         log(libs_clean), False, 0, 0),
        # `CARGO_TERM_COLOR: always` in the workflow means the log this guard reads is full of
        # SGR escapes. Pinned as a case rather than trusted, because the failure mode is silent
        # in the wrong direction: unstripped, `RUNNING` matches nothing, every target looks
        # absent, and on a SUCCESSFUL run the guard would fail the cell for the whole suite.
        # The shape the CI log of run 31073592771 actually has: `smear`'s lib binary and the
        # since-dissolved `smear-apollo-bench`'s, both spelled `Running unittests src/lib.rs`,
        # distinguished only by the path. More of them than `MIRI_PACKAGES` names means the
        # selection is wider than the scripts pass.
        ("one lib binary too many fails (#77, from the other end)",
         log(libs_clean + [("lib:smear", 7)] + clean[len(libs_clean):]), True, 0, 1),
        # And the direction the crate split introduced: a package silently leaving the selection
        # takes its unit tests with it and every other check here stays green.
        ("one lib binary too few fails (a package left the selection)",
         log(libs_clean[:-1] + clean[len(libs_clean):]), True, 0, 1),
        ("a colourised log parses the same as a plain one",
         log(clean).replace("     Running", "\x1b[1m\x1b[32m     Running\x1b[0m"), True, 0, 0),
        # The per-test half of the same property. `#[cfg_attr(miri, ignore)]` skips a test without
        # removing the target, so the target still runs and still passes; only the `ignored` digit
        # moves. Both directions are findings: one more than the files declare is an undocumented
        # skip, and one fewer means this guard's count is stale.
        ("one more ignored test than the source declares fails",
         log(clean).replace(f"{floor - a_declared} passed; 0 failed; {a_declared} ignored",
                            f"{floor - a_declared - 1} passed; 0 failed; {a_declared + 1} ignored",
                            1),
         True, 0, 1),
        ("a target whose every test is ignored fails",
         log([(n, declared.get(n, 0)) if n == a_skipping else (n, c) for n, c in clean]),
         True, 0, 1),
        # THE HALF THAT WAS NOT BEING ASKED. A lib binary's `ignored` digit was parsed and compared
        # to nothing, so a `#[cfg_attr(miri, ignore)]` on a lib unit test moved no number this guard
        # read. Both directions, keyed by binary path rather than by the collapsed `<lib>` line
        # every package shares.
        ("a lib binary reporting one more ignored test than its `src/` declares fails",
         log(clean, {f"lib:{a_lib_skipping}": a_lib_declared + 1}), True, 0, 1),
    ]
    if a_lib_declared:
        cases.append(
            ("a lib binary reporting one fewer ignored test than its `src/` declares fails",
             log(clean, {f"lib:{a_lib_skipping}": a_lib_declared - 1}), True, 0, 1)
        )

    # Read first, and printed first, because every case below is built out of `declared` and so
    # agrees with the tree by construction: if the tree has drifted off the budget these will say
    # so once, and the synthetic cases will then fail as collateral rather than as findings.
    #
    # These three are also the only cases here that touch the SOURCE side. `#[ignore]`s are the
    # one input this guard both reads and is constrained by, so a case that only synthesises a log
    # exercises the half that was never the problem.
    budget_cases = (BUDGET_OVER, BUDGET_UNDER, BUDGET_STATED)
    found = budget_findings(ignore_sources(tests_dir, compiled))
    bad = 0
    for name in budget_cases:
        finding = found.get(name)
        print(f"  {'FAIL' if finding else 'ok  '} {name}")
        if finding:
            bad += 1
            print(f"       {finding}")

    tmp = pathlib.Path(tempfile.mkdtemp(prefix="miri-scope-selftest-"))
    try:
        for i, (name, text, sel, status, want) in enumerate(cases):
            path = tmp / f"case{i}.log"
            path.write_text(text, encoding="utf-8")
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                got = check(tests_dir, manifest, path, tests_selected=sel, miri_status=status)
            mark = "ok  " if got == want else "FAIL"
            if got != want:
                bad += 1
            print(f"  {mark} [{got} vs want {want}] {name}")
            if got != want:
                print("       ---- guard output ----")
                for line in buf.getvalue().splitlines():
                    print(f"       {line}")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    # The cfg evaluator is the new load-bearing part, and every one of these is a shape the real
    # tree either has today or is one edit away from having. An evaluator that cannot say no is
    # the same defect as a guard that cannot fail.
    cfg_cases: list[tuple[str, bool | str]] = [
        ('feature = "parser"', True),
        ('feature = "rowan"', False),
        ('all(feature = "graphql", feature = "parser")', True),
        ('all(feature = "graphql", feature = "parser", not(miri))', False),
        ('any(feature = "rowan", feature = "graphql")', True),
        ('any(feature = "rowan", feature = "introspection")', False),
        ('all(feature = "std", any(feature = "graphql", feature = "graphqlx"))', True),
        ("not(miri)", False),
        ('feature = "test-support"', True),
        ('target_os = "linux"', "raises"),
        ("nightly", "raises"),
        ('all(feature = "graphql"', "raises"),
    ]
    for text, want in cfg_cases:
        try:
            got: bool | str = eval_cfg(parse_cfg(text), features)
        except CfgError:
            got = "raises"
        if got != want:
            bad += 1
            print(f"  FAIL [{got!r} vs want {want!r}] cfg `{text}`")

    total_cases = len(budget_cases) + len(cases) + len(cfg_cases)
    if bad:
        print(f"::error::miri_scope selftest: {bad} of {total_cases} cases did "
              "not behave as written — this guard does not check what its header claims")
        return 1
    sources = ignore_sources(tests_dir, compiled)
    print(f"miri_scope selftest OK: {total_cases} cases, "
          f"{len(excluded)} excluded / {len(compiled)} compiled targets in {tests_dir}, "
          f"{sum(sources.values())} individual tests skipped in {len(sources)} source file(s), "
          f"against a declared set of {MIRI_IGNORE_BUDGET} in {len(MIRI_DECLARED_IGNORES)}, "
          f"feature set {{{', '.join(sorted(features))}}}")
    return 0


def check(
    tests_dir: pathlib.Path,
    manifest: pathlib.Path,
    log: pathlib.Path,
    *,
    tests_selected: bool,
    miri_status: int,
    cargo_features: list[str] | None = None,
    default_features: bool = True,
) -> int:
    """Print the covered/excluded split, then return 0 if the run matched it and 1 if it did not."""
    try:
        features = resolved_features(manifest, cargo_features or [], default_features)
        excluded, compiled = partition(tests_dir, features)
    except CfgError as err:
        print(f"::error::miri_scope: {err}")
        return 1
    if not excluded and not compiled:
        print(f"::error::miri_scope: {tests_dir} holds no test targets — this guard would "
              "pass by having nothing to check, which is the defect it exists to catch")
        return 1

    counts, libs, ignored, lib_ignored = ran_counts(log)
    declared = declared_ignores(tests_dir)
    lib_declared = declared_lib_ignores()
    sources = ignore_sources(tests_dir, compiled)

    print()
    print("── Miri scope ──────────────────────────────────────────────────────────────────")
    print(f"features resolved for `smear`: {', '.join(sorted(features))}")
    print(f"NOT COVERED by this cell: {len(excluded)} of {len(excluded) + len(compiled)} "
          f"integration targets in {tests_dir}")
    for reason in sorted(set(excluded.values())):
        print(f"  reason: {reason}")
        for name in sorted(n for n, r in excluded.items() if r == reason):
            print(f"    - {name}")
    print(f"COVERED: the lib unit tests of {len(MIRI_PACKAGES)} packages plus {len(compiled)} "
          "integration targets"
          + ("" if tests_selected else " (this cell runs `--lib` only; see the script header)"))
    for package in MIRI_PACKAGES:
        skip = lib_declared.get(package, 0)
        note = f"  ({skip} of its lib unit tests carry `#[ignore]` here)" if skip else ""
        print(f"    + {package} (lib){note}")
    for name in compiled:
        skip = declared.get(name, 0)
        note = f"  ({skip} of its tests carry `#[ignore]` here)" if skip else ""
        print(f"    + {name}{note}")
    print(f"SKIPPED INSIDE THOSE: {sum(sources.values())} individual tests in "
          f"{len(sources)} source file(s), against a declared set of {MIRI_IGNORE_BUDGET} in "
          f"{len(MIRI_DECLARED_IGNORES)}")
    for path in sorted(sources):
        print(f"    ! {path}  ({sources[path]})")
    print("────────────────────────────────────────────────────────────────────────────────")
    print()

    aborted = miri_status != 0
    failures: list[str] = []
    notes: list[str] = []

    if not libs:
        (notes if aborted else failures).append(
            "no lib unit-test binary ran; every cell must interpret one per selected package"
        )
    elif len(libs) != len(MIRI_PACKAGES):
        wider = len(libs) > len(MIRI_PACKAGES)
        failures.append(
            f"{len(libs)} lib unit-test binaries ran and these scripts select "
            f"{len(MIRI_PACKAGES)} package(s) ({', '.join(MIRI_PACKAGES)}), so the selection is "
            + ("WIDER than they pass — that is #77's mechanism seen from the other end"
               if wider else
               "NARROWER than they pass — a package's unit tests stopped being interpreted and "
               "nothing else would have said so")
            + "; the binaries were: "
            + ", ".join(path for path, _ in libs)
        )
    elif any(count == 0 for _, count in libs):
        failures.append(
            "a lib unit-test binary reported `running 0 tests`: "
            + ", ".join(path for path, count in libs if count == 0)
        )

    # The per-test half, for the half of a cell that was never asked. Each lib binary's `ignored`
    # digit is held against what that package's `src/` declares — the check the integration targets
    # have had since #73's repair and the libraries did not, even though they are the larger part of
    # what a cell interprets.
    for path, _ in libs:
        package = package_of(path)
        if package is None:
            failures.append(
                f"{path}: a lib unit-test binary whose file name matches no selected package "
                f"({', '.join(MIRI_PACKAGES)}). Cargo names it after the package it belongs to, so "
                "either the selection is wider than these scripts pass or this guard can no longer "
                "tell whose unit tests it is reading — and it cannot check a skip count it cannot "
                "attribute"
            )
            continue
        want = lib_declared.get(package, 0)
        got = lib_ignored.get(path)
        if got is None:
            (notes if aborted else failures).append(
                f"{package}: its lib unit-test binary printed no `test result:` line, so this "
                "guard cannot tell how many of its tests were skipped"
            )
        elif got != want:
            failures.append(
                f"{package}: its lib unit-test binary reported {got} ignored test(s) and its "
                f"`src/` declares {want}. A per-test `#[cfg_attr(miri, ignore)]` on a lib test is "
                "the quietest coverage cut in this repository — the binary still runs, still "
                "passes, and one digit moves — so it is counted out of the sources and required to "
                "match, the same way an integration target's is"
            )

    for name in compiled:
        got = counts.get(name)
        if got is None:
            if not tests_selected:
                continue
            if aborted:
                notes.append(f"{name}: not reached — the run aborted before it")
            else:
                failures.append(
                    f"{name}: `--tests` was passed and this cell's feature set compiles this "
                    "target, but the run never reached it — it is no longer being built, and the "
                    "cell is narrower than it claims (#73)"
                )
        elif got == 0:
            failures.append(
                f"{name}: ran and reported `running 0 tests`. A target that compiles to an empty "
                "harness passes without testing anything — that is #73's exact mechanism. Either "
                "it acquired a `cfg` the Miri feature set does not satisfy — in which case say so "
                "in its crate-level `#![cfg(...)]`, which is what this guard reads — or its tests "
                "were removed"
            )
        else:
            skipped = ignored.get(name)
            want = declared.get(name, 0)
            if skipped is None:
                (notes if aborted else failures).append(
                    f"{name}: ran {got} tests but printed no `test result:` line, so this guard "
                    "cannot tell how many of them were skipped"
                )
            elif skipped != want:
                failures.append(
                    f"{name}: reported {skipped} ignored test(s) and the source declares {want}. "
                    "A per-test `#[cfg_attr(miri, ignore)]` is a coverage decision that shows up "
                    "as one digit nobody reads, so it is counted out of the file and required to "
                    "match. Either an `#[ignore]` was added without the file's header saying why, "
                    "or one was removed and this count is stale"
                )
            elif skipped == got:
                failures.append(
                    f"{name}: all {got} of its tests are ignored here, so the target runs and "
                    "proves nothing. Exclude it at the crate level instead, where this guard "
                    "prints it with a reason"
                )

    for name in sorted(excluded):
        got = counts.get(name)
        if got:
            failures.append(
                f"{name}: excluded here — {excluded[name]} — and yet it ran {got} tests, so this "
                "cell compiles more than these scripts select. Something widened it; a new "
                "workspace member enabling a `smear` feature and unifying into the resolve is how "
                "#70 and #84 both did it. Pin the selection rather than let it be resolved"
            )

    # Everything above compares the run to the tree. This compares the tree to the decision, and
    # is the only check here that a log cannot satisfy by agreeing with the sources.
    failures.extend(budget_findings(sources).values())

    if notes:
        print(f"miri_scope: `cargo miri test` exited {miri_status}, so the run stopped early "
              "and the following are truncation, not scope:")
        for line in notes:
            print(f"  . {line}")

    if failures:
        print("::error::miri_scope: the run did not interpret what this cell claims to cover")
        for line in failures:
            print(f"  - {line}")
        return 1

    covered = sorted(n for n in compiled if n in counts)
    where = "so far" if aborted else "interpreted"
    # Each selected package's lib count, named rather than summed: a total would hide one package
    # shrinking while another grew, which is the shape this guard exists to refuse.
    lib_counts = " + ".join(str(count) for _, count in libs) if libs else "not reached"
    print(f"miri_scope OK: {len(libs)} lib unit-test binaries ({lib_counts} tests) + "
          f"{len(covered)} integration targets {where}; {len(excluded)} excluded for the recorded "
          "reasons and none of them ran")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--log", type=pathlib.Path)
    ap.add_argument("--tests-dir", default=pathlib.Path("smear/tests"), type=pathlib.Path)
    ap.add_argument(
        "--manifest",
        default=pathlib.Path("smear/Cargo.toml"),
        type=pathlib.Path,
        help="The package whose feature closure decides which test targets this cell compiles.",
    )
    ap.add_argument(
        "--cargo-features",
        default="",
        help="Comma-separated features the run passed with `--features`, if it passed any. "
        "`ci/miri_sb.sh` and `ci/miri_tb.sh` pass none, so this is normally empty; it exists so "
        "that a script which starts passing some does not silently disagree with this guard.",
    )
    ap.add_argument(
        "--no-default-features",
        action="store_true",
        help="Set when the run passed `--no-default-features`, for the same reason.",
    )
    ap.add_argument(
        "--verify-exclusions",
        action="store_true",
        help="Re-measure every MIRI_NOT_SELECTED member with the cargo flags its reason names, and "
        "fail if any has a non-empty harness. This is the reason being EXECUTED rather than read: "
        "an exclusion table whose entries are checked only for being non-blank is the shape "
        "`EQ_EXEMPT` already had once. Both Miri scripts run it before `cargo miri setup`.",
    )
    ap.add_argument(
        "--print-packages",
        action="store_true",
        help="Print the `-p` arguments for `cargo miri test`, one token per line, and exit. This "
        "is how ci/miri_sb.sh and ci/miri_tb.sh build their selection: MIRI_PACKAGES is then the "
        "single source of truth for what RUNS as well as for what the guard EXPECTS. They used to "
        "be two hard-coded lists and they did drift — the constant grew to six members while the "
        "scripts still passed two, so the guard was reading a declaration the workflow did not "
        "execute.",
    )
    ap.add_argument(
        "--selftest",
        action="store_true",
        help="Prove every branch of the guard can fail, against synthesised logs, and exit. "
        "Takes no --log, --tests-selected or --miri-status.",
    )
    ap.add_argument(
        "--tests-selected",
        choices=("0", "1"),
        help="1 when the run passed `--tests`, so every compiled target is expected to appear; "
        "0 for the i686 cell, which runs `--lib` alone and can only be checked on what it ran.",
    )
    ap.add_argument(
        "--miri-status",
        type=int,
        help="`cargo miri test`'s own exit status. A run that ABORTED never reaches the targets "
        "after the one that killed it, so on a non-zero status an absent target is reported and "
        "not failed — the cell is already red for the real reason, and blaming the truncation on "
        "a missing target would bury it. Every other check here still applies: a target that DID "
        "run and reported zero tests, or an excluded one that ran at all, is a finding no matter "
        "how the run ended.",
    )
    args = ap.parse_args()

    if not args.tests_dir.is_dir():
        print(f"::error::miri_scope: no such directory: {args.tests_dir}", file=sys.stderr)
        return 1
    if not args.manifest.is_file():
        print(f"::error::miri_scope: no such manifest: {args.manifest}", file=sys.stderr)
        return 1

    if args.verify_exclusions:
        return verify_exclusions()

    if args.print_packages:
        if not MIRI_PACKAGES:
            print("::error::miri_scope: MIRI_PACKAGES is empty", file=sys.stderr)
            return 1
        for package in MIRI_PACKAGES:
            print("-p")
            print(package)
        return 0

    if args.selftest:
        return selftest(args.tests_dir, args.manifest)

    missing = [
        flag
        for flag, value in (("--log", args.log),
                            ("--tests-selected", args.tests_selected),
                            ("--miri-status", args.miri_status))
        if value is None
    ]
    if missing:
        ap.error("required without --selftest: " + ", ".join(missing))
    if not args.log.is_file():
        print(f"::error::miri_scope: no such log: {args.log}", file=sys.stderr)
        return 1

    return check(
        args.tests_dir,
        args.manifest,
        args.log,
        tests_selected=args.tests_selected == "1",
        miri_status=args.miri_status,
        cargo_features=[f for f in args.cargo_features.split(",") if f],
        default_features=not args.no_default_features,
    )


if __name__ == "__main__":
    sys.exit(main())

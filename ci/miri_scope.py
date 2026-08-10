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

Exit 0 on agreement, 1 on any disagreement. The excluded set is printed on every run, pass or
fail, with the reason each target is on it, because "what this matrix does not cover" belongs in
the log rather than in a reader's assumption.
"""

from __future__ import annotations

import argparse
import contextlib
import io
import pathlib
import re
import shutil
import sys
import tempfile
import tomllib

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


def ran_counts(log: pathlib.Path) -> tuple[dict[str, int], list[tuple[str, int]]]:
    """Read the log into (integration targets -> test count, lib binaries in order).

    The two are kept apart because cargo spells every package's unit-test binary
    `Running unittests src/lib.rs (<path>)` — the same text for all of them, distinguished only
    by the path in parentheses. Collapsing them onto one key silently keeps whichever ran first,
    and on a workspace-wide selection that is a coin toss between packages: the CI log of run
    31073592771 has two, `smear`'s and `smear-apollo-bench`'s, both empty. So they are returned
    as a LIST and the caller requires exactly one, which is both the truth under `-p smear` and
    a direct check that the selection is still one package.
    """
    counts: dict[str, int] = {}
    libs: list[tuple[str, int]] = []
    current: str | None = None
    current_path: str = ""
    for raw in log.read_text(encoding="utf-8", errors="replace").splitlines():
        line = ANSI.sub("", raw)
        running = RUNNING.match(line)
        if running:
            target = running.group(1)
            current = "<lib>" if target.endswith("lib.rs") else pathlib.Path(target).stem
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
    return counts, libs


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

    def log(entries: list[tuple[str, int | None]]) -> str:
        out = []
        for i, (name, count) in enumerate(entries):
            path = "unittests src/lib.rs" if name == "<lib>" else f"tests/{name}.rs"
            out.append(f"     Running {path} (/tmp/build/pkg{i}/{name}-deadbeef)")
            if count is not None:
                out.append(f"running {count} tests")
                out.append(f"test result: ok. {count} passed; 0 failed")
        return "\n".join(out) + "\n"

    clean = ([("<lib>", 400)]
             + [(n, 0) for n in sorted(excluded)]
             + [(n, 7) for n in sorted(compiled)])
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
        ("the lib binary reporting zero tests fails",
         log([("<lib>", 0)] + clean[1:]), True, 0, 1),
        ("the lib binary never running fails",
         log(clean[1:]), True, 0, 1),
        ("`--lib` alone does not require the integration targets to appear",
         log([("<lib>", 400)]), False, 0, 0),
        # `CARGO_TERM_COLOR: always` in the workflow means the log this guard reads is full of
        # SGR escapes. Pinned as a case rather than trusted, because the failure mode is silent
        # in the wrong direction: unstripped, `RUNNING` matches nothing, every target looks
        # absent, and on a SUCCESSFUL run the guard would fail the cell for the whole suite.
        # The shape the CI log of run 31073592771 actually has: `smear`'s lib binary and
        # `smear-apollo-bench`'s, both spelled `Running unittests src/lib.rs`, distinguished only
        # by the path. Two of them means the selection is not one package.
        ("a second package's lib unit tests running fails (#77, from the other end)",
         log([("<lib>", 400), ("<lib>", 0)] + clean[1:]), True, 0, 1),
        ("a colourised log parses the same as a plain one",
         log(clean).replace("     Running", "\x1b[1m\x1b[32m     Running\x1b[0m"), True, 0, 0),
    ]

    tmp = pathlib.Path(tempfile.mkdtemp(prefix="miri-scope-selftest-"))
    bad = 0
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

    if bad:
        print(f"::error::miri_scope selftest: {bad} of {len(cases) + len(cfg_cases)} cases did "
              "not behave as written — this guard does not check what its header claims")
        return 1
    print(f"miri_scope selftest OK: {len(cases) + len(cfg_cases)} cases, "
          f"{len(excluded)} excluded / {len(compiled)} compiled targets in {tests_dir}, "
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

    counts, libs = ran_counts(log)

    print()
    print("── Miri scope ──────────────────────────────────────────────────────────────────")
    print(f"features resolved for `smear`: {', '.join(sorted(features))}")
    print(f"NOT COVERED by this cell: {len(excluded)} of {len(excluded) + len(compiled)} "
          f"integration targets in {tests_dir}")
    for reason in sorted(set(excluded.values())):
        print(f"  reason: {reason}")
        for name in sorted(n for n, r in excluded.items() if r == reason):
            print(f"    - {name}")
    print(f"COVERED: the lib unit tests plus {len(compiled)} integration targets"
          + ("" if tests_selected else " (this cell runs `--lib` only; see the script header)"))
    for name in compiled:
        print(f"    + {name}")
    print("────────────────────────────────────────────────────────────────────────────────")
    print()

    aborted = miri_status != 0
    failures: list[str] = []
    notes: list[str] = []

    lib = libs[0][1] if libs else None
    if not libs:
        (notes if aborted else failures).append(
            "the lib unit-test binary never ran; every cell must interpret it"
        )
    elif len(libs) > 1:
        failures.append(
            f"{len(libs)} lib unit-test binaries ran, and `-p smear` selects ONE package — so "
            "the selection is wider than these scripts pass. That is #77's mechanism seen from "
            "the other end; the extra packages were: "
            + ", ".join(path for path, _ in libs[1:])
        )
    elif lib == 0:
        failures.append("the lib unit-test binary reported `running 0 tests`")

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

    for name in sorted(excluded):
        got = counts.get(name)
        if got:
            failures.append(
                f"{name}: excluded here — {excluded[name]} — and yet it ran {got} tests, so this "
                "cell compiles more than these scripts select. Something widened it; a new "
                "workspace member enabling a `smear` feature and unifying into the resolve is how "
                "#70 and #84 both did it. Pin the selection rather than let it be resolved"
            )

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
    print(f"miri_scope OK: lib ({lib if lib is not None else 'not reached'}) + {len(covered)} "
          f"integration targets {where}; {len(excluded)} excluded for the recorded reasons and "
          "none of them ran")
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

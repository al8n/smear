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

  * a target that is NOT `rowan`-gated and ran zero tests  -> #73's silence, and
  * a target that IS `rowan`-gated and ran ANY test        -> #77's unchosen widening.

The `rowan`-gated set is DERIVED, by reading the `#![cfg(...)]` header of every file in
`smear/tests/`, and never written down here. A hand-maintained list would be a third thing that
agrees with the source on the day it is written, which is the shape of both defects above.

Exit 0 on agreement, 1 on any disagreement. The excluded set is printed on every run, pass or
fail, because "what this matrix does not cover" belongs in the log rather than in a reader's
assumption.
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

# The one reason a target is allowed to be excluded, and the issue that records it.
EXCLUSION_REASON = (
    "gated on `rowan`, which cannot be interpreted: rowan 0.16.1 has undefined behaviour under "
    "Stacked Borrows in `src/arc.rs` and under Tree Borrows in `src/cursor.rs`, live in every "
    "published version through 0.17.0 (al8n/smear#77, rust-analyzer/rowan#108 and #192)"
)


def gated_targets(tests_dir: pathlib.Path) -> tuple[set[str], set[str]]:
    """Partition `smear/tests/*.rs` into (rowan-gated, not-rowan-gated) target names."""
    gated: set[str] = set()
    plain: set[str] = set()
    for path in sorted(tests_dir.glob("*.rs")):
        # Whole file, not a prefix window: several of these open with a module doc comment long
        # enough that a fixed window would miss the attribute below it, and misclassifying a
        # gated target as un-gated would fail the cell with the wrong explanation.
        match = INNER_CFG.search(path.read_text(encoding="utf-8"))
        (gated if match and "rowan" in match.group(1) else plain).add(path.stem)
    return gated, plain


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


def selftest(tests_dir: pathlib.Path) -> int:
    """Prove every branch of `check` can fail, against the REAL test tree.

    A guard that cannot fail is the defect this guard exists to catch, so this runs first in
    `ci/miri_sb.sh` and `ci/miri_tb.sh` — before `rustup`, before `cargo miri setup`, before
    anything that costs minutes. The logs below are synthesised, but the gated/plain partition
    is read from `smear/tests/` exactly as a real run reads it: a case is stated in terms of
    "some gated target" and "some plain target", so it keeps meaning when a test file is added.
    """
    gated, plain = gated_targets(tests_dir)
    if not gated or not plain:
        print("::error::miri_scope selftest: the test tree has no gated targets, or no "
              f"un-gated ones ({len(gated)} / {len(plain)}), so the cases below would not "
              "exercise what they name")
        return 1
    a_gated, a_plain = sorted(gated)[0], sorted(plain)[0]

    def log(entries: list[tuple[str, int | None]]) -> str:
        out = []
        for i, (name, count) in enumerate(entries):
            path = "unittests src/lib.rs" if name == "<lib>" else f"tests/{name}.rs"
            out.append(f"     Running {path} (/tmp/build/pkg{i}/{name}-deadbeef)")
            if count is not None:
                out.append(f"running {count} tests")
                out.append(f"test result: ok. {count} passed; 0 failed")
        return "\n".join(out) + "\n"

    clean = [("<lib>", 400)] + [(n, 0) for n in sorted(gated)] + [(n, 7) for n in sorted(plain)]
    cases: list[tuple[str, str, bool, int, int]] = [
        # (name, log text, tests_selected, miri_status, expected exit)
        ("a correct run passes", log(clean), True, 0, 0),
        ("a plain target that ran zero tests fails (#73)",
         log([(n, 0 if n == a_plain else c) for n, c in clean]), True, 0, 1),
        ("a gated target that ran any test fails (#77)",
         log([(n, 3 if n == a_gated else c) for n, c in clean]), True, 0, 1),
        ("a plain target absent from a run that SUCCEEDED fails",
         log([e for e in clean if e[0] != a_plain]), True, 0, 1),
        ("a plain target absent from a run that ABORTED is truncation, not a finding",
         log([e for e in clean if e[0] != a_plain]), True, 1, 0),
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
                got = check(tests_dir, path, tests_selected=sel, miri_status=status)
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

    if bad:
        print(f"::error::miri_scope selftest: {bad} of {len(cases)} cases did not behave as "
              "written — this guard does not check what its header claims")
        return 1
    print(f"miri_scope selftest OK: {len(cases)} cases, "
          f"{len(gated)} gated / {len(plain)} un-gated targets in {tests_dir}")
    return 0


def check(
    tests_dir: pathlib.Path,
    log: pathlib.Path,
    *,
    tests_selected: bool,
    miri_status: int,
) -> int:
    """Print the covered/excluded split, then return 0 if the run matched it and 1 if it did not."""
    gated, plain = gated_targets(tests_dir)
    if not gated and not plain:
        print(f"::error::miri_scope: {tests_dir} holds no test targets — this guard would "
              "pass by having nothing to check, which is the defect it exists to catch")
        return 1

    counts, libs = ran_counts(log)

    print()
    print("── Miri scope ──────────────────────────────────────────────────────────────────")
    print(f"NOT COVERED by this cell: {len(gated)} of {len(gated) + len(plain)} integration "
          f"targets in {tests_dir}")
    print(f"  reason: {EXCLUSION_REASON}")
    for name in sorted(gated):
        print(f"    - {name}")
    print(f"COVERED: the lib unit tests plus {len(plain)} integration targets"
          + ("" if tests_selected else " (this cell runs `--lib` only; see the script header)"))
    for name in sorted(plain):
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

    for name in sorted(plain):
        got = counts.get(name)
        if got is None:
            if not tests_selected:
                continue
            if aborted:
                notes.append(f"{name}: not reached — the run aborted before it")
            else:
                failures.append(
                    f"{name}: `--tests` was passed and this target is not `rowan`-gated, but the "
                    "run never reached it — it is no longer being built, and the cell is "
                    "narrower than it claims (#73)"
                )
        elif got == 0:
            failures.append(
                f"{name}: ran and reported `running 0 tests`. A target that compiles to an empty "
                "harness passes without testing anything — that is #73's exact mechanism. Either "
                "it acquired a `cfg` the Miri feature set does not satisfy, or its tests were "
                "removed"
            )

    for name in sorted(gated):
        got = counts.get(name)
        if got:
            failures.append(
                f"{name}: `rowan`-gated and yet ran {got} tests, so `rowan` is ON in this "
                "selection. Something widened it — a new workspace member enabling "
                "`smear/rowan` and unifying into the resolve is how #70 and #84 both did it. "
                "The tower cannot be interpreted while rowan is UB (#77); pin the selection "
                "rather than let it be resolved"
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

    covered = sorted(n for n in plain if n in counts)
    where = "so far" if aborted else "interpreted"
    print(f"miri_scope OK: lib ({lib if lib is not None else 'not reached'}) + {len(covered)} "
          f"integration targets {where}; {len(gated)} excluded for the recorded reason and none "
          "of them ran")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--log", type=pathlib.Path)
    ap.add_argument("--tests-dir", default=pathlib.Path("smear/tests"), type=pathlib.Path)
    ap.add_argument(
        "--selftest",
        action="store_true",
        help="Prove every branch of the guard can fail, against synthesised logs, and exit. "
        "Takes no --log, --tests-selected or --miri-status.",
    )
    ap.add_argument(
        "--tests-selected",
        choices=("0", "1"),
        help="1 when the run passed `--tests`, so every non-gated target is expected to appear; "
        "0 for the i686 cell, which runs `--lib` alone and can only be checked on what it ran.",
    )
    ap.add_argument(
        "--miri-status",
        type=int,
        help="`cargo miri test`'s own exit status. A run that ABORTED never reaches the targets "
        "after the one that killed it, so on a non-zero status an absent target is reported and "
        "not failed — the cell is already red for the real reason, and blaming the truncation on "
        "a missing target would bury it. Every other check here still applies: a target that DID "
        "run and reported zero tests, or a `rowan`-gated one that ran at all, is a finding no "
        "matter how the run ended.",
    )
    args = ap.parse_args()

    if not args.tests_dir.is_dir():
        print(f"::error::miri_scope: no such directory: {args.tests_dir}", file=sys.stderr)
        return 1

    if args.selftest:
        return selftest(args.tests_dir)

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
        args.log,
        tests_selected=args.tests_selected == "1",
        miri_status=args.miri_status,
    )


if __name__ == "__main__":
    sys.exit(main())

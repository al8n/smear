#!/usr/bin/env python3
"""Fails a build when a heap gate names `alloc` alone or a fence refuses `std` without `alloc`.

`std` and `alloc` are separate tiers, and the allocating API must exist when EITHER is on: every
heap gate is spelled `any(feature = "std", feature = "alloc")`, and the lossless tower, which is
written against the standard library, is gated on `std` itself.

NOTHING COMPILES THE DEFECT RED, and that is why this is a static check. A gate left on
`feature = "alloc"` alone compiles in every configuration; in a build with `std` and without
`alloc` its item is simply absent. A compile row fails only where something in that row names the
item, and a test file gated that way compiles to zero tests and passes. The `compile_error!` fence
the crate roots used to carry made `std` without `alloc` loud; that combination is valid, so the
fence is gone, and this gate stands in its place.

    H  Every `feature = "alloc"` in the raw text, comments and doctest lines included, lies inside
       one of:
         - `any(feature = "std", feature = "alloc")`, `std` first;
         - the crate root's alias attribute,
           `#[cfg(all(not(feature = "std"), feature = "alloc"[, feature = "<cap>"]*))]`, followed
           only by attributes and then `extern crate alloc as std;`;
         - a feature witness, `pub const ALLOC: bool = cfg!(feature = "alloc");`, or the umbrella's
           assertion over one, `__features::ALLOC == cfg!(feature = "alloc")`.
    F  No `#[cfg(...)] compile_error!` names both `feature = "std"` and `feature = "alloc"`. Every
       combination of the two tiers is valid, so such a fence either refuses a valid build or,
       respelled with `any(...)`, can never fire while still passing rule H.
    R  Every target whose `required-features` names `rowan_0_17` or `lossless-coverage` names `std`
       too, the tier the tower is gated on. `cargo metadata` is the only place this is visible: a
       target whose `required-features` are unmet is skipped in silence, so no compile row can
       refuse one.

WHAT IS READ. Every tracked `*.rs` file (`git ls-files`), and every Markdown file a crate root
includes as its crate docs with `#![doc = include_str!("...")]`: those are doctests, and a
`# #[cfg(...)]` line in one gates its example exactly as an attribute gates an item. Symlinks are
followed, so `smear/README.md` is read as the workspace root's `README.md`. Changelog prose is not
code and is not read.

WHAT MAKES IT NON-VACUOUS. `--selftest` plants every shape of the defect and every allowed spelling
as synthetic text, never the tree, and requires each to produce exactly the number of findings
written beside it. A run over the tree that read no `.rs` file, or no `alloc` atom at all, is a
finding, and so is an exemption that matches nothing.

# Interpreter

Stdlib only, and 3.9-safe like every gate here but `ci/miri_scope.py`: `ci/feature_reachability.py`
fails if this file imports a floor-bearing module or runs another gate. It runs two commands of its
own, `git ls-files` and `cargo metadata --no-deps`.

Run from the repository root:

    python3 ci/tier_gates.py              # exit 1 on any finding
    python3 ci/tier_gates.py --verbose    # also say what was read
    python3 ci/tier_gates.py --selftest   # plant every shape and require its count
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import sys

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent

ATOM = re.compile(r'feature\s*=\s*"alloc"')
STD_ATOM = re.compile(r'feature\s*=\s*"std"')
HEAP = re.compile(r'any\(\s*feature\s*=\s*"std"\s*,\s*feature\s*=\s*"alloc"\s*,?\s*\)')
REVERSED = re.compile(r'any\(\s*feature\s*=\s*"alloc"\s*,\s*feature\s*=\s*"std"\s*,?\s*\)')
ALIAS = re.compile(
  r'#\[cfg\(all\(\s*not\(\s*feature\s*=\s*"std"\s*\)\s*,\s*feature\s*=\s*"alloc"'
  r'(?:\s*,\s*feature\s*=\s*"[A-Za-z0-9_-]+")*\s*,?\s*\)\)\]\s*'
  r'(?:#\[[^\]]*\]\s*)*extern\s+crate\s+alloc\s+as\s+std\s*;'
)
WITNESS = re.compile(
  r'pub\s+const\s+ALLOC\s*:\s*bool\s*=\s*cfg!\(\s*feature\s*=\s*"alloc"\s*\)\s*;'
  r'|__features::ALLOC\s*==\s*cfg!\(\s*feature\s*=\s*"alloc"\s*\)'
)
FENCE = re.compile(r'#\[cfg\(([^\]]*)\)\]\s*compile_error!')
DOC_INCLUDE = re.compile(r'#!\[doc\s*=\s*include_str!\(\s*"([^"]+)"\s*\)\s*\]')

# The features that compile the lossless tower, whose tier is `std`.
TOWER = ("rowan_0_17", "lossless-coverage")

# Files allowed to spell `feature = "alloc"` outside the three forms above — a fixture that probes
# the feature itself, say — each with the argument for it. Empty today. An entry that matches
# nothing fails the run: the file is gone, or no longer spells what it was exempted for.
EXEMPT: dict[str, str] = {}


def line_of(text: str, offset: int) -> int:
  return text.count("\n", 0, offset) + 1


def findings_in(name: str, text: str) -> list[str]:
  """Rules H and F over one file's raw text."""
  allowed = [m.span() for rx in (HEAP, ALIAS, WITNESS) for m in rx.finditer(text)]
  lines = text.split("\n")
  out = []
  for m in REVERSED.finditer(text):
    out.append(
      f"{name}:{line_of(text, m.start())}: H: write `std` first, "
      f"`any(feature = \"std\", feature = \"alloc\")`"
    )
  for m in ATOM.finditer(text):
    if any(start <= m.start() and m.end() <= end for start, end in allowed):
      continue
    line = line_of(text, m.start())
    out.append(
      f"{name}:{line}: H: `feature = \"alloc\"` outside `any(feature = \"std\", feature = "
      f"\"alloc\")`, so what it gates is absent from every `std` build without `alloc`: "
      f"{lines[line - 1].strip()}"
    )
  for m in FENCE.finditer(text):
    if STD_ATOM.search(m.group(1)) and ATOM.search(m.group(1)):
      out.append(
        f"{name}:{line_of(text, m.start())}: F: a fence over `std` and `alloc`; every "
        f"combination of the two tiers is valid, so delete it"
      )
  return out


def check_files(files: dict[str, str], exempt: dict[str, str]) -> list[str]:
  """Rules H and F over every file, with the exemption table applied and audited."""
  findings: list[str] = []
  used = set()
  for name in sorted(files):
    found = findings_in(name, files[name])
    if found and name in exempt:
      used.add(name)
      continue
    findings.extend(found)
  for name, why in sorted(exempt.items()):
    if name not in used:
      findings.append(
        f"the exemption for `{name}` matches nothing ({why}): the file is gone, or no longer "
        f"spells what it was exempted for"
      )
  return findings


def required_features_findings(packages: list[dict]) -> list[str]:
  """Rule R over `cargo metadata`'s packages."""
  out = []
  for package in packages:
    for target in package.get("targets", []):
      required = target.get("required-features") or []
      if set(TOWER) & set(required) and "std" not in required:
        out.append(
          f"{package['name']} {'/'.join(target.get('kind', []))} target `{target['name']}`: R: "
          f"required-features {required} names the lossless tower without `std`, the tier it is "
          f"gated on"
        )
  return out


def run(argv: list[str], what: str) -> str:
  """A command's stdout, or the run ends as a finding rather than a traceback."""
  out = subprocess.run(argv, cwd=REPO_ROOT, capture_output=True, text=True)
  if out.returncode != 0:
    print(f"::error::tier_gates: {what} failed, so this gate read nothing", file=sys.stderr)
    print(out.stderr.strip()[:1200], file=sys.stderr)
    raise SystemExit(1)
  return out.stdout


def doc_included_markdown(rust_files: list[str]) -> tuple[list[str], list[str]]:
  """The Markdown files the crate roots include as their docs, and what could not be resolved."""
  found, problems = set(), []
  root = REPO_ROOT.resolve()
  for rel in rust_files:
    if not rel.endswith("src/lib.rs") or not (REPO_ROOT / rel).is_file():
      continue
    text = (REPO_ROOT / rel).read_text(encoding="utf-8")
    for m in DOC_INCLUDE.finditer(text):
      target = ((REPO_ROOT / rel).parent / m.group(1)).resolve()
      try:
        found.add(str(target.relative_to(root)))
      except ValueError:
        problems.append(f"{rel} includes `{m.group(1)}`, which resolves outside the repository")
        continue
      if not target.is_file():
        problems.append(f"{rel} includes `{m.group(1)}`, which is not a file")
  return sorted(found), problems


def check(verbose: bool) -> int:
  rust = [f for f in run(["git", "ls-files", "-z", "--", "*.rs"], "`git ls-files`").split("\0")
          if f]
  markdown, findings = doc_included_markdown(rust)
  files = {}
  for rel in rust + markdown:
    path = REPO_ROOT / rel
    if path.is_file():
      files[rel] = path.read_text(encoding="utf-8")
  findings.extend(check_files(files, EXEMPT))
  atoms = sum(len(ATOM.findall(text)) for text in files.values())

  meta = json.loads(run(["cargo", "metadata", "--no-deps", "--format-version", "1"],
                        "`cargo metadata`"))
  findings.extend(required_features_findings(meta["packages"]))
  gated = sum(1 for p in meta["packages"] for t in p["targets"] if t.get("required-features"))

  # The vacuity guard: a reader that stopped finding the tree must not report a clean one.
  if not rust:
    findings.append("`git ls-files` listed no `.rs` file, so this gate read nothing")
  if atoms == 0:
    findings.append("no `feature = \"alloc\"` anywhere, so this gate checked nothing")

  if findings:
    print("::error::tier_gates: a heap gate, a fence or a tower target spells the tiers wrong")
    for f in findings:
      print(f"  - {f}")
    print(
      "  The allocating API must exist when either `std` or `alloc` is on, and the lossless tower "
      "needs `std` itself. Respell the gate, delete the fence, or add an entry to EXEMPT with the "
      "argument for it."
    )
    return 1
  if verbose:
    for rel in markdown:
      print(f"  read      {rel}, a crate root's docs")
  print(
    f"tier_gates OK: {len(rust)} .rs files and {len(markdown)} doc-included Markdown files, "
    f"{atoms} `alloc` atoms, every one in an allowed spelling; {gated} targets with "
    f"required-features, every tower one naming `std`"
  )
  return 0


# Every plant is synthetic text, and each must produce exactly the count beside it. The defect
# shapes come first; the allowed spellings after them must produce none.
TEXT_PLANTS = {
  "a bare #[cfg]": ('#[cfg(feature = "alloc")]\nmod a;\n', 1),
  "inside all(...)": ('#[cfg(all(feature = "alloc", feature = "graphql"))]\nmod a;\n', 1),
  "a bare doc(cfg)": ('#[cfg_attr(docsrs, doc(cfg(feature = "alloc")))]\npub mod a;\n', 1),
  "a not(...) twin": ('#[cfg_attr(not(feature = "alloc"), doc = "x")]\npub mod a;\n', 1),
  "cfg! outside a witness": ('if cfg!(feature = "alloc") {}\n', 1),
  "a doctest line": ('/// # #[cfg(feature = "alloc")] {\n', 1),
  "a multi-line all(...)": (
    '#[cfg(all(\n  feature = "alloc",\n  feature = "graphql"\n))]\nmod a;\n', 1),
  "the reversed any(...), its order and its atom": (
    '#[cfg(any(feature = "alloc", feature = "std"))]\nmod a;\n', 2),
  "an alias cfg without its extern crate": (
    '#[cfg(all(not(feature = "std"), feature = "alloc"))]\nmod a;\n', 1),
  "a fence, under F and under H": (
    '#[cfg(all(feature = "std", not(feature = "alloc")))]\ncompile_error!("x");\n', 2),
  "ok: any(...) across lines": (
    '#[cfg(any(\n  feature = "std",\n  feature = "alloc"\n))]\nmod a;\n', 0),
  "ok: all(any(...), X)": (
    '#[cfg(all(any(feature = "std", feature = "alloc"), feature = "graphql"))]\nmod a;\n', 0),
  "ok: not(any(...))": (
    '#[cfg_attr(not(any(feature = "std", feature = "alloc")), doc = "x")]\npub mod a;\n', 0),
  "ok: the alias": (
    '#[cfg(all(not(feature = "std"), feature = "alloc"))]\n#[allow(unused_extern_crates)]\n'
    'extern crate alloc as std;\n', 0),
  "ok: the facade's alias": (
    '#[cfg(all(not(feature = "std"), feature = "alloc", feature = "proto"))]\n'
    '#[allow(unused_extern_crates)]\nextern crate alloc as std;\n', 0),
  "ok: a witness": ('  pub const ALLOC: bool = cfg!(feature = "alloc");\n', 0),
  "ok: the umbrella's assertion": (
    '  smear_lexer::__features::ALLOC == cfg!(feature = "alloc"),\n', 0),
}


def selftest() -> int:
  results = []
  for name, (text, want) in TEXT_PLANTS.items():
    results.append((name, len(findings_in("plant.rs", text)), want))
  planted_target = [{"name": "plant", "targets": [
    {"name": "plant", "kind": ["bench"], "required-features": ["rowan_0_17", "validator"]}]}]
  results.append(("required-features = [\"rowan_0_17\", \"validator\"]",
                  len(required_features_findings(planted_target)), 1))
  clean = {"plant.rs": '#[cfg(any(feature = "std", feature = "alloc"))]\nmod a;\n'}
  results.append(("a stale exemption", len(check_files(clean, {"plant.rs": "planted"})), 1))

  bad = 0
  for name, got, want in results:
    ok = got == want
    bad += not ok
    print(f"  {'ok  ' if ok else 'FAIL'} [{got} vs want {want}] {name}")
  if bad:
    print(f"::error::tier_gates selftest: {bad} plants gave the wrong count, so the gate does not "
          f"implement its rules")
    return 1
  shapes = sum(1 for _, _, want in results if want)
  print(f"tier_gates selftest OK: {len(results)} plants, {shapes} defect shapes and "
        f"{len(results) - shapes} allowed spellings, each with its count")
  return 0


def main() -> int:
  ap = argparse.ArgumentParser(description=__doc__)
  ap.add_argument("--selftest", action="store_true", help="plant every shape and require its count")
  ap.add_argument("--verbose", action="store_true", help="also say what was read")
  args = ap.parse_args()
  if args.selftest:
    return selftest()
  return check(args.verbose)


if __name__ == "__main__":
  sys.exit(main())

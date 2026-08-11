#!/usr/bin/env python3
"""Certifies the downstream frame and prints the legs `ci/downstream.sh` runs.

# What this gate answers, and why nothing inside the workspace can

`smear` re-exports whole member crates. A feature of `smear` therefore gates what it advertises
ONLY where `smear/src/lib.rs` writes the `#[cfg]` itself; where it merely forwards to a member
feature, the `#[cfg]` lives inside the member — and cargo unifies a package's features across the
whole graph, so any other dependency naming that member can switch the capability on.

Every gate in this repository builds THIS workspace, where nothing else requests those member
features. The leak needs a **second requester in a downstream graph**, so it needs a downstream.
`ci/downstream/` is one: its own workspace root, path dependencies on all six crates, no
dev-dependencies, and no smear feature in its manifest.

Measured before the repair, with `smear = { default-features = false, features = ["std",
"parser"] }` and a second dependency on `smear-parser` at its own defaults: `smear::parser::graphqlx`
COMPILED. The same source without the second dependency failed with `unresolved import`. Ten of ten
forwarded pairs behaved that way. The four features `smear` `#[cfg]`s itself — `parser`,
`validator`, `proto`, and `smear-schema/build` behind them — did not.

# The two leg families, and why both are needed

    EQ-POS(member, f)   smear/std, smear/f, member/f          must COMPILE
    EQ-LEAK(member, f)  smear/std,           member/f         must FAIL, on the equivalence assert

EQ is **total over every member feature** — the pair list comes out of `cargo metadata`, so a
feature added to a member without a leg here is an error rather than a silent gap. It needs no
consumer code: the assertion lives in `smear`, so the disagreeing graph fails to compile on its own.

    CTL(pair)   uses-<pair>, smear/std[, …]                   must FAIL, unresolved path
    POS(pair)   uses-<pair>, smear/std[, …], smear/f          must COMPILE
    LEAK(pair)  uses-<pair>, smear/std[, …], member/f         must FAIL, on the equivalence assert

The path family is smaller — only the pairs with an OBSERVABLE public path or impl — and it is what
makes the EQ family mean something. EQ proves the assertion fires; CTL/POS prove there was a real
capability behind the feature to leak in the first place. Without CTL a `uses-` module that stopped
naming its path would leave POS compiling vacuously forever, which is pql's R5.

# What is deliberately NOT derived, and what checks it

`PROBES` below is written down: which public path a feature gates is a judgement, and `cargo
metadata` does not carry it. What is derived is its COMPLETENESS in the direction that matters —
C1 requires a `uses-` feature in the fixture for every probe, and C2 requires an EQ leg for every
(member, feature) pair cargo reports. A probe naming a pair that no longer exists is C4.

Usage:
  ci/downstream_pairs.py plan <workspace-manifest> <fixture-manifest>   certify, print the legs
  ci/downstream_pairs.py judge <kind> <label> <cargo-json>              judge one failed leg
"""

from __future__ import annotations

import json
import pathlib
import subprocess
import sys

UMBRELLA = "smear"

# Members whose features `smear` forwards. `smear-smoke`, `smear-noatomic` and `source-census` are
# not re-exported by anything and have no features; the planner rejects an unexpected member rather
# than skipping it, so a sixth layer cannot arrive unnoticed.
MEMBERS = ("smear-lexer", "smear-parser", "smear-schema", "smear-compiler", "graphql-proto")

# (member, feature) pairs with no same-named umbrella feature, and the argument for owing no
# equivalence. An entry matching nothing is a failure, like every other table in this repository.
EQ_EXEMPT = {
  ("smear-schema", "build"): (
    "there is no `smear/build`. The builder is reachable only through `smear::validator::schema`, "
    "and `smear::validator` is gated by a `#[cfg]` smear writes itself — measured: a consumer with "
    "`smear-schema/build` on and `smear/validator` off cannot name `SchemaBuilder`."
  ),
}

# The observable half. Each entry is
#   (label, member, feature, extra smear features the path needs, extra fixture features)
# and requires a `uses-<label>` feature in the fixture manifest.
PROBES = (
  ("lexer-graphql", "smear-lexer", "graphql", (), ()),
  ("lexer-graphqlx", "smear-lexer", "graphqlx", (), ()),
  ("lexer-bytes", "smear-lexer", "bytes", (), ()),
  ("parser-graphql", "smear-parser", "graphql", ("parser",), ()),
  ("parser-graphqlx", "smear-parser", "graphqlx", ("parser",), ()),
  ("parser-rowan", "smear-parser", "rowan", ("parser",), ()),
  ("parser-test-support", "smear-parser", "test-support", ("parser", "graphql", "rowan"), ()),
  ("schema-introspection", "smear-schema", "introspection",
   ("parser", "graphql", "validator"), ()),
  ("compiler-rowan", "smear-compiler", "rowan", ("parser", "graphql", "validator"), ()),
  ("compiler-introspection", "smear-compiler", "introspection",
   ("parser", "graphql", "validator"), ()),
)

# The smear features a member's dependency edge needs before that member is in the graph at all.
PRESENCE = {
  "smear-lexer": (),
  "smear-schema": (),
  "smear-parser": ("parser",),
  "smear-compiler": ("parser", "graphql", "validator"),
  "graphql-proto": ("parser", "graphql", "validator", "proto"),
}


def die(message: str) -> None:
  print(f"::error::downstream_pairs: {message}", file=sys.stderr)
  raise SystemExit(1)


def metadata(manifest: pathlib.Path) -> dict:
  out = subprocess.run(
    ["cargo", "metadata", "--no-deps", "--format-version", "1",
     "--manifest-path", str(manifest)],
    capture_output=True, text=True,
  )
  if out.returncode != 0:
    die(f"cargo metadata failed for {manifest}: {out.stderr.strip()}")
  return json.loads(out.stdout)


def plan(workspace_manifest: pathlib.Path, fixture_manifest: pathlib.Path) -> int:
  ws = metadata(workspace_manifest)
  ids = set(ws["workspace_members"])
  pkgs = {p["name"]: p for p in ws["packages"] if p["id"] in ids}

  if UMBRELLA not in pkgs:
    die(f"no `{UMBRELLA}` in the workspace, so there is no umbrella to check against")
  umbrella_features = set(pkgs[UMBRELLA]["features"])

  missing_members = [m for m in MEMBERS if m not in pkgs]
  if missing_members:
    die(f"MEMBERS names packages the workspace does not have: {missing_members}")

  # C4 — a member that gained public API and is not in MEMBERS would be unchecked. Derived: any
  # workspace member with a `[features]` table other than the umbrella and the three tripwires.
  known = set(MEMBERS) | {UMBRELLA, "smear-smoke", "smear-noatomic", "source-census"}
  unknown = sorted(n for n, p in pkgs.items()
                   if n not in known and [f for f in p["features"] if f != "default"])
  if unknown:
    die(f"workspace members with features that this gate does not know about: {unknown}. "
        f"Add them to MEMBERS, with a probe if they carry a gated public path.")

  # C2 — the EQ family, total over cargo's own view of every member feature.
  eq_pairs = []
  used_exempt = set()
  for member in MEMBERS:
    for feature in sorted(f for f in pkgs[member]["features"] if f != "default"):
      if (member, feature) in EQ_EXEMPT:
        used_exempt.add((member, feature))
        continue
      if feature not in umbrella_features:
        die(f"`{member}/{feature}` has no `{UMBRELLA}/{feature}` and no EQ_EXEMPT entry, so this "
            f"gate cannot say what it should be equivalent to")
      eq_pairs.append((member, feature))
  stale = sorted(set(EQ_EXEMPT) - used_exempt)
  if stale:
    die(f"EQ_EXEMPT entries that match nothing: {stale}")

  # C1 — every probe names a real pair, and the fixture declares its `uses-` feature.
  fixture = metadata(fixture_manifest)
  fx = next(p for p in fixture["packages"] if p["name"] == "smear-downstream")
  fixture_features = set(fx["features"])
  for label, member, feature, _, _ in PROBES:
    if member not in pkgs or feature not in pkgs[member]["features"]:
      die(f"probe `{label}` names `{member}/{feature}`, which no longer exists")
    if f"uses-{label}" not in fixture_features:
      die(f"probe `{label}` has no `uses-{label}` feature in the fixture manifest")

  # C3 — no dev- or build-dependency in the fixture. `cargo metadata` resolves those and
  # `cargo build` does not, so one would split this plan from what the legs actually build.
  bad = sorted({d["name"] for d in fx["dependencies"] if d["kind"] in ("dev", "build")})
  if bad:
    die(f"the fixture has dev/build dependencies, which the plan cannot see: {bad}")

  # C5 — the fixture enters `smear` with no features of its own. A feature here would be an
  # ambient input to every leg at once, which is the shape of pql's R6.
  entry = next((d for d in fx["dependencies"] if d["name"] == UMBRELLA), None)
  if entry is None:
    die("the fixture does not depend on `smear`")
  if entry.get("features") or entry.get("uses_default_features", True):
    die(f"the fixture's `smear` entry must be `default-features = false` with no features; it is "
        f"features={entry.get('features')} default={entry.get('uses_default_features')}")

  legs = []
  for member, feature in eq_pairs:
    base = ["smear/std"] + [f"smear/{f}" for f in PRESENCE[member]]
    legs.append(("EQ-POS", f"{member}/{feature}",
                 ",".join(base + [f"smear/{feature}", f"{member}/{feature}"])))
    # A pair whose presence features already imply it cannot be withheld, so there is no
    # disagreeing graph to build and no leg to run. Recorded rather than skipped in silence.
    if feature in PRESENCE[member] or feature == "std":
      legs.append(("EQ-LEAK-NA", f"{member}/{feature}", ",".join(base)))
    else:
      legs.append(("EQ-LEAK", f"{member}/{feature}",
                   ",".join(base + [f"{member}/{feature}"])))

  for label, member, feature, extra, fx_extra in PROBES:
    base = ["smear/std"] + [f"smear/{f}" for f in extra]
    uses = [f"uses-{label}"] + list(fx_extra)
    legs.append(("CTL", label, ",".join(uses + base)))
    legs.append(("POS", label, ",".join(uses + base + [f"smear/{feature}"])))
    legs.append(("LEAK", label, ",".join(uses + base + [f"{member}/{feature}"])))

  # UNION — every probe at once, with every feature it needs. The only configuration worth
  # linting: the negative legs are SUPPOSED not to compile. The fixture is outside smear's
  # workspace, so `cargo fmt --all` and `cargo clippy --workspace` at the root do not reach it, and
  # a gate this repository's own gates do not check is a gate that rots.
  union_uses = sorted({f"uses-{label}" for label, *_ in PROBES})
  union_smear = sorted({f"smear/{f}" for _, _, f, _, _ in PROBES}
                       | {f"smear/{f}" for _, _, _, extra, _ in PROBES for f in extra}
                       | {"smear/std"})
  legs.append(("UNION", "all-probes", ",".join(union_uses + union_smear)))

  print(f"certified: {len(eq_pairs)} member features, {len(PROBES)} observable probes, "
        f"{len(EQ_EXEMPT)} exemption, {len(legs)} legs", file=sys.stderr)
  for kind, label, features in legs:
    print(f"{kind}\t{label}\t{features}")
  return 0


# The reasons a failing leg is allowed to fail. Anything else means the leg proved something other
# than the property — pql's R6, where "failed for the right reason" was a grep for a token the
# fixture itself supplied.
REASONS = {
  # The equivalence assertion is a `const _: () = { assert!(…) }` in `smear`, so a violating graph
  # fails const evaluation. The message is required too: E0080 alone would also accept an unrelated
  # const panic from anywhere in the graph.
  "EQ-LEAK": ("E0080", "disagree"),
  "LEAK": ("E0080", "disagree"),
  # The control must fail because the PATH is absent, which is E0432 for a `use` and E0433 for a
  # path in an expression. A CTL that failed on the equivalence would mean the leg is not testing
  # what it claims.
  "CTL": ("E0432|E0433|E0599|E0277", None),
}


def judge(kind: str, label: str, json_log: pathlib.Path) -> int:
  if kind not in REASONS:
    die(f"no reason is recorded for a `{kind}` leg, so its failure cannot be judged")
  codes, needle = REASONS[kind]
  wanted = set(codes.split("|"))
  seen_codes: set[str] = set()
  text = []
  for line in json_log.read_text().splitlines():
    try:
      msg = json.loads(line)
    except json.JSONDecodeError:
      continue
    if msg.get("reason") != "compiler-message":
      continue
    m = msg.get("message", {})
    code = (m.get("code") or {}).get("code")
    if code:
      seen_codes.add(code)
    if m.get("level") == "error":
      text.append(m.get("rendered") or m.get("message") or "")
  if not seen_codes & wanted:
    print(f"::error::{kind}({label}) failed, but with {sorted(seen_codes) or 'no error code'} and "
          f"not one of {sorted(wanted)}", file=sys.stderr)
    return 1
  if needle and not any(needle in t for t in text):
    print(f"::error::{kind}({label}) failed with the right code and the wrong message: "
          f"'{needle}' appears nowhere, so this is some other const panic", file=sys.stderr)
    return 1
  return 0


def main() -> int:
  if len(sys.argv) >= 2 and sys.argv[1] == "plan":
    if len(sys.argv) != 4:
      die("plan takes <workspace-manifest> <fixture-manifest>")
    return plan(pathlib.Path(sys.argv[2]), pathlib.Path(sys.argv[3]))
  if len(sys.argv) >= 2 and sys.argv[1] == "judge":
    if len(sys.argv) != 5:
      die("judge takes <kind> <label> <cargo-json>")
    return judge(sys.argv[2], sys.argv[3], pathlib.Path(sys.argv[4]))
  die("usage: downstream_pairs.py plan <workspace> <fixture> | judge <kind> <label> <json>")
  return 1


if __name__ == "__main__":
  sys.exit(main())

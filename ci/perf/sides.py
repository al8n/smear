#!/usr/bin/env python3
"""What each side of a perf comparison is, apart from what it measures.

`ci/perf/run.sh` builds two trees: the head's, and the merge-base's own with the head's instrument
laid over it. This answers the four questions about them that come before and beside a reading:

  instrument  which files the base side takes from the head
  profile     whether the two trees declare a build-profile setting the script's pin cannot override
  lock        which packages the two sides resolved differently
  features    which features each side compiled its own packages with

Usage:
    sides.py instrument <head-root> <head-metadata> <base-root> <base-metadata> <package>
                        <target>...
    sides.py profile <base-root> <head-root> <pinned-key>...
    sides.py lock <base-lock> <head-lock>
    sides.py features <base-root> <base-build.json> <head-root> <head-build.json>

`<*-metadata>` is the output of `cargo metadata --no-deps --format-version 1` in that tree, and
`<*-build.json>` the `--message-format=json` stream of that side's build. Every diagnostic goes to
stdout; `instrument` prints its file list there too, one path per line, and only when it succeeds.

Exit status 3 means the two revisions are not comparable by this instrument and the reason has been
printed; 1 is any other refusal; 2 is a usage error. Python 3.9, like the rest of `ci/perf/`.
"""

import json
import os
import re
import sys

NOT_COMPARABLE = 3


class Refusal(Exception):
  """A reason to stop, printed by `main` as an error."""


# ── instrument ───────────────────────────────────────────────────────────────────────────────
#
# The base side takes from the head exactly the files the bench targets read at compile time: their
# crate roots, every module those declare, and every file an `include_str!` or `include_bytes!`
# names. The set is derived here from the sources, not listed, because a list goes stale SILENTLY: a
# fixture added to the corpus and missing from it would be read from the merge-base's tree on one
# side and from the head's on the other, and the difference would be reported as the library's.
#
# The derivation follows rustc's rules for the forms these sources use and REFUSES every other way
# of reaching a file, so a construct it does not follow stops the run instead of shrinking the set:
# `include!`, an include whose path is not one string literal, `CARGO_MANIFEST_DIR` (which points
# into each side's own tree), an out-of-line `mod` inside any block, and a `path` attribute in any
# form but `#[path = "..."]` on an out-of-line `mod`. Files read at RUN time need nothing: `run.sh`
# runs both binaries from the head's checkout, so a relative path names one file for both.

TOKEN = re.compile(
  r"""
    (?P<newline>\n)
  | (?P<space>[^\S\n]+)
  | (?P<line_comment>//[^\n]*)
  | (?P<block_comment>/\*)
  | (?P<raw_string>[bc]?r(?P<hashes>\#*)")
  | (?P<string>[bc]?")
  | (?P<char>'(?:[^'\\\n]|\\(?:x[0-9a-fA-F]{2}|u\{[0-9a-fA-F_]{1,8}\}|.))')
  | (?P<lifetime>'[A-Za-z_][A-Za-z0-9_]*)
  | (?P<ident>[A-Za-z_][A-Za-z0-9_]*)
  | (?P<number>[0-9][A-Za-z0-9_]*)
  | (?P<punct>.)
  """,
  re.VERBOSE | re.DOTALL,
)
STRING_BODY = re.compile(r'(?:[^"\\]|\\.)*"', re.DOTALL)
INCLUDES = ("include_str", "include_bytes")


def lex(where, text):
  """`(kind, value, line)` for every token of a Rust source, comments and whitespace dropped.

  A string's value is its body as written, escapes undecoded. Only what the scanner below needs is
  distinguished: a brace or a quote inside a string, a char literal or a comment is not a token.
  """
  tokens, i, line = [], 0, 1
  while i < len(text):
    match = TOKEN.match(text, i)
    kind = match.lastgroup
    if kind == "hashes":
      kind = "raw_string"
    if kind == "newline":
      line += 1
      i = match.end()
    elif kind in ("space", "line_comment"):
      i = match.end()
    elif kind == "block_comment":
      depth, j = 1, match.end()
      while depth:
        if j >= len(text):
          raise Refusal(f"{where}:{line}: unterminated block comment")
        if text.startswith("/*", j):
          depth, j = depth + 1, j + 2
        elif text.startswith("*/", j):
          depth, j = depth - 1, j + 2
        else:
          j += 1
      line += text.count("\n", i, j)
      i = j
    elif kind == "raw_string":
      close = '"' + match.group("hashes")
      end = text.find(close, match.end())
      if end < 0:
        raise Refusal(f"{where}:{line}: unterminated raw string")
      tokens.append(("string", text[match.end():end], line))
      line += text.count("\n", i, end)
      i = end + len(close)
    elif kind == "string":
      body = STRING_BODY.match(text, match.end())
      if not body:
        raise Refusal(f"{where}:{line}: unterminated string")
      tokens.append(("string", text[match.end():body.end() - 1], line))
      line += text.count("\n", i, body.end())
      i = body.end()
    else:
      tokens.append((kind, match.group(), line))
      i = match.end()
  return tokens


def _is(token, kind, value=None):
  return token is not None and token[0] == kind and (value is None or token[1] == value)


def _at(tokens, i):
  return tokens[i] if 0 <= i < len(tokens) else None


def _closing(tokens, i, opening, closing):
  """The index of the token closing the group `tokens[i]` opens."""
  depth = 0
  for j in range(i, len(tokens)):
    if _is(tokens[j], "punct", opening):
      depth += 1
    elif _is(tokens[j], "punct", closing):
      depth -= 1
      if depth == 0:
        return j
  raise Refusal(f"line {tokens[i][2]}: unbalanced `{opening}`")


def _attribute(tokens, i):
  """`(inner, open, close)` when `tokens[i]` starts `#[...]` or `#![...]`, else None."""
  if not _is(_at(tokens, i), "punct", "#"):
    return None
  inner = _is(_at(tokens, i + 1), "punct", "!")
  start = i + 2 if inner else i + 1
  if not _is(_at(tokens, start), "punct", "["):
    return None
  return inner, start, _closing(tokens, start, "[", "]")


def scan(where, text):
  """What one source file reaches: `("include", path, line)` and `("mod", name, path, line)`.

  A `mod` item's path is the literal of its `#[path]` attribute, or None for the default lookup.
  """
  tokens = lex(where, text)
  found, consumed, depth, i = [], set(), 0, 0
  while i < len(tokens):
    kind, value, line = tokens[i]
    here = f"{where}:{line}"
    attribute = _attribute(tokens, i)
    if kind == "punct" and value in "{}":
      depth += 1 if value == "{" else -1
    elif kind == "string" and "CARGO_MANIFEST_DIR" in value:
      raise Refusal(
        f"{here}: names CARGO_MANIFEST_DIR, which points into each side's own tree, so a file "
        "reached through it is not laid over from the head"
      )
    elif attribute is not None:
      inner, start, end = attribute
      body = tokens[start + 1:end]
      names_path = any(
        _is(token, "ident", "path") and _is(_at(body, k + 1), "punct", "=")
        for k, token in enumerate(body)
      )
      if names_path:
        exact = (
          not inner and len(body) == 3 and _is(body[0], "ident", "path")
          and _is(body[1], "punct", "=") and _is(body[2], "string")
        )
        if not exact:
          raise Refusal(f"{here}: a `path` attribute in a form this derivation does not follow")
        # The item it sits on: skip the other attributes and a visibility, then it must be
        # `mod <name>;` at the top level of the file.
        k = end + 1
        while _attribute(tokens, k) is not None:
          k = _attribute(tokens, k)[2] + 1
        if _is(_at(tokens, k), "ident", "pub"):
          k += 1
          if _is(_at(tokens, k), "punct", "("):
            k = _closing(tokens, k, "(", ")") + 1
        if not (
          _is(_at(tokens, k), "ident", "mod") and _is(_at(tokens, k + 1), "ident")
          and _is(_at(tokens, k + 2), "punct", ";") and depth == 0
        ):
          raise Refusal(f"{here}: `#[path]` on something other than a top-level out-of-line `mod`")
        found.append(("mod", tokens[k + 1][1], body[2][1], line))
        consumed.add(k)
    elif kind == "ident" and value == "include" and _is(_at(tokens, i + 1), "punct", "!"):
      raise Refusal(f"{here}: `include!` splices a file this derivation does not follow")
    elif kind == "ident" and value in INCLUDES:
      opening = _at(tokens, i + 2)
      closing = {"(": ")", "[": "]", "{": "}"}.get(opening[1]) if _is(opening, "punct") else None
      literal = _at(tokens, i + 3)
      after = i + 5 if _is(_at(tokens, i + 4), "punct", ",") else i + 4
      if not (
        _is(_at(tokens, i + 1), "punct", "!") and closing and _is(literal, "string")
        and "\\" not in literal[1] and _is(_at(tokens, after), "punct", closing)
      ):
        raise Refusal(
          f"{here}: `{value}` in a form this derivation does not follow; it reads "
          f"`{value}!(\"<one plain string literal>\")` and nothing else"
        )
      found.append(("include", literal[1], line))
    elif kind == "ident" and value == "mod" and i not in consumed:
      if _is(_at(tokens, i + 1), "ident") and _is(_at(tokens, i + 2), "punct", ";"):
        if depth:
          raise Refusal(f"{here}: an out-of-line `mod` inside a block")
        found.append(("mod", tokens[i + 1][1], None, line))
    i += 1
  return found


def _contained(root, here, literal, where):
  """A path a source names, relative to the root; refused if it leaves the tree or is absent."""
  if os.path.isabs(literal):
    raise Refusal(f"{where}: reads `{literal}`, an absolute path outside the tree")
  relative = os.path.normpath(os.path.join(here, literal))
  if relative == os.pardir or relative.startswith(os.pardir + os.sep):
    raise Refusal(f"{where}: reads `{literal}`, which is outside the tree")
  if not os.path.isfile(os.path.join(root, relative)):
    raise Refusal(f"{where}: reads `{literal}`, which does not exist at the head")
  return relative


def instrument_files(root, entry_points):
  """Every file the given crate roots read at compile time, relative to `root`, sorted."""
  sources, data = set(), set()
  # (path, is it a "mod.rs" file): a crate root, a `mod.rs` and every `#[path]` file is one, so
  # its child modules are its siblings; a `name.rs` keeps them in `name/`. rustc's rule.
  queue = [(entry, True) for entry in entry_points]
  while queue:
    relative, mod_rs = queue.pop()
    if relative in sources:
      continue
    sources.add(relative)
    with open(os.path.join(root, relative), encoding="utf-8") as handle:
      text = handle.read()
    here = os.path.dirname(relative)
    for item in scan(relative, text):
      where = f"{relative}:{item[-1]}"
      if item[0] == "include":
        data.add(_contained(root, here, item[1], where))
        continue
      _, name, attribute, _ = item
      if attribute is not None:
        queue.append((_contained(root, here, attribute, where), True))
        continue
      stem = os.path.splitext(os.path.basename(relative))[0]
      owner = here if mod_rs else os.path.join(here, stem)
      candidates = [os.path.join(owner, name + ".rs"), os.path.join(owner, name, "mod.rs")]
      present = [c for c in candidates if os.path.isfile(os.path.join(root, c))]
      if len(present) != 1:
        raise Refusal(f"{where}: `mod {name};` resolves to {len(present)} of {candidates}")
      queue.append((present[0], os.path.basename(present[0]) == "mod.rs"))
  return sorted(sources | data)


def declared_benches(metadata_path, root, package, names):
  """`{target: path relative to root}` for the named bench targets `package`'s manifest declares."""
  with open(metadata_path, encoding="utf-8") as handle:
    metadata = json.load(handle)
  root = os.path.realpath(root)
  found = {}
  for candidate in metadata.get("packages", []):
    if candidate.get("name") != package:
      continue
    for target in candidate.get("targets", []):
      if "bench" in target.get("kind", []) and target.get("name") in names:
        found[target["name"]] = os.path.relpath(os.path.realpath(target["src_path"]), root)
  return found


def cmd_instrument(args):
  head_root, head_metadata, base_root, base_metadata, package = args[:5]
  names = args[5:]
  head = declared_benches(head_metadata, head_root, package, names)
  base = declared_benches(base_metadata, base_root, package, names)

  absent = [name for name in names if name not in head]
  if absent:
    raise Refusal(f"the head's `{package}` declares no bench target named {', '.join(absent)}")

  problems = []
  for name in names:
    if name not in base:
      problems.append(
        f"the merge-base's `{package}` declares no bench target `{name}`, so it has nothing to "
        "build the instrument as, and the head's manifest is not borrowed to give it one"
      )
    elif base[name] != head[name]:
      problems.append(
        f"`{name}` is `{base[name]}` in the merge-base's manifest and `{head[name]}` in the "
        "head's, so the base side would compile a file the instrument does not lay over"
      )
  if problems:
    for problem in problems:
      print(f"::error::{problem}")
    return NOT_COMPARABLE

  for relative in instrument_files(head_root, [head[name] for name in names]):
    print(relative)
  return 0


# ── profile ──────────────────────────────────────────────────────────────────────────────────
#
# `run.sh` pins the bench profile through `CARGO_PROFILE_BENCH_<KEY>`, which cargo ranks above
# `.cargo/config.toml` and both above `Cargo.toml`. What the environment cannot reach is any other
# key, and a per-package table, for which cargo reads no environment variable at all. So the two
# trees' profile declarations are compared, wherever cargo reads one inside a tree, and a difference
# outside the pinned keys of `bench` and the `release` it inherits from is refused rather than
# measured. Identical declarations, overrides included, are the same build on both sides and pass.
#
# The reading is a line scanner, not a TOML parser: Python 3.9 has none. Anything it cannot place —
# an inline table, a multi-line value, a key spelled oddly — comes out as a key the pin does not
# reach, so a declaration it misreads is refused, never waved through.

PROFILE_FILES = (
  "Cargo.toml",
  os.path.join(".cargo", "config.toml"),
  os.path.join(".cargo", "config"),
)


def _dotted(key):
  return ".".join(part.strip().strip("\"'") for part in key.split("."))


def profile_settings(root):
  """`{(file, dotted key): value}` for each `profile` setting declared where cargo reads one."""
  settings = {}
  for name in PROFILE_FILES:
    path = os.path.join(root, name)
    if not os.path.isfile(path):
      continue
    table = ""
    with open(path, encoding="utf-8") as handle:
      for raw in handle:
        line = raw.split("#", 1)[0].strip()
        if not line:
          continue
        if line.startswith("["):
          table = _dotted(line.strip("[] \t"))
          continue
        key, equals, value = line.partition("=")
        full = _dotted(f"{table}.{key}" if table else key) if equals else _dotted(f"{table}.{line}")
        if full == "profile" or full.startswith("profile."):
          settings[(name, full)] = value.strip()
  return settings


def _reach(full, pinned):
  parts = full.split(".")
  if len(parts) >= 2 and parts[1] not in ("bench", "release"):
    return "unused"
  if len(parts) == 3 and parts[2] in pinned:
    return "pinned"
  return "unreached"


VERDICTS = {
  "pinned": "overridden by the pin on both sides",
  "unused": "not read by a `--profile bench` build",
  "unreached": "NOT reached by the pin",
}


def cmd_profile(args):
  base_root, head_root, pinned = args[0], args[1], set(args[2:])
  base, head = profile_settings(base_root), profile_settings(head_root)
  rows = [
    (name, full, base.get((name, full)), head.get((name, full)), _reach(full, pinned))
    for name, full in sorted(set(base) | set(head))
    if base.get((name, full)) != head.get((name, full))
  ]
  if not rows:
    return 0
  print("perf: the two trees declare the build profile differently:")
  for name, full, b, h, verdict in rows:
    unset = "(unset)"
    print(
      f"perf:   {name}: {full}: base {b if b is not None else unset}, "
      f"head {h if h is not None else unset} — {VERDICTS[verdict]}"
    )
  if any(row[4] == "unreached" for row in rows):
    print(
      "::error::a profile setting differs between the two trees and the pin cannot override it, so "
      "the two sides would be compiled differently"
    )
    return NOT_COMPARABLE
  return 0


# ── lock ─────────────────────────────────────────────────────────────────────────────────────

LOCK_FIELD = re.compile(r'^(name|version|source) = "([^"]*)"\s*$')
CRATES_IO = "registry+https://github.com/rust-lang/crates.io-index"


def lock_packages(path):
  """`(name, version, source)` for every package a `Cargo.lock` records."""
  packages, current = set(), {}

  def flush():
    if "name" in current:
      packages.add((current["name"], current.get("version", "?"), current.get("source", "")))
    current.clear()

  with open(path, encoding="utf-8") as handle:
    for line in handle:
      if line.startswith("["):
        flush()
        continue
      match = LOCK_FIELD.match(line)
      if match:
        current[match.group(1)] = match.group(2)
  flush()
  return packages


def cmd_lock(args):
  base_path, head_path = args
  base, head = lock_packages(base_path), lock_packages(head_path)
  if base == head:
    with open(base_path, "rb") as b, open(head_path, "rb") as h:
      edges = "" if b.read() == h.read() else (
        "; the dependency edges between them differ, which is a manifest change, not a version one"
      )
    print(f"perf: lock parity: both sides resolved the same {len(head)} packages{edges}")
    return 0
  print(
    "perf: the two sides resolved different packages. Each resolved what its own manifests ask for,"
  )
  print(
    "perf: so this is a dependency change between the two revisions, and a delta below may be its"
  )
  print("perf: (- the merge-base only, + the head only):")
  rows = [(name, "-", version, source) for name, version, source in base - head]
  rows += [(name, "+", version, source) for name, version, source in head - base]
  for name, sign, version, source in sorted(rows, key=lambda row: (row[0], row[1] == "+", row[2])):
    origin = f" ({source})" if source and source != CRATES_IO else ""
    print(f"perf:   {sign} {name} {version}{origin}")
  return 0


# ── features ─────────────────────────────────────────────────────────────────────────────────


def _package_name(message):
  package_id = message.get("package_id", "")
  if "#" in package_id:
    url, fragment = package_id.rsplit("#", 1)
    if "@" in fragment:
      return fragment.split("@", 1)[0]
    return url.rstrip("/").rsplit("/", 1)[-1]
  return package_id.split(" ", 1)[0]


def compiled_features(root, stream):
  """`{package: {features: {target kinds}}}` for every package inside `root` the build compiled."""
  root = os.path.realpath(root) + os.sep
  found = {}
  with open(stream, encoding="utf-8") as handle:
    for line in handle:
      line = line.strip()
      if not line.startswith("{"):
        continue
      try:
        message = json.loads(line)
      except json.JSONDecodeError:
        continue
      if message.get("reason") != "compiler-artifact":
        continue
      if not os.path.realpath(message.get("manifest_path", "")).startswith(root):
        continue
      features = tuple(sorted(message.get("features", [])))
      kinds = "/".join(message.get("target", {}).get("kind", []))
      found.setdefault(_package_name(message), {}).setdefault(features, set()).add(kinds)
  return found


def cmd_features(args):
  sides = (
    ("base", compiled_features(args[0], args[1])),
    ("head", compiled_features(args[2], args[3])),
  )
  names = sorted(set(sides[0][1]) | set(sides[1][1]))
  width = max([len(name) for name in names] + [8])
  print("perf: the features each side compiled its own packages with (printed, not gated):")
  for name in names:
    for side, found in sides:
      sets = found.get(name, {})
      if not sets:
        print(f"perf:   {name:<{width}}  {side}  (not compiled)")
      for features, kinds in sorted(sets.items()):
        listed = ", ".join(features) or "(none)"
        targets = f"   [{', '.join(sorted(kinds))}]" if len(sets) > 1 else ""
        print(f"perf:   {name:<{width}}  {side}  {listed}{targets}")

  differing = []
  for name in names:
    base = set().union(*sides[0][1].get(name, {}).keys())
    head = set().union(*sides[1][1].get(name, {}).keys())
    if base != head:
      differing.append((name, sorted(base - head), sorted(head - base)))
  if not differing:
    print("perf: both sides compiled every one of their packages with the same features")
    return 0
  print("perf: the two sides compiled these with different features:")
  for name, only_base, only_head in differing:
    print(
      f"perf:   {name:<{width}}  base only: {', '.join(only_base) or '-'};  "
      f"head only: {', '.join(only_head) or '-'}"
    )
  return 0


COMMANDS = {
  "instrument": (cmd_instrument, 6, None),
  "profile": (cmd_profile, 2, None),
  "lock": (cmd_lock, 2, 2),
  "features": (cmd_features, 4, 4),
}


def main():
  if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
    print(__doc__, file=sys.stderr)
    return 2
  command, least, most = COMMANDS[sys.argv[1]]
  args = sys.argv[2:]
  if len(args) < least or (most is not None and len(args) > most):
    print(__doc__, file=sys.stderr)
    return 2
  try:
    return command(args)
  except Refusal as refusal:
    print(f"::error::{refusal}")
    return 1


if __name__ == "__main__":
  sys.exit(main())

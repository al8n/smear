#!/usr/bin/env python3
"""The Z1 code check's digest: design 2026-11-21 section 9.4, as gate v3 amends it (R2, R3, R4).

Usage:
    python3 -I codecheck_digest.py <base prefix> <head prefix> [--label R.Z] [--builds F',ZH]
                                   [--third <prefix>] [--no-third] [--no-siblings]
                                   [--sibling-root <batch directory>]...
    python3 -I codecheck_digest.py --chain [label=]<run>/base [label=]<run>/base ...   (or @<list file>)

The first form reads one pair. Two runs' bases make a step of the floor plan (design 11.6): the first
prefix is the base before, the second the base after, and the digest calls them base and head. Sections G.1
and G.2 are the floor plan's first two additions: the holders' own counts apart from the rest of each row,
and the token's Clone as a line of its own.

The second form is the third addition: one table over the chain, the bases in the order given, each a run's
`base` files. For each base the three GraphQLx rows for each lex call, the step from the base before it, the
holders' own counts for each entry, the shifts and how many decisions moved; and amendment R1's sum of the
steps that 11.6's step 5 leaves unread, judged once against the allowance.

A prefix is a directory plus `base` or `head`, as `ci/perf/codecheck.py collect` writes it:
<prefix>.sha256, <prefix>.nm.txt, <prefix>.objdump.txt.gz and <prefix>.callgrind.out.gz, with
versions.txt beside them. Standard library only. It reads those files, and nothing else except:
the sibling runs' .sha256 and callgrind totals (9.4's SHA-256 check, and the gate's determinism
check), and for R.Z and R.V the other run's head (question 3 sets F', ZH and V1 side by side).
The label is the directory's name unless --label names it.

Both of rustc's manglings are read. The legacy one names an instance by a hash; v0 spells the
instance out and tells crates apart by a disambiguator. "Names without the hashes" (9.4's
fallback) is the legacy name less its hash, or the v0 name demangled without disambiguators.
"""

import bisect
import gzip
import hashlib
import os
import re
import sys

# ---------------------------------------------------------------------------------------------
# The constants 9.4 asks the digest to hold at its top
# ---------------------------------------------------------------------------------------------

# Tokens one call of a row's closure lexes, at (lo, hi), and the documents it lexes: each document's
# end is one `lex` call more. Design 10.5: a copy of the executable fixture is 471 tokens, so that
# row's two sizes are 30,144 and 60,288 tokens; the schemas' are 7,548 and 14,535. A parse row's
# four closure calls are then 180,868 `lex` calls on the executable row and 44,190 on the
# type-system row, the counts the test itself executed in R.C (design 10). Each GraphQL twin parses
# the same text as its GraphQLx row, and each lexer row lexes the type-system row's schemas.
TOKENS = {
    "parse_graphqlx_executable": (30_144, 60_288),
    "parse_executable": (30_144, 60_288),
    "parse_graphqlx_type_system": (7_548, 14_535),
    "parse_type_system": (7_548, 14_535),
}
DOCS = {
    "parse_graphqlx_executable": (1, 1),
    "parse_executable": (1, 1),
    "parse_graphqlx_type_system": (4, 8),
    "parse_type_system": (4, 8),
}
CLOSURE_CALLS = 4  # a probe and one batch, at each of two sizes (9.4; the gate verified it)
EXEC, TS, LEXX = "parse_graphqlx_executable", "parse_graphqlx_type_system", "lex_graphqlx_syntactic"
TWIN = {EXEC: "parse_executable", TS: "parse_type_system", LEXX: "lex_graphql_syntactic"}
ROWS = [EXEC, TS, LEXX, TWIN[EXEC], TWIN[TS], TWIN[LEXX]]
PARSE_ROWS = [EXEC, TS, TWIN[EXEC], TWIN[TS]]
LEX_ROWS = [LEXX, TWIN[LEXX]]
TEST_MAX = {"Ir": 5.0, "Dr": 1.5, "Dw": 0.5}  # question 1, per lex call: the test and no more
SECTION2 = (3.0, 9.0)  # question 3: section 2's predicted excess of driver reads and writes, per lex call
HOT_SHARE = 0.005  # hot: own instructions >= 0.5% of the two GraphQLx parse rows' inclusive ones
NAMES_COMPARE = 0.90  # 9.4: nine in ten of the base's text symbols found on the head by full name
FRAME_SCAN = 24  # 9.4: the frame is the first `sub rsp` among the first 24 instructions
# Part E: the answer a twin is handed. V1's commit message: "the copy (72 bytes) is smaller than the
# temporary (80)": an item is 72 bytes, and V1's `InBlock` 80.
ITEM_BYTES = {"V1": 80}
ITEM_DEFAULT = 72
TOP_F = 30
TOP_OFFSETS = 12
COUNTING_TOKENS = (7_548 + 4) + (14_535 + 8)  # a lexer row's counting run: each size once, in lex calls
B_EVENTS = ["Ir", "Dr", "Dw", "Bc", "Bcm", "Bi", "Bim", "I1mr", "D1mr", "D1mw"]
DET_EVENTS = ["Ir", "Dr", "Dw", "Bc", "Bi"]  # host-independent under valgrind (the gate)

# The runs of 9.6: label -> (base build, head build).
RUNS = {"R.Z": ("F'", "ZH"), "R.V": ("F'", "V1"), "R.T": ("F'", "PT"), "R.C": ("F'", "PC"),
        "R.C2": ("F'", "PC2"), "R.M": ("M", "F'"),
        # design 10.8: the second reading of the dispatch, and V2 against F', ZH and main
        "R.Z2": ("F'", "ZH"), "W.F1": ("F'", "V2"), "W.F2": ("F'", "V2"), "W.Z1": ("ZH", "V2"),
        "W.Z2": ("ZH", "V2"), "W.M1": ("M", "V2"), "W.M2": ("M", "V2")}
# Which of GraphQLx's `lex_in_block` and `prefixed` each build holds: V2 deletes the twin and keeps
# `prefixed`, cold, answering the prefixed string's payload (design 10.6).
HAS_TWIN = {"F'": (False, False), "PT": (False, False), "PC": (False, False), "PC2": (False, False),
            "M": (False, False), "ZH": (True, True), "V1": (True, True), "V2": (False, True)}
# Design 10.7: V2's holders are in order up to 7 instructions, 4 reads and 2 writes above F''s for
# each `lex` call; and 10.7's predicted rows, V2 less F', for each `lex` call.
V2_MARGIN = {"Ir": 7.0, "Dr": 4.0, "Dw": 2.0}
V2_PREDICTED = {EXEC: {"Ir": 3.6, "Dr": 1.8, "Dw": 0.0, "Bc": 1.8},
                TS: {"Ir": 2.9, "Dr": 1.5, "Dw": 0.0, "Bc": 1.5}}
# The call targets a probe may add (9.5, R3). PC2 is PC with the callee made opaque (the gate's R1).
INGREDIENT = {"PT": (), "PC": ("probe_block",), "PC2": ("probe_block",)}
Q3_SIBLING = {"R.Z": "R.V", "R.V": "R.Z"}

sys.setrecursionlimit(20000)


def tokens_of(row):
    """The `lex` calls of a parse row's four closure calls: a probe and one batch at each size."""
    lo, hi = TOKENS[row]
    dlo, dhi = DOCS[row]
    return 2 * (lo + dlo) + 2 * (hi + dhi)


# ---------------------------------------------------------------------------------------------
# Names
# ---------------------------------------------------------------------------------------------

_VENDOR = re.compile(r"(?:\.llvm\.\d+)+$")
_LEGACY_HASH = re.compile(r"17h[0-9a-f]{16}E$")
_IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_LEGACY_ESC = {"SP": "@", "BP": "*", "RF": "&", "LT": "<", "GT": ">", "LP": "(", "RP": ")", "C": ","}


def strip_vendor(name):
    """A mangled name without thin LTO's `.llvm.<n>` promotion suffix, which names no instance."""
    return _VENDOR.sub("", name)


def legacy_components(name):
    if not name.startswith("_ZN"):
        return None
    out, i, n = [], 3, len(name)
    while i < n and name[i] != "E":
        j = i
        while j < n and name[j].isdigit():
            j += 1
        if j == i:
            return None
        ln = int(name[i:j])
        out.append(name[j:j + ln])
        i = j + ln
    return out if i < n else None


def legacy_unescape(c):
    if c.startswith("_$"):
        c = c[1:]
    out, i, n = [], 0, len(c)
    while i < n:
        ch = c[i]
        if ch == ".":
            if c.startswith("..", i):
                out.append("::")
                i += 2
            else:
                out.append(".")
                i += 1
        elif ch == "$":
            j = c.find("$", i + 1)
            if j < 0:
                out.append(c[i:])
                break
            esc = c[i + 1:j]
            if esc in _LEGACY_ESC:
                out.append(_LEGACY_ESC[esc])
            elif len(esc) > 1 and esc[0] == "u" and all(x in "0123456789abcdef" for x in esc[1:]):
                out.append(chr(int(esc[1:], 16)))
            else:
                out.append(c[i:j + 1])
            i = j + 1
        else:
            j = i
            while j < n and c[j] not in ".$":
                j += 1
            out.append(c[i:j])
            i = j
    return "".join(out)


def demangle_legacy(name):
    comps = legacy_components(strip_vendor(name))
    if comps is None:
        return None
    if comps and re.fullmatch(r"h[0-9a-f]{16}", comps[-1]):
        comps = comps[:-1]
    return "::".join(legacy_unescape(c) for c in comps)


class _Bad(Exception):
    pass


_BASIC = {"b": "bool", "c": "char", "e": "str", "u": "()", "a": "i8", "s": "i16", "l": "i32",
          "x": "i64", "n": "i128", "i": "isize", "h": "u8", "t": "u16", "m": "u32", "y": "u64",
          "o": "u128", "j": "usize", "f": "f32", "d": "f64", "z": "!", "p": "_", "v": "..."}


class _V0:
    """Rust's v0 mangling (RFC 2603), printed as rustc-demangle prints it. `verbose` adds the crate
    disambiguators; `args` false drops every generic argument list."""

    LIMIT = 400000

    def __init__(self, inner, verbose, args):
        self.s, self.i, self.verbose, self.args, self.depth, self.bound = inner, 0, verbose, args, 0, 0

    def peek(self):
        return self.s[self.i] if self.i < len(self.s) else ""

    def next(self):
        if self.i >= len(self.s):
            raise _Bad("end")
        c = self.s[self.i]
        self.i += 1
        return c

    def eat(self, c):
        if self.i < len(self.s) and self.s[self.i] == c:
            self.i += 1
            return True
        return False

    def int62(self):
        if self.eat("_"):
            return 0
        x = 0
        while True:
            c = self.next()
            if c == "_":
                return x + 1
            if "0" <= c <= "9":
                d = ord(c) - 48
            elif "a" <= c <= "z":
                d = ord(c) - 87
            elif "A" <= c <= "Z":
                d = ord(c) - 29
            else:
                raise _Bad("base62")
            x = x * 62 + d

    def opt62(self, tag):
        return self.int62() + 1 if self.eat(tag) else 0

    def ident(self):
        puny = self.eat("u")
        c = self.next()
        if not ("0" <= c <= "9"):
            raise _Bad("ident")
        n = int(c)
        if n:
            while "0" <= self.peek() <= "9" and self.peek():
                n = n * 10 + int(self.next())
        self.eat("_")
        if self.i + n > len(self.s):
            raise _Bad("ident length")
        name = self.s[self.i:self.i + n]
        self.i += n
        if puny:
            k = name.rfind("_")
            ascii_part, code = (name[:k], name[k + 1:]) if k >= 0 else ("", name)
            try:
                name = (ascii_part + "-" + code).encode("ascii").decode("punycode")
            except Exception:  # noqa: BLE001
                name = "u" + name
        return name

    def backref(self, fn):
        start = self.i - 1
        pos = self.int62()
        if pos >= start:
            raise _Bad("backref")
        saved = self.i
        self.depth += 1
        if self.depth > 400:
            raise _Bad("depth")
        self.i = pos
        try:
            return fn()
        finally:
            self.i = saved
            self.depth -= 1

    def lifetime(self, lt):
        if lt == 0:
            return "'_"
        depth = self.bound - lt
        if depth < 0:
            raise _Bad("lifetime")
        return "'" + chr(97 + depth) if depth < 26 else "'_" + str(depth)

    def in_binder(self, fn):
        n = self.opt62("G")
        pre = ""
        if n:
            names = []
            for _ in range(n):
                self.bound += 1
                names.append(self.lifetime(1))
            pre = "for<" + ", ".join(names) + "> "
        try:
            return pre + fn()
        finally:
            self.bound -= n

    def path(self, in_value):
        tag = self.next()
        if tag == "C":
            dis = self.opt62("s")
            name = self.ident()
            return name + ("[%x]" % dis if self.verbose and dis else "")
        if tag == "N":
            ns = self.next()
            inner = self.path(in_value)
            dis = self.opt62("s")
            name = self.ident()
            if "A" <= ns <= "Z":
                kind = {"C": "closure", "S": "shim"}.get(ns, ns)
                return inner + "::{" + kind + (":" + name if name else "") + "#" + str(dis) + "}"
            if "a" <= ns <= "z":
                return inner + ("::" + name if name else "")
            raise _Bad("namespace")
        if tag in "MXY":
            if tag != "Y":
                self.opt62("s")
                self.path(False)
            ty = self.type()
            if tag == "M":
                return "<" + ty + ">"
            return "<" + ty + " as " + self.path(False) + ">"
        if tag == "I":
            p = self.path(in_value)
            args = []
            while not self.eat("E"):
                args.append(self.generic_arg())
            if not self.args:
                return p
            out = p + ("::" if in_value else "") + "<" + ", ".join(args) + ">"
            if len(out) > self.LIMIT:
                raise _Bad("long")
            return out
        if tag == "B":
            return self.backref(lambda: self.path(in_value))
        raise _Bad("path " + tag)

    def generic_arg(self):
        if self.eat("L"):
            return self.lifetime(self.int62())
        if self.eat("K"):
            return self.const(False)
        return self.type()

    def type(self):
        tag = self.next()
        basic = _BASIC.get(tag)
        if basic is not None:
            return basic
        self.depth += 1
        if self.depth > 400:
            raise _Bad("depth")
        try:
            if tag in "RQ":
                out = "&"
                if self.eat("L"):
                    lt = self.int62()
                    if lt:
                        out += self.lifetime(lt) + " "
                if tag == "Q":
                    out += "mut "
                return out + self.type()
            if tag in "PO":
                return ("*const " if tag == "P" else "*mut ") + self.type()
            if tag in "AS":
                t = self.type()
                return "[" + t + ("; " + self.const(True) if tag == "A" else "") + "]"
            if tag == "T":
                ts = []
                while not self.eat("E"):
                    ts.append(self.type())
                return "(" + ", ".join(ts) + ("," if len(ts) == 1 else "") + ")"
            if tag == "F":
                return self.in_binder(self.fn_sig)
            if tag == "D":
                out = "dyn " + self.in_binder(self.dyn_traits)
                if not self.eat("L"):
                    raise _Bad("dyn lifetime")
                lt = self.int62()
                return out + (" + " + self.lifetime(lt) if lt else "")
            if tag == "B":
                return self.backref(self.type)
            self.i -= 1
            return self.path(False)
        finally:
            self.depth -= 1

    def fn_sig(self):
        unsafe = self.eat("U")
        abi = None
        if self.eat("K"):
            abi = "C" if self.eat("C") else self.ident().replace("_", "-")
        out = ("unsafe " if unsafe else "") + ('extern "' + abi + '" ' if abi is not None else "")
        ts = []
        while not self.eat("E"):
            ts.append(self.type())
        out += "fn(" + ", ".join(ts) + ")"
        if self.eat("u"):
            return out
        return out + " -> " + self.type()

    def dyn_traits(self):
        parts = []
        while not self.eat("E"):
            parts.append(self.dyn_trait())
        return " + ".join(parts)

    def dyn_trait(self):
        text, is_open = self.path_maybe_open()
        while self.eat("p"):
            name = self.ident()
            ty = self.type()
            if not self.args:
                continue
            text += (", " if is_open else "<") + name + " = " + ty
            is_open = True
        return text + (">" if is_open else "")

    def path_maybe_open(self):
        if self.eat("B"):
            return self.backref(self.path_maybe_open)
        if self.eat("I"):
            p = self.path(False)
            args = []
            while not self.eat("E"):
                args.append(self.generic_arg())
            if not self.args:
                return p, False
            return p + "<" + ", ".join(args), True
        return self.path(False), False

    def hex_nibbles(self):
        start = self.i
        while self.peek() and self.peek() in "0123456789abcdef":
            self.i += 1
        digits = self.s[start:self.i]
        if not self.eat("_"):
            raise _Bad("hex")
        return digits

    def const(self, in_value):
        tag = self.next()
        if tag == "p":
            return "_"
        if tag == "B":
            return self.backref(lambda: self.const(in_value))
        if tag in "htmyoj" or tag in "aslxni":
            neg = tag in "aslxni" and self.eat("n")
            v = int(self.hex_nibbles() or "0", 16)
            return ("-" if neg else "") + str(v) + (_BASIC[tag] if self.verbose else "")
        if tag == "b":
            v = int(self.hex_nibbles() or "0", 16)
            if v > 1:
                raise _Bad("bool")
            return "true" if v else "false"
        if tag == "c":
            return repr(chr(int(self.hex_nibbles() or "0", 16)))
        lb, rb = ("", "") if in_value else ("{", "}")
        if tag == "e":
            return lb + "*" + self.str_lit() + rb
        if tag in "RQ":
            if tag == "R" and self.eat("e"):
                return self.str_lit()
            return lb + "&" + ("mut " if tag == "Q" else "") + self.const(True) + rb
        if tag in "AT":
            items = []
            while not self.eat("E"):
                items.append(self.const(True))
            if tag == "A":
                return lb + "[" + ", ".join(items) + "]" + rb
            return lb + "(" + ", ".join(items) + ("," if len(items) == 1 else "") + ")" + rb
        if tag == "V":
            p = self.path(True)
            k = self.next()
            if k == "U":
                return lb + p + rb
            items = []
            while not self.eat("E"):
                if k == "S":
                    self.opt62("s")
                    items.append(self.ident() + ": " + self.const(True))
                else:
                    items.append(self.const(True))
            if k == "T":
                return lb + p + "(" + ", ".join(items) + ")" + rb
            if k == "S":
                return lb + p + " { " + ", ".join(items) + " }" + rb
            raise _Bad("variant")
        raise _Bad("const " + tag)

    def str_lit(self):
        digits = self.hex_nibbles()
        try:
            text = bytes.fromhex(digits).decode("utf-8")
        except ValueError:
            raise _Bad("str")
        return '"' + text.replace("\\", "\\\\").replace('"', '\\"') + '"'


def demangle_v0(name, verbose=False, args=True):
    name = strip_vendor(name)
    if not name.startswith("_R"):
        return None
    inner = name[2:]
    if not inner or not ("A" <= inner[0] <= "Z"):
        return None
    try:
        return _V0(inner, verbose, args).path(True)
    except (_Bad, RecursionError, ValueError, IndexError):
        return None


class Names:
    """Each raw name's readable forms, cached."""

    def __init__(self):
        self._bare, self._fallback, self._tokens = {}, {}, {}

    def bare(self, raw):
        """The path without generic arguments, hashes or disambiguators: what the matchers read."""
        v = self._bare.get(raw)
        if v is None:
            if raw.startswith("_R"):
                v = demangle_v0(raw, args=False) or raw
            elif raw.startswith("_ZN"):
                v = demangle_legacy(raw) or raw
            else:
                v = raw
            self._bare[raw] = v
        return v

    def fallback(self, raw):
        """9.4's "names without the hashes": legacy less its hash; v0 demangled, no disambiguators."""
        v = self._fallback.get(raw)
        if v is None:
            if raw.startswith("_R"):
                v = demangle_v0(raw) or raw
            elif raw.startswith("_ZN"):
                v = _LEGACY_HASH.sub("E", strip_vendor(raw))
            else:
                v = raw
            self._fallback[raw] = v
        return v

    def tokens(self, raw):
        v = self._tokens.get(raw)
        if v is None:
            v = _IDENT.findall(self.bare(raw)) if raw.startswith(("_R", "_ZN")) else _IDENT.findall(raw)
            self._tokens[raw] = v
        return v


NAMES = Names()


def marker_dialect(raw):
    """9.4's marker: the lexer's cold, never-inlined `over_recursion_limit`, by dialect."""
    t = NAMES.tokens(raw)
    if len(t) >= 3 and t[0] == "smear_lexer" and t[-1] == "over_recursion_limit" and t[1] in ("graphqlx", "graphql"):
        return t[1]
    return None


def lexer_item(raw, item):
    """A function `item` of GraphQLx's syntactic lexer (`lex_in_block`, `prefixed`, `probe_block`)."""
    t = NAMES.tokens(raw)
    return t[:3] == ["smear_lexer", "graphqlx", "syntactic"] and t[-1] == item


def row_closure(raw, row):
    t = NAMES.tokens(raw)
    return t[:3] == ["wall_clock", "perf", row] and "closure" in t[3:]


def row_function(raw, row):
    return NAMES.tokens(raw) == ["wall_clock", "perf", row]


def kind_of(raw):
    """9.4's kinds, in its order."""
    if lexer_item(raw, "lex_in_block"):
        return "twin"
    if lexer_item(raw, "prefixed"):
        return "prefixed"
    t = NAMES.tokens(raw)
    if t[:1] == ["wall_clock"]:
        return "loop"
    if "tokora" in t:
        return "driver"
    if "smear_parser" in t:
        return "production"
    return "other"


def key_tokens(key):
    return NAMES.tokens(key) if key.startswith(("_R", "_ZN")) else _IDENT.findall(key)


def tag_of(key):
    return hashlib.sha1(key.encode()).hexdigest()[:6]


def show(raw, width=150):
    text = NAMES.bare(raw) if raw.startswith(("_R", "_ZN")) else raw
    if len(text) > width:
        text = text[:width - 3] + "..."
    return text


# ---------------------------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------------------------


def open_text(path):
    if path.endswith(".gz"):
        return gzip.open(path, "rt", encoding="utf-8", errors="replace")
    return open(path, "r", encoding="utf-8", errors="replace")


def existing(*paths):
    for p in paths:
        if os.path.exists(p) and os.path.getsize(p) > 0:
            return p
    return None


def fint(x):
    if x is None:
        return "-"
    if isinstance(x, float) and not x.is_integer():
        return "{:,.1f}".format(x)
    return "{:,}".format(int(x))


def fsigned(x, digits=3):
    if x is None:
        return "-"
    return ("{:+,.%df}" % digits).format(x)


def fpct(x, digits=2):
    return "-" if x is None else ("{:.%df}%%" % digits).format(100.0 * x)


def vadd(a, b):
    for k in range(len(b)):
        a[k] += b[k]


def _num(t):
    return int(t, 16) if t.startswith("0x") else int(t)


_PREFIXES = {"bnd", "notrack", "lock", "rep", "repz", "repnz", "repe", "repne", "data16", "addr32",
             "cs", "ds", "es", "fs", "gs", "ss"}
_DIRECT = re.compile(r"^(?:0x)?([0-9a-f]+) <([^>]*)>")
_DIRECT_ANY = re.compile(r"(?<=\s)(?:0x)?([0-9a-f]+) <([^>]*)>$")
_GOT = re.compile(r"#\s*(?:0x)?([0-9a-f]+) <([^>]*)>")
_RIP = re.compile(r"rip\+0x[0-9a-f]+")
_LLVM_ANY = re.compile(r"\.llvm\.\d+")
_STACK = re.compile(r"(?:(BYTE|WORD|DWORD|QWORD|TBYTE|XMMWORD|YMMWORD|ZMMWORD) PTR )?(?:[a-z]s:)?"
                    r"\[(rsp|rbp)(?:([+-])0x([0-9a-f]+))?\]")
_WIDTH = {"BYTE": 1, "WORD": 2, "DWORD": 4, "QWORD": 8, "TBYTE": 10, "XMMWORD": 16, "YMMWORD": 32,
          "ZMMWORD": 64}
_READS_FIRST = {"cmp", "test", "bt", "ptest", "vptest", "ucomiss", "ucomisd", "comiss", "comisd",
                "vucomiss", "vucomisd", "vcomiss", "vcomisd", "push", "call", "jmp", "ldmxcsr",
                "vldmxcsr", "fld", "fild", "fldcw", "fcom", "fcomp", "ficom", "clflush", "clflushopt"}
_STORES = ("mov", "vmov", "set", "stos", "pextr", "vpextr", "extractps", "vextract", "maskmov",
           "vmaskmov", "vpmaskmov", "vcvtps2ph", "fst", "fist", "fnst", "stmxcsr", "vstmxcsr",
           "fxsave", "xsave", "vpscatter", "vscatter", "vcompress", "vpcompress", "pop")
_ARG_REGS = ("rdi", "rsi", "rdx", "rcx", "r8", "r9")


def split_insn(text):
    parts = text.split(None, 1)
    if not parts:
        return "", ""
    mn, ops = parts[0], (parts[1] if len(parts) > 1 else "")
    while (mn in _PREFIXES or mn.startswith("rex")) and ops:
        parts = ops.split(None, 1)
        mn, ops = parts[0], (parts[1] if len(parts) > 1 else "")
    return mn, ops.strip()


def site_kind(mn, index, count):
    """9.4's stack site: an address for `lea`, a store when the operand is a move's destination, a
    load otherwise, and both for a read-modify-write."""
    if mn == "lea":
        return "address"
    if mn.startswith(("nop", "prefetch")):
        return None
    if mn in ("xchg", "xadd", "cmpxchg", "cmpxchg8b", "cmpxchg16b"):
        return "both"
    if index > 0:
        return "load"
    if mn in _READS_FIRST or mn.startswith("j"):
        return "load"
    if count == 1 and mn in ("mul", "div", "idiv", "imul"):
        return "load"
    if mn.startswith(_STORES) and not mn.startswith(("movzx", "movsx", "movsxd")):
        return "store"
    return "both"


_MEM4 = re.compile(r"^DWORD PTR (?:[a-z]s:)?\[([a-z][a-z0-9]*)((?:\+[a-z][a-z0-9]*\*[1248])?)"
                   r"(?:([+-])0x([0-9a-f]+))?\]$")
_REG32 = {"eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp"} | {"r%dd" % i for i in range(8, 16)}
_SHIFTS = {"shl", "shr", "sar", "sal", "shld", "shrd", "shlx", "shrx", "sarx"}
_NOT_WRITES = {"cmp", "test", "push", "call", "jmp", "bt", "ucomiss", "ucomisd", "comiss", "comisd"}


def reg_family(r):
    r = r.strip()
    m = re.fullmatch(r"r(\d+)[dwb]?", r)
    if m:
        return "r" + m.group(1)
    for fam in ("ax", "bx", "cx", "dx"):
        if r in ("r" + fam, "e" + fam, fam, fam[0] + "l", fam[0] + "h"):
            return fam
    for fam in ("si", "di", "sp", "bp"):
        if r in ("r" + fam, "e" + fam, fam, fam + "l"):
            return fam
    return r


def seven_byte_moves_and_shifts(insns, start, end):
    """Design 10: a seven-byte move is two four-byte moves to or from memory through one base
    register, at offsets three apart: two loads, or two stores, in one block with the base unchanged
    between them. And every shift, of any width. Sites, not counts: the counts are callgrind's."""
    targets = set()
    for a, t in insns:
        mn, ops = split_insn(t)
        if mn.startswith("j"):
            m = _DIRECT.match(ops)
            if m and start <= int(m.group(1), 16) < end:
                targets.add(int(m.group(1), 16))
    movs, shifts, mns = [], [], []
    for i, (a, t) in enumerate(insns):
        mn, ops = split_insn(t)
        mns.append(mn)
        if mn in _SHIFTS:
            o = [x.strip() for x in ops.split("#", 1)[0].split(",")]
            shifts.append((a, mn, o[-1] if len(o) > 1 else "1", t))
        if mn != "mov":
            continue
        o = [x.strip() for x in ops.split("#", 1)[0].split(",")]
        if len(o) != 2:
            continue
        m0, m1 = _MEM4.match(o[0]), _MEM4.match(o[1])
        if m0 and (o[1] in _REG32 or o[1].startswith("0x")):
            m, d = m0, "store"
        elif m1 and o[0] in _REG32:
            m, d = m1, "load"
        else:
            continue
        disp = int(m.group(4), 16) if m.group(4) else 0
        if m.group(3) == "-":
            disp = -disp
        movs.append((i, a, d, m.group(1), m.group(2), disp, t))
    used, pairs = set(), []
    for k, (i, a, d, base, idx, disp, t) in enumerate(movs):
        if i in used:
            continue
        for (j, a2, d2, base2, idx2, disp2, t2) in movs[k + 1:]:
            if j - i > 6:
                break
            if j in used or d2 != d or base2 != base or idx2 != idx or abs(disp2 - disp) != 3:
                continue
            if any(insns[x][0] in targets for x in range(i + 1, j + 1)):
                continue
            if any(mns[x].startswith(("j", "call", "ret")) for x in range(i, j)):
                continue
            fam = reg_family(base)
            clobbered = False
            for x in range(i + 1, j):
                mnx, opsx = split_insn(insns[x][1])
                first = opsx.split(",", 1)[0].strip()
                if mnx not in _NOT_WRITES and "[" not in first and reg_family(first) == fam:
                    clobbered = True
                    break
            if clobbered:
                continue
            used.add(i)
            used.add(j)
            pairs.append((a, a2, d, base, min(disp, disp2), t, t2, idx))
            break
    return pairs, shifts


def stack_sites(insns):
    """Every instruction with an operand [rsp+k] or [rbp+-k]: (address, base, displacement, width,
    kind, mnemonic, text, register a `lea` writes)."""
    out = []
    for a, t in insns:
        if "[rsp" not in t and "[rbp" not in t:
            continue
        mn, ops = split_insn(t)
        body = ops.split("#", 1)[0].strip()
        operands = [o.strip() for o in body.split(",")] if body else []
        for idx, op in enumerate(operands):
            m = _STACK.search(op)
            if not m:
                continue
            kind = site_kind(mn, idx, len(operands))
            if kind is None:
                continue
            disp = int(m.group(4), 16) if m.group(4) else 0
            if m.group(3) == "-":
                disp = -disp
            lea_reg = operands[0] if (mn == "lea" and operands) else None
            out.append((a, m.group(2), disp, _WIDTH.get(m.group(1), 0), kind, mn, t, lea_reg))
    return out


# ---------------------------------------------------------------------------------------------
# The binary's GOT: a PIE built with rustc's default `-Z plt=no` calls another crate's non-generic
# function through a GOT slot, which objdump names only as `_DYNAMIC+0x...`. The slot's target is
# in the binary's dynamic relocations, which the artifact's own copy of the binary holds.
# ---------------------------------------------------------------------------------------------


def _cstr(data, off):
    end = data.find(b"\0", off)
    return data[off:end if end >= 0 else len(data)].decode("utf-8", "replace")


def read_got(path):
    """GOT slot address -> (target address or None, symbol name or None), from SHT_RELA's
    R_X86_64_RELATIVE, GLOB_DAT, JUMP_SLOT and 64, and SHT_RELR. ELF64 little-endian only."""
    import struct
    try:
        with open(path, "rb") as h:
            data = h.read()
    except OSError:
        return None
    if len(data) < 64 or data[:4] != b"\x7fELF" or data[4] != 2 or data[5] != 1:
        return None
    try:
        e_shoff, = struct.unpack_from("<Q", data, 0x28)
        e_shentsize, e_shnum, e_shstrndx = struct.unpack_from("<HHH", data, 0x3a)
        secs = []
        for i in range(e_shnum):
            (name, typ, _flags, addr, offset, size, link, _info, _align,
             entsize) = struct.unpack_from("<IIQQQQIIQQ", data, e_shoff + i * e_shentsize)
            secs.append((typ, addr, offset, size, link, entsize))
        out = {}

        def file_offset(vaddr):
            for typ, addr, offset, size, _, _ in secs:
                if typ != 8 and addr and addr <= vaddr < addr + size:  # not SHT_NOBITS
                    return offset + (vaddr - addr)
            return None

        for typ, addr, offset, size, link, entsize in secs:
            if typ == 4:  # SHT_RELA
                sym = secs[link] if link < len(secs) else None
                strtab = secs[sym[4]] if sym is not None and sym[4] < len(secs) else None
                for k in range(size // 24):
                    r_offset, r_info, r_addend = struct.unpack_from("<QQq", data, offset + 24 * k)
                    rtype, rsym = r_info & 0xFFFFFFFF, r_info >> 32
                    if rtype == 8:  # R_X86_64_RELATIVE
                        out[r_offset] = (r_addend, None)
                    elif rtype in (1, 6, 7) and sym is not None:  # 64, GLOB_DAT, JUMP_SLOT
                        st_name, _i, _o, st_shndx, st_value, _s = struct.unpack_from(
                            "<IBBHQQ", data, sym[2] + 24 * rsym)
                        nm = _cstr(data, strtab[2] + st_name) if strtab is not None else None
                        target = (st_value + (r_addend if rtype == 1 else 0)) if st_shndx else None
                        out[r_offset] = (target, nm)
            elif typ == 19:  # SHT_RELR: implicit addends, stored in place
                base = None
                for k in range(size // 8):
                    word, = struct.unpack_from("<Q", data, offset + 8 * k)
                    slots = []
                    if word & 1 == 0:
                        slots.append(word)
                        base = word + 8
                    elif base is not None:
                        bits, j = word >> 1, 0
                        while bits:
                            if bits & 1:
                                slots.append(base + 8 * j)
                            bits >>= 1
                            j += 1
                        base += 63 * 8
                    for slot in slots:
                        fo = file_offset(slot)
                        if fo is not None and fo + 8 <= len(data):
                            out[slot] = (struct.unpack_from("<Q", data, fo)[0], None)
        return out
    except struct.error:
        return None


# ---------------------------------------------------------------------------------------------
# Callgrind's file
# ---------------------------------------------------------------------------------------------


class Callgrind:
    def __init__(self):
        self.events, self.idx, self.totals, self.summary, self.self_total = [], {}, None, None, None
        self.addr_cost, self.addr_fn = {}, {}
        self.fn_self, self.fn_min, self.fn_ob = {}, {}, {}
        self.arcs, self.sites = {}, []
        self.desc, self.cmd, self.creator, self.parts = [], "", "", 0
        self.main_obs, self.obs = set(), set()

    def ev(self, vec, name):
        k = self.idx.get(name)
        return None if (k is None or vec is None) else vec[k]


def read_callgrind(path):
    cg = Callgrind()
    tables = {"ob": {}, "fl": {}, "fn": {}}

    def named(rest, table):
        rest = rest.strip()
        if rest.startswith("("):
            j = rest.find(")")
            if j > 0:
                key, nm = rest[1:j], rest[j + 1:].strip()
                if nm:
                    table[key] = nm
                    return nm
                return table.get(key, "(" + key + ")")
        return rest

    ob = fn = cfn = ""
    cob = None
    E, npos, icol, last = 0, 1, -1, [0]
    call, skip, is_main = None, False, False
    main_cache, cmd_base = {}, None
    with open_text(path) as f:
        for line in f:
            c = line[:1]
            if not c or c in "#\n":
                continue
            if ("0" <= c <= "9") or c in "+-*":
                parts = line.split()
                if len(parts) < npos:
                    continue
                pos = []
                for k in range(npos):
                    tok = parts[k]
                    t0 = tok[0]
                    if t0 == "*":
                        v = last[k]
                    elif t0 == "+":
                        v = last[k] + _num(tok[1:])
                    elif t0 == "-":
                        v = last[k] - _num(tok[1:])
                    else:
                        v = _num(tok)
                    pos.append(v)
                last = pos
                if skip:
                    skip = False
                    continue
                vec = [0] * E
                k = 0
                for t in parts[npos:npos + E]:
                    vec[k] = int(t)
                    k += 1
                if call is not None:
                    count, taddr = call
                    call = None
                    rec = cg.arcs.get((fn, cfn))
                    if rec is None:
                        cg.arcs[(fn, cfn)] = [count, vec]
                    else:
                        rec[0] += count
                        vadd(rec[1], vec)
                    if cfn not in cg.fn_ob:
                        cg.fn_ob[cfn] = cob if cob is not None else ob
                    if is_main and icol >= 0:
                        cg.sites.append((pos[icol], fn, cfn, count, vec, taddr))
                    cob = None
                    continue
                if cg.self_total is None:
                    cg.self_total = vec[:]
                else:
                    vadd(cg.self_total, vec)
                fs = cg.fn_self.get(fn)
                if fs is None:
                    cg.fn_self[fn] = vec[:]
                else:
                    vadd(fs, vec)
                if is_main and icol >= 0:
                    a = pos[icol]
                    d = cg.addr_cost.get(a)
                    if d is None:
                        cg.addr_cost[a] = vec
                        cg.addr_fn[a] = fn
                    else:
                        vadd(d, vec)
                    m = cg.fn_min.get(fn)
                    if m is None or a < m:
                        cg.fn_min[fn] = a
                continue
            if line.startswith("fn="):
                fn = named(line[3:], tables["fn"])
                if fn not in cg.fn_ob:
                    cg.fn_ob[fn] = ob
                continue
            if line.startswith("calls="):
                parts = line[6:].split()
                count = int(parts[0])
                taddr = None
                if icol >= 0 and len(parts) > 1 + icol:
                    tok = parts[1 + icol]
                    if tok[0] == "+":
                        taddr = last[icol] + _num(tok[1:])
                    elif tok[0] == "-":
                        taddr = last[icol] - _num(tok[1:])
                    elif tok[0] == "*":
                        taddr = last[icol]
                    else:
                        taddr = _num(tok)
                call = (count, taddr)
                continue
            if line.startswith("cfn="):
                cfn = named(line[4:], tables["fn"])
                continue
            if line.startswith("cob="):
                cob = named(line[4:], tables["ob"])
                continue
            if line.startswith("ob="):
                ob = named(line[3:], tables["ob"])
                cg.obs.add(ob)
                r = main_cache.get(ob)
                if r is None:
                    base = os.path.basename(ob)
                    r = (base == cmd_base) if cmd_base else base.endswith("wall_clock")
                    main_cache[ob] = r
                is_main = r
                if r:
                    cg.main_obs.add(ob)
                continue
            if line.startswith(("fl=", "fi=", "fe=")):
                named(line[3:], tables["fl"])
                continue
            if line.startswith(("cfi=", "cfl=")):
                named(line[4:], tables["fl"])
                continue
            if line.startswith(("jump=", "jcnd=")):
                skip = True
                continue
            if line.startswith("events:"):
                cg.events = line[7:].split()
                cg.idx = {e: i for i, e in enumerate(cg.events)}
                E = len(cg.events)
                continue
            if line.startswith("positions:"):
                ps = line[10:].split()
                npos = len(ps)
                icol = ps.index("instr") if "instr" in ps else -1
                last = [0] * npos
                continue
            if line.startswith(("totals:", "summary:")):
                vals = [int(x) for x in line.split(":", 1)[1].split()]
                vals += [0] * (E - len(vals))
                if line.startswith("totals:"):
                    if cg.totals is None:
                        cg.totals = vals
                    else:
                        vadd(cg.totals, vals)
                else:
                    cg.summary = vals
                continue
            if line.startswith("cmd:"):
                cg.cmd = line[4:].strip()
                if cg.cmd.split():
                    cmd_base = os.path.basename(cg.cmd.split()[0])
                continue
            if line.startswith("creator:"):
                cg.creator = line[8:].strip()
                continue
            if line.startswith("desc:"):
                cg.desc.append(line[5:].strip())
                continue
            if line.startswith("part:"):
                cg.parts += 1
                continue
    if cg.self_total is None:
        cg.self_total = [0] * E
    return cg


def callgrind_totals(path):
    """Only the totals line of a sibling's callgrind file, with its event names."""
    events, totals = [], None
    try:
        with open_text(path) as f:
            for line in f:
                if line.startswith("events:"):
                    events = line[7:].split()
                elif line.startswith("totals:"):
                    vals = [int(x) for x in line.split(":", 1)[1].split()]
                    totals = vals if totals is None else [a + b for a, b in zip(totals, vals)]
    except (OSError, EOFError, ValueError):
        return None
    if totals is None:
        return None
    return dict(zip(events, totals))


# ---------------------------------------------------------------------------------------------
# One side: its binary's symbols, code and counts
# ---------------------------------------------------------------------------------------------


class Body:
    __slots__ = ("idx", "start", "size", "end", "names", "ninsn", "targets", "holder", "gholder",
                 "insns", "own", "fncost", "attr", "ctxs", "exact", "info", "kind", "codehash")

    def __init__(self, idx, start, size):
        self.idx, self.start, self.size, self.end = idx, start, size, start + size
        self.names, self.ninsn, self.targets = [], 0, {}
        self.holder = self.gholder = False
        self.insns = self.own = self.fncost = self.attr = self.ctxs = self.exact = self.info = None
        self.kind = "other"
        self.codehash = None


class Side:
    def __init__(self, prefix, role, build):
        self.prefix, self.role, self.build = prefix, role, build
        self.notes = []
        self.sha = self.size = self.sha_check = None
        self.bodies, self.starts, self.at_start, self.by_name = [], [], {}, {}
        self.marker_x, self.marker_g = {}, {}
        self.has_nm = self.has_objdump = False
        self.cg = None
        self.offset, self.offset_from, self.offset_votes = None, [], (0, 0)
        self.mapped_ir = self.unmapped_ir = 0
        self.rows, self.rowfn = {}, {}
        self.ctx, self.frac = [], {}
        self.calls_at = {}
        self.copies = self.bkeys = self.anchor = None
        self.got, self.got_sites = None, [0, 0]

    # -- the files ------------------------------------------------------------------------------

    def load(self):
        self.read_sha()
        if os.path.exists(self.prefix + ".wall_clock"):
            self.got = read_got(self.prefix + ".wall_clock")
            if self.got is None:
                self.notes.append("the binary's relocations are unreadable: GOT calls stay unresolved")
        else:
            self.notes.append("no binary: GOT calls stay unresolved")
        self.read_nm()
        self.read_objdump()
        self.read_counts()
        if self.cg is not None:
            self.find_rows()
            self.join()
            self.attribute()
        for b in self.bodies:
            if b.holder:
                b.info = self.holder_info(b)
        return self

    def read_sha(self):
        p = self.prefix + ".sha256"
        if os.path.exists(p):
            with open(p, "r", encoding="utf-8", errors="replace") as h:
                parts = h.read().split()
            if parts:
                self.sha = parts[0]
                if len(parts) >= 3 and parts[-1].isdigit():
                    self.size = int(parts[-1])
        else:
            self.notes.append("no .sha256")
        binary = self.prefix + ".wall_clock"
        if os.path.exists(binary):
            digest = hashlib.sha256()
            with open(binary, "rb") as h:
                for chunk in iter(lambda: h.read(1 << 20), b""):
                    digest.update(chunk)
            got = digest.hexdigest()
            self.sha_check = (self.sha is None or got == self.sha, got, os.path.getsize(binary))
            if self.sha is None:
                self.sha, self.size = got, os.path.getsize(binary)

    def read_nm(self):
        p = existing(self.prefix + ".nm.txt", self.prefix + ".nm.txt.gz")
        if p is None:
            self.notes.append("no nm")
            return
        groups = {}
        with open_text(p) as f:
            for line in f:
                parts = line.split(None, 3)
                if len(parts) != 4 or len(parts[2]) != 1 or parts[2] not in "tTwW":
                    continue
                try:
                    start, size = int(parts[0], 16), int(parts[1], 16)
                except ValueError:
                    continue
                if size:
                    groups.setdefault((start, size), set()).add(strip_vendor(parts[3].strip()))
        for start, size in sorted(groups):
            b = Body(len(self.bodies), start, size)
            b.names = sorted(groups[(start, size)])
            b.kind = kind_of(b.names[0])
            for n in b.names[1:]:
                k = kind_of(n)
                if b.kind == "other" and k != "other":
                    b.kind = k
            self.bodies.append(b)
        self.starts = [b.start for b in self.bodies]
        for b in self.bodies:
            self.at_start[b.start] = b
            for n in b.names:
                self.by_name.setdefault(n, []).append(b)
                d = marker_dialect(n)
                if d == "graphqlx":
                    self.marker_x.setdefault(b.start, []).append(n)
                elif d == "graphql":
                    self.marker_g.setdefault(b.start, []).append(n)
        self.has_nm = bool(self.bodies)

    def lookup(self, a):
        i = bisect.bisect_right(self.starts, a) - 1
        while i >= 0:
            b = self.bodies[i]
            if b.start <= a < b.end:
                return b
            if a - b.start > (1 << 24) or i < len(self.bodies) - 1 and self.bodies[i].start < a - (1 << 20):
                return None
            i -= 1
            if i >= 0 and self.bodies[i].end <= a and (i == 0 or self.bodies[i - 1].end <= a):
                return None
        return None

    def read_objdump(self):
        p = existing(self.prefix + ".objdump.txt.gz", self.prefix + ".objdump.txt")
        if p is None or not self.bodies:
            self.notes.append("no objdump" if p is None else "no nm, so objdump unread")
            return
        cur, buf = None, []
        try:
            with open_text(p) as f:
                for line in f:
                    k = line.find(":\t")
                    if k <= 0:
                        continue
                    try:
                        a = int(line[:k], 16)
                    except ValueError:
                        continue
                    if cur is None or not (cur.start <= a < cur.end):
                        if cur is not None and buf:
                            self._analyze(cur, buf)
                        buf = []
                        cur = self.lookup(a)
                        if cur is None:
                            continue
                    buf.append((a, line[k + 2:].rstrip("\n")))
            if cur is not None and buf:
                self._analyze(cur, buf)
            self.has_objdump = True
        except (OSError, EOFError) as error:
            self.notes.append("objdump unreadable: %s" % error)

    def _codehash(self, b, buf):
        """A hash of the function's instructions with every address replaced by what it names."""
        h = hashlib.sha1()
        for a, t in buf:
            k = t.find("#")
            if k >= 0:
                g = _GOT.search(t, k)
                name = None
                if g:
                    res = self.got.get(int(g.group(1), 16)) if self.got else None
                    if res and res[0] is not None:
                        tb = self.at_start.get(res[0])
                        name = tb.names[0] if tb is not None else None
                    if name is None:
                        name = g.group(2)
                t = _RIP.sub("rip+X", t[:k]) + "<" + (name or "") + ">"
            else:
                m = _DIRECT_ANY.search(t)
                if m:
                    tgt = int(m.group(1), 16)
                    if b.start <= tgt < b.end:
                        rest = "<+0x%x>" % (tgt - b.start)
                    else:
                        tb = self.at_start.get(tgt)
                        rest = "<" + (tb.names[0] if tb is not None else m.group(2)) + ">"
                    t = t[:m.start()] + rest
            h.update(_LLVM_ANY.sub("", t).encode("utf-8", "replace"))
            h.update(b"\n")
        b.codehash = h.hexdigest()

    def _analyze(self, b, buf):
        self._codehash(b, buf)
        b.ninsn += len(buf)
        targets = b.targets
        for a, t in buf:
            ch = t[:1]
            if ch not in ("c", "j", "b", "n"):
                continue
            mn, ops = split_insn(t)
            if mn == "call":
                kind = 0
            elif mn.startswith("j"):
                kind = 1
            else:
                continue
            m = _DIRECT.match(ops)
            if m:
                tgt = int(m.group(1), 16)
                if kind == 1 and b.start <= tgt < b.end:
                    continue
                key = ("a", tgt, m.group(2))
            else:
                g = _GOT.search(ops)
                if g:
                    self.got_sites[0] += 1
                    res = self.got.get(int(g.group(1), 16)) if self.got else None
                    if res is not None and res[0] is not None:
                        self.got_sites[1] += 1
                        tgt = res[0]
                        key = ("a", tgt, res[1] or g.group(2))
                    else:
                        key = ("g", (res[1] if res else None) or g.group(2))
                        tgt = None
                elif kind == 0:
                    key = ("i",)
                    tgt = None
                else:
                    continue
            if tgt is not None:
                if tgt in self.marker_x:
                    b.holder = True
                if tgt in self.marker_g:
                    b.gholder = True
            rec = targets.get(key)
            if rec is None:
                targets[key] = rec = [0, 0, []]
            rec[kind] += 1
            rec[2].append(a)
        if b.holder:
            b.insns = (b.insns or []) + buf

    def read_counts(self):
        p = existing(self.prefix + ".callgrind.out.gz", self.prefix + ".callgrind.out")
        if p is None:
            self.notes.append("no callgrind counts")
            return
        try:
            cg = read_callgrind(p)
        except (OSError, EOFError, ValueError) as error:
            self.notes.append("callgrind unreadable: %s" % error)
            return
        if not cg.events or not cg.fn_self:
            self.notes.append("callgrind file holds no counts")
            return
        self.cg = cg

    # -- the rows, the join, the attribution -------------------------------------------------------

    def incl(self, fn):
        cg = self.cg
        v = list(cg.fn_self.get(fn, [0] * len(cg.events)))
        for (p, c), (_, vec) in self._out.get(fn, {}).items():
            if c != fn:
                vadd(v, vec)
        return v

    def find_rows(self):
        cg = self.cg
        self._out, self._in = {}, {}
        for (p, c), rec in cg.arcs.items():
            self._out.setdefault(p, {})[(p, c)] = rec
            self._in.setdefault(c, {})[(p, c)] = rec
        names = set(cg.fn_self) | set(self._in)
        measure = set()
        for fn in names:
            t = NAMES.tokens(strip_vendor(fn))
            if t[:2] == ["wall_clock", "measure"] and "closure" in t[2:]:
                measure.add(fn)
        self.row_fns_without_closure = []
        self.measure_arcs = {}  # every row's function: its calls into measure's closure, and their cost
        for fn in names:
            t = NAMES.tokens(strip_vendor(fn))
            if t[:2] == ["wall_clock", "perf"] and len(t) == 3:
                arcs = [rec for (p, c), rec in self._out.get(fn, {}).items() if c in measure]
                if arcs:
                    self.measure_arcs[t[2]] = (sum(r[0] for r in arcs), sum(r[1][0] for r in arcs))
                    if not any(row_closure(strip_vendor(x), t[2]) for x in names):
                        self.row_fns_without_closure.append(t[2])
        for row in ROWS:
            cands = [fn for fn in names if row_closure(strip_vendor(fn), row)]
            best = None
            for fn in cands:
                inc = self.incl(fn)
                calls = sum(rec[0] for (p, c), rec in self._in.get(fn, {}).items() if p != fn)
                if best is None or inc[0] > best["incl"][0]:
                    best = {"fn": fn, "incl": inc, "calls": calls, "cands": len(cands), "via": "closure"}
            # The row is read at its own function's calls (design 10.7): two rows can share one
            # closure, and the arcs from a row's function into `measure`'s closure are that row's alone.
            fns = [fn for fn in names if row_function(strip_vendor(fn), row)]
            arcs = []
            for fn in fns:
                arcs += [rec for (p, c), rec in self._out.get(fn, {}).items() if c in measure]
            if arcs:
                inc = [0] * len(cg.events)
                for rec in arcs:
                    vadd(inc, rec[1])
                rec = dict(best or {"fn": None, "calls": None, "cands": 0})
                rec["closure_incl"] = rec.get("incl")
                rec["fn_incl"], rec["mcalls"] = inc, sum(r[0] for r in arcs)
                # A closure of the row's own, run 4 times, is read where it is exact; one that rows
                # share is read at the row's own function's calls, which count measure's timing too.
                if rec.get("calls") != CLOSURE_CALLS or rec.get("closure_incl") is None:
                    rec["incl"], rec["via"] = inc, "function"
                best = rec
            if best is not None:
                self.rows[row] = best
            fns = [fn for fn in names if row_function(strip_vendor(fn), row)]
            if fns:
                fn = fns[0]
                v = list(cg.fn_self.get(fn, [0] * len(cg.events)))
                for (p, c), (_, vec) in self._out.get(fn, {}).items():
                    if c != fn and NAMES.tokens(strip_vendor(c))[:1] != ["wall_clock"]:
                        vadd(v, vec)
                calls = sum(rec[0] for (p, c), rec in self._in.get(fn, {}).items() if p != fn)
                self.rowfn[row] = {"fn": fn, "vec": v, "calls": calls}

    def join(self):
        """The constant between callgrind's addresses and nm's, from each row closure's lowest
        executed address against its nm start (an entry always executes), checked across closures
        and across every function callgrind names."""
        cg = self.cg
        per_closure = []
        for row, r in self.rows.items():
            bodies = self.by_name.get(strip_vendor(r["fn"]), [])
            lo = cg.fn_min.get(r["fn"])
            if len(bodies) == 1 and lo is not None:
                per_closure.append((row, lo - bodies[0].start))
        self.offset_from = per_closure
        votes = {}
        for fn, lo in cg.fn_min.items():
            bodies = self.by_name.get(strip_vendor(fn), [])
            if len(bodies) == 1:
                d = lo - bodies[0].start
                votes[d] = votes.get(d, 0) + 1
        if per_closure:
            vals = [d for _, d in per_closure]
            self.offset = max(set(vals), key=vals.count)
        elif votes:
            self.offset = max(votes, key=votes.get)
        if self.offset is None:
            return
        total_votes = sum(votes.values())
        self.offset_votes = (votes.get(self.offset, 0), total_votes)
        ir = cg.idx.get("Ir", 0)
        off = self.offset
        for a, vec in cg.addr_cost.items():
            b = self.lookup(a - off)
            if b is None:
                self.unmapped_ir += vec[ir]
                continue
            self.mapped_ir += vec[ir]
            if b.own is None:
                b.own = vec[:]
                b.fncost = {}
            else:
                vadd(b.own, vec)
            fn = cg.addr_fn.get(a, "")
            fc = b.fncost.get(fn)
            if fc is None:
                b.fncost[fn] = vec[:]
            else:
                vadd(fc, vec)
        self.entries_at = {}
        for (site, caller, callee, count, incl, taddr) in cg.sites:
            self.calls_at.setdefault(site - off, []).append((callee, count, incl))
            if taddr is not None:
                self.entries_at[taddr - off] = self.entries_at.get(taddr - off, 0) + count

    def ir_at(self, a):
        if self.cg is None or self.offset is None:
            return None
        v = self.cg.addr_cost.get(a + self.offset)
        return v[self.cg.idx.get("Ir", 0)] if v else 0

    def attribute(self):
        """Each function's share of each row, through callgrind's call arcs. A callee's cost is
        split among its callers by the arcs' inclusive instructions (the gprof assumption); a row's
        closure takes nothing from above it. One context alone reaching a function makes its
        attribution exact; several make it an estimate, and the digest says which."""
        cg = self.cg
        ctx_fns = [self.rows[r]["fn"] for r in ROWS if r in self.rows]
        self.ctx = [r for r in ROWS if r in self.rows] + ["other"]
        K = len(self.ctx)
        other = K - 1
        ctx_of = {fn: i for i, fn in enumerate(ctx_fns)}
        parents, children = {}, {}
        for (p, c), (cnt, vec) in cg.arcs.items():
            if p == c or c in ctx_of:
                continue
            w = vec[0] if vec and vec[0] > 0 else cnt
            if w <= 0:
                continue
            parents.setdefault(c, []).append((p, w))
            children.setdefault(p, []).append(c)
        nodes = set(cg.fn_self) | set(parents) | set(children) | set(ctx_of)
        onehot = []
        for i in range(K):
            v = [0.0] * K
            v[i] = 1.0
            onehot.append(v)
        frac = {}

        def average(n, require_all):
            if n in ctx_of:
                return onehot[ctx_of[n]]
            ps = parents.get(n)
            if not ps:
                return onehot[other]
            acc, tot = [0.0] * K, 0.0
            for p, w in ps:
                fp = frac.get(p)
                if fp is None:
                    if require_all:
                        fp = onehot[other]
                    else:
                        continue
                tot += w
                for i in range(K):
                    if fp[i]:
                        acc[i] += w * fp[i]
            if tot <= 0:
                return onehot[other]
            return [x / tot for x in acc]

        for comp in reversed(tarjan(nodes, children)):
            if len(comp) == 1:
                frac[comp[0]] = average(comp[0], True)
                continue
            members = set(comp)
            acc, tot = [0.0] * K, 0.0
            for n in comp:
                for p, w in parents.get(n, ()):
                    if p not in members:
                        fp = frac.get(p) or onehot[other]
                        tot += w
                        for i in range(K):
                            acc[i] += w * fp[i]
            start = [x / tot for x in acc] if tot > 0 else onehot[other]
            for n in comp:
                frac[n] = onehot[ctx_of[n]] if n in ctx_of else list(start)
            for _ in range(3000):
                delta = 0.0
                for n in comp:
                    new = average(n, True)
                    old = frac[n]
                    delta = max(delta, max(abs(x - y) for x, y in zip(new, old)))
                    frac[n] = new
                if delta < 1e-10:
                    break
        self.frac = frac
        E = len(cg.events)
        for b in self.bodies:
            if b.fncost is None:
                continue
            attr = [[0.0] * E for _ in range(K)]
            ctxs, exact = set(), True
            for fn, vec in b.fncost.items():
                fr = frac.get(fn) or onehot[other]
                nz = [i for i in range(K) if fr[i] > 1e-6]
                ctxs.update(nz)
                if len(nz) != 1:
                    exact = False
                for i in nz:
                    w = fr[i]
                    row = attr[i]
                    for k in range(E):
                        row[k] += w * vec[k]
            b.attr, b.ctxs = attr, ctxs
            b.exact = exact and len(ctxs) == 1

    def row_calls(self, row):
        """The closure calls that are this row's: its own closure's, or its share of a closure that
        rows without a closure of their own also call (design 10.2: `parse_executable`'s and
        `parse_deep_values`'s are one function). Answers (calls, note)."""
        r = self.rows.get(row)
        if r is None or r.get("calls") is None:
            return None, "no closure"
        c = r["calls"]
        if c == CLOSURE_CALLS:
            return c, ""
        k = c // CLOSURE_CALLS
        others = [x for x in getattr(self, "row_fns_without_closure", []) if x != row]
        if c % CLOSURE_CALLS == 0 and 1 < k <= 1 + len(others) and r.get("closure_incl") and row in self.measure_arcs:
            import itertools
            target = r["closure_incl"][0]
            best = None
            for group in itertools.combinations(others, k - 1):
                total = self.measure_arcs[row][1] + sum(self.measure_arcs[g][1] for g in group)
                if best is None or abs(total - target) < best[0]:
                    best = (abs(total - target), group, total)
            if best is not None and best[0] <= 0.001 * target:
                return CLOSURE_CALLS, ("its closure, %d calls, is one function with %s's, which has no closure of "
                                       "its own: their functions' calls into measure cost %s, the closure %s" % (
                                           c, ", ".join(best[1]), fint(best[2]), fint(target)))
        return None, "its closure ran %d times" % c

    def lex_calls(self, row):
        """The `lex` calls a row's four closure calls make, or None."""
        calls, _ = self.row_calls(row)
        if calls != CLOSURE_CALLS:
            return None
        if row in TOKENS:
            return tokens_of(row)
        if row in LEX_ROWS:
            return tokens_of(TS)
        return None

    def ctx_index(self, row):
        try:
            return self.ctx.index(row)
        except ValueError:
            return None

    def attributed(self, b, row, event="Ir"):
        i = self.ctx_index(row)
        if i is None or b.attr is None:
            return 0.0 if b.attr is not None or self.cg is not None else None
        return b.attr[i][self.cg.idx[event]]

    def share_den(self):
        if self.cg is None or EXEC not in self.rows or TS not in self.rows:
            return None
        return self.rows[EXEC]["incl"][self.cg.idx["Ir"]] + self.rows[TS]["incl"][self.cg.idx["Ir"]]

    def share(self, b):
        den = self.share_den()
        if den is None or self.offset is None:
            return None
        if b.attr is None or not den:
            return 0.0
        return (self.attributed(b, EXEC) + self.attributed(b, TS)) / den

    def rows_reaching(self, b):
        if b.ctxs is None:
            return ""
        names = []
        for i in sorted(b.ctxs):
            c = self.ctx[i]
            names.append({EXEC: "x-exec", TS: "x-ts", LEXX: "x-lex", TWIN[EXEC]: "g-exec",
                          TWIN[TS]: "g-ts", TWIN[LEXX]: "g-lex"}.get(c, "other"))
        return ",".join(names) + (" exact" if b.exact else " estimated")

    # -- holders ----------------------------------------------------------------------------------

    def holder_info(self, b):
        insns = b.insns or []
        k = 1 if insns and insns[0][1].startswith("endbr64") else 0
        pushes = []
        while k < len(insns):
            mn, ops = split_insn(insns[k][1])
            if mn != "push":
                break
            pushes.append(ops)
            k += 1
        frame, fp = None, False
        for a, t in insns[:FRAME_SCAN]:
            mn, ops = split_insn(t)
            o = [x.strip() for x in ops.split("#")[0].split(",")]
            if mn == "sub" and frame is None and len(o) == 2 and o[0] == "rsp":
                try:
                    frame = _num(o[1])
                except ValueError:
                    frame = o[1]
            if mn == "mov" and o[:2] == ["rbp", "rsp"]:
                fp = True
        sites = stack_sites(insns)
        pairs, shifts = seven_byte_moves_and_shifts(insns, b.start, b.end)
        info = {"pushes": pushes, "frame": frame, "fp": fp, "sites": sites, "pairs": pairs, "shifts": shifts}
        counts = {}
        for s in sites:
            key = (s[1], s[4])
            counts[key] = counts.get(key, 0) + 1
        info["static"] = counts
        if self.cg is not None and self.offset is not None:
            ex = {}
            for s in sites:
                key = (s[1], s[4])
                ex[key] = ex.get(key, 0) + (self.ir_at(s[0]) or 0)
            info["executed"] = ex
            calls, arcs = {}, []
            for site, lst in self.calls_at.items():
                if b.start <= site < b.end:
                    for callee, count, incl in lst:
                        calls[callee] = calls.get(callee, 0) + count
                        arcs.append((site, callee, count, incl))
            info["calls"] = calls
            info["arcs"] = arcs
        return info


def tarjan(nodes, children):
    """Strongly connected components, sinks first."""
    index, low, on, stack, out = {}, {}, set(), [], []
    counter = 0
    for root in nodes:
        if root in index:
            continue
        index[root] = low[root] = counter
        counter += 1
        stack.append(root)
        on.add(root)
        work = [(root, iter(children.get(root, ())))]
        while work:
            v, it = work[-1]
            advanced = False
            for w in it:
                if w not in index:
                    index[w] = low[w] = counter
                    counter += 1
                    stack.append(w)
                    on.add(w)
                    work.append((w, iter(children.get(w, ()))))
                    advanced = True
                    break
                if w in on and index[w] < low[v]:
                    low[v] = index[w]
            if advanced:
                continue
            work.pop()
            if work:
                u = work[-1][0]
                if low[v] < low[u]:
                    low[u] = low[v]
            if low[v] == index[v]:
                comp = []
                while True:
                    w = stack.pop()
                    on.discard(w)
                    comp.append(w)
                    if w == v:
                        break
                out.append(comp)
    return out


# ---------------------------------------------------------------------------------------------
# The pair
# ---------------------------------------------------------------------------------------------


class Entry:
    __slots__ = ("key", "bc", "hc", "holder", "gholder", "kind", "only", "mode", "size_diff",
                 "gained", "lost", "share_b", "share_h", "own_b", "own_h", "hot", "differs", "pairs",
                 "ub", "uh", "refined", "hot_refined", "moved", "by_calls")


class Pair:
    def __init__(self, base, head, label):
        self.base, self.head, self.label = base, head, label
        bn, hn = set(base.by_name), set(head.by_name)
        self.name_frac = (len(bn & hn) / len(bn)) if bn else 0.0
        self.full = self.name_frac >= NAMES_COMPARE
        self.key = (lambda n: n) if self.full else NAMES.fallback
        for s in (base, head):
            self.index(s)
        self._tk = {}
        self._nb = {}
        self.compare()

    def index(self, s):
        s.copies, s.bkeys = {}, {}
        for b in s.bodies:
            ks = sorted({self.key(n) for n in b.names})
            s.bkeys[b.idx] = ks
            for k in ks:
                s.copies.setdefault(k, []).append(b)

    def tkey(self, s, t):
        if t[0] == "a":
            tb = s.at_start.get(t[1])
            if tb is not None:
                return min(s.bkeys[tb.idx])
            ann = t[2]
            if "+0x" in ann:
                return self.key(strip_vendor(ann.split("+0x")[0])) + "+..."
            return ann
        if t[0] == "g":
            return "GOT:" + t[1]
        return "<indirect call>"

    def tkeys(self, s, b):
        ck = (s.role, b.idx)
        v = self._tk.get(ck)
        if v is None:
            v = {}
            for t, rec in b.targets.items():
                k = self.tkey(s, t)
                r = v.get(k)
                if r is None:
                    v[k] = [rec[0], rec[1], list(rec[2])]
                else:
                    r[0] += rec[0]
                    r[1] += rec[1]
                    r[2].extend(rec[2])
            self._tk[ck] = v
        return v

    def neighbours(self, s, o, b, k=6):
        """The keys of the k nearest functions before and after `b` whose names are unique on both
        sides: the evidence of the unit `b` was compiled in (R4)."""
        ck = (s.role, b.idx)
        v = self._nb.get(ck)
        if v is None:
            v = set()
            for step in (-1, 1):
                i, got = b.idx + step, 0
                while 0 <= i < len(s.bodies) and got < k:
                    for key in s.bkeys[i]:
                        if len(s.copies.get(key, ())) == 1 and len(o.copies.get(key, ())) == 1:
                            v.add((step, key))
                            got += 1
                            break
                    i += step
            self._nb[ck] = v
        return v

    def match(self, e):
        """Copies of one name matched across the sides by the units they sit in, not by address."""
        B, H = self.base, self.head
        sb = [self.neighbours(B, H, b) for b in e.bc]
        sh = [self.neighbours(H, B, h) for h in e.hc]
        cand = []
        for i, x in enumerate(sb):
            for j, y in enumerate(sh):
                u = len(x | y)
                jac = len(x & y) / u if u else 0.0
                if jac >= 0.5:
                    cand.append((jac, i, j))
        cand.sort(key=lambda t: (-t[0], t[1], t[2]))
        used_b, used_h, pairs = set(), set(), []
        for jac, i, j in cand:
            if i in used_b or j in used_h:
                continue
            used_b.add(i)
            used_h.add(j)
            pairs.append((e.bc[i], e.hc[j]))
        pairs.sort(key=lambda bh: bh[0].start)
        moved = []
        irb = B.cg.idx["Ir"] if B.cg else 0
        irh = H.cg.idx["Ir"] if H.cg else 0

        def ran(c, ix):
            return c.own[ix] if c.own else 0

        left_b = sorted((i for i in range(len(e.bc)) if i not in used_b), key=lambda i: -ran(e.bc[i], irb))
        for i in left_b:
            b = e.bc[i]
            if b.codehash is None:
                continue
            cands = [j for j in range(len(e.hc)) if j not in used_h and e.hc[j].codehash == b.codehash
                     and e.hc[j].size == b.size]
            if not cands:
                continue
            j = min(cands, key=lambda j: (abs(ran(e.hc[j], irh) - ran(b, irb)), j))
            used_b.add(i)
            used_h.add(j)
            moved.append((b, e.hc[j]))
        # Last, copies called the same number of times on both sides: a copy's calls are what it
        # serves, and a count that no other copy left shares names one copy on each side.
        if B.cg is not None and H.cg is not None and B.offset is not None and H.offset is not None:
            calls_b = {i: B.entries_at.get(e.bc[i].start, 0) for i in range(len(e.bc)) if i not in used_b}
            calls_h = {j: H.entries_at.get(e.hc[j].start, 0) for j in range(len(e.hc)) if j not in used_h}
            for i, n in sorted(calls_b.items(), key=lambda kv: -kv[1]):
                if n <= 0 or list(calls_b.values()).count(n) != 1:
                    continue
                js = [j for j, m in calls_h.items() if m == n and j not in used_h]
                if len(js) == 1:
                    used_b.add(i)
                    used_h.add(js[0])
                    e.by_calls.append((e.bc[i], e.hc[js[0]]))
        e.moved = moved
        return (pairs, [b for i, b in enumerate(e.bc) if i not in used_b],
                [h for j, h in enumerate(e.hc) if j not in used_h])

    def copy_differs(self, b, h):
        return b.size != h.size or set(self.tkeys(self.base, b)) != set(self.tkeys(self.head, h))

    def compare(self):
        B, H = self.base, self.head
        ir_b = B.cg.idx["Ir"] if B.cg else None
        ir_h = H.cg.idx["Ir"] if H.cg else None
        self.entries = {}
        for k in set(B.copies) | set(H.copies):
            e = Entry()
            e.key, e.bc, e.hc = k, B.copies.get(k, []), H.copies.get(k, [])
            e.holder = any(b.holder for b in e.bc) or any(b.holder for b in e.hc)
            e.gholder = any(b.gholder for b in e.bc) or any(b.gholder for b in e.hc)
            e.kind = (e.bc or e.hc)[0].kind
            e.only = None if (e.bc and e.hc) else ("base" if e.bc else "head")
            e.mode, e.size_diff, e.gained, e.lost = None, False, set(), set()
            e.pairs, e.ub, e.uh, e.refined, e.hot_refined, e.moved = [], list(e.bc), list(e.hc), None, None, []
            e.by_calls = []
            if e.only is None:
                if len(e.bc) == 1 and len(e.hc) == 1:
                    e.mode, pairs = "single", [(e.bc[0], e.hc[0])]
                    e.pairs, e.ub, e.uh = pairs, [], []
                else:
                    e.pairs, e.ub, e.uh = self.match(e)
                    if not e.ub and not e.uh and not e.moved and not e.by_calls:
                        e.mode, pairs = "paired", e.pairs
                    else:
                        e.mode, pairs = "aggregate", None
                if pairs:
                    for b, h in pairs:
                        if b.size != h.size:
                            e.size_diff = True
                        tb, th = set(self.tkeys(B, b)), set(self.tkeys(H, h))
                        e.gained |= th - tb
                        e.lost |= tb - th
                else:
                    e.size_diff = sum(b.size for b in e.bc) != sum(h.size for h in e.hc) or len(e.bc) != len(e.hc)
                    tb, th = set(), set()
                    for b in e.bc:
                        tb |= set(self.tkeys(B, b))
                    for h in e.hc:
                        th |= set(self.tkeys(H, h))
                    e.gained, e.lost = th - tb, tb - th
            e.differs = e.only is not None or e.size_diff or bool(e.gained or e.lost)
            sb = [B.share(b) for b in e.bc]
            sh = [H.share(h) for h in e.hc]
            e.share_b = None if (not e.bc or any(x is None for x in sb)) else sum(sb)
            e.share_h = None if (not e.hc or any(x is None for x in sh)) else sum(sh)
            e.own_b = sum(b.own[ir_b] for b in e.bc if b.own) if (B.cg and B.offset is not None) else None
            e.own_h = sum(h.own[ir_h] for h in e.hc if h.own) if (H.cg and H.offset is not None) else None
            known = [x for x in (e.share_b, e.share_h) if x is not None]
            e.hot = (max(known) >= HOT_SHARE) if known else None
            if e.mode == "aggregate":
                hot_copies = []
                differing = [(b, h) for b, h in e.pairs + e.by_calls if self.copy_differs(b, h)]
                for b, h in differing:
                    hot_copies += [B.share(b), H.share(h)]
                hot_copies += [B.share(b) for b in e.ub] + [H.share(h) for h in e.uh]
                known = [x for x in hot_copies if x is not None]
                e.hot_refined = (max(known) >= HOT_SHARE) if known else (False if not hot_copies else None)
                e.refined = "by units: %d copies match, %d more by equal calls into them (%d of these differ); %d " \
                            "more have identical code in another unit (executed %s -> %s); unmatched %d on the base " \
                            "(Ir %s), %d on the head (Ir %s)" % (
                                len(e.pairs), len(e.by_calls), len(differing), len(e.moved),
                                fint(sum(b.own[ir_b] for b, _ in e.moved if b.own) if ir_b is not None else None),
                                fint(sum(h.own[ir_h] for _, h in e.moved if h.own) if ir_h is not None else None),
                                len(e.ub),
                                fint(sum(b.own[ir_b] for b in e.ub if b.own) if ir_b is not None else None),
                                len(e.uh), fint(sum(h.own[ir_h] for h in e.uh if h.own) if ir_h is not None else None))
            self.entries[k] = e


# ---------------------------------------------------------------------------------------------
# The report
# ---------------------------------------------------------------------------------------------


class Report:
    def __init__(self):
        self.lines = []

    def __call__(self, text=""):
        self.lines.append(text)


def label_of(prefix):
    return os.path.basename(os.path.dirname(os.path.abspath(prefix)))


def read_versions(prefix):
    p = os.path.join(os.path.dirname(prefix), "versions.txt")
    if not os.path.exists(p):
        return None
    with open(p, "r", encoding="utf-8", errors="replace") as h:
        return h.read().strip().splitlines()


def entry_line(pair, e, with_targets=True):
    B, H = pair.base, pair.head
    bits = ["[%s]" % e.kind, show((e.bc or e.hc)[0].names[0] if not pair.full else e.key, 120),
            "{k:%s}" % tag_of(e.key)]
    if e.only:
        bits.append("ONLY ON %s" % e.only.upper())
    if e.mode and e.mode != "single":
        bits.append("copies %d/%d %s" % (len(e.bc), len(e.hc), "matched by unit" if e.mode == "paired" else
                                          "compared as an aggregate (R4)"))
    if e.size_diff:
        sb, sh = sum(b.size for b in e.bc), sum(h.size for h in e.hc)
        bits.append("size %d -> %d (%+d B)" % (sb, sh, sh - sb))
    if with_targets and (e.gained or e.lost):
        if e.gained:
            bits.append("gains " + "; ".join(sorted(show(t, 80) for t in e.gained)[:6]) +
                        (" (+%d more)" % (len(e.gained) - 6) if len(e.gained) > 6 else ""))
        if e.lost:
            bits.append("loses " + "; ".join(sorted(show(t, 80) for t in e.lost)[:6]) +
                        (" (+%d more)" % (len(e.lost) - 6) if len(e.lost) > 6 else ""))
    bits.append("share %s / %s" % (fpct(e.share_b, 3), fpct(e.share_h, 3)))
    if e.own_b is not None or e.own_h is not None:
        bits.append("own Ir %s -> %s" % (fint(e.own_b), fint(e.own_h)))
    if e.refined:
        bits.append("[" + e.refined + ("; no hot copy differs" if e.hot_refined is False else
                                       "; a hot copy differs" if e.hot_refined else "") + "]")
    return "  ".join(bits)


def drivers_q3(s):
    """Question 3's figures for one side: what the GraphQLx drivers execute for each lex call. `b` is
    9.4's scale, both GraphQLx parse rows' work over both rows' tokens; `x` and `t` split it by row
    through the call arcs, an estimate where both rows reach a driver."""
    if s.cg is None or s.offset is None or EXEC not in s.rows or TS not in s.rows:
        return None
    ix = s.cg.idx
    keys = ("Dr", "Dw", "Ir", "st", "ld", "bst", "bld")
    out = {"x": dict.fromkeys(keys, 0.0), "t": dict.fromkeys(keys, 0.0), "b": dict.fromkeys(keys, 0.0),
           "n": 0, "estimated": 0}
    for b in s.bodies:
        if not (b.holder and b.kind == "driver" and b.attr is not None):
            continue
        out["n"] += 1
        if not b.exact:
            out["estimated"] += 1
        own_ir = b.own[ix["Ir"]] if b.own else 0
        ex = (b.info or {}).get("executed", {})
        st = ex.get(("rsp", "store"), 0) + ex.get(("rsp", "both"), 0)
        ld = ex.get(("rsp", "load"), 0) + ex.get(("rsp", "both"), 0)
        bst = ex.get(("rbp", "store"), 0) + ex.get(("rbp", "both"), 0)
        bld = ex.get(("rbp", "load"), 0) + ex.get(("rbp", "both"), 0)
        for tag, rows in (("x", (EXEC,)), ("t", (TS,)), ("b", (EXEC, TS))):
            tok = sum(tokens_of(r) for r in rows)
            vec = [0.0] * len(ix)
            for r in rows:
                i = s.ctx_index(r)
                if i is not None:
                    vadd(vec, b.attr[i])
            out[tag]["Dr"] += vec[ix["Dr"]] / tok
            out[tag]["Dw"] += vec[ix["Dw"]] / tok
            out[tag]["Ir"] += vec[ix["Ir"]] / tok
            f = (vec[ix["Ir"]] / own_ir) if own_ir else 0.0
            out[tag]["st"] += f * st / tok
            out[tag]["ld"] += f * ld / tok
            out[tag]["bst"] += f * bst / tok
            out[tag]["bld"] += f * bld / tok
    return out


def classify_q3(d_reads, d_writes):
    """Against F': `as F'` within the test's own allowance (question 1's 1.5 reads, 0.5 writes),
    `above` when both exceed it, `mixed` otherwise."""
    if d_reads <= TEST_MAX["Dr"] and d_writes <= TEST_MAX["Dw"]:
        return "as base"
    if d_reads > TEST_MAX["Dr"] and d_writes > TEST_MAX["Dw"]:
        return "above"
    return "mixed"


def nm_diagnosis(prefix_a, prefix_b):
    """Two binaries of one build that differ: which crates' disambiguators differ, and which
    functions differ in size when names are compared without disambiguators (backrefs resolved)."""
    _cs = re.compile(r"Cs([0-9A-Za-z]+)_(\d+)")

    def load(prefix):
        p = existing(prefix + ".nm.txt", prefix + ".nm.txt.gz")
        dis, sizes = {}, {}
        if p is None:
            return None, None
        with open_text(p) as f:
            for line in f:
                parts = line.split(None, 3)
                if len(parts) != 4:
                    continue
                n = strip_vendor(parts[3].strip())
                if n.startswith("_R"):
                    for m in _cs.finditer(n):
                        dis.setdefault(n[m.end():m.end() + int(m.group(2))], set()).add(m.group(1))
                if parts[2] in ("t", "T", "w", "W"):
                    try:
                        sizes.setdefault(NAMES.fallback(n), []).append(int(parts[1], 16))
                    except ValueError:
                        pass
        return dis, {k: sorted(v) for k, v in sizes.items()}

    da, sa = load(prefix_a)
    db, sb = load(prefix_b)
    if sa is None or sb is None:
        return "no nm to compare"
    crates = sorted(n for n in set(da) & set(db) if da[n] != db[n])
    common = set(sa) & set(sb)
    differ = sorted((k for k in common if sa[k] != sb[k]), key=lambda k: -abs(sum(sb[k]) - sum(sa[k])))
    only_a, only_b = sorted(set(sa) - set(sb)), sorted(set(sb) - set(sa))
    text = "crate disambiguators that differ: %s; without them, %d of %d function names differ in size%s; " \
           "%d names on the first only, %d on the second only%s" % (
               ", ".join(crates) or "none", len(differ), len(common),
               (" (" + "; ".join("%s %s -> %s B" % (show_key(k, 80), "+".join(map(str, sa[k])),
                                                     "+".join(map(str, sb[k]))) for k in differ[:4]) + ")")
               if differ else "", len(only_a), len(only_b),
               (" (second only: " + "; ".join(show_key(k, 70) for k in only_b[:6]) + ")") if only_b else "")
    return text


def show_key(key, width=100):
    if key.startswith(("_R", "_ZN")):
        return show(key, width)
    text = re.sub(r"<[^<>]*>", "", key)
    while re.search(r"<[^<>]*>", text):
        text = re.sub(r"<[^<>]*>", "", text)
    return text if len(text) <= width else text[:width - 3] + "..."


def cpu_model(prefix):
    lines = read_versions(prefix) or []
    for line in lines:
        if "Model name" in line:
            return line.split(":", 1)[1].strip()
    return "?"


def det_rows(figs):
    """The rows of several runs of one binary: (text, failed)."""
    equal, jitter, other_calls, differ = [], [], [], []
    for row in ROWS:
        vals = []
        for who, s, model in figs:
            r = s.rows.get(row)
            if r is not None:
                vals.append((who, r.get("calls"), tuple(s.cg.ev(r["incl"], e) for e in DET_EVENTS),
                             r.get("via")))
        if len(vals) < 2:
            continue
        if len({v[1] for v in vals}) > 1:
            other_calls.append("%s (%s)" % (row, "/".join(str(v[1]) for v in vals)))
            continue
        irs = [v[2][0] for v in vals]
        spread = max(irs) - min(irs)
        if len({v[2] for v in vals}) == 1:
            equal.append(row)
        elif all(v[3] == "function" for v in vals) and spread <= 64:
            jitter.append("%s (%d Ir)" % (row, spread))
        else:
            differ.append("%s (Ir spread %s)" % (row, fint(spread)))
    text = "each row's calls and inclusive %s: equal for %s%s; other call counts: %s; DIFFER: %s" % (
        "/".join(DET_EVENTS), ", ".join(equal) or "none",
        ("; within measure's timing, in a row read at its function's calls: " + ", ".join(jitter)) if jitter else "",
        ", ".join(other_calls) or "none", ", ".join(differ) or "none")
    return text, bool(differ)


def cross_runs(label, base, head, out, checks, read_siblings, roots_extra=()):
    """9.4's SHA-256 check across runs, and the gate's determinism check for equal binaries. The
    runs are read from this artifact's own batch and from any further batch roots named."""
    if label not in RUNS:
        out("  [SKIP] across runs: the label %r is not one of the runs this digest knows" % label)
        return
    own_root = os.path.dirname(os.path.dirname(os.path.abspath(base.prefix)))
    roots = [own_root] + [os.path.abspath(r) for r in roots_extra if os.path.abspath(r) != own_root]
    seen = {}
    for root in roots:
        batch = os.path.basename(root)
        for lab, builds in RUNS.items():
            for role, build in zip(("base", "head"), builds):
                prefix = os.path.join(root, lab, role)
                p = prefix + ".sha256"
                sha = None
                if root == own_root and lab == label:
                    sha = base.sha if role == "base" else head.sha
                elif os.path.exists(p):
                    with open(p, "r", encoding="utf-8", errors="replace") as h:
                        parts = h.read().split()
                    sha = parts[0] if parts else None
                if sha:
                    seen.setdefault(build, []).append((batch, lab, role, sha, prefix))
    status = "PASS"
    across = []
    for build in sorted(seen):
        apps = seen[build]
        if len(apps) < 2:
            continue
        out("  %s: " % build + ", ".join("%s%s %s %s" % ("" if len(roots) == 1 else batch + "/", lab, role, sha[:12])
                                         for batch, lab, role, sha, _ in apps))
        for batch in sorted({a[0] for a in apps}):
            for role in ("base", "head"):
                same_side = {sha for b2, lab, r, sha, _ in apps if b2 == batch and r == role}
                if len(same_side) > 1:
                    status = "FAIL"
                    out("    %s as %s in %s: the same-side builds differ, so a build of one source is not "
                        "reproducible" % (build, role, batch))
            sides = {r: {sha for b2, lab, rr, sha, _ in apps if b2 == batch and rr == r} for r in ("base", "head")}
            if sides["base"] and sides["head"] and not (sides["base"] & sides["head"]):
                out("    %s built as base and as head in %s differ; the gate's first suspect is the side's "
                    "--remap-path-prefix (not a reproducibility failure)" % (build, batch))
        by_batch = {}
        for a in apps:
            by_batch.setdefault(a[0], set()).add(a[3])
        if len(by_batch) > 1 and len(set().union(*by_batch.values())) > 1:
            status = "FAIL"
            across.append(build)
            ref = next(a for a in apps if a[0] == os.path.basename(own_root))
            other = next(a for a in apps if a[3] != ref[3])
            out("    %s DIFFERS BETWEEN BATCHES, so a run of one batch is not a second reading of the other's "
                "binary (9.4): %s/%s %s against %s/%s %s: %s" % (
                    build, other[0], other[1], other[2], ref[0], ref[1], ref[2],
                    nm_diagnosis(other[4], ref[4])))
        if read_siblings:
            by_sha = {}
            for batch, lab, role, sha, prefix in apps:
                by_sha.setdefault(sha, []).append((batch, lab, role, prefix))
            for sha, group in by_sha.items():
                if len(group) < 2:
                    continue
                figs = []
                for batch, lab, role, prefix in group:
                    if batch == os.path.basename(own_root) and lab == label:
                        s = base if role == "base" else head
                    else:
                        s = Side(prefix, role, build)
                        s.read_counts()
                        if s.cg is not None:
                            s.find_rows()
                    if s.cg is not None:
                        figs.append(("%s%s %s" % ("" if len(roots) == 1 else batch + "/", lab, role), s,
                                     cpu_model(prefix)))
                if len(figs) < 2:
                    continue
                # One CPU model: the counts must be equal, except measure's timing in a row read at its
                # function's calls. Across models, glibc picks its copy routines by the host's caches.
                for model in sorted({f[2] for f in figs}):
                    same = [f for f in figs if f[2] == model]
                    if len(same) < 2:
                        continue
                    res = det_rows(same)
                    out("    determinism (the gate), %d runs of one SHA-256 %s on one CPU model (%s; %s): %s" % (
                        len(same), sha[:12], model, ", ".join(f[0] for f in same), res[0]))
                    if res[1] and status == "PASS":
                        status = "FAIL"
                models = sorted({f[2] for f in figs})
                if len(models) > 1:
                    res = det_rows([next(f for f in figs if f[2] == m) for m in models])
                    out("    across CPU models (%s): %s (a NOTE: glibc chooses its copy routines by the host's "
                        "caches, so counts across models are compared row by row and not required equal)" % (
                            ", ".join(models), res[0]))
    checks.append((status, "runs" + ("(%s differ between batches)" % ", ".join(across) if across else ""),
                   "SHA-256 across runs, same side first (9.4, the gate)%s" % (
                       "" if len(roots) == 1 else "; batches: " + ", ".join(os.path.basename(r) for r in roots))))


def main(argv):
    if argv and argv[0] == "--chain":
        return chain_main(argv[1:])
    args, opts, roots = [], {}, []
    i = 0
    while i < len(argv):
        a = argv[i]
        if a in ("--label", "--third", "--builds"):
            opts[a] = argv[i + 1]
            i += 2
            continue
        if a == "--sibling-root":
            roots.append(argv[i + 1])
            i += 2
            continue
        if a.startswith("--"):
            opts[a] = True
        else:
            args.append(a)
        i += 1
    opts["roots"] = roots
    if len(args) != 2:
        sys.exit(__doc__)
    base_prefix, head_prefix = args
    label = opts.get("--label") or label_of(base_prefix)
    builds = RUNS.get(label, ("base", "head"))
    if opts.get("--builds"):
        builds = tuple(opts["--builds"].split(",", 1))
    base = Side(base_prefix, "base", builds[0]).load()
    head = Side(head_prefix, "head", builds[1]).load()
    third = None
    if not opts.get("--no-third"):
        tp = opts.get("--third")
        if tp is None and label in Q3_SIBLING:
            root = os.path.dirname(os.path.dirname(os.path.abspath(base_prefix)))
            cand = os.path.join(root, Q3_SIBLING[label], "head")
            if existing(cand + ".nm.txt"):
                tp = cand
        if tp:
            third = Side(tp, "third", RUNS.get(label_of(tp), ("", "third"))[1]).load()
    pair = Pair(base, head, label)
    rep = digest(pair, label, builds, third, not opts.get("--no-siblings"), opts["roots"])
    sys.stdout.write("\n".join(rep) + "\n")


# ---------------------------------------------------------------------------------------------
# The digest itself
# ---------------------------------------------------------------------------------------------


def digest(pair, label, builds, third, read_siblings, roots=()):
    B, H = pair.base, pair.head
    body = Report()
    checks = []  # (status, short name, text)
    counts = {"base": B.cg is not None and B.offset is not None, "head": H.cg is not None and H.offset is not None}
    sides = (("base", B), ("head", H))

    body("Z1 code-check digest: design 9.4, as gate v3 amends it (R2, R3, R4)")
    body("run %s: base %s, head %s" % (label, builds[0], builds[1]))
    body("base prefix: %s" % B.prefix)
    body("head prefix: %s" % H.prefix)
    body("constants: tokens a row closure's call lexes, lo/hi: executable %s/%s, type system %s/%s; "
         "a parse row's 4 calls lex 2*lo + 2*hi" % tuple(fint(x) for x in TOKENS[EXEC] + TOKENS[TS]))
    body("           hot = own instructions >= %s of the two GraphQLx parse rows' inclusive ones, either side; "
         "question 1's test: <= %g instructions, %g reads, %g writes per lex call"
         % (fpct(HOT_SHARE, 1), TEST_MAX["Ir"], TEST_MAX["Dr"], TEST_MAX["Dw"]))
    mangling = sorted({("v0" if n.startswith("_R") else "legacy") for s in (B, H) for n in list(s.by_name)[:5000]
                       if n.startswith(("_R", "_ZN"))})
    body("           mangling seen: %s; names compare by %s" % (
        "/".join(mangling) or "none", "full mangled name" if pair.full else
        "names without hashes or crate disambiguators (9.4's fallback)"))
    body()

    # ---- 0. The instrument's own checks -------------------------------------------------------
    body("== 0. The instrument's own checks (9.4; the gate's R4 and its recommendations) ==")
    for role, s in sides:
        for n in s.notes:
            body("  [NOTE] %s: %s" % (role, n))

    # holders: a driver and a loop on each side
    ok, parts = True, []
    for role, s in sides:
        hs = [b for b in s.bodies if b.holder]
        kinds = {}
        for b in hs:
            kinds[b.kind] = kinds.get(b.kind, 0) + 1
        parts.append("%s %d holders (%s); GraphQL's %d" % (role, len(hs), ", ".join(
            "%s %d" % kv for kv in sorted(kinds.items())), sum(1 for b in s.bodies if b.gholder)))
        if not kinds.get("driver") or not kinds.get("loop"):
            ok = False
    checks.append(("PASS" if ok else "FAIL", "holders", "each side has a driver and a loop: " + "; ".join(parts)))

    # twin presence
    ok, parts, judged = True, [], label in RUNS
    for role, s in sides:
        has_twin = any(lexer_item(n, "lex_in_block") for n in s.by_name)
        n_pref = sum(len(bs) for n, bs in s.by_name.items() if lexer_item(n, "prefixed"))
        exp = HAS_TWIN.get(s.build)
        parts.append("%s %s: lex_in_block %s, prefixed %s%s" % (
            role, s.build, "yes" if has_twin else "no", ("yes, %d copy" % n_pref) if n_pref == 1 else
            ("yes, %d copies" % n_pref if n_pref else "no"),
            "" if exp is None else " (expected: lex_in_block %s, prefixed %s)" % (
                "yes" if exp[0] else "no", "yes, once" if exp[1] else "no")))
        if exp is not None and (has_twin != exp[0] or (n_pref == 1) != exp[1] or (n_pref > 1)):
            ok = False
    checks.append((("PASS" if ok else "FAIL") if judged else "NOTE", "twin",
                   "GraphQLx's lex_in_block and prefixed (9.4; the gate: PT and PC too): " + "; ".join(parts)))

    # the marker
    ok, parts = True, []
    for role, s in sides:
        shared = set(s.marker_x) & set(s.marker_g)
        mixed = [b for b in s.bodies if b.start in s.marker_x and any(marker_dialect(n) == "graphql" for n in b.names)]
        parts.append("%s: GraphQLx at %s, GraphQL at %s" % (
            role, ",".join("0x%x" % a for a in sorted(s.marker_x)) or "none",
            ",".join("0x%x" % a for a in sorted(s.marker_g)) or "none"))
        if not s.marker_x or not s.marker_g or shared or mixed:
            ok = False
            if shared or mixed:
                parts.append("%s: SHARED ADDRESS, the dialects cannot be told apart" % role)
            elif not s.marker_g:
                parts.append("%s: GraphQL's helper has no symbol of its own (merged away?)" % role)
    checks.append(("PASS" if ok else "FAIL", "marker",
                   "over_recursion_limit shares no address across dialects (R4): " + "; ".join(parts)))

    # totals
    if B.cg or H.cg:
        ok, parts = True, []
        for role, s in sides:
            if s.cg is None:
                parts.append("%s: no counts" % role)
                continue
            if s.cg.totals is None:
                ok = False
                parts.append("%s: no totals line" % role)
                continue
            bad = [e for e, a, b2 in zip(s.cg.events, s.cg.self_total, s.cg.totals) if a != b2]
            if bad:
                ok = False
            parts.append("%s: %s" % (role, "equal for all %d events" % len(s.cg.events) if not bad else
                                     "DIFFER in " + ",".join(bad)))
        checks.append(("PASS" if ok else "FAIL", "totals", "the sum of every instruction's own cost is callgrind's total: "
                       + "; ".join(parts)))

    # the join
    if B.cg or H.cg:
        ok, parts = True, []
        for role, s in sides:
            if s.cg is None:
                continue
            if s.offset is None:
                ok = False
                parts.append("%s: no offset found" % role)
                continue
            vals = sorted({d for _, d in s.offset_from})
            if len(vals) > 1:
                ok = False
            tot = s.mapped_ir + s.unmapped_ir
            mfrac = s.mapped_ir / tot if tot else 0.0
            if mfrac < 0.999:
                ok = False
            ir = s.cg.idx["Ir"]
            by_names = {}
            for fn, v in s.cg.fn_self.items():
                if s.cg.fn_ob.get(fn) in s.cg.main_obs:
                    k = strip_vendor(fn)
                    by_names[k] = by_names.get(k, 0) + v[ir]
            groups = {}
            for b in s.bodies:
                g = groups.setdefault(tuple(b.names), [0, 0, []])
                g[0] += b.own[ir] if b.own else 0
                g[2].append(b)
            for names, g in groups.items():
                g[1] = sum(by_names.get(n, 0) for n in names)
            bad = [names for names, g in groups.items() if g[0] != g[1]]
            drivers = [b for b in s.bodies if b.holder and b.kind == "driver"]
            bad_drivers = [b for b in drivers if tuple(b.names) in set(bad)]
            executed = [b for b in drivers if b.own and b.own[ir] > 0]
            if bad_drivers or not executed:
                ok = False
            parts.append("%s: offset 0x%x from %d closures (%s), %d of %d named functions agree; %s of the main "
                         "object's instructions fall in nm functions; callgrind's cost for each name equals the cost "
                         "at its copies' addresses for %d of %d names%s; drivers %d, of them executed %d (an "
                         "unexecuted copy serves no row)" % (
                             role, s.offset, len(s.offset_from),
                             "agreeing" if len(vals) <= 1 else "DISAGREEING: " + ",".join("0x%x" % v for v in vals),
                             s.offset_votes[0], s.offset_votes[1], fpct(mfrac, 3), len(groups) - len(bad), len(groups),
                             (("; JOIN BROKEN for %d drivers" % len(bad_drivers)) if bad_drivers else "") +
                             ((" (the other: %s)" % "; ".join("%s nm %s callgrind %s" % (
                                 show(n[0], 60), fint(groups[n][0]), fint(groups[n][1])) for n in bad[:3]))
                              if bad else ""),
                             len(drivers), len(executed)))
        checks.append(("PASS" if ok else "FAIL", "join",
                       "addresses join by one constant offset, and each driver's executed addresses lie in its nm "
                       "range: " + "; ".join(parts)))

    # four calls
    if B.cg or H.cg:
        ok, parts, missed = True, [], []
        for role, s in sides:
            if s.cg is None:
                continue
            got, notes = [], []
            for row in ROWS:
                c, note = s.row_calls(row)
                r = s.rows.get(row) or {}
                got.append("%s %s%s" % (row, "-" if c is None else c,
                                        " (read at its function's calls)" if r.get("via") == "function" else ""))
                if note:
                    notes.append("%s: %s" % (row, note))
                if c != CLOSURE_CALLS and row in PARSE_ROWS:
                    ok = False
                    missed.append("%s %s %s" % (row, role, r.get("calls", "-")))
            parts.append("%s: %s%s" % (role, ", ".join(got), ("; " + "; ".join(notes)) if notes else ""))
        checks.append(("PASS" if ok else "FAIL", "calls" + ("(%s)" % ", ".join(missed) if missed else ""),
                       "each row is read at its own function's calls into measure's closure (design 10.7), and "
                       "ran its closure exactly 4 times, a probe and one batch at each size: " + "; ".join(parts) +
                       ("; a row that did not is read for each call, not for each lex call" if missed else "")))

    # names
    _cs = re.compile(r"Cs([0-9A-Za-z]+)_(\d+)")

    def crate_dis(side):
        out = {}
        for n in side.by_name:
            if n.startswith("_R"):
                for m in _cs.finditer(n):
                    out.setdefault(n[m.end():m.end() + int(m.group(2))], set()).add(m.group(1))
        return out

    db, dh = crate_dis(B), crate_dis(H)
    dis_differ = sorted(n for n in set(db) & set(dh) if db[n] != dh[n])
    dis_note = ("; v0 crate disambiguators: %s" % (
        "the same for all %d crates on both sides, so the names that differ are code that differs" % len(set(db) & set(dh))
        if not dis_differ else "DIFFER for " + ", ".join(dis_differ))) if (db and dh) else ""
    checks.append(("PASS" if pair.full else "FALLBACK", "names",
                   "%s of the base's text symbols found on the head by full mangled name (.llvm.N suffixes "
                   "stripped)%s%s" % (fpct(pair.name_frac, 1), "" if pair.full else
                                      ": below nine in ten, so the digest compares names without hashes",
                                      dis_note)))

    # copies (R4)
    multi = [e for e in pair.entries.values() if len(e.bc) > 1 or len(e.hc) > 1]
    agg = [e for e in multi if e.mode == "aggregate"]
    agg_h = [e for e in agg if e.holder]
    checks.append(("NOTE", "copies", "%d names have several copies; %d are compared as aggregates (count, total "
                   "size, union of targets, total executed) because their copy counts or units differ, %d of them "
                   "holders (R4). Copies are also matched by unit, a unit read from the six nearest names unique on both "
                   "sides before and after a copy; that match is reported beside the aggregate, not in its place" %
                   (len(multi), len(agg), len(agg_h))))

    cross = Report()
    cross_runs(label, B, H, cross, checks, read_siblings, roots)

    for status, name, text in checks:
        body("  [%s] %s: %s" % (status, name, text))
    for line in cross.lines:
        body(line)
    body()

    # ---- A. Identity -------------------------------------------------------------------------
    body("== A. Identity ==")
    for role, s in sides:
        chk = ""
        if s.sha_check is not None:
            chk = " (binary re-hashed: %s)" % ("matches" if s.sha_check[0] else "DIFFERS: %s" % s.sha_check[1])
        body("  %s %s: sha256 %s, %s bytes%s" % (role, s.build, s.sha or "-", fint(s.size), chk))
        if s.cg is not None:
            body("    callgrind: %s; cmd %s; %d parts; events %s" % (s.cg.creator, s.cg.cmd, s.cg.parts,
                                                                    " ".join(s.cg.events)))
            for d in s.cg.desc:
                if "cache" in d:
                    body("      " + d)
    versions = read_versions(B.prefix)
    if versions:
        for line in versions:
            if line.strip():
                body("  " + line.strip())
    else:
        body("  versions.txt: absent")
    if third is not None:
        body("  third (question 3): %s %s: sha256 %s" % (third.prefix, third.build, third.sha or "-"))
    body()

    # ---- B. The rows -------------------------------------------------------------------------
    body("== B. The rows, inclusive (callgrind); per lex call: "
         "the executable rows over %s, the type-system and lexer rows over %s ==" % (
             fint(tokens_of(EXEC)), fint(tokens_of(TS))))
    if not (counts["base"] or counts["head"]):
        body("  no counts on either side")
    body("  a row with a closure of its own, run 4 times, is read at that closure, exactly. A row whose closure "
         "rows share is read at its own function's calls into measure's closure (design 10.7): those count measure's "
         "own timing too, a few hundred instructions that vary by a few between runs. calls: the row's closure calls")
    hdr = "  %-28s %-8s %6s " % ("row", "", "calls") + " ".join("%15s" % e for e in B_EVENTS)
    body(hdr)
    bres, bcall, bcount = {}, {}, {}
    for row in ROWS:
        rb = B.rows.get(row) if B.cg else None
        rh = H.rows.get(row) if H.cg else None
        ev_b = [B.cg.ev(rb["incl"], e) for e in B_EVENTS] if rb else [None] * len(B_EVENTS)
        ev_h = [H.cg.ev(rh["incl"], e) for e in B_EVENTS] if rh else [None] * len(B_EVENTS)
        cb = B.row_calls(row)[0] if rb else None
        ch = H.row_calls(row)[0] if rh else None
        body("  %-28s %-8s %6s " % (row, "base", fint(cb)) + " ".join("%15s" % fint(x) for x in ev_b))
        body("  %-28s %-8s %6s " % ("", "head", fint(ch)) + " ".join("%15s" % fint(x) for x in ev_h))
        if rb and rh:
            diff = [(h - b) if (h is not None and b is not None) else None for b, h in zip(ev_b, ev_h)]
            body("  %-28s %-8s %6s " % ("", "diff", "") + " ".join("%15s" % (fint(x) if x is None else "{:+,}".format(x)) for x in diff))
            if cb == CLOSURE_CALLS and ch == CLOSURE_CALLS and B.lex_calls(row):
                tok = B.lex_calls(row)
                per = [(x / tok if x is not None else None) for x in diff]
                body("  %-28s %-8s %6s " % ("", "diff/lex", "") + " ".join("%15s" % fsigned(x) for x in per))
                bres[row] = dict(zip(B_EVENTS, per))
            else:
                if row in TOKENS:
                    body("  %-28s calls are not %d on both sides: no figure for each lex call, only for each call"
                         % ("", CLOSURE_CALLS))
                if cb and ch:
                    pcb = [(x / cb if x is not None else None) for x in ev_b]
                    pch = [(x / ch if x is not None else None) for x in ev_h]
                    per = [(h - b) if (h is not None and b is not None) else None for b, h in zip(pcb, pch)]
                    body("  %-28s %-8s %6s " % ("", "diff/call", "") + " ".join("%15s" % fsigned(x, 1) for x in per))
                    body("  %-28s per call: the lo and hi calls are mixed%s" % (
                        "", "" if cb == ch else "; the call counts DIFFER, so the mix differs and the two are not comparable"))
                    if cb == ch:
                        bcall[row] = dict(zip(B_EVENTS, per))
            if ev_b[0]:
                body("  %-28s Ir %s" % ("", fpct((ev_h[0] - ev_b[0]) / ev_b[0], 3) if ev_h[0] is not None else "-"))
        if row in LEX_ROWS:
            fb, fh = B.rowfn.get(row), H.rowfn.get(row)
            if fb and fh:
                vb = [B.cg.ev(fb["vec"], e) for e in ("Ir", "Dr", "Dw", "Bc")]
                vh = [H.cg.ev(fh["vec"], e) for e in ("Ir", "Dr", "Dw", "Bc")]
                body("  %-28s counting run (the row function's own work and calls outside the bench, %s/%s calls): "
                     "Ir/Dr/Dw/Bc base %s, head %s; diff per lex call (%s): %s" % (
                         "", fint(fb["calls"]), fint(fh["calls"]), "/".join(fint(x) for x in vb),
                         "/".join(fint(x) for x in vh), fint(COUNTING_TOKENS),
                         "/".join(fsigned((h - b) / COUNTING_TOKENS) for b, h in zip(vb, vh))))
                if fb["calls"] == fh["calls"]:
                    bcount[row] = [(h - b) / COUNTING_TOKENS for b, h in zip(vb, vh)]
    for row in (TWIN[EXEC], TWIN[TS], TWIN[LEXX]):
        src = None
        if row in bres:
            src, d = "per lex call", bres[row]
        elif row in bcall:
            src, d = "per call (equal calls, lo and hi mixed)", bcall[row]
        if src is not None:
            moved = any(abs(d[e] or 0) > 0 for e in ("Ir", "Dr", "Dw"))
            body("  twin check (the gate): GraphQL's %s %s %s (Ir %s, Dr %s, Dw %s)%s" % (
                row, "MOVED" if moved else "is unchanged", src, fsigned(d["Ir"]), fsigned(d["Dr"]),
                fsigned(d["Dw"]), ": a decision moved in code the variant did not touch" if moved else ""))
        if row in bcount:
            c = bcount[row]
            moved = any(abs(x) > 0 for x in c[:3])
            body("  twin check (the gate): GraphQL's %s counting run %s per lex call (Ir %s, Dr %s, Dw %s)" % (
                row, "MOVED" if moved else "is unchanged", fsigned(c[0]), fsigned(c[1]), fsigned(c[2])))
    for role, s in sides:
        if s.cg is None or s.offset is None:
            continue
        for row in (EXEC, TS):
            r = s.rows.get(row)
            i = s.ctx_index(row)
            if r is None or i is None:
                continue
            got = sum(b.attr[i][s.cg.idx["Ir"]] for b in s.bodies if b.attr) + sum(
                s.frac.get(fn, [0] * len(s.ctx))[i] * v[s.cg.idx["Ir"]] for fn, v in s.cg.fn_self.items()
                if s.cg.fn_ob.get(fn) not in s.cg.main_obs)
            body("  attribution check, %s %s: the call arcs place %s of the row's inclusive instructions" % (
                role, row, fpct(got / r["incl"][s.cg.idx["Ir"]] if r["incl"][s.cg.idx["Ir"]] else 0, 2)))
    body()

    # ---- C. Structure ------------------------------------------------------------------------
    body("== C. Structure: functions on one side only, of another size, or with other call targets ==")
    differ = [e for e in pair.entries.values() if e.differs]
    hold = [e for e in differ if e.holder]
    else_ = [e for e in differ if not e.holder]

    def sort_key(e):
        return -max(e.share_b or 0, e.share_h or 0), -(max(e.own_b or 0, e.own_h or 0)), e.key

    hot_else = [e for e in else_ if e.hot]
    unknown = [e for e in else_ if e.hot is None]
    body("  holds lex: %d differ (one side only %d, size %d, targets %d)" % (
        len(hold), sum(1 for e in hold if e.only), sum(1 for e in hold if e.size_diff),
        sum(1 for e in hold if e.gained or e.lost)))
    for e in sorted(hold, key=sort_key):
        body("    " + entry_line(pair, e))
    body("  elsewhere: %d differ (one side only %d, size %d, targets %d); %d of them hot (share >= %s)%s" % (
        len(else_), sum(1 for e in else_ if e.only), sum(1 for e in else_ if e.size_diff),
        sum(1 for e in else_ if e.gained or e.lost), len(hot_else), fpct(HOT_SHARE, 1),
        ("; hotness unknown for %d (no counts): sorted by structure alone" % len(unknown)) if unknown else ""))
    shown = 0
    for e in sorted(else_, key=sort_key):
        if e.hot or (shown < 40 and ((e.share_b or 0) > 0 or (e.share_h or 0) > 0 or e.hot is None)):
            body("    " + ("HOT " if e.hot else "    ") + entry_line(pair, e))
            if not e.hot:
                shown += 1
    rest = [e for e in sorted(else_, key=lambda e: e.key) if not e.hot and not (
        shown and ((e.share_b or 0) > 0 or (e.share_h or 0) > 0 or e.hot is None))]
    if rest:
        body("    and %d more outside the holders, which the two GraphQLx parse rows do not execute:" % len(rest))
        for side in ("base", "head", None):
            group = [e for e in rest if e.only == side]
            if group:
                body("      %s: %s%s" % (
                    {"base": "only on the base", "head": "only on the head", None: "on both, of another size or "
                     "with other targets"}[side],
                    "; ".join("[%s] %s" % (e.kind, show((e.bc or e.hc)[0].names[0], 70)) for e in group[:30]),
                    " (+%d more)" % (len(group) - 30) if len(group) > 30 else ""))
    gh = [e for e in else_ if e.gholder]
    if gh:
        body("  of those elsewhere, %d hold GraphQL's lex (code the variant does not touch):" % len(gh))
        for e in sorted(gh, key=sort_key)[:20]:
            body("    " + entry_line(pair, e))
    body()

    # ---- D. The holders ------------------------------------------------------------------------
    body("== D. The holders: each function that holds GraphQLx's lex, on each side ==")
    body("  the drivers, for each entry (design 10.7): instructions, reads, writes, conditional branches; "
         "seven-byte moves (two four-byte moves through one base register, three bytes apart, design 10), "
         "pairs through a base and an index register counted apart; shifts of any width")
    drivers_table(body, pair)
    body("  each holder: executed figures for each lex call are divided by the lex calls of the rows that reach it")
    body("  (the call arcs; 'estimated' when several contexts reach it). x-exec %s lex calls, x-ts %s." % (
        fint(tokens_of(EXEC)), fint(tokens_of(TS))))
    holders = sorted([e for e in pair.entries.values() if e.holder],
                     key=lambda e: (-(max(e.own_b or 0, e.own_h or 0)), e.key))
    for e in holders:
        body("  [%s] %s {k:%s}%s" % (e.kind, show((e.bc or e.hc)[0].names[0], 140), tag_of(e.key),
                                     " copies %d/%d %s" % (len(e.bc), len(e.hc), "matched by unit" if e.mode == "paired"
                                                           else "compared as an aggregate (R4)")
                                     if (e.mode and e.mode != "single") else
                                     (" ONLY ON %s" % e.only.upper() if e.only else "")))
        if e.refined:
            body("    " + e.refined)
        for b, h in e.pairs:
            fb = describe_holder(body, pair, "base", B, b)
            fh = describe_holder(body, pair, "head", H, h)
            holder_delta(body, fb, fh, pair, b, h)
        for b, h in e.by_calls:
            fb = describe_holder(body, pair, "base", B, b, " (matched to the head's copy by equal calls)")
            fh = describe_holder(body, pair, "head", H, h, " (matched by equal calls)")
            holder_delta(body, fb, fh, pair, b, h)
        for b, h in e.moved:
            fb = describe_holder(body, pair, "base", B, b, " (identical code in another unit on the head)")
            fh = describe_holder(body, pair, "head", H, h, " (identical code in another unit on the base)")
            holder_delta(body, fb, fh, pair, b, h)
        for b in e.ub:
            describe_holder(body, pair, "base", B, b, " (no matching copy on the head)" if e.only is None else "")
        for h in e.uh:
            describe_holder(body, pair, "head", H, h, " (no matching copy on the base)" if e.only is None else "")
        if e.gained or e.lost:
            body("    targets on the head only: %s" % (", ".join(sorted(show(t, 90) for t in e.gained)) or "none"))
            body("    targets on the base only: %s" % (", ".join(sorted(show(t, 90) for t in e.lost)) or "none"))
    body()

    # ---- E. The answer's slot --------------------------------------------------------------------
    body("== E. The answer's slot ==")
    part_e(body, pair)
    body()

    # ---- F. Where the work is ------------------------------------------------------------------
    body("== F. Where the work is: the %d functions with the largest difference in their own instructions ==" % TOP_F)
    flist = part_f(pair)
    if flist is None:
        body("  no counts on one side: part F needs both")
    else:
        body("  %-11s %-5s %16s %16s %14s %11s %11s  %s" % ("kind", "lex", "base Ir", "head Ir", "diff", "x-exec/lex",
                                                           "x-ts/lex", "function (x-exec/lex: the difference the "
                                                           "executable row's share accounts for, per lex call)"))
        moved = [f for f in flist if f["h"] != f["b"]]
        body("  %d functions differ in their own instructions; %s" % (
            len(moved), "all are listed" if len(moved) <= TOP_F else "the %d largest are listed" % TOP_F))
        for f in moved[:TOP_F]:
            body("  %-11s %-5s %16s %16s %14s %11s %11s  %s" % (
                f["kind"], f["lex"], fint(f["b"]), fint(f["h"]), "{:+,}".format(f["h"] - f["b"]),
                fsigned(f["dx"]), fsigned(f["dt"]), f["name"]))
    body()

    paths = part_f_paths(pair)
    if paths:
        body("  by function path (every executed instance of a path summed; lex: x holds GraphQLx's, g GraphQL's); "
             "per lex call of the executable row: instructions, reads, writes; of the type-system row: instructions")
        body("  %-11s %-3s %16s %16s %14s %9s %8s %8s %9s  %s" % (
            "kind", "lex", "base Ir", "head Ir", "diff", "x Ir/lex", "x Dr", "x Dw", "t Ir/lex",
            "path (executed copies base/head)"))
        for (path, cls), v in paths[:25]:
            body("  %-11s %-3s %16s %16s %14s %9s %8s %8s %9s  %s (%d/%d)" % (
                v["kind"], cls, fint(v["b"]), fint(v["h"]), "{:+,}".format(v["h"] - v["b"]), fsigned(v["dx"]),
                fsigned(v["dxr"]), fsigned(v["dxw"]), fsigned(v["dt"]), path[:100], v["nb"], v["nh"]))
        libs = sum(r["dx"] for r in (flist or []) if r["kind"] == "[lib]")
        total = sum(v["dx"] for _, v in paths) + libs
        ranked = sorted(paths, key=lambda kv: -abs(kv[1]["dx"]))
        body("  where the executable row's difference is, by function path, per lex call: %s in all (libraries %s), "
             "the type-system row's %s; the 25 largest:" % (
                 fsigned(total), fsigned(libs), fsigned(sum(v["dt"] for _, v in paths))))
        acc = 0.0
        for (path, cls), v in ranked[:25]:
            acc += v["dx"]
            body("    %+9.3f Ir %+8.3f Dr %+8.3f Dw | type-system %+9.3f Ir  %-2s %s (%d/%d)" % (
                v["dx"], v["dxr"], v["dxw"], v["dt"], cls, path[:100], v["nb"], v["nh"]))
        body("    the 25 sum to %s; the rest %s" % (fsigned(acc), fsigned(total - acc)))
    body()

    floor_additions(body, pair)

    selection(body, pair, builds, bres)

    # ---- The four questions --------------------------------------------------------------------
    body("== The four questions (9.4), with its thresholds ==")
    answers = questions(body, pair, label, builds, bres, hot_else, else_, unknown, holders, third, flist)
    body()

    # ---- the first line ----------------------------------------------------------------------
    fails = [name for status, name, _ in checks if status == "FAIL"]
    first = []
    no_counts = [role for role, s in sides if not counts[role]]
    if no_counts:
        first.append("NO COUNTS on %s: sorted by structure alone (the gate's R2, fifth hole)" % " and ".join(no_counts))
    if fails:
        first.append("SELF-CHECKS: FAIL [%s] (9.4: a digest that fails one is read by hand from the artifact); "
                     "the others PASS" % ", ".join(fails))
    else:
        first.append("SELF-CHECKS: PASS (%d checks)" % len([c for c in checks if c[0] in ("PASS", "FALLBACK")]))
    if not pair.full:
        first.append("names: FALLBACK to names without hashes")
    first.append("run %s, %s -> %s" % (label, builds[0], builds[1]))
    return [" | ".join(first), ""] + body.lines + answers


def driver_counts(s, b):
    """What a holder executes for each entry (design 10): instructions, reads and writes, the
    seven-byte moves and the shifts. An entry of a driver is one `lex` call."""
    info = b.info or {}
    out = {"entries": None, "pairs": info.get("pairs", []), "shifts": info.get("shifts", [])}
    if s.cg is None or s.offset is None:
        return out
    entries = s.entries_at.get(b.start, 0)
    out["entries"] = entries
    ix = s.cg.idx
    own = b.own or [0] * len(ix)
    mv_ld = mv_st = mv_idx = 0
    live = []
    for (a1, a2, d, base, disp, t1, t2, idx) in out["pairs"]:
        n = min(s.ir_at(a1) or 0, s.ir_at(a2) or 0)
        if idx:
            mv_idx += n
        elif d == "load":
            mv_ld += n
        else:
            mv_st += n
        if n:
            live.append((a1, a2, d + (" (indexed)" if idx else ""), n, t1, t2))
    sh, by = 0, {}
    for (a, mn, amount, t) in out["shifts"]:
        n = s.ir_at(a) or 0
        sh += n
        if n:
            k = "%s %s" % (mn, amount)
            by[k] = by.get(k, 0) + n
    out.update(mv_ld=mv_ld, mv_st=mv_st, mv_idx=mv_idx, live=live, shifts_ex=sh, shifts_by=by,
               Ir=own[ix["Ir"]], Dr=own[ix["Dr"]], Dw=own[ix["Dw"]], Bc=own[ix["Bc"]])
    return out


def holder_figs(s, b):
    """A holder's figures on one side: structure from objdump, and what callgrind says it executed,
    for each `lex` call of the rows that reach it, and for each entry where it is a driver."""
    info = b.info or {}
    st = info.get("static", {})
    f = {"size": b.size, "ninsn": b.ninsn, "pushes": info.get("pushes", []), "frame": info.get("frame"),
         "fp": info.get("fp", False),
         "st_store": st.get(("rsp", "store"), 0) + st.get(("rsp", "both"), 0),
         "st_load": st.get(("rsp", "load"), 0) + st.get(("rsp", "both"), 0),
         "st_addr": st.get(("rsp", "address"), 0),
         "n_pairs": len(info.get("pairs", [])), "n_shifts": len(info.get("shifts", [])),
         "own": None, "per": {}, "dc": None}
    if s.cg is None or s.offset is None or not b.own:
        return f
    ix = s.cg.idx
    ex = info.get("executed", {})
    f["own"] = b.own
    f["ex_store"] = ex.get(("rsp", "store"), 0) + ex.get(("rsp", "both"), 0)
    f["ex_load"] = ex.get(("rsp", "load"), 0) + ex.get(("rsp", "both"), 0)
    f["calls"] = info.get("calls", {})
    f["rows"] = s.rows_reaching(b)
    f["dc"] = driver_counts(s, b)
    own_ir = b.own[ix["Ir"]] or 1
    per = {}
    for tag, rows in (("both", (EXEC, TS)), ("x-exec", (EXEC,)), ("x-ts", (TS,))):
        tok = sum(tokens_of(r) for r in rows)
        vec = [0.0] * len(ix)
        for r in rows:
            i = s.ctx_index(r)
            if i is not None and b.attr:
                vadd(vec, b.attr[i])
        if vec[ix["Ir"]] <= 0:
            continue
        frac = vec[ix["Ir"]] / own_ir
        per[tag] = {"Ir": vec[ix["Ir"]] / tok, "Dr": vec[ix["Dr"]] / tok, "Dw": vec[ix["Dw"]] / tok,
                    "Bc": vec[ix["Bc"]] / tok, "st": frac * f["ex_store"] / tok, "ld": frac * f["ex_load"] / tok}
    f["per"] = per
    return f


def entry_figs(dc):
    """A driver's figures for each entry, or None when it was never entered."""
    if not dc or not dc.get("entries"):
        return None
    e = dc["entries"]
    return {"e": e, "Ir": dc["Ir"] / e, "Dr": dc["Dr"] / e, "Dw": dc["Dw"] / e, "Bc": dc["Bc"] / e,
            "mv": (dc["mv_ld"] + dc["mv_st"]) / e, "mv_ld": dc["mv_ld"] / e, "mv_st": dc["mv_st"] / e,
            "mv_idx": dc["mv_idx"] / e, "sh": dc["shifts_ex"] / e,
            "sh_by": {k: v / e for k, v in dc["shifts_by"].items()}}


def describe_holder(body, pair, role, s, b, note=""):
    f = holder_figs(s, b)
    frame = f["frame"]
    body("    %s @0x%x%s: size %d B, %d instructions, pushes [%s], frame %s%s; stack sites: %d stores, %d loads, "
         "%d addresses; seven-byte move sites %d, shift sites %d" % (
             role, b.start, note, f["size"], f["ninsn"], " ".join(f["pushes"]),
             ("0x%x" % frame) if isinstance(frame, int) else (frame or "none"), ", rbp frame" if f["fp"] else "",
             f["st_store"], f["st_load"], f["st_addr"], f["n_pairs"], f["n_shifts"]))
    tk = pair.tkeys(s, b)
    top = sorted(tk.items(), key=lambda kv: (-(kv[1][0] + kv[1][1]), kv[0]))
    body("      call sites by target (%d targets): " % len(top) + "; ".join(
        "%s x%d%s" % (show(k, 64), v[0] + v[1], " (jmp)" if v[1] and not v[0] else "") for k, v in top[:12]) +
        ("; ..." if len(top) > 12 else ""))
    if s.cg is None or s.offset is None:
        body("      executed: no counts")
        return f
    if f["own"] is None:
        body("      executed: never")
        return f
    ix = s.cg.idx
    o = f["own"]
    body("      executed: own Ir %s Dr %s Dw %s Bc %s; stack stores %s, loads %s; reached by %s" % (
        fint(o[ix["Ir"]]), fint(o[ix["Dr"]]), fint(o[ix["Dw"]]), fint(o[ix["Bc"]]), fint(f["ex_store"]),
        fint(f["ex_load"]), f["rows"]))
    ef = entry_figs(f["dc"])
    if ef is not None:
        body("      for each of its %s entries: Ir %.2f Dr %.2f Dw %.2f Bc %.2f; seven-byte moves %.2f (loads %.2f, "
             "stores %.2f; indexed pairs besides %.2f); shifts %.2f (%s)" % (
                 fint(ef["e"]), ef["Ir"], ef["Dr"], ef["Dw"], ef["Bc"], ef["mv"], ef["mv_ld"], ef["mv_st"],
                 ef["mv_idx"], ef["sh"], ", ".join("%s %.2f" % kv for kv in sorted(ef["sh_by"].items())) or "none"))
        for (a1, a2, d, n, t1, t2) in f["dc"]["live"][:8]:
            body("        seven-byte move at +0x%x, %s, executed %s (%.2f an entry): %s | %s" % (
                a1 - b.start, d, fint(n), n / ef["e"], t1.strip()[:60], t2.strip()[:60]))
    if f["per"]:
        body("      for each lex call: " + " | ".join(
            "%s: Ir %.2f Dr %.2f Dw %.2f Bc %.2f; stack stores %.2f loads %.2f" % (
                tag, v["Ir"], v["Dr"], v["Dw"], v["Bc"], v["st"], v["ld"])
            for tag, v in f["per"].items()))
    if f["calls"]:
        body("      executed calls by target: " + "; ".join(
            "%s x%s" % (show(strip_vendor(k), 64), fint(v)) for k, v in sorted(f["calls"].items(), key=lambda kv: -kv[1])))
    return f


def norm_body(pair, s, b):
    """A holder's instructions with every address replaced by what it names: a branch inside the
    function by its offset, a call by its target's key (through the GOT where it goes through it),
    a rip-relative operand by `rip+X`."""
    out = []
    for a, t in (b.insns or []):
        mn, ops = split_insn(t)
        g = _GOT.search(ops)
        if g:
            res = s.got.get(int(g.group(1), 16)) if s.got else None
            name = None
            if res and res[0] is not None:
                tb = s.at_start.get(res[0])
                name = min(s.bkeys[tb.idx]) if tb is not None else None
            if name is None:
                name = (res[1] if res else None) or g.group(2)
            ops = ops[:g.start()].rstrip() + " <" + name + ">"
        else:
            m = _DIRECT.match(ops)
            if m:
                tgt = int(m.group(1), 16)
                if b.start <= tgt < b.end:
                    ops = "<+0x%x>" % (tgt - b.start)
                else:
                    tb = s.at_start.get(tgt)
                    ops = "<" + (min(s.bkeys[tb.idx]) if tb is not None else m.group(2)) + ">"
        out.append(mn + " " + _LLVM_ANY.sub("", _RIP.sub("rip+X", ops)))
    return out


def code_compare(pair, b, h):
    import difflib
    x, y = norm_body(pair, pair.base, b), norm_body(pair, pair.head, h)
    if not x or not y:
        return "code not compared (no instructions kept)"
    if x == y:
        return "code identical, instruction for instruction (addresses normalised)"
    ops = difflib.SequenceMatcher(None, x, y, autojunk=False).get_opcodes()
    changed = sum(max(i2 - i1, j2 - j1) for tag, i1, i2, j1, j2 in ops if tag != "equal")
    return "code differs in %d places, %d instructions (addresses normalised)" % (
        sum(1 for o in ops if o[0] != "equal"), changed)


def holder_delta(body, fb, fh, pair=None, b=None, h=None):
    """One line: how a matched copy differs, head less base."""
    bits = ["size %+d B" % (fh["size"] - fb["size"]), "instructions %+d" % (fh["ninsn"] - fb["ninsn"])]
    if pair is not None:
        bits.append(code_compare(pair, b, h))
    if fb["pushes"] != fh["pushes"]:
        bits.append("pushes [%s] -> [%s]" % (" ".join(fb["pushes"]), " ".join(fh["pushes"])))
    else:
        bits.append("pushes the same")
    if fb["frame"] != fh["frame"]:
        bits.append("frame %s -> %s" % (("0x%x" % fb["frame"]) if isinstance(fb["frame"], int) else fb["frame"],
                                        ("0x%x" % fh["frame"]) if isinstance(fh["frame"], int) else fh["frame"]))
    else:
        bits.append("frame the same")
    bits.append("stack store sites %+d, load sites %+d" % (fh["st_store"] - fb["st_store"], fh["st_load"] - fb["st_load"]))
    eb, eh = entry_figs(fb.get("dc")), entry_figs(fh.get("dc"))
    if eb and eh:
        bits.append("for each entry: Ir %s Dr %s Dw %s Bc %s, seven-byte moves %s, shifts %s" % (
            fsigned(eh["Ir"] - eb["Ir"], 2), fsigned(eh["Dr"] - eb["Dr"], 2), fsigned(eh["Dw"] - eb["Dw"], 2),
            fsigned(eh["Bc"] - eb["Bc"], 2), fsigned(eh["mv"] - eb["mv"], 2), fsigned(eh["sh"] - eb["sh"], 2)))
    for tag in ("both", "x-exec"):
        pb, ph = (fb.get("per") or {}).get(tag), (fh.get("per") or {}).get(tag)
        if pb and ph:
            bits.append("%s per lex call: Ir %s Dr %s Dw %s Bc %s, stack stores %s loads %s" % (
                tag, fsigned(ph["Ir"] - pb["Ir"], 2), fsigned(ph["Dr"] - pb["Dr"], 2), fsigned(ph["Dw"] - pb["Dw"], 2),
                fsigned(ph["Bc"] - pb["Bc"], 2), fsigned(ph["st"] - pb["st"], 2), fsigned(ph["ld"] - pb["ld"], 2)))
    body("      DIFF (head less base): " + "; ".join(bits))


def drivers_table(body, pair):
    """Design 10.7's first two additions: for each GraphQLx driver that was entered, on each side,
    what it executes for each entry. An entry of a driver is one `lex` call."""
    for role, s in (("base", pair.base), ("head", pair.head)):
        if s.cg is None or s.offset is None:
            body("  %s %s: no counts" % (role, s.build))
            continue
        rows, total = [], 0
        for b in s.bodies:
            if not (b.holder and b.kind == "driver"):
                continue
            dc = driver_counts(s, b)
            ef = entry_figs(dc)
            if ef is None:
                continue
            total += ef["e"]
            rows.append((b, ef))
        rows.sort(key=lambda r: -r[1]["e"])
        body("  %s %s: %d drivers entered, %s entries in all (the two parse rows' lex calls are %s)" % (
            role, s.build, len(rows), fint(total), fint(tokens_of(EXEC) + tokens_of(TS))))
        body("    %-16s %-10s %9s %8s %7s %7s %6s %7s %8s %7s %-7s %s" % (
            "driver", "at", "entries", "Ir", "Dr", "Dw", "Bc", "7-byte", "indexed", "shifts", "frame", "shift forms"))
        for b, ef in rows:
            frame = (b.info or {}).get("frame")
            body("    %-16s 0x%-8x %9s %8.2f %7.2f %7.2f %6.2f %7.2f %8.2f %7.2f %-7s %s" % (
                NAMES.tokens(b.names[0])[-1][:16], b.start, fint(ef["e"]), ef["Ir"], ef["Dr"], ef["Dw"], ef["Bc"],
                ef["mv"], ef["mv_idx"], ef["sh"], ("0x%x" % frame) if isinstance(frame, int) else (frame or "-"),
                ", ".join("%s %.2f" % kv for kv in sorted(ef["sh_by"].items())) or "none"))


def part_e(body, pair):
    for role, s in (("base", pair.base), ("head", pair.head)):
        width = ITEM_BYTES.get(s.build, ITEM_DEFAULT)
        drivers = [b for b in s.bodies if b.holder and b.kind == "driver"]
        twin_starts = {b.start for b in s.bodies if any(lexer_item(n, "lex_in_block") for n in b.names)}
        callers = []
        for b in drivers:
            sites = sorted(a for t, rec in b.targets.items() if t[0] == "a" and t[1] in twin_starts for a in rec[2])
            if sites:
                callers.append((b, sites))
        body("  %s: %d drivers call lex_in_block (directly or through the GOT)" % (role, len(callers)))
        for b, call_sites in callers:
            insns = b.insns or []
            index = {a: i for i, (a, _) in enumerate(insns)}
            sites = (b.info or {}).get("sites", [])
            body("    driver %s {k:%s} @0x%x, executed %s" % (
                show(b.names[0], 110), tag_of(pair.key(b.names[0])), b.start,
                "yes" if b.own else "never"))
            for a in call_sites:
                idx = index.get(a)
                args = resolve_args(insns, idx) if idx is not None else {}
                body("      call at +0x%x, executed %s: %s" % (
                    a - b.start, fint(s.ir_at(a)), ", ".join("%s = %s" % kv for kv in args.items()) or
                    "no argument register set within 24 instructions"))
                slot = args.get("rdi")
                if not (isinstance(slot, str) and slot.startswith("[rsp")):
                    body("        rdi is not a stack slot of this frame: the answer's place is handed through")
                    continue
                k = int(slot[5:-1], 16) if "+" in slot else 0
                body("        the slot [rsp+0x%x, +%d): the answer's %d bytes (%s); each site that writes or reads "
                     "it, which may be another temporary sharing the space at another time:" % (
                         k, width, width, "V1's InBlock" if width == 80 else "an item"))
                for x in sites:
                    if x[1] != "rsp":
                        continue
                    lo, hi = x[2], x[2] + max(1, x[3])
                    if hi > k and lo < k + width:
                        body("          +0x%-6x %-7s %2s B [rsp+0x%x] executed %-12s %s" % (
                            x[0] - b.start, x[4], x[3] or "?", x[2], fint(s.ir_at(x[0])), x[6][:90]))
        body("  %s: the %d stack offsets most often written, in each executed driver (offset, widths, "
             "store sites/executed, load sites/executed)" % (role, TOP_OFFSETS))
        if s.cg is None or s.offset is None:
            body("    no counts: static store sites only")
        for b in drivers:
            sites = (b.info or {}).get("sites", [])
            by = {}
            for x in sites:
                if x[1] != "rsp":
                    continue
                r = by.setdefault(x[2], [0, 0, 0, 0, set()])
                ex = s.ir_at(x[0]) or 0
                if x[4] in ("store", "both"):
                    r[0] += 1
                    r[2] += ex
                if x[4] in ("load", "both"):
                    r[1] += 1
                    r[3] += ex
                if x[3]:
                    r[4].add(x[3])
            if not by or (s.cg is not None and not b.own):
                continue
            top = sorted(by.items(), key=lambda kv: (-kv[1][2], -kv[1][0], kv[0]))[:TOP_OFFSETS]
            body("    %s {k:%s} @0x%x: " % (show(b.names[0], 70), tag_of(pair.key(b.names[0])), b.start) + "; ".join(
                "+0x%x w%s st %d/%s ld %d/%s" % (off, "/".join(str(w) for w in sorted(r[4])) or "?", r[0], fint(r[2]),
                                                  r[1], fint(r[3])) for off, r in top))


def resolve_args(insns, idx):
    """The argument registers set before the call at `idx`: `lea reg,[rsp+k]`, or a move from a
    register so set, looking back at most 24 instructions and not past another call."""
    found, moves = {}, {}
    j = idx - 1
    while j >= 0 and idx - j <= 24:
        mn, ops = split_insn(insns[j][1])
        if mn == "call" or mn == "ret" or mn == "jmp":
            break
        o = [x.strip() for x in ops.split("#")[0].split(",")]
        if len(o) == 2:
            dst, src = o
            if mn == "lea" and dst not in found:
                m = _STACK.search(src)
                found[dst] = ("[%s+0x%x]" % (m.group(2), int(m.group(4), 16) * (-1 if m.group(3) == "-" else 1))
                              if m and m.group(4) else "[%s]" % m.group(2)) if m else src
            elif mn == "mov" and dst not in found and dst not in moves:
                moves[dst] = src
        j -= 1
    out = {}
    for r in _ARG_REGS:
        if r in found:
            out[r] = found[r]
        elif r in moves:
            src = moves[r]
            out[r] = found.get(src, src)
    return out


def part_f(pair):
    B, H = pair.base, pair.head
    if not (B.cg and H.cg and B.offset is not None and H.offset is not None):
        return None
    out = {}

    def attributed(s, vec_attr, row, ev):
        i = s.ctx_index(row)
        return 0.0 if (i is None or vec_attr is None) else vec_attr[i][s.cg.idx[ev]]

    for e in pair.entries.values():
        b = e.own_b or 0
        h = e.own_h or 0
        if b == 0 and h == 0:
            continue
        d = {"Ir": 0.0, "Dr": 0.0, "Dw": 0.0, "ts": 0.0}
        for s, copies, sign in ((B, e.bc, -1), (H, e.hc, 1)):
            for c in copies:
                if c.attr:
                    for ev in ("Ir", "Dr", "Dw"):
                        d[ev] += sign * attributed(s, c.attr, EXEC, ev) / tokens_of(EXEC)
                    d["ts"] += sign * attributed(s, c.attr, TS, "Ir") / tokens_of(TS)
        name = show((e.bc or e.hc)[0].names[0], 110) + " {k:%s}" % tag_of(e.key)
        if len(e.bc) > 1 or len(e.hc) > 1:
            name += " x%d/%d copies" % (len(e.bc), len(e.hc))
        out[("main", e.key)] = {"kind": e.kind, "lex": "yes" if e.holder else ("gql" if e.gholder else ""),
                                "b": b, "h": h, "dx": d["Ir"], "dr": d["Dr"], "dw": d["Dw"], "dt": d["ts"],
                                "name": name}
    for s, sign in ((B, -1), (H, 1)):
        ix = s.cg.idx
        ix_e, ix_t = s.ctx_index(EXEC), s.ctx_index(TS)
        for fn, v in s.cg.fn_self.items():
            if s.cg.fn_ob.get(fn) in s.cg.main_obs:
                continue
            ob = os.path.basename(s.cg.fn_ob.get(fn) or "?")
            nm = "(unnamed code)" if re.match(r"^0x[0-9a-fA-F]+$", fn) else fn
            r = out.setdefault(("lib", nm, ob), {"kind": "[lib]", "lex": "", "b": 0, "h": 0, "dx": 0.0, "dr": 0.0,
                                                 "dw": 0.0, "dt": 0.0, "name": "%s (%s)" % (show(nm, 90), ob)})
            r["h" if sign > 0 else "b"] += v[ix["Ir"]]
            fr = s.frac.get(fn)
            if fr:
                if ix_e is not None:
                    r["dx"] += sign * fr[ix_e] * v[ix["Ir"]] / tokens_of(EXEC)
                    r["dr"] += sign * fr[ix_e] * v[ix["Dr"]] / tokens_of(EXEC)
                    r["dw"] += sign * fr[ix_e] * v[ix["Dw"]] / tokens_of(EXEC)
                if ix_t is not None:
                    r["dt"] += sign * fr[ix_t] * v[ix["Ir"]] / tokens_of(TS)
    rows = list(out.values())
    rows.sort(key=lambda r: (-abs(r["h"] - r["b"]), r["name"]))
    return rows


def part_f_paths(pair):
    """Part F again, with every instance of one function path summed: where a change of types
    renames the instances (main against F'), the names no longer pair, and the paths do. GraphQLx's
    holders, GraphQL's and the rest are kept apart."""
    B, H = pair.base, pair.head
    if not (B.cg and H.cg and B.offset is not None and H.offset is not None):
        return None
    out = {}
    for s, sign in ((B, -1), (H, 1)):
        ix = s.cg.idx
        ie, it = s.ctx_index(EXEC), s.ctx_index(TS)
        for b in s.bodies:
            if not b.own:
                continue
            path = NAMES.bare(b.names[0])
            cls = "x" if b.holder else ("g" if b.gholder else "")
            r = out.setdefault((path, cls), {"b": 0, "h": 0, "dx": 0.0, "dt": 0.0, "dxr": 0.0, "dxw": 0.0,
                                             "nb": 0, "nh": 0, "kind": b.kind})
            r["h" if sign > 0 else "b"] += b.own[ix["Ir"]]
            r["nh" if sign > 0 else "nb"] += 1
            if ie is not None and b.attr:
                r["dx"] += sign * b.attr[ie][ix["Ir"]] / tokens_of(EXEC)
                r["dxr"] += sign * b.attr[ie][ix["Dr"]] / tokens_of(EXEC)
                r["dxw"] += sign * b.attr[ie][ix["Dw"]] / tokens_of(EXEC)
            if it is not None and b.attr:
                r["dt"] += sign * b.attr[it][ix["Ir"]] / tokens_of(TS)
    rows = [(k, v) for k, v in out.items() if v["h"] != v["b"]]
    rows.sort(key=lambda kv: -abs(kv[1]["h"] - kv[1]["b"]))
    return rows


F_PRIME_SHIFTS = 2.0  # design 10.7: F''s drivers execute two shifts for each entry, one right by 32 and one left


def is_prefixed_key(t):
    tok = key_tokens(t)
    return bool(tok) and tok[-1] == "prefixed" and tok[:3] == ["smear_lexer", "graphqlx", "syntactic"]


def holders_r2(pair):
    """The gate's v4 R2. A holder's own work is compared with the base's where its call targets are
    the base's, `prefixed` aside. Where a target came or went, the inlining of that callee moved, and
    the holder is compared together with it: its own work, plus the inclusive cost of its calls to
    the targets the other side's copy does not call. Each side's figure is attributed to the two
    parse rows by the holder's own share of them. Answers (totals, details)."""
    B, H = pair.base, pair.head
    if not (B.cg and H.cg and B.offset is not None and H.offset is not None):
        return None
    E = len(B.cg.events)

    def callee_key(s, callee):
        n = strip_vendor(callee)
        bs = s.by_name.get(n)
        return min(s.bkeys[bs[0].idx]) if bs else pair.key(n)

    moved_cost = {"base": {}, "head": {}}

    def value(s, b, extra, side=None):
        v = list(b.own) if b.own else [0] * E
        for (site, callee, count, incl) in (b.info or {}).get("arcs", []):
            k = callee_key(s, callee)
            if extra and k in extra:
                vadd(v, incl)
                if side:
                    moved_cost[side][k] = moved_cost[side].get(k, 0) + incl[s.cg.idx["Ir"]]
        if side and extra:
            for k in extra:
                moved_cost[side].setdefault(k, 0)
        return v

    def by_row(s, b, v):
        ix = s.cg.idx
        out = {}
        own_ir = (b.own[ix["Ir"]] if b.own else 0) or 0
        for row in (EXEC, TS):
            i = s.ctx_index(row)
            f = (b.attr[i][ix["Ir"]] / own_ir) if (own_ir and b.attr and i is not None) else 0.0
            out[row] = [f * x for x in v]
        return out

    tot = {row: {"base": [0.0] * E, "head": [0.0] * E} for row in (EXEC, TS)}
    details = []

    def add(side, rows):
        for row in (EXEC, TS):
            vadd(tot[row][side], rows[row])

    for e in pair.entries.values():
        if not e.holder:
            continue
        for b, h in list(e.pairs) + list(e.by_calls) + list(e.moved):
            tb = {t for t in pair.tkeys(B, b) if not is_prefixed_key(t)}
            th = {t for t in pair.tkeys(H, h) if not is_prefixed_key(t)}
            gone, came = tb - th, th - tb
            vb, vh = value(B, b, gone, "base"), value(H, h, came, "head")
            rb, rh = by_row(B, b, vb), by_row(H, h, vh)
            add("base", rb)
            add("head", rh)
            details.append((e, b, h, not gone and not came, gone, came, rb, rh))
        for b in e.ub:
            rb = by_row(B, b, value(B, b, None))
            add("base", rb)
            details.append((e, b, None, None, set(), set(), rb, None))
        for h in e.uh:
            rh = by_row(H, h, value(H, h, None))
            add("head", rh)
            details.append((e, None, h, None, set(), set(), None, rh))
    return tot, details, moved_cost


def selection(body, pair, builds, bres):
    """Design 10.8's conditions for V2 that the artifact can answer: 1, exactly, and 2, by count,
    with the gate's v4 R2; with 10.7's predictions beside the rows."""
    B, H = pair.base, pair.head
    if builds[1] != "V2":
        return
    body("== Design 10.8: V2's selection, what the artifact can answer ==")
    if H.cg is None or H.offset is None:
        body("  NO COUNTS on the head: V2 is read by structure alone, which shows the padding's move sites and "
             "not the tests' count (the gate's R2, hole 5)")
    # condition 1
    bad_moves, bad_shifts, lines = [], [], []
    base_shifts = {}
    if builds[0] == "F'" and B.cg is not None and B.offset is not None:
        for e in pair.entries.values():
            if e.holder:
                for b, h in list(e.pairs) + list(e.by_calls) + list(e.moved):
                    if b.kind == "driver":
                        ef = entry_figs(driver_counts(B, b))
                        if ef:
                            base_shifts[h.idx] = ef["sh"]
    for b in H.bodies:
        if not (b.holder and b.kind == "driver"):
            continue
        dc = driver_counts(H, b)
        ef = entry_figs(dc)
        static = len((b.info or {}).get("pairs", []))
        if ef is None:
            if static:
                lines.append("    never entered: %s @0x%x, %d seven-byte move sites" % (
                    NAMES.tokens(b.names[0])[-1], b.start, static))
            continue
        ref = base_shifts.get(b.idx, F_PRIME_SHIFTS)
        lines.append("    %s @0x%x: %s entries; seven-byte moves %.2f an entry (indexed pairs %.2f); shifts %.2f an "
                     "entry (%s), F''s %.2f" % (
                         NAMES.tokens(b.names[0])[-1], b.start, fint(ef["e"]), ef["mv"], ef["mv_idx"], ef["sh"],
                         ", ".join("%s %.2f" % kv for kv in sorted(ef["sh_by"].items())) or "none", ref))
        if ef["mv"] > 0 or ef["mv_idx"] > 0:
            bad_moves.append(b)
        if ef["sh"] > ref + 1e-9:
            bad_shifts.append(b)
    have = H.cg is not None and H.offset is not None
    body("  1. The padding is gone, and no word is cut finer (exact): %s" % (
        "UNDECIDED without counts" if not have else
        ("HOLDS: no seven-byte move executed in any GraphQLx driver, and no driver executes more shifts for each "
         "entry than F''s" if not bad_moves and not bad_shifts else
         "DOES NOT HOLD: %s%s" % (
             ("seven-byte moves executed in %d drivers" % len(bad_moves)) if bad_moves else "",
             ("; more shifts than F''s in %d drivers" % len(bad_shifts)) if bad_shifts else ""))))
    for line in lines:
        body(line)
    for b in bad_moves:
        for (a1, a2, d, n, t1, t2) in driver_counts(H, b)["live"][:6]:
            body("      the site: %s @0x%x +0x%x, %s, executed %s: %s | %s" % (
                NAMES.tokens(b.names[0])[-1], b.start, a1 - b.start, d, fint(n), t1.strip()[:60], t2.strip()[:60]))
    # condition 2
    if builds[0] != "F'":
        body("  2. At the floor, by count: read against F' only (W.F); this pair's base is %s" % builds[0])
    else:
        r2 = holders_r2(pair)
        if r2 is None:
            body("  2. At the floor, by count: UNDECIDED without counts on both sides")
        else:
            tot, details, moved_cost = r2
            ix = H.cg.idx
            res = {}
            for row in (EXEC, TS):
                res[row] = {ev: (tot[row]["head"][ix[ev]] - tot[row]["base"][ix[ev]]) / tokens_of(row)
                            for ev in ("Ir", "Dr", "Dw", "Bc")}
            both = {ev: sum(tot[row]["head"][ix[ev]] - tot[row]["base"][ix[ev]] for row in (EXEC, TS)) /
                    (tokens_of(EXEC) + tokens_of(TS)) for ev in ("Ir", "Dr", "Dw", "Bc")}
            over = [(row, ev) for row in (EXEC, TS) for ev in ("Ir", "Dr", "Dw") if res[row][ev] > V2_MARGIN[ev]]
            body("  2. At the floor, by count (10.7's margin, 7 instructions, 4 reads, 2 writes for each lex call, with "
                 "the gate's v4 R2): %s" % ("HOLDS" if not over else "DOES NOT HOLD: " + ", ".join(
                     "%s %s %s" % (row, ev, fsigned(res[row][ev])) for row, ev in over)))
            for row in (EXEC, TS):
                body("    the holders, V2 less F', for each lex call of %s: instructions %s, reads %s, writes %s, "
                     "branches %s" % (row, fsigned(res[row]["Ir"]), fsigned(res[row]["Dr"]), fsigned(res[row]["Dw"]),
                                      fsigned(res[row]["Bc"])))
            body("    both parse rows together (%s lex calls): instructions %s, reads %s, writes %s, branches %s" % (
                fint(tokens_of(EXEC) + tokens_of(TS)), fsigned(both["Ir"]), fsigned(both["Dr"]), fsigned(both["Dw"]),
                fsigned(both["Bc"])))
            moved = [d for d in details if d[3] is False]
            single = [d for d in details if d[3] is None]
            for side in ("base", "head"):
                mc = moved_cost[side]
                if mc:
                    body("    the callees whose inlining moved, on the %s, and what the holders' calls to them executed: %s" % (
                        side, "; ".join("%s %s Ir" % (show_key(k, 60), fint(v)) for k, v in
                                        sorted(mc.items(), key=lambda kv: (-kv[1], kv[0])))))
            body("    holders compared by their own counts: %d copies; together with a callee whose inlining moved: "
                 "%d%s; copies with no counterpart, compared as their own counts: %d" % (
                     sum(1 for d in details if d[3] is True), len(moved),
                     "" if not moved else " (" + "; ".join(
                         "%s @0x%x: %s%s" % (NAMES.tokens((d[2] or d[1]).names[0])[-1], (d[2] or d[1]).start,
                                            ("base calls " + ", ".join(show_key(t, 50) for t in sorted(d[4]))) if d[4] else "",
                                            ((", " if d[4] else "") + "head calls " + ", ".join(
                                                show_key(t, 50) for t in sorted(d[5]))) if d[5] else "")
                         for d in moved[:6]) + ")",
                     len(single)))
    # 10.7's other expectations
    for row in (EXEC, TS):
        x = bres.get(row)
        pred = V2_PREDICTED.get(row)
        if x and pred and builds[0] == "F'":
            body("  10.7's prediction for the %s row, V2 less F' for each lex call: instructions %+.1f, reads %+.1f, "
                 "writes %+.1f, branches %+.1f; part B reads %s, %s, %s, %s" % (
                     row, pred["Ir"], pred["Dr"], pred["Dw"], pred["Bc"], fsigned(x["Ir"]), fsigned(x["Dr"]),
                     fsigned(x["Dw"]), fsigned(x["Bc"])))
    pref = [b for b in H.bodies if any(lexer_item(n, "prefixed") for n in b.names)]
    twin = [b for b in H.bodies if any(lexer_item(n, "lex_in_block") for n in b.names)]
    sites = ex = holders_calling = 0
    pref_starts = {b.start for b in pref}
    for b in H.bodies:
        if not b.holder:
            continue
        n = sum(rec[0] + rec[1] for t, rec in b.targets.items() if t[0] == "a" and t[1] in pref_starts)
        if n:
            holders_calling += 1
            sites += n
            for t, rec in b.targets.items():
                if t[0] == "a" and t[1] in pref_starts:
                    for a in rec[2]:
                        ex += sum(c for _, c, _ in H.calls_at.get(a, []))
    body("  10.7's part C: lex_in_block %s; prefixed %s, called at %d sites in %d of %d holders, executed %s times" % (
        "is gone" if not twin else "IS STILL THERE", "is there once" if len(pref) == 1 else "%d times" % len(pref),
        sites, holders_calling, sum(1 for b in H.bodies if b.holder), fint(ex) if have else "-"))
    def frames(side):
        return ", ".join(("0x%x" % f) if isinstance(f, int) else str(f) for f in sorted(
            {(b.info or {}).get("frame") for b in side.bodies if b.holder and b.kind == "driver" and b.own},
            key=lambda x: (not isinstance(x, int), x))) or "-"
    body("  10.7's part D: the executed drivers' frames: base %s %s; head %s %s" % (
        builds[0], frames(B), builds[1], frames(H)))
    body("  3. At the floor, on the clock: not in the artifact. 4. No worse than ZH: read on W.Z's clock")
    body()


def questions(body, pair, label, builds, bres, hot_else, else_, unknown, holders, third, flist):
    B, H = pair.base, pair.head
    out = body
    have_counts = B.cg is not None and H.cg is not None and B.offset is not None and H.offset is not None

    # Question 1
    x = bres.get(EXEC)
    if not have_counts or x is None:
        out("Q1 (is the dispatch's +10 work?): UNANSWERED: %s" % (
            "no counts on a side" if not have_counts else "the executable row has no figure for each lex call"))
    else:
        over = [ev for ev in ("Ir", "Dr", "Dw") if x[ev] > TEST_MAX[ev]]
        out("Q1 (is the dispatch's +10 work?): the executable row, head less base, per lex call: instructions %s, "
            "reads %s, writes %s (conditional branches %s, indirect %s)" % (
                fsigned(x["Ir"]), fsigned(x["Dr"]), fsigned(x["Dw"]), fsigned(x["Bc"]), fsigned(x["Bi"])))
        if not over:
            out("    -> THE TEST AND NO MORE (at most 5 instructions, 1.5 reads and 0.5 writes for each lex call)")
        else:
            out("    -> MORE WORK: above the test's allowance in %s" % ", ".join(
                "%s (%s > %g)" % ({"Ir": "instructions", "Dr": "reads", "Dw": "writes"}[ev], fsigned(x[ev]),
                                  TEST_MAX[ev]) for ev in over))
        if flist:
            for ev, tag in (("Ir", "dx"), ("Dr", "dr"), ("Dw", "dw")):
                top = sorted([r for r in flist if abs(r[tag]) >= 0.005], key=lambda r: -abs(r[tag]))[:6]
                out("    part F, the executable row's share of each function's difference in %s, per lex call: %s" % (
                    {"Ir": "instructions", "Dr": "reads", "Dw": "writes"}[ev],
                    "; ".join("%s %s" % (r["name"][:90], fsigned(r[tag])) for r in top) or "none above 0.005"))
        t = bres.get(TS)
        if t is not None:
            out("    the type-system row beside it, per lex call: instructions %s, reads %s, writes %s, branches %s" % (
                fsigned(t["Ir"]), fsigned(t["Dr"]), fsigned(t["Dw"]), fsigned(t["Bc"])))

    # Question 2
    hot_units = [e for e in hot_else if not (e.mode == "aggregate" and e.hot_refined is False)]
    if not have_counts:
        out("Q2 (where is it?): STRUCTURE ALONE, no counts: %d functions differ outside the holders, hotness "
            "unknown; %d holders differ" % (len(else_), sum(1 for e in holders if e.differs)))
    elif not hot_else:
        out("Q2 (where is it?): IN THE FUNCTIONS THAT HOLD LEX: nothing hot differs elsewhere (%d differ outside "
            "the holders, none with a share of %s or more)" % (len(else_), fpct(HOT_SHARE, 1)))
    else:
        out("Q2 (where is it?): by R4's aggregates, ELSEWHERE: %d hot functions outside the holders differ; read copy "
            "by copy, %s" % (len(hot_else), ("ELSEWHERE: %d have a hot copy that differs or has no counterpart" %
                                             len(hot_units)) if hot_units else
                             "IN THE FUNCTIONS THAT HOLD LEX: no hot copy outside them differs"))
        for e in sorted(hot_else, key=lambda e: -max(e.share_b or 0, e.share_h or 0)):
            out("    " + entry_line(pair, e))
    if have_counts:
        dh = de = 0.0
        for e in pair.entries.values():
            d = 0.0
            for s, copies, sign in ((B, e.bc, -1), (H, e.hc, 1)):
                for c in copies:
                    if c.attr:
                        i = s.ctx_index(EXEC)
                        if i is not None:
                            d += sign * c.attr[i][s.cg.idx["Ir"]] / tokens_of(EXEC)
            if e.holder:
                dh += d
            else:
                de += d
        lib = sum(r["dx"] for r in (flist or []) if r["kind"] == "[lib]")
        out("    the executable row's difference in own instructions, per lex call: in the holders %s, elsewhere in the "
            "binary %s, in libraries %s" % (fsigned(dh), fsigned(de), fsigned(lib)))

    # Question 3
    q = {"base": drivers_q3(B), "head": drivers_q3(H)}
    if q["base"] is None or q["head"] is None:
        static = []
        for role, s in (("base", B), ("head", H)):
            st = ld = 0
            for b in s.bodies:
                if b.holder and b.kind == "driver":
                    f = holder_figs(s, b)
                    st += f["st_store"]
                    ld += f["st_load"]
            static.append("%s %d store sites, %d load sites" % (role, st, ld))
        out("Q3 (does the driver hold the item in a stack slot?): FROM SITES, no counts (9.10): the drivers' stack "
            "sites: " + "; ".join(static))
    else:
        bq, hq = q["base"], q["head"]
        dr, dw = hq["b"]["Dr"] - bq["b"]["Dr"], hq["b"]["Dw"] - bq["b"]["Dw"]
        out("Q3 (does the driver hold the item in a stack slot?): the GraphQLx drivers, for each lex call of both "
            "parse rows (9.4's scale): reads %.3f -> %.3f (%s), writes %.3f -> %.3f (%s); stack stores %.3f -> "
            "%.3f, loads %.3f -> %.3f; drivers executed %d/%d (the seven-byte moves, which say what reads and "
            "writes cannot, are part D's and the selection's)" % (
                bq["b"]["Dr"], hq["b"]["Dr"], fsigned(dr), bq["b"]["Dw"], hq["b"]["Dw"], fsigned(dw),
                bq["b"]["st"], hq["b"]["st"], bq["b"]["ld"], hq["b"]["ld"], bq["n"], hq["n"]))
        out("    by row, through the call arcs (estimated for %d/%d drivers): executable reads %s, writes %s, stack "
            "stores %s, loads %s; type system reads %s, writes %s" % (
                bq["estimated"], hq["estimated"], fsigned(hq["x"]["Dr"] - bq["x"]["Dr"]),
                fsigned(hq["x"]["Dw"] - bq["x"]["Dw"]), fsigned(hq["x"]["st"] - bq["x"]["st"]),
                fsigned(hq["x"]["ld"] - bq["x"]["ld"]), fsigned(hq["t"]["Dr"] - bq["t"]["Dr"]),
                fsigned(hq["t"]["Dw"] - bq["t"]["Dw"])))

        def verdict(d_r, d_w):
            cls = classify_q3(d_r, d_w)
            band = SECTION2[0] <= d_r <= SECTION2[1] and SECTION2[0] <= d_w <= SECTION2[1]
            return cls, "%s%s" % (cls.upper(), "" if cls != "above" else (
                "; both within section 2's predicted 3 to 9" if band else "; not both within section 2's predicted 3 to 9"))

        cls, text = verdict(dr, dw)
        out("    %s against %s: %s (as base: within the test's own 1.5 reads and 0.5 writes; above: both beyond it)" % (
            builds[1], builds[0], text))
        if third is not None:
            tq = drivers_q3(third)
            if tq is not None:
                tdr, tdw = tq["b"]["Dr"] - bq["b"]["Dr"], tq["b"]["Dw"] - bq["b"]["Dw"]
                tcls, ttext = verdict(tdr, tdw)
                same = third.sha is not None and B.sha is not None
                out("    %s, the other run's head, against this run's %s: reads %s, writes %s: %s" % (
                    third.build, builds[0], fsigned(tdr), fsigned(tdw), ttext))
                zh, v1 = (cls, tcls) if builds[1] == "ZH" else (tcls, cls)
                if v1 == "above" and zh == "above":
                    case = "V1 AS ZH, BOTH ABOVE F': V1 did not free the slot"
                elif v1 == "as base" and zh == "above":
                    case = "V1 AS F', ZH ABOVE: V1 freed the slot and the clock did not care"
                elif v1 == "as base" and zh == "as base":
                    case = "ALL THREE ALIKE: section 2 is wrong about the code"
                else:
                    case = "none of 9.4's three cases (ZH %s, V1 %s)" % (zh, v1)
                out("    -> " + case)
        elif label in Q3_SIBLING:
            out("    the other run (%s) is absent: 9.4's three cases need F', ZH and V1 together" % Q3_SIBLING[label])

    # Question 4
    def sites_and_calls(s, copies, t):
        st = ex = 0
        for c in copies:
            tk = pair.tkeys(s, c).get(t)
            if tk:
                st += tk[0] + tk[1]
                for a in tk[2]:
                    for callee, count, _incl in s.calls_at.get(a, []):
                        ex += count
        return st, ex

    gained, lost = {}, {}
    for e in holders:
        for t in e.gained:
            gained.setdefault(t, []).append(e)
        for t in e.lost:
            lost.setdefault(t, []).append(e)
    out("Q4 (which callees does one side inline that the other calls?): the call targets a holder has on one "
        "side only:")
    if not gained and not lost:
        out("    none, among holders on both sides")
    for title, d, s, side_copies in (("head only", gained, H, lambda e: e.hc), ("base only", lost, B, lambda e: e.bc)):
        for t in sorted(d):
            st = ex = 0
            for e in d[t]:
                a, b_ = sites_and_calls(s, side_copies(e), t)
                st += a
                ex += b_
            out("    %s: %s: %d sites in %d holders, executed %s" % (
                title, show(t, 110), st, len(d[t]), fint(ex) if have_counts else "-"))
    for e in holders:
        if e.only:
            s, copies = (H, e.hc) if e.only == "head" else (B, e.bc)
            tk = {}
            for c in copies:
                for k, v in pair.tkeys(s, c).items():
                    tk[k] = tk.get(k, 0) + v[0] + v[1]
            out("    a holder only on the %s, [%s] %s: %d call targets: %s" % (
                e.only, e.kind, show(copies[0].names[0], 80), len(tk),
                "; ".join("%s x%d" % (show(k, 60), v) for k, v in sorted(tk.items(), key=lambda kv: -kv[1])[:14])))
    tdiff = [e for e in else_ if e.gained or e.lost]
    out("    outside the holders, %d functions' call targets differ (%d hot)%s" % (
        len(tdiff), sum(1 for e in tdiff if e.hot), ":" if tdiff else ""))
    for e in sorted(tdiff, key=lambda e: -max(e.share_b or 0, e.share_h or 0))[:25]:
        out("      " + entry_line(pair, e))

    # R3, for the probes
    ub, uh = set(), set()
    for e in holders:
        for b in e.bc:
            ub |= set(pair.tkeys(B, b))
        for h in e.hc:
            uh |= set(pair.tkeys(H, h))
    union_gained = uh - ub
    allowed = INGREDIENT.get(builds[1], ())

    def is_ingredient(t):
        tok = key_tokens(t)
        return bool(tok) and tok[-1] in allowed and tok[:3] == ["smear_lexer", "graphqlx", "syntactic"]

    beyond = sorted(t for t in union_gained if not is_ingredient(t))
    if builds[1] in INGREDIENT and builds[0] == "F'":
        if not have_counts:
            out("R3 (is this probe's digest clean?): UNDECIDED: no counts, so hotness is unknown")
        else:
            agg = "CLEAN" if (not hot_else and not beyond) else "NOT CLEAN"
            units = "CLEAN" if (not hot_units and not beyond) else "NOT CLEAN"
            out("R3 (is this probe's digest clean?): %s" % (
                agg if agg == units else "by R4's aggregates %s; read copy by copy %s" % (agg, units)))
        out("    outside the holders: %s" % (
            ("%d hot functions differ by R4's aggregates%s" % (
                len(hot_else), ", %d copy by copy (%s)" % (len(hot_units), "; ".join(
                    show((e.bc or e.hc)[0].names[0], 70) for e in hot_units)) if hot_units else
                ", none copy by copy")) if have_counts else "structure alone: %d differ" % len(else_)))
        out("    the holders' call targets beyond the ingredient's (%s): %s" % (
            ", ".join(allowed) or "the ingredient adds none", ", ".join(show(t, 90) for t in beyond) or "none"))
        for t in sorted(union_gained):
            if t not in beyond:
                st = ex = 0
                for e in holders:
                    a, b_ = sites_and_calls(H, e.hc, t)
                    st += a
                    ex += b_
                out("    the ingredient's call, %s: %d sites, executed %s (9.5 expects none executed)" % (
                    show(t, 90), st, fint(ex) if have_counts else "-"))
        changed = [e for e in holders if e.only]
        if changed:
            out("    holders on one side only (noted; R3 prices spills, frame and order inside a holder as the "
                "ingredient's): " + "; ".join("%s %s" % (e.only, show((e.bc or e.hc)[0].names[0], 80)) for e in changed))
    else:
        out("R3: not a probe against F' (%s against %s); the holders' call targets new on the head, all holders "
            "together: %s" % (builds[1], builds[0], ", ".join(show(t, 80) for t in sorted(union_gained)[:20]) or "none"))

    if builds == ("M", "F'"):
        for row in (EXEC, TS):
            rb, rh = B.rows.get(row), H.rows.get(row)
            if rb and rh and B.cg and H.cg:
                ib, ih = rb["incl"][B.cg.idx["Ir"]], rh["incl"][H.cg.idx["Ir"]]
                out("9.9.3 (the residue, as work): %s: F' executes %s of main's instructions (%s): %s half a percent" % (
                    row, fpct(ih / ib if ib else 0, 3), fsigned(100.0 * (ih - ib) / ib if ib else 0, 3) + "%",
                    "within" if ib and abs(ih - ib) / ib <= 0.005 else "NOT within"))
    return []


# ---------------------------------------------------------------------------------------------
# Design 11.6, the floor plan: the digest's three additions, and amendment R1 (11.10)
# ---------------------------------------------------------------------------------------------

TWIN_ROWS = (TWIN[EXEC], TWIN[TS], TWIN[LEXX])
CHAIN_ROWS = [EXEC, TS, LEXX]
FIG_EVENTS = ("Ir", "Dr", "Dw", "Bc")
# 11.6's allowance, until B1 fixes its own: as a share of a row's count, for instructions and for reads
# and writes together. A parse row's is 0.5%, a lexer row's 0.1%.
ALLOWANCE = {EXEC: 0.005, TS: 0.005, LEXX: 0.001}
ROW_SHORT = {EXEC: "exec", TS: "type-system", LEXX: "lexer"}
JITTER_PER_ROW = 64  # det_rows': a row read at its function's calls counts measure's timing, up to 64 Ir


def row_split(s, row):
    """Addition 1: a row's inclusive counts for each `lex` call, and what its holders execute apart
    from the rest: the drivers' own counts, the loop's, and the row less the two. A row's holders are
    the functions that hold its dialect's `lex`: GraphQLx's for the GraphQLx rows, GraphQL's for their
    twins. A holder's share of a row is callgrind's attribution through the call arcs, exact where one
    row reaches the holder and an estimate where several do. None where the row, its counts or its
    lex calls are not there."""
    if s.cg is None or s.offset is None or row not in s.rows:
        return None
    i, n = s.ctx_index(row), s.lex_calls(row)
    if i is None or not n:
        return None
    ix = s.cg.idx
    flag = "gholder" if row in TWIN_ROWS else "holder"
    parts = {"driver": dict.fromkeys(FIG_EVENTS, 0.0), "loop": dict.fromkeys(FIG_EVENTS, 0.0)}
    held = {"driver": [], "loop": []}
    for b in s.bodies:
        if b.kind not in parts or not getattr(b, flag) or b.attr is None or b.attr[i][ix["Ir"]] <= 0:
            continue
        held[b.kind].append(b)
        for ev in FIG_EVENTS:
            parts[b.kind][ev] += b.attr[i][ix[ev]]
    incl = s.rows[row]["incl"]
    out = {"n": n, "holders": held, "estimated": sum(1 for k in held for b in held[k] if not b.exact),
           "row": {}, "driver": {}, "loop": {}, "rest": {}}
    for ev in FIG_EVENTS:
        tot = incl[ix[ev]] / n
        d, lp = parts["driver"][ev] / n, parts["loop"][ev] / n
        out["row"][ev], out["driver"][ev], out["loop"][ev], out["rest"][ev] = tot, d, lp, tot - d - lp
    return out


def token_clone(s, dialect):
    """Addition 2: the token's `Clone` as a function of its own. 11.5 found it in every Z1 build as one
    function out of line (`Spanned::map` over `<SyntacticToken as Clone>::clone`), where `main`'s is
    inline in its callers. Read by name, over both of rustc's manglings where the name keeps its
    arguments: present or not, each copy's size, its entries, and its own count."""
    needle = "smear_lexer::%s::syntactic::SyntacticToken<" % dialect
    copies = []
    for b in s.bodies:
        for n in b.names:
            if "SyntacticToken" in n and "5clone" in n:
                t = NAMES.fallback(n)
                if needle in t and "core::clone::Clone>::clone" in t:
                    copies.append(b)
                    break
    out = {"present": bool(copies), "copies": len(copies), "sizes": sorted(b.size for b in copies),
           "ninsn": sum(b.ninsn for b in copies), "entries": None, "own": None, "per": {}, "by_copy": [],
           "legacy": not any(n.startswith("_R") for n in list(s.by_name)[:2000])}
    if copies and s.cg is not None and s.offset is not None:
        ix = s.cg.idx
        out["by_copy"] = [(b.size, s.entries_at.get(b.start, 0), (b.own[ix["Ir"]] if b.own else 0)) for b in copies]
        out["entries"] = sum(e for _, e, _ in out["by_copy"])
        out["own"] = {ev: sum((b.own[ix[ev]] if b.own else 0) for b in copies) for ev in ("Ir", "Dr", "Dw")}
        out["per"] = {row: sum((s.attributed(b, row) or 0.0) for b in copies) / tokens_of(row) for row in (EXEC, TS)}
    return out


def clone_text(c):
    if c["legacy"]:
        return "unreadable by name: the names are legacy-mangled and keep no generic arguments"
    if not c["present"]:
        return "absent: no function of its own (inline in its callers, or never built)"
    text = "present: %d cop%s, %s B (%s B in all, %d instructions)" % (
        c["copies"], "y" if c["copies"] == 1 else "ies", " + ".join(str(x) for x in c["sizes"]), fint(sum(c["sizes"])),
        c["ninsn"])
    if c["entries"] is None:
        return text + "; no counts"
    e = c["entries"]
    text += "; entries %s (%s), own Ir %s Dr %s Dw %s" % (
        fint(e), " + ".join(fint(x) for _, x, _ in c["by_copy"]), fint(c["own"]["Ir"]), fint(c["own"]["Dr"]),
        fint(c["own"]["Dw"]))
    if e:
        text += "; for each entry Ir %.2f Dr %.2f Dw %.2f" % (c["own"]["Ir"] / e, c["own"]["Dr"] / e, c["own"]["Dw"] / e)
    return text + "; for each lex call, executable row Ir %.3f, type-system row Ir %.3f" % (
        c["per"].get(EXEC, 0.0), c["per"].get(TS, 0.0))


def clone_delta(cb, ch):
    if cb["legacy"] or ch["legacy"]:
        return "head less base: not read (legacy names)"
    bits = ["copies %+d" % (ch["copies"] - cb["copies"]), "size %+d B" % (sum(ch["sizes"]) - sum(cb["sizes"]))]
    if cb["entries"] is not None and ch["entries"] is not None:
        bits.append("entries %+d" % (ch["entries"] - cb["entries"]))
        bits.append("own Ir %+d" % (ch["own"]["Ir"] - cb["own"]["Ir"]))
        for row in (EXEC, TS):
            bits.append("%s row Ir for each lex call %s" % (ROW_SHORT[row], fsigned(ch["per"].get(row, 0.0) - cb["per"].get(row, 0.0))))
    return "head less base: " + ", ".join(bits)


def floor_additions(body, pair):
    """Additions 1 and 2, for one pair: the holders apart from the rest, and the token's clone."""
    B, H = pair.base, pair.head
    body("== G. The floor plan's additions to this digest (design 11.6) ==")
    body("  G.1 The holders apart from the rest, for each row, per lex call. A row's holders are the functions that hold "
         "its dialect's lex (GraphQLx's for the GraphQLx rows, GraphQL's for their twins): the drivers, and the loop (the "
         "lexer row's closure). 'rest' is the row less the two.")
    body("      A holder's share of a row is callgrind's attribution through the call arcs: exact where one row reaches it, "
         "an estimate where several do ('est' counts those).")
    body("  %-27s %-8s | %-39s | %-39s | %s" % ("row", "part", "base: Ir, Dr, Dw, Bc", "head: Ir, Dr, Dw, Bc",
                                                  "head less base: Ir, Dr, Dw, Bc"))
    for row in ROWS:
        fb, fh = row_split(B, row), row_split(H, row)
        if fb is None or fh is None:
            body("  %-27s no figure for each lex call on %s: no counts, or the row's closure did not run %d times" % (
                row, "the base" if fb is None else "the head", CLOSURE_CALLS))
            continue
        for part in ("row", "driver", "loop", "rest"):
            vb, vh = fb[part], fh[part]
            name = {"row": "the row", "driver": "drivers", "loop": "loop", "rest": "rest"}[part]
            extra = ""
            if part in ("driver", "loop"):
                extra = "  (%d/%d bodies" % (len(fb["holders"][part]), len(fh["holders"][part])) + (
                    ", est %d/%d)" % (sum(1 for b in fb["holders"][part] if not b.exact),
                                       sum(1 for b in fh["holders"][part] if not b.exact)))
            body("  %-27s %-8s | %s | %s | %s%s" % (
                row if part == "row" else "", name,
                " ".join("%9.3f" % vb[ev] for ev in FIG_EVENTS), " ".join("%9.3f" % vh[ev] for ev in FIG_EVENTS),
                " ".join("%+9.3f" % (vh[ev] - vb[ev]) for ev in FIG_EVENTS), extra))
    body()
    body("  G.2 The token's Clone as a line of its own (design 11.5: out of line from #6 or #8, 589 bytes, 48.8 "
         "instructions an entry; inline in main's callers)")
    for dialect in ("graphqlx", "graphql"):
        cb, ch = token_clone(B, dialect), token_clone(H, dialect)
        body("    %s's token, on the base (%s): %s" % (dialect, B.build, clone_text(cb)))
        body("    %s's token, on the head (%s): %s" % (dialect, H.build, clone_text(ch)))
        body("    %s's token, %s" % (dialect, clone_delta(cb, ch)))
    body()


def decisions_moved(pair):
    """A decision that moved is a function whose call targets changed between the two sides (11.6's
    reading of a step, 3). Counted in all, in the holders, and for each row in the functions the row
    executes (either side's attribution of the function to the row is above zero)."""
    B, H = pair.base, pair.head
    changed = [e for e in pair.entries.values() if e.gained or e.lost]
    out = {"all": len(changed), "holders": sum(1 for e in changed if e.holder), "rows": {}}
    for row in CHAIN_ROWS + list(TWIN_ROWS):
        n = 0
        for e in changed:
            a = sum((B.attributed(b, row) or 0.0) for b in e.bc) + sum((H.attributed(h, row) or 0.0) for h in e.hc)
            if a > 0:
                n += 1
        out["rows"][row] = n
    return out


def holder_sig(s, sp):
    """What a step must leave alone for 'nothing in a holder': the code of every holder the row reaches,
    and what each executes in all (not the row's share of it, which moves when another row's does)."""
    if sp is None:
        return None
    ix = s.cg.idx
    key = []
    for kind in ("driver", "loop"):
        for b in sp["holders"][kind]:
            dc = driver_counts(s, b)
            key.append((kind, b.codehash, b.size, tuple(b.own[ix[ev]] for ev in FIG_EVENTS) if b.own else None,
                        dc.get("shifts_ex"), dc.get("mv_ld"), dc.get("mv_st")))
    key.sort(key=repr)
    return tuple(key)


def chain_figs(s):
    """Everything the chain table reads from one base, so that the base can be dropped."""
    f = {"sha": s.sha, "size": s.size, "cpu": "?", "rows": {}, "sig": {}, "bad": [], "drv": None, "loop": None,
         "clone": token_clone(s, "graphqlx")}
    versions = read_versions(s.prefix) or []
    for line in versions:
        if "Model name" in line:
            f["cpu"] = line.split(":", 1)[1].strip()
    if s.sha_check is not None and not s.sha_check[0]:
        f["bad"].append("the binary does not hash to its .sha256")
    if s.cg is None or s.offset is None:
        f["bad"].append("no counts" if s.cg is None else "no address offset")
        return f
    if s.cg.totals is None or s.cg.self_total != s.cg.totals:
        f["bad"].append("the instructions' own costs do not sum to callgrind's total")
    for row in ROWS:
        c, _ = s.row_calls(row)
        if c != CLOSURE_CALLS:
            f["bad"].append("%s: its closure did not run %d times" % (row, CLOSURE_CALLS))
    ix = s.cg.idx
    for row in ROWS:
        sp = row_split(s, row)
        f["rows"][row] = None if sp is None else {k: sp[k] for k in ("n", "row", "driver", "loop", "rest")}
        f["sig"][row] = holder_sig(s, sp)
        if row == LEXX and sp is not None:
            sh = sum(driver_counts(s, b).get("shifts_ex", 0) for b in sp["holders"]["loop"])
            f["loop"] = {"Ir": sp["loop"]["Ir"], "Dr": sp["loop"]["Dr"], "Dw": sp["loop"]["Dw"], "Bc": sp["loop"]["Bc"],
                         "shifts": sh / sp["n"], "bodies": len(sp["holders"]["loop"])}
    ents = nd = sh = mv = 0
    own = dict.fromkeys(FIG_EVENTS, 0)
    for b in s.bodies:
        if b.holder and b.kind == "driver":
            dc = driver_counts(s, b)
            if not dc.get("entries"):
                continue
            nd += 1
            ents += dc["entries"]
            sh += dc["shifts_ex"]
            mv += dc["mv_ld"] + dc["mv_st"]
            for ev in FIG_EVENTS:
                own[ev] += b.own[ix[ev]] if b.own else 0
    if ents:
        f["drv"] = {"n": nd, "entries": ents, "shifts": sh / ents, "moves": mv / ents,
                    **{ev: own[ev] / ents for ev in FIG_EVENTS}}
    return f


def f3(x, w=9):
    return "%*s" % (w, "-") if x is None else "%*.3f" % (w, x)


def fs3(x, w=9):
    return "%*s" % (w, "-") if x is None else "%+*.3f" % (w, x)


def rw(v):
    return v["Dr"] + v["Dw"]


def head_prefix_of(prefix):
    """The run's `head` beside its `base`, where the base prefix names one."""
    d, name = os.path.split(prefix)
    return os.path.join(d, "head") if name == "base" else None


def chain_main(items):
    """Addition 3: one table over the chain. Each argument is `[label=]<run directory>/base`, in chain
    order: a base's artifact is its run's `base` files. For each base the three GraphQLx rows for each
    lex call, the step from the base before it, the holders' own counts for each entry, the shifts, and
    how many decisions moved; and amendment R1's sum of the steps that 11.6's step 5 leaves unread."""
    listed = []
    for it in items:
        if it.startswith("@"):
            with open(it[1:], "r", encoding="utf-8") as h:
                listed += [ln.strip() for ln in h if ln.strip() and not ln.startswith("#")]
        else:
            listed.append(it)
    specs = []
    for it in listed:
        head, sep, tail = it.partition("=")
        label, prefix = (head, tail) if sep and "/" not in head else (label_of(it), it)
        specs.append((label, prefix))
    if len(specs) < 2:
        sys.exit(__doc__)
    figs, moved, prev = [], [None], None
    for k, (label, prefix) in enumerate(specs):
        sys.stderr.write("chain %d/%d: %s\n" % (k + 1, len(specs), label))
        s = Side(prefix, "head", label).load()
        figs.append(chain_figs(s))
        if prev is not None:
            prev.role, s.role = "base", "head"
            moved.append(decisions_moved(Pair(prev, s, label)))
        prev = s
    prev = None
    out = Report()
    out("Z1 floor plan: the chain table (design 11.6, addition 3; amendment R1, 11.10)")
    out("  Each line is one base's artifact. A step is a base less the one before it. 'lex' is one lex call; the rows are "
        "the three GraphQLx rows, over %s lex calls (executable) and %s (type-system, lexer)." % (
            fint(tokens_of(EXEC)), fint(tokens_of(TS))))
    out()
    out("== bases, in chain order ==")
    out("  %3s %-18s %-12s %10s  %-14s %-34s %s" % ("#", "label", "sha256", "bytes", "same binary as", "CPU", "the artifact's own checks"))
    first_of, same = {}, {}
    for k, f in enumerate(figs):
        if f["sha"] in first_of:
            same[k] = first_of[f["sha"]]
        else:
            first_of[f["sha"]] = k
        out("  %3d %-18s %-12s %10s  %-14s %-34s %s" % (
            k + 1, specs[k][0][:18], (f["sha"] or "-")[:12], fint(f["size"]),
            ("#%d" % (same[k] + 1)) if k in same else "", f["cpu"][:34], "ok" if not f["bad"] else "; ".join(f["bad"])))
    out()
    out("== H's side, the check of every run (11.6): one SHA-256, and one set of counts for each CPU model ==")
    heads = []
    for k, (label, prefix) in enumerate(specs):
        hp = head_prefix_of(prefix)
        if hp is None or not os.path.exists(hp + ".sha256"):
            continue
        with open(hp + ".sha256", "r", encoding="utf-8", errors="replace") as h:
            parts = h.read().split()
        cgp = existing(hp + ".callgrind.out.gz", hp + ".callgrind.out")
        tot = callgrind_totals(cgp) if cgp else None
        heads.append((k, parts[0] if parts else None, tot, figs[k]["cpu"]))
    if not heads:
        out("  no run's head files beside these bases: not read")
    else:
        shas = sorted({h[1] for h in heads if h[1]})
        out("  H's SHA-256: %s over %d runs%s" % (
            "ONE, " + shas[0] if len(shas) == 1 else "%d DIFFERENT: %s" % (len(shas), ", ".join(x[:12] for x in shas)),
            len(heads), "" if len(shas) == 1 else " (the batch's head is not one binary)"))
        for model in sorted({h[3] for h in heads}):
            grp = [h for h in heads if h[3] == model and h[2]]
            if not grp:
                continue
            ev = {e: [h[2].get(e) for h in grp] for e in ("Ir", "Dr", "Dw")}
            spread = {e: (max(v) - min(v)) for e, v in ev.items()}
            ok = ("one set of counts" if not any(spread.values()) else
                  "one set of counts, within measure's timing (at most %d Ir for each of the %d rows)" % (
                      JITTER_PER_ROW, len(ROWS)) if max(spread.values()) <= JITTER_PER_ROW * len(ROWS) else
                  "NOT one set of counts")
            out("  %s: %d runs; totals Ir %s, Dr %s, Dw %s; spread Ir %d, Dr %d, Dw %d: %s" % (
                model, len(grp), fint(ev["Ir"][0]), fint(ev["Dr"][0]), fint(ev["Dw"][0]), spread["Ir"], spread["Dr"],
                spread["Dw"], ok))
    out()

    out("== 1. The three rows, for each lex call: instructions, reads, writes ==")
    out("  %3s %-16s | %-29s | %-29s | %s" % ("#", "label", "executable: Ir Dr Dw", "type-system: Ir Dr Dw", "lexer: Ir Dr Dw"))
    for k, f in enumerate(figs):
        cells = []
        for row in CHAIN_ROWS:
            r = f["rows"].get(row)
            cells.append(" ".join(f3(r["row"][ev] if r else None) for ev in ("Ir", "Dr", "Dw")))
        out("  %3d %-16s | %s" % (k + 1, specs[k][0][:16], " | ".join(cells)))
    out()

    out("== 2. The step from the base before it, for each lex call: this base less the one before ==")
    out("  moved: decisions moved, i.e. functions whose call targets changed, in the functions each row executes; all: "
        "in the whole binary (holders: of those, holders)")
    out("  %3s %-16s | %-29s | %-29s | %-29s | %-5s %-5s %-5s %-5s %-7s" % (
        "#", "label", "executable: dIr dDr dDw", "type-system: dIr dDr dDw", "lexer: dIr dDr dDw",
        "mv-x", "mv-t", "mv-l", "all", "holders"))
    for k, f in enumerate(figs):
        if k == 0:
            out("  %3d %-16s | (the first base: no step)" % (k + 1, specs[k][0][:16]))
            continue
        cells = []
        for row in CHAIN_ROWS:
            a, b = figs[k - 1]["rows"].get(row), f["rows"].get(row)
            cells.append(" ".join(fs3(b["row"][ev] - a["row"][ev] if a and b else None) for ev in ("Ir", "Dr", "Dw")))
        m = moved[k]
        out("  %3d %-16s | %s | %-5d %-5d %-5d %-5d %-7d" % (
            k + 1, specs[k][0][:16], " | ".join(cells), m["rows"][EXEC], m["rows"][TS], m["rows"][LEXX], m["all"], m["holders"]))
    out()
    out("  GraphQL's twin rows' step, for each lex call (the code the pull request does not touch: a new build's own "
        "effect, and decisions that moved)")
    out("  %3s %-16s | %-29s | %-29s | %s" % ("#", "label", "executable twin: dIr dDr dDw", "type-system twin: dIr dDr dDw",
                                              "lexer twin: dIr dDr dDw"))
    for k, f in enumerate(figs):
        if k == 0:
            continue
        cells = []
        for row in TWIN_ROWS:
            a, b = figs[k - 1]["rows"].get(row), f["rows"].get(row)
            cells.append(" ".join(fs3(b["row"][ev] - a["row"][ev] if a and b else None) for ev in ("Ir", "Dr", "Dw")))
        m = moved[k]
        out("  %3d %-16s | %s   moved %d/%d/%d" % (k + 1, specs[k][0][:16], " | ".join(cells),
                                                    m["rows"][TWIN[EXEC]], m["rows"][TWIN[TS]], m["rows"][TWIN[LEXX]]))
    out()

    out("== 3. The holders' own counts for each entry, the shifts, and the token's clone ==")
    out("  drivers: GraphQLx's, each entered driver's own counts over its entries (an entry is one lex call), summed over "
        "the drivers; the loop: the lexer row's, for each lex call; shifts and seven-byte moves are executed counts")
    out("  %3s %-16s | %-3s %9s %8s %8s %8s %8s %7s %7s | %8s %8s %8s %8s %7s | %s" % (
        "#", "label", "drv", "entries", "Ir", "Dr", "Dw", "Bc", "shifts", "7-byte", "loop Ir", "Dr", "Dw", "Bc", "shifts",
        "token's clone"))
    for k, f in enumerate(figs):
        d, lp, c = f["drv"], f["loop"], f["clone"]
        dtext = ("%-3d %9s %8.2f %8.2f %8.2f %8.2f %7.2f %7.2f" % (
            d["n"], fint(d["entries"]), d["Ir"], d["Dr"], d["Dw"], d["Bc"], d["shifts"], d["moves"])) if d else (
            "%-3s %9s %8s %8s %8s %8s %7s %7s" % ("-", "-", "-", "-", "-", "-", "-", "-"))
        ltext = ("%8.2f %8.2f %8.2f %8.2f %7.2f" % (lp["Ir"], lp["Dr"], lp["Dw"], lp["Bc"], lp["shifts"])) if lp else (
            "%8s %8s %8s %8s %7s" % ("-", "-", "-", "-", "-"))
        if c["legacy"]:
            ctext = "unreadable (legacy names)"
        elif not c["present"]:
            ctext = "absent"
        else:
            ctext = "%d cop%s, %d B, %s entries, own Ir %s%s" % (
                c["copies"], "y" if c["copies"] == 1 else "ies", sum(c["sizes"]), fint(c["entries"]), fint(
                    c["own"]["Ir"] if c["own"] else None),
                (" (%.1f an entry)" % (c["own"]["Ir"] / c["entries"])) if c["entries"] else "")
        out("  %3d %-16s | %s | %s | %s" % (k + 1, specs[k][0][:16], dtext, ltext, ctext))
    out()

    out("== R1 (amendment, 11.10): the steps 11.6's step 5 leaves unread, summed once per row ==")
    out("  A step is unread when, for the row, all three hold: it is inside the allowance (instructions, and reads and "
        "writes together, each against the base before it); no decision moved in a function the row executes; nothing "
        "changed in a holder the row reaches (its code, or what it executes in all, its shifts and its seven-byte "
        "moves). Allowance: %s." % ", ".join("%s %s" % (ROW_SHORT[r], fpct(ALLOWANCE[r], 1)) for r in CHAIN_ROWS))
    out("  per step and row: U unread; or R with why: a allowance exceeded, d a decision moved, h a holder changed; "
        "n no figure. Each cell: dIr and d(Dr+Dw), as a share of the base before it.")
    out("  %3s %-16s | %-26s | %-26s | %s" % ("#", "label", "executable", "type-system", "lexer"))
    sums = {row: {"n": 0, "unread": 0, "Ir": 0.0, "RW": 0.0, "aIr": 0.0, "aRW": 0.0, "none": 0} for row in CHAIN_ROWS}
    for k, f in enumerate(figs):
        if k == 0:
            continue
        cells = []
        for row in CHAIN_ROWS:
            a, b = figs[k - 1]["rows"].get(row), f["rows"].get(row)
            sm = sums[row]
            sm["n"] += 1
            if not a or not b:
                sm["none"] += 1
                cells.append("%-26s" % "n")
                continue
            dIr = b["row"]["Ir"] - a["row"]["Ir"]
            dRW = rw(b["row"]) - rw(a["row"])
            pIr = dIr / a["row"]["Ir"] if a["row"]["Ir"] else 0.0
            pRW = dRW / rw(a["row"]) if rw(a["row"]) else 0.0
            why = ""
            if abs(pIr) > ALLOWANCE[row] or abs(pRW) > ALLOWANCE[row]:
                why += "a"
            if moved[k]["rows"][row] > 0:
                why += "d"
            if figs[k - 1]["sig"].get(row) != f["sig"].get(row):
                why += "h"
            if why:
                tag = "R:" + why
            else:
                tag = "U"
                sm["unread"] += 1
                sm["Ir"] += dIr
                sm["RW"] += dRW
                sm["aIr"] += abs(dIr)
                sm["aRW"] += abs(dRW)
            cells.append("%-4s %+8.3f%% %+8.3f%%  " % (tag, 100 * pIr, 100 * pRW))
        out("  %3d %-16s | %s" % (k + 1, specs[k][0][:16], " | ".join(cells)))
    out()
    out("  The sum of each row's unread steps, judged once against the allowance (as a share of the chain's first base; "
        "the signed sum is what is judged, the sum of sizes is beside it):")
    out("  %-12s %-14s %12s %9s %12s %9s %10s  %s" % ("row", "unread/steps", "sum dIr", "of #1", "sum d(Dr+Dw)", "of #1",
                                                    "allowance", "verdict (signed sums; sum of sizes)"))
    for row in CHAIN_ROWS:
        sm, a0 = sums[row], figs[0]["rows"].get(row)
        if not a0 or not sm["n"]:
            out("  %-12s no figure" % ROW_SHORT[row])
            continue
        pIr = sm["Ir"] / a0["row"]["Ir"] if a0["row"]["Ir"] else 0.0
        pRW = sm["RW"] / rw(a0["row"]) if rw(a0["row"]) else 0.0
        within = abs(pIr) <= ALLOWANCE[row] and abs(pRW) <= ALLOWANCE[row]
        out("  %-12s %-14s %+12.3f %+8.3f%% %+12.3f %+8.3f%% %9s  %s (sizes: dIr %.3f, d(Dr+Dw) %.3f)%s" % (
            ROW_SHORT[row], "%d/%d" % (sm["unread"], sm["n"]), sm["Ir"], 100 * pIr, sm["RW"], 100 * pRW,
            fpct(ALLOWANCE[row], 1), "WITHIN the allowance" if within else "EXCEEDS the allowance", sm["aIr"], sm["aRW"],
            ("; %d steps had no figure" % sm["none"] if sm["none"] else "") +
            ("; no step was unread, so the sum is empty" if not sm["unread"] else "")))
    sys.stdout.write("\n".join(out.lines) + "\n")


if __name__ == "__main__":
    main(sys.argv[1:])

//! The tree dump, and the four byte-identity constants it is checked against.
//!
//! This module holds the machinery that `treedump.rs` used to own outright. It lives in the
//! shared harness so that **two** callers can reach it: the `treedump` example, which prints a
//! dump for a human to read or pipe into `shasum`, and the `byte_identity` test, which
//! recomputes the four hashes and fails if any of them moved. Both sit one directory up, beside
//! `support/`, for the reason [`super`]'s header gives.
//!
//! That second caller is the point. Before it existed, the four hashes lived in an issue and in a
//! commit message — which is a note, not a baseline. Nothing recomputed them, so a change that
//! altered every tree in the corpus would go green through every gate this repository had.
//!
//! # What the constants are pinned to
//!
//! A hash here is a function of **three** inputs, and all three have to be named or the number is
//! not reproducible:
//!
//! * the corpus, which is committed beside this file under `../corpus/` and does not move;
//! * smear's own parser, which is what the gate is *for* — if this moves, the gate reds;
//! * tokora, which this workspace takes from crates.io as a published version, not from a sibling
//!   checkout, so the constants reproduce on any machine with no path patch in play.
//!
//! Blessed against **published `tokora 0.9.0`**. Two of the four moved when this harness was
//! rebased onto the trunk, and both moved for one reason: `NamedType` now opens *after* its
//! leading trivia rather than before. The other two were byte-identical across that move, because
//! no `NamedType` in either of them has leading trivia to relocate.
//!
//! All four are unmoved from the values blessed against published `tokora 0.8.0`, and that is a
//! measured result rather than an expectation: the 0.8 → 0.9 bump carried tokora's `rowan` from
//! 0.16 to 0.17, and every removal 0.17 made is on the **red** side of the tree. The dump is
//! walked over the red tree but its content — kinds, spans and token text — is a projection of
//! the green one, which 0.17 does not touch.
//!
//! # Re-blessing
//!
//! Do not edit a constant to make the gate pass. A hash that moved is a claim about the tree, and
//! the diff names where: dump the corpus before and after and `diff` them. Re-bless only once the
//! change is understood and intended, and record *what* moved it in the same commit.

use core::fmt::Write as _;

use sha2::{Digest as _, Sha256};
use smear::parser::graphql::lossless::SyntaxNode;

use super::{CORPUS, Entry, smear_parse};

/// One element of a CST, projected onto the four axes the dump prints.
///
/// Materialised rather than streamed straight to the output so that the example's `selfcheck`
/// mode can compare two trees **column by column** — and therefore state "these two differ only
/// in kind" as a checked fact rather than as a hope about the source pair it chose.
pub struct Row {
  /// Levels of nesting below the root. The root itself is 0.
  pub depth: usize,
  /// `{:?}` of the node's or token's `SyntaxKind`.
  pub kind: String,
  /// Absolute byte offset of the element's first byte.
  pub start: u32,
  /// Absolute byte offset one past the element's last byte.
  pub end: u32,
  /// `Some` for a token, carrying its escaped text; `None` for an interior node.
  ///
  /// This is also what tells a node line from a token line on the page: a token always carries a
  /// quoted text and a node never does.
  pub text: Option<String>,
}

/// Escape a token's text so that one token is always one line, forever.
///
/// Backslash, quote and the three whitespace forms that would break the layout are spelled out;
/// every other C0/C1 control, plus the byte-order mark, becomes `\u{…}`. Anything else —
/// including ordinary multi-byte text — is written through verbatim.
///
/// `char::escape_debug` is the wrong tool here: its notion of "printable" is a Unicode table that
/// moves between toolchain releases, so a dump hashed on one toolchain and compared on another
/// could differ for a reason that has nothing to do with the tree. `char::is_control` is a fixed
/// two-range test and cannot drift — which is what makes the constants in this module portable.
pub fn escape(text: &str) -> String {
  let mut out = String::with_capacity(text.len());
  for ch in text.chars() {
    match ch {
      '\\' => out.push_str("\\\\"),
      '"' => out.push_str("\\\""),
      '\n' => out.push_str("\\n"),
      '\r' => out.push_str("\\r"),
      '\t' => out.push_str("\\t"),
      c if c.is_control() || c == '\u{feff}' => {
        let _ = write!(out, "\\u{{{:04x}}}", c as u32);
      }
      c => out.push(c),
    }
  }
  out
}

/// Walk a tree with `preorder_with_tokens` and project every element onto a [`Row`].
///
/// `preorder_with_tokens` rather than `descendants_with_tokens` because only the former reports
/// `Leave`, and without `Leave` there is no depth — which would cost the dump its one defence
/// against two differently-nested trees flattening to the same sequence.
pub fn rows(root: &SyntaxNode) -> Vec<Row> {
  let mut out = Vec::new();
  let mut depth = 0usize;
  for event in root.preorder_with_tokens() {
    match event {
      rowan::WalkEvent::Enter(element) => {
        let range = element.text_range();
        out.push(Row {
          depth,
          kind: match &element {
            rowan::NodeOrToken::Node(node) => format!("{:?}", node.kind()),
            rowan::NodeOrToken::Token(token) => format!("{:?}", token.kind()),
          },
          start: u32::from(range.start()),
          end: u32::from(range.end()),
          text: match &element {
            rowan::NodeOrToken::Node(_) => None,
            rowan::NodeOrToken::Token(token) => Some(escape(token.text())),
          },
        });
        depth += 1;
      }
      rowan::WalkEvent::Leave(_) => depth -= 1,
    }
  }
  out
}

/// Render rows in `.rast` style: `<indent><Kind>@<start>..<end>` for a node, plus ` "<text>"` for
/// a token.
pub fn render(rows: &[Row]) -> String {
  let mut out = String::new();
  for row in rows {
    for _ in 0..row.depth {
      out.push_str("  ");
    }
    let _ = write!(out, "{}@{}..{}", row.kind, row.start, row.end);
    if let Some(text) = &row.text {
      let _ = write!(out, " \"{text}\"");
    }
    out.push('\n');
  }
  out
}

/// The full dump for one source: a header, the tree, a footer, one line per diagnostic, a blank.
///
/// The footer is a summary of facts already implied by the lines above it, restated in one place
/// so that a `diff` of two dumps that disagree opens with *what* disagreed — a node count, a root
/// span, a round-trip verdict — before the reader has to find the first differing element line.
pub fn dump(header: &str, src: &str) -> String {
  let parse = smear_parse(src);
  let root = parse.syntax();
  let rows = rows(&root);

  let nodes = rows.iter().filter(|r| r.text.is_none()).count();
  let tokens = rows.len() - nodes;
  let round_trips = root.text() == src;

  let mut out = String::new();
  let _ = writeln!(out, "== {header} ==");
  out.push_str(&render(&rows));
  let _ = writeln!(
    out,
    "-- {nodes} nodes, {tokens} tokens, root {}..{}, round-trip {}, {} diagnostics --",
    u32::from(root.text_range().start()),
    u32::from(root.text_range().end()),
    if round_trips {
      "byte-exact"
    } else {
      "MISMATCH"
    },
    parse.diagnostics().len(),
  );
  for (i, d) in parse.diagnostics().iter().enumerate() {
    let span = d.span();
    let _ = writeln!(
      out,
      "   diagnostic {i}: {}..{} {:?} skipped={}",
      span.start,
      span.end,
      d.severity(),
      match d.skipped_tokens() {
        Some(n) => n.to_string(),
        None => "-".to_string(),
      },
    );
  }
  out.push('\n');
  out
}

/// Replace the entry's first `:` with a space, and report which byte moved.
///
/// The requirement on a control is that it change the *tree* and not merely the page. A colon in
/// GraphQL is a grammatical separator, and turning one into whitespace makes the production that
/// required it fail and recover. What makes it a good control specifically is what it does *not*
/// change: a one-byte substitution, so the document's length is identical, the root span is
/// unchanged, and the parse still round-trips byte-exactly. The hash moves because the tree moved.
pub fn perturb(src: &str) -> Option<(String, usize)> {
  // `:` is ASCII, so the byte index `find` returns is a char boundary in both directions.
  let at = src.find(':')?;
  let mut out = String::with_capacity(src.len());
  out.push_str(&src[..at]);
  out.push(' ');
  out.push_str(&src[at + 1..]);
  Some((out, at))
}

/// Hand-written broken documents — the corpus the clean entries cannot be.
///
/// A clean parse of this grammar takes **zero** emitter checkpoints, so it exercises no rewind,
/// no recovery-hole wrap, no gap tile and no error-coverage decision. Every one of those lives
/// on the recovery path, and the recovery path is only reachable from input that is wrong.
pub const MALFORMED: &[(&str, &str)] = &[
  ("unclosed_brace", "query Q { a { b "),
  ("stray_colon", "query Q { : a }"),
  ("bad_directive", "query Q @ @@ { a }"),
  ("unterminated_string", "{ f(a: \"abc) }"),
  ("junk_prefix", "%%% query Q { a }"),
  ("empty_args", "{ f() }"),
  ("bad_type", "query Q($v: ) { a }"),
  ("dangling_spread", "{ ... }"),
  ("bad_variable", "query Q($ $x: Int = ) { a }"),
  ("mixed_garbage", "type T { f: Int } ### $$$ ### { a }"),
  ("deep_nest_broken", "{ a { b { c { d { e ("),
  ("bad_default", "query Q($x: Int = @) { a }"),
  ("unterminated_block_string", "{ f(a: \"\"\"body ) }"),
  ("lone_dollar", "$"),
  ("nothing_lexable", "\u{7}\u{7}\u{7}"),
  ("empty", ""),
  ("only_trivia", "   \n\t # comment\n"),
  ("trailing_garbage_after_doc", "{ a } %%%"),
  ("leading_and_trailing_garbage", "%% { a } %%"),
  ("bad_escape_in_string", "{ f(a: \"x\\q\") }"),
  ("unclosed_paren_then_brace", "{ f(a: 1 }"),
  ("repeated_colons", "{ a:::: b }"),
  ("sdl_and_executable_mixed", "schema { query: } { a }"),
  ("number_garbage", "{ f(a: 1.2.3e) }"),
];

/// One corpus entry's dump, in whichever of the two modes was asked for.
pub fn entry_dump(entry: &Entry, perturbed: bool) -> String {
  if perturbed {
    match perturb(entry.source) {
      Some((src, at)) => dump(
        &format!(
          "{} : {} bytes : PERTURBED byte {at} ':' -> ' '",
          entry.name,
          src.len()
        ),
        &src,
      ),
      // Not a case any current entry hits, and not one to paper over: a corpus entry with no
      // colon would silently make the control identical to the dump it is a control for.
      None => format!(
        "== {} : {} bytes : NOT PERTURBED (no ':' in the source) ==\n\n",
        entry.name,
        entry.len()
      ),
    }
  } else {
    dump(
      &format!("{} : {} bytes", entry.name, entry.len()),
      entry.source,
    )
  }
}

/// The stride at which [`Corpus::Prefixes`] cuts an entry.
///
/// Every byte of the small entries; every 37th of the big one — a prime, so the cut points do not
/// align with any repeating structure in the document.
fn prefix_stride(entry: &Entry) -> usize {
  if entry.source.len() > 20_000 { 37 } else { 1 }
}

/// Which corpus a dump covers.
///
/// The four are not interchangeable and none is redundant. `Clean` is the only one over real
/// documents; `Perturbed` is its one-byte-broken control; `Malformed` reaches the recovery path
/// that clean input leaves entirely dark; `Prefixes` reaches thousands of *independent* recovery
/// sites by truncating each clean document at every cut point.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Corpus {
  /// The three corpus entries, as they stand.
  Clean,
  /// The three corpus entries, each with its first `:` turned into a space.
  Perturbed,
  /// [`MALFORMED`], 24 hand-broken documents.
  Malformed,
  /// Every cut point of every clean entry — roughly 9,300 documents.
  Prefixes,
}

impl Corpus {
  /// All four, in the order the gate reports them.
  pub const ALL: [Self; 4] = [
    Self::Clean,
    Self::Perturbed,
    Self::Malformed,
    Self::Prefixes,
  ];

  /// The argument that selects this corpus on the example's command line.
  pub const fn name(self) -> &'static str {
    match self {
      Self::Clean => "clean",
      Self::Perturbed => "perturbed",
      Self::Malformed => "malformed",
      Self::Prefixes => "prefixes",
    }
  }

  /// The recorded SHA-256 of this corpus's dump.
  ///
  /// See the module documentation for what these are pinned to and how to re-bless one. Editing a
  /// value here to make the `byte_identity` test pass converts the only byte-identity gate this
  /// repository has into a tautology.
  pub const fn expected(self) -> &'static str {
    match self {
      Self::Clean => "00f2b8b2bce48001aff45f8049141a13ca740fc6fe8eb4d9e287d1ccd4a46fc9",
      Self::Perturbed => "a42a39d41d4b8495f1ab5420292f9c76d194fd89697867fae1e2546ca64486b9",
      Self::Malformed => "2b5f293717f54346007f48459a2f0bd8cde76ea9344a56fb4010599296dab1c7",
      Self::Prefixes => "ab57d222a70151c1473072cb5b0d949719cc5c9958e0bd03fb1db96e9db30858",
    }
  }

  /// Roughly how much output this corpus produces, for a human deciding whether to redirect.
  pub const fn is_large(self) -> bool {
    matches!(self, Self::Prefixes)
  }
}

/// Stream this corpus's dump, one top-level unit at a time.
///
/// The concatenation of every chunk is exactly the dump — the same bytes the example prints and
/// the same bytes the recorded hashes are over. Streaming rather than returning one `String`
/// matters for [`Corpus::Prefixes`], whose dump is roughly 1.4 GB: the gate hashes it in bounded
/// memory instead of materialising it.
pub fn for_each_chunk(corpus: Corpus, mut f: impl FnMut(&str)) {
  match corpus {
    Corpus::Clean | Corpus::Perturbed => {
      let perturbed = corpus == Corpus::Perturbed;
      for entry in CORPUS {
        f(&entry_dump(entry, perturbed));
      }
    }
    Corpus::Malformed => {
      for (name, src) in MALFORMED {
        f(&dump(&format!("{name} : {} bytes", src.len()), src));
      }
    }
    Corpus::Prefixes => {
      for entry in CORPUS {
        let stride = prefix_stride(entry);
        let mut at = 0usize;
        while at <= entry.source.len() {
          if entry.source.is_char_boundary(at) {
            f(&dump(
              &format!("{} prefix {at}", entry.name),
              &entry.source[..at],
            ));
          }
          at += stride;
        }
      }
    }
  }
}

/// The SHA-256 of this corpus's dump, as lowercase hex.
///
/// Computed by streaming [`for_each_chunk`] into the hasher, so this is O(1) in memory even for
/// [`Corpus::Prefixes`], and is by construction the same value as
/// `treedump … | shasum -a 256`.
pub fn hash(corpus: Corpus) -> String {
  let mut hasher = Sha256::new();
  for_each_chunk(corpus, |chunk| hasher.update(chunk.as_bytes()));
  let digest = hasher.finalize();
  let mut out = String::with_capacity(64);
  for byte in digest {
    let _ = write!(out, "{byte:02x}");
  }
  out
}

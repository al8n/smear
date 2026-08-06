//! The syntactic span-extent derivation, owned in one place and read by both dialects' gates.
//!
//! # What this measures
//!
//! A composite AST node's span must be the **extent of the tokens it contains**: it opens at the
//! start of its first token and closes at the end of its last one. The parser's other option — and
//! the one it took before issue #68 — is to close at wherever the lookahead cursor happened to be,
//! which on compact input is the same number and on spaced input is the next token's start. That
//! is why a corpus cannot find this by being large: **on compact input every rule coincides**, so
//! only trivia deliberately injected at a junction can tell a correct span from an artifact.
//!
//! # Why the walk reads `Debug` rather than a visitor
//!
//! The check has to see **every** span, and the only total projection of these ASTs that exists is
//! the derived [`Debug`]. A hand-written visitor is precisely the thing that can fall one node
//! behind the AST and go on reporting success about the nodes it still knows; `#[derive(Debug)]`
//! renders every field of every node, so a node type added tomorrow enters this sweep without
//! anybody remembering to add it. The cost is a string walk over a pretty-printed tree, which is
//! paid once per padded parse and is not on any hot path.
//!
//! The walk's assumption about the rendering — a `span: SimpleSpan { start: N, end: M }` block
//! nested inside its owner's frame — is not taken on faith: each dialect's gate pins it with a
//! positive control over a hand-built rendering, and [`check`] is proved able to answer "no" by
//! the same controls.
//!
//! # The invariant, in four parts
//!
//! [`check`] applies all four to every span it is given, against the token extents of the same
//! bytes as reported by the **lexer** — never by the tree under test, which is the artifact being
//! asserted about.
//!
//! - `inverted` — `end` before `start`. Structural; no span may be unusable as a range.
//! - `start-not-token-start` / `end-not-token-end` — a **non-empty** node's endpoints are token
//!   endpoints of the matching kind. This is the pair that #68 is about, and between them they
//!   name every production that closed on a peek or opened on a cold cache.
//! - `empty-off-boundary` — an **empty** node (an absent optional collection carries a zero-width
//!   span as its position marker) sits on a token boundary rather than inside the ignorable bytes
//!   between two tokens. Weaker than the non-empty rule on purpose: a zero-width span records a
//!   *position*, and the two candidate positions on either side of a run of trivia are both
//!   defensible, so pinning it tighter than "not in the middle of the whitespace" would be pinning
//!   a choice rather than a fact. A source the lexer yields **no token at all** for — an empty
//!   file, a comment-only one, one the lexer rejects on its first byte — has no boundaries for the
//!   rule to name, and there the two input edges are the only positions there are; see [`check`].
//! - `not-contained` — a node's span lies inside its parent's. Free from the walk's nesting, and
//!   the check that catches an endpoint that ran past the construct it belongs to.
//!
//! # The liveness floor
//!
//! [`discriminating`] answers the question a silent sweep cannot: **was this span at a position
//! where the two rules disagree at all?** A span whose end is followed immediately by another
//! token, and whose start is preceded immediately by one, is a span the buggy rule and the correct
//! rule agree about — checking it proves nothing. Each gate asserts every node type it observed
//! was observed at least once at a discriminating site, so a sweep that padded nothing, or padded
//! only the edges, fails loudly instead of reporting a green wall of vacuous checks.

use std::collections::BTreeSet;

/// The eight ignorable forms, one variant of each corpus entry per form.
///
/// The same eight `is_trivia` admits, in the same order as the lossless trivia gates' alphabet —
/// `tests/support/graphqlx_padding.rs` owns that copy and the GraphQLx gate here asserts the two
/// agree element for element, so the pinning is by assertion rather than by hoping.
///
/// The comment carries its own line terminator: a comment runs to the end of the line, so `"# c"`
/// alone would swallow the token after it and the variant would stop being an injection.
pub const ALPHABET: &[(&str, &str)] = &[
  ("space", " "),
  ("tab", "\t"),
  ("newline", "\n"),
  ("carriage-return", "\r"),
  ("crlf", "\r\n"),
  ("comment", "# c\n"),
  ("comma", ","),
  ("bom", "\u{FEFF}"),
];

/// One span found in a pretty-printed `Debug` rendering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundSpan {
  /// The node type that owns it — the nearest enclosing named frame in the rendering.
  pub owner: String,
  /// The index in the same vector of the span of the nearest enclosing *node*, when there is one.
  pub parent: Option<usize>,
  /// The start offset.
  pub start: usize,
  /// The end offset.
  pub end: usize,
}

/// Every `SimpleSpan` in a `{:#?}` rendering, attributed to its owning node type and its parent
/// node's span.
///
/// The walk is a brace/paren/bracket stack over the pretty-printed lines. A line that ends in an
/// opener pushes a frame named after its type — `Foo {`, `field: Foo {`, `Some(` all name their
/// last identifier — and a line that begins with a closer pops one. When the frame being pushed is
/// a `SimpleSpan`, the owner is the nearest enclosing named frame and the parent is the nearest
/// enclosing frame that has already published a span index.
///
/// A `Debug`-rendered string never contains a raw newline (they are escaped) and never ends a line
/// with a bare opener, so no string literal in the source can be mistaken for a frame.
pub fn spans_of(dump: &str) -> Vec<FoundSpan> {
  /// One open frame in the rendering: its type name, and the index of the span it owns once that
  /// span has been read.
  struct Frame {
    name: String,
    span: Option<usize>,
  }

  /// The `SimpleSpan { .. }` block currently being read.
  struct Pending {
    owner: String,
    parent: Option<usize>,
    start: Option<usize>,
    end: Option<usize>,
  }

  let mut stack: Vec<Frame> = Vec::new();
  let mut out: Vec<FoundSpan> = Vec::new();
  let mut pending: Option<Pending> = None;

  for raw in dump.lines() {
    let line = raw.trim();
    if line.is_empty() {
      continue;
    }

    if let Some(rest) = line.strip_prefix("start: ")
      && let Some(number) = rest.strip_suffix(',')
      && let Some(slot) = pending.as_mut()
      && let Ok(value) = number.parse::<usize>()
    {
      slot.start = Some(value);
      continue;
    }
    if let Some(rest) = line.strip_prefix("end: ")
      && let Some(number) = rest.strip_suffix(',')
      && let Some(slot) = pending.as_mut()
      && let Ok(value) = number.parse::<usize>()
    {
      slot.end = Some(value);
      continue;
    }

    if line.ends_with('{') || line.ends_with('(') || line.ends_with('[') {
      let label = line[..line.len() - 1].trim();
      let name = label
        .rsplit_once(": ")
        .map_or(label, |(_, ty)| ty)
        .to_string();
      if name == "SimpleSpan" {
        let owner = stack
          .iter()
          .rev()
          .find(|frame| !frame.name.is_empty())
          .map_or_else(|| "<root>".to_string(), |frame| frame.name.clone());
        let parent = stack.iter().rev().find_map(|frame| frame.span);
        pending = Some(Pending {
          owner,
          parent,
          start: None,
          end: None,
        });
      }
      stack.push(Frame { name, span: None });
      continue;
    }

    if line.starts_with('}') || line.starts_with(')') || line.starts_with(']') {
      let popped = stack.pop();
      if popped.as_ref().map(|frame| frame.name.as_str()) == Some("SimpleSpan")
        && let Some(Pending {
          owner,
          parent,
          start: Some(start),
          end: Some(end),
        }) = pending.take()
      {
        let index = out.len();
        out.push(FoundSpan {
          owner,
          parent,
          start,
          end,
        });
        // The frame that owns this span can now be somebody else's parent.
        if let Some(frame) = stack.iter_mut().rev().find(|frame| !frame.name.is_empty()) {
          frame.span = Some(index);
        }
      }
      continue;
    }
  }
  out
}

/// One failed part of the invariant, named so a red gate says which node and which rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Violation {
  /// The node type whose span is wrong.
  pub owner: String,
  /// Which part of the invariant it broke — see the module docs.
  pub rule: &'static str,
  /// The span and what the source says about it.
  pub detail: String,
}

/// The token extents of a source: every token's start, and every token's end.
#[derive(Debug, Clone, Default)]
pub struct Extents {
  /// Every offset a token begins at.
  pub starts: BTreeSet<usize>,
  /// Every offset a token ends at.
  pub ends: BTreeSet<usize>,
}

/// Every way `spans` fails the extent invariant against `extents`.
///
/// An empty result is the pass. `src` is used only to quote the offending bytes in the message.
///
/// # The tokenless source
///
/// `empty-off-boundary` asks whether a zero-width span sits on a token boundary, and a source the
/// lexer yields **no** token for has none: an empty file, a comment-only one, a byte the lexer
/// refuses outright. The invalid corpora contain all three, and a parse of one under a recovering
/// emitter still produces a document node — a zero-width one, because there is nothing for it to
/// contain. The only two positions such a node can carry are the input's own edges, so both are
/// accepted **and only when the token set is empty**: with even one token in the source the
/// ordinary rule applies at offset 0 like anywhere else, so an optional-collection marker parked in
/// front of a leading run of trivia is still named. `the_checker_can_answer_no` pins both halves.
pub fn check(src: &str, spans: &[FoundSpan], extents: &Extents) -> Vec<Violation> {
  let tokenless = extents.starts.is_empty() && extents.ends.is_empty();
  let mut out = Vec::new();
  let mut note = |owner: &str, rule: &'static str, detail: String| {
    out.push(Violation {
      owner: owner.to_string(),
      rule,
      detail,
    });
  };

  for span in spans {
    let quoted = if span.start <= span.end && span.end <= src.len() {
      format!("{:?}", &src[span.start..span.end])
    } else {
      "<out of range>".to_string()
    };
    let where_ = format!("{}..{} {quoted}", span.start, span.end);

    if span.end < span.start {
      note(&span.owner, "inverted", where_);
      continue;
    }

    if span.start < span.end {
      if !extents.starts.contains(&span.start) {
        note(
          &span.owner,
          "start-not-token-start",
          format!("{where_} — opens inside the ignorable bytes before its first token"),
        );
      }
      if !extents.ends.contains(&span.end) {
        note(
          &span.owner,
          "end-not-token-end",
          format!("{where_} — closes past its last token, in the ignorable bytes that follow"),
        );
      }
    } else if !extents.starts.contains(&span.start)
      && !extents.ends.contains(&span.start)
      && !(tokenless && (span.start == 0 || span.start == src.len()))
    {
      note(
        &span.owner,
        "empty-off-boundary",
        format!("{where_} — a zero-width span parked inside a run of trivia"),
      );
    }

    if let Some(parent) = span.parent.map(|index| &spans[index])
      && (span.start < parent.start || span.end > parent.end)
    {
      note(
        &span.owner,
        "not-contained",
        format!(
          "{where_} — escapes its {} parent at {}..{}",
          parent.owner, parent.start, parent.end
        ),
      );
    }
  }
  out
}

/// Is this span at a position where the token-extent rule and the lookahead rule **disagree**?
///
/// True when an ignorable byte sits immediately after the span's end or immediately before its
/// start — the only situation in which closing on a peek, or opening on a cold cache, produces a
/// different number from closing and opening on the tokens. A span for which this is false was
/// checked, but nothing about it could have failed, which is what the liveness floor exists to
/// notice.
///
/// Derived from the lexer's extents rather than from a byte classifier: an offset that is a token
/// end and not also a token start has ignorable bytes after it, by definition of "the tokens tile
/// everything that is not trivia".
pub fn discriminating(span: &FoundSpan, extents: &Extents, len: usize) -> bool {
  let trivia_after =
    span.end < len && extents.ends.contains(&span.end) && !extents.starts.contains(&span.end);
  let trivia_before =
    span.start > 0 && extents.starts.contains(&span.start) && !extents.ends.contains(&span.start);
  trivia_after || trivia_before
}

/// `src` with `pad` inserted at every boundary in `boundaries`.
pub fn inject(src: &str, boundaries: &[usize], pad: &str) -> String {
  let mut out = String::with_capacity(src.len() + boundaries.len() * pad.len());
  let mut cursor = 0usize;
  for &boundary in boundaries {
    out.push_str(&src[cursor..boundary]);
    out.push_str(pad);
    cursor = boundary;
  }
  out.push_str(&src[cursor..]);
  out
}

/// The distinct node types among `spans`.
pub fn owners(spans: &[FoundSpan]) -> BTreeSet<String> {
  spans.iter().map(|span| span.owner.clone()).collect()
}

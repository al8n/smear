//! The GraphQLx trivia-injection derivation, owned in one place and read by both gates that pad.
//!
//! # Why this is a module and not two agreeing constants
//!
//! Gate 1 (`lossless_x_parity.rs`) runs the corpus twice, compact and padded; gate 2
//! (`lossless_x_trivia.rs`) is the padded sweep proper. The plan requires gate 1's padded set to be
//! **exactly the one gate 2 derives** — and an `assert_eq!` between two `const ALPHABET`s cannot be
//! that, for a reason that is structural rather than stylistic: a Rust integration test is its own
//! crate, so no assertion inside one of these files can name an item in the other. Two copies would
//! be two copies, agreeing on the day they were written and free to drift after.
//!
//! So there is one derivation. [`ALPHABET`], [`token_boundaries`], [`inject`], [`corpus_files`] and
//! [`UNPADDABLE`] live here and nowhere else, and the two gates are pinned to each other by
//! identity. What each gate then adds on top is its own: gate 1 pads **every** lexable entry and
//! runs both suites over the result, gate 2 pads the **valid** half and compares tree shapes.
//!
//! Directory placement is load-bearing. Cargo's test autodiscovery takes `tests/*.rs` and
//! `tests/*/main.rs`; a file one level down under any other name is not a target, which is what
//! lets this be a shared module rather than a fifteenth test binary that runs nothing.

use std::path::{Path, PathBuf};

use smear::{
  lexer::graphqlx::lossless::{LosslessLexer, LosslessToken},
  parser::graphqlx::{kinds::SyntaxKind, lossless::parse_document},
};
use tokora::{Lexer as _, SimpleSpan};

/// The eight ignorable forms, one variant of each corpus entry per form.
///
/// GraphQLx's trivia block is the same six images as GraphQL's — BOM, comma, space, tab, line
/// terminator, comment — with the three line terminators folded onto one kind, so the eight *forms*
/// are the same eight and this alphabet is deliberately identical to the GraphQL twin's.
/// `lossless_x_trivia.rs` re-measures both halves of that claim every run: every form is trivia at
/// every position, and the eight together reach every trivia image the kind space has.
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

/// The corpus entries that cannot be padded, because they cannot be lexed.
///
/// Injection is defined at *token* boundaries, so a source the lexer refuses byte for byte has no
/// boundaries to inject at. Four entries are in that state, all inherited from the GraphQL corpus
/// and all still unlexable under GraphQLx's lexer — which is not automatic: GraphQLx lexes seven
/// images GraphQL does not (`*`, `+`, `-`, `<`, `>`, `::`, `=>`), so an "illegal character" in one
/// dialect is not necessarily one in the other, and this list is the measured answer rather than
/// the copied one.
///
/// Pinned as a set rather than skipped silently: a **new** unlexable entry is a corpus decision
/// somebody should have to make on purpose. Every one of the four is `invalid_*`, which is what
/// lets gate 2 pad its whole half of the corpus — `lossless_x_trivia.rs` asserts that rather than
/// assuming it.
pub const UNPADDABLE: &[&str] = &[
  "invalid_lex_illegal_character.graphqlx",
  "invalid_lex_unterminated_block_string.graphqlx",
  "invalid_lex_unterminated_string.graphqlx",
  "invalid_unterminated_string.graphqlx",
];

/// Every `.graphqlx` file in the GraphQLx corpus, in a deterministic order.
pub fn corpus_files() -> Vec<PathBuf> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpusx");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| {
      panic!(
        "the GraphQLx corpus at {} is unreadable: {e}",
        dir.display()
      )
    })
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphqlx"))
    .collect();
  files.sort();
  files
}

/// The file name of `path`, as a `String`.
pub fn name_of(path: &Path) -> String {
  path
    .file_name()
    .expect("a corpus entry has a file name")
    .to_string_lossy()
    .to_string()
}

/// The outermost `const { … }` blocks of `src`, each its start and its end, as the lossless parser
/// reads them.
///
/// A block's region is lexed in the block's mode, and only a parser can say where one is:
/// `{ const { id } }` is a field with a selection set. So the extents come from a parse and the
/// tokens from a lexer, and [`lexed`]'s own control in `lossless_x_trivia.rs` holds the two to each
/// other.
pub fn block_extents(src: &str) -> Vec<(usize, usize)> {
  parse_document(src)
    .syntax()
    .descendants()
    .filter(|node| {
      node.kind() == SyntaxKind::ConstBlock
        && !node
          .ancestors()
          .skip(1)
          .any(|ancestor| ancestor.kind() == SyntaxKind::ConstBlock)
    })
    .map(|node| {
      let range = node.text_range();
      (usize::from(range.start()), usize::from(range.end()))
    })
    .collect()
}

/// Every token of `src` with its span, lexed as the parser lexes it: the lexer enters a block's
/// mode after the `{` of each outermost block of [`block_extents`] and leaves it after the block's
/// `}`, and lexes everything else from the default state.
///
/// `None` when the source does not lex — see [`UNPADDABLE`].
pub fn lexed(src: &str) -> Option<Vec<(LosslessToken<&str>, SimpleSpan)>> {
  let blocks = block_extents(src);
  let mut lexer = LosslessLexer::<'_, &str>::new(src);
  let mut out = Vec::new();
  // The end of the block whose mode the lexer is in.
  let mut inside: Option<usize> = None;
  while let Some(result) = lexer.lex() {
    let token = result.ok()?;
    let span = lexer.span();
    match inside {
      None => {
        if matches!(token, LosslessToken::LBrace)
          && let Some(&(_, end)) = blocks
            .iter()
            .find(|(start, end)| *start <= span.start() && span.end() < *end)
        {
          lexer.state_mut().enter_const_block();
          inside = Some(end);
        }
      }
      Some(end) => {
        if span.end() == end {
          lexer.state_mut().leave_const_block();
          inside = None;
        }
      }
    }
    out.push((token, span));
  }
  Some(out)
}

/// Every token boundary in `src`: offset 0, the end of each token, and therefore `src.len()`.
///
/// `None` when the source does not lex — see [`UNPADDABLE`]. Measured with the **lexer**, not with
/// the tree under test: reading the boundaries off a parse would make the padding a function of the
/// artifact being asserted about, so a parser that lost a token would quietly stop being padded
/// there. The lexer reads each block in the block's mode, as [`lexed`] does.
pub fn token_boundaries(src: &str) -> Option<Vec<usize>> {
  let mut boundaries = vec![0usize];
  boundaries.extend(lexed(src)?.into_iter().map(|(_, span)| span.end()));
  boundaries.dedup();
  Some(boundaries)
}

/// `src` with `pad` inserted at every boundary in `boundaries`.
///
/// **A comma is no padding inside a block:** between a block's braces the lexer reads a comma as a
/// separator, so the comma form is injected at no boundary inside an outermost block. The seven
/// other forms pad there as they do everywhere.
pub fn inject(src: &str, boundaries: &[usize], pad: &str) -> String {
  let blocks = if pad == "," {
    block_extents(src)
  } else {
    Vec::new()
  };
  let mut out = String::with_capacity(src.len() + boundaries.len() * pad.len());
  let mut cursor = 0usize;
  for &boundary in boundaries {
    out.push_str(&src[cursor..boundary]);
    if !blocks
      .iter()
      .any(|(start, end)| *start < boundary && boundary < *end)
    {
      out.push_str(pad);
    }
    cursor = boundary;
  }
  out.push_str(&src[cursor..]);
  out
}

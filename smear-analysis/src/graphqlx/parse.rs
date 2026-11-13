//! Parsing: every file, with a GraphQLx parser layer, into the documents the model is lowered
//! from.
//!
//! The caller's text type is gone before this module is reached: a file is a `View`, text or
//! bytes. A byte text that is UTF-8 is parsed as text, so the layer reads `str` and nothing else.
//! A byte text that is not UTF-8 is read by the syntactic layer over `[u8]`, whatever layer the
//! analysis was given: such a text is a syntax error wherever its bad byte stands, so no layer has
//! a document for it.
//!
//! A file that holds nothing but what the lexer skips, white space, commas, byte order marks and
//! comments, is accepted here as a document with no item, before any layer sees it: the
//! type-system document production wants one item at least, and a directory may hold a file that
//! only documents it. Such a file has no tree.
//!
//! The built-in packages' files are read by the syntactic layer under its default limits, whatever
//! layer the analysis was given, and have no tree.

use core::ops::ControlFlow;
use std::{
  string::{String, ToString},
  vec::Vec,
};

use smear_parser::graphqlx::{Layer, Report, Syntactic, ast::TypeSystemDocument};
use tokora::{
  SimpleSpan,
  diagnostic::{Diagnose, DiagnoseExt, Location},
  utils::IntoComponents,
};

use super::{
  code::DiagnosticKind,
  diagnostic::{Diagnostic, Reporter},
  id::{SourceId, dense},
  set::{PackageView, ROOT},
  source::SourceMap,
  text::View,
};

/// One file's document: the hand-over from parsing to the phases that read documents.
///
/// It borrows the caller's text, so it lives inside one analysis and no longer. The phases read it
/// and copy into the model every spelling the model keeps; once the model is built, nothing of the
/// caller's text type remains.
pub(crate) enum Parsed<'a> {
  /// A text, or a byte text that is UTF-8.
  Text(TypeSystemDocument<&'a str>),
  /// A byte text that is not UTF-8. Such a text was refused with a syntax diagnostic, so this
  /// variant exists for a parser that recovers; the fail-fast parser this module runs answers no
  /// document for it.
  Bytes(TypeSystemDocument<&'a [u8]>),
}

/// The parsed documents of one analysis, one entry per source in id order.
pub(crate) struct Documents<'a> {
  /// `None` for a package source, a file the parser refused, and every supplied file when the
  /// supplied sources were over their size limit.
  entries: Vec<Option<Parsed<'a>>>,
}

impl<'a> Documents<'a> {
  /// How many sources the documents cover: every source of the analysis.
  pub(crate) fn len(&self) -> usize {
    self.entries.len()
  }

  /// The document of `source`: `None` for a package source, a file the parser refused, and a
  /// supplied file past the size limit.
  pub(crate) fn get(&self, source: SourceId) -> Option<&Parsed<'a>> {
    self.entries.get(source.get() as usize)?.as_ref()
  }
}

/// Parses every file of `packages`, which are in id order and numbered by `sources`, reporting
/// each file's syntax errors at that file.
///
/// The built-in packages' files are always parsed, by the syntactic layer. The supplied packages'
/// files are parsed only when `supplied` is true, past the source size limit none is, and each one
/// that is text, or bytes that are UTF-8, is parsed by `parser`. What `parser` keeps for such a
/// file goes into `trees`, in source order, when the caller keeps trees at all.
pub(crate) fn files<'a, P>(
  parser: &P,
  packages: &[PackageView<'a>],
  sources: &SourceMap,
  supplied: bool,
  reporter: &mut Reporter,
  mut trees: Option<&mut Vec<(SourceId, P::Tree)>>,
) -> Documents<'a>
where
  P: Layer<'a>,
{
  let built_in = Syntactic::new();
  let mut entries: Vec<Option<Parsed<'a>>> = Vec::with_capacity(sources.len() as usize);
  for (index, package) in packages.iter().enumerate() {
    // The package source comes first, then the files in the order the map numbered them.
    entries.push(None);
    let first = sources.package_sources(index).start() + 1;
    for (offset, file) in package.files.iter().enumerate() {
      let source = SourceId::new(first + dense(offset));
      let document = if index < ROOT {
        parse(&built_in, file.text, source, reporter).0
      } else if supplied {
        let (document, tree) = parse(parser, file.text, source, reporter);
        if let (Some(kept), Some(tree)) = (trees.as_deref_mut(), tree) {
          kept.push((source, tree));
        }
        document
      } else {
        None
      };
      entries.push(document);
    }
  }
  Documents { entries }
}

/// Parses one file with `parser`, reporting its syntax errors at `source`: the file's document, and
/// what `parser` kept for it.
///
/// A blank text holds a document with no item, and a byte text that is not UTF-8 is read by the
/// syntactic layer over its bytes; neither reaches `parser`, and neither has a tree.
fn parse<'a, P>(
  parser: &P,
  text: View<'a>,
  source: SourceId,
  reporter: &mut Reporter,
) -> (Option<Parsed<'a>>, Option<P::Tree>)
where
  P: Layer<'a>,
{
  let text: &'a str = match text {
    View::Text(text) => text,
    View::Bytes(bytes) => match core::str::from_utf8(bytes) {
      Ok(text) => text,
      Err(_) => {
        return (
          parse_bytes(bytes, source, reporter).map(Parsed::Bytes),
          None,
        );
      }
    },
  };
  if blank(text.as_bytes()) {
    let document = TypeSystemDocument::<&'a str>::new(SimpleSpan::new(0, text.len()), Vec::new());
    return (Some(Parsed::Text(document)), None);
  }
  let mut sink = Syntax::new(reporter, source);
  match parser.type_system_document(text, &mut sink) {
    Ok(parsed) => {
      let (document, _, errors, tree) = parsed.into_components();
      sink.found(errors);
      (Some(Parsed::Text(document)), Some(tree))
    }
    Err(_) => {
      sink.unread();
      (None, None)
    }
  }
}

/// The type-system document the bytes hold, or `None` once their syntax errors are reported: the
/// syntactic layer over `[u8]`, under its default limits.
fn parse_bytes<'a>(
  bytes: &'a [u8],
  source: SourceId,
  reporter: &mut Reporter,
) -> Option<TypeSystemDocument<&'a [u8]>> {
  let mut sink = Syntax::new(reporter, source);
  match Syntactic::new().type_system_document(bytes, &mut sink) {
    Ok(parsed) => Some(parsed.into_components().0),
    Err(_) => {
      sink.unread();
      None
    }
  }
}

/// Where a layer lends a file's syntax errors: each one becomes a diagnostic at the file, in the
/// analysis's one list, and marks nothing.
struct Syntax<'r> {
  reporter: &'r mut Reporter,
  source: SourceId,
  /// How many errors the layer lent.
  lent: usize,
}

impl<'r> Syntax<'r> {
  /// The sink for the file `source`, reporting into `reporter`.
  fn new(reporter: &'r mut Reporter, source: SourceId) -> Self {
    Self {
      reporter,
      source,
      lent: 0,
    }
  }

  /// The layer answered no document for the file. When it lent no error either, reports at the
  /// whole file that the file could not be read as a document, so that a file with no document
  /// always has a diagnostic.
  fn unread(self) {
    if self.lent > 0 {
      return;
    }
    let kind = DiagnosticKind::ResourceLimit;
    self.reporter.report(
      Diagnostic::builder(kind.code(), Location::entire(self.source.get()))
        .severity(kind.severity())
        .message("the file could not be read as a document")
        .primary_label(kind.title())
        .help("supply the file in a form the parser can read: a shorter text")
        .build(),
    );
  }

  /// The layer answered a document for the file, and found `errors` syntax errors in it. Those it
  /// did not lend, which a layer that keeps a bounded number of records refuses past its ceiling,
  /// are counted as errors of the analysis, and they truncate its list.
  ///
  /// Exact because this sink never breaks: a layer lends it every error it kept, so `errors` less
  /// what it was lent is what the layer did not lend.
  fn found(self, errors: usize) {
    self.reporter.count_unlent(errors.saturating_sub(self.lent));
  }
}

/// The adapter is sized in `E`: it hands each error on as `&dyn Diagnose`, which is the one
/// conversion the analysis compiles, whatever the layer.
impl<E: Diagnose> Report<E> for Syntax<'_> {
  fn error(&mut self, error: &E) -> ControlFlow<()> {
    self.lent = self.lent.saturating_add(1);
    self.reporter.report_syntax(syntax(error, self.source));
    ControlFlow::Continue(())
  }
}

/// Whether `text` holds nothing but what the lexer skips: spaces, tabs, line terminators, commas,
/// byte order marks, and comments, each from its `#` to the end of its line.
///
/// It accepts only those bytes, so a text it calls blank is one the lexer reads as no token at
/// all; anything else, whatever it is, goes to the parser.
fn blank(text: &[u8]) -> bool {
  let mut rest = text;
  loop {
    match rest {
      [] => return true,
      [b' ' | b'\t' | b'\r' | b'\n' | b',', tail @ ..] => rest = tail,
      [0xEF, 0xBB, 0xBF, tail @ ..] => rest = tail,
      [b'#', tail @ ..] => {
        let end = tail
          .iter()
          .position(|byte: &u8| *byte == b'\n' || *byte == b'\r')
          .unwrap_or(tail.len());
        rest = &tail[end..];
      }
      _ => return false,
    }
  }
}

/// A parser's syntax error as an analysis diagnostic at `source`: the parser's own code,
/// severity, message, primary label, labels and help, with every location moved from the one
/// input the parser saw to `source`.
fn syntax(error: &dyn Diagnose, source: SourceId) -> Diagnostic {
  let mut builder = Diagnostic::builder(error.code(), relocate(error.primary(), source))
    .severity(error.severity())
    .message(error.to_string());
  if let Some(text) = error.primary_label() {
    builder = builder.primary_label(String::from(text));
  }
  for label in error.labels_iter() {
    builder = builder.label(
      relocate(label.location(), source),
      String::from(label.text()),
    );
  }
  if let Some(text) = error.help() {
    builder = builder.help(String::from(text));
  }
  builder.build()
}

/// `location`, in `source` rather than in the parser's single input.
fn relocate(location: Location, source: SourceId) -> Location {
  match location.span() {
    Some(span) => Location::new(source.get(), span),
    None => Location::entire(source.get()),
  }
}

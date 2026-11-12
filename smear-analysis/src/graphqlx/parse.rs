//! Parsing: every file, with the GraphQLx syntactic parser, into the documents the model is lowered
//! from.
//!
//! The caller's text type is gone before this module is reached: a file is a `View`, text or
//! bytes. A byte text that is UTF-8 is parsed as text, so only two parsers are compiled, one over
//! `str` and one over `[u8]`, and the second only ever reads bytes that are not UTF-8.
//!
//! A file that holds nothing but what the lexer skips, white space, commas, byte order marks and
//! comments, is accepted here as a document with no item, before the parser sees it: the
//! type-system document production wants one item at least, and a directory may hold a file that
//! only documents it.

use std::{
  string::{String, ToString},
  vec::Vec,
};

use smear_parser::graphqlx::{
  GraphQLx,
  ast::TypeSystemDocument,
  error::GraphqlxErrors,
  syntactic::{GraphqlxLexer, type_system_document},
};
use tokora::{
  Parse as _, Parser, SimpleSpan,
  diagnostic::{Diagnose, DiagnoseExt, Location},
};

use super::{
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
/// The built-in packages' files are always parsed. The supplied packages' files are parsed only
/// when `supplied` is true: past the source size limit, none is.
pub(crate) fn files<'a>(
  packages: &[PackageView<'a>],
  sources: &SourceMap,
  supplied: bool,
  reporter: &mut Reporter,
) -> Documents<'a> {
  let mut entries: Vec<Option<Parsed<'a>>> = Vec::with_capacity(sources.len() as usize);
  for (index, package) in packages.iter().enumerate() {
    // The package source comes first, then the files in the order the map numbered them.
    entries.push(None);
    let first = sources.package_sources(index).start() + 1;
    for (offset, file) in package.files.iter().enumerate() {
      let source = SourceId::new(first + dense(offset));
      let document = if index < ROOT || supplied {
        parse(file.text, source, reporter)
      } else {
        None
      };
      entries.push(document);
    }
  }
  Documents { entries }
}

/// Parses one file, reporting its syntax errors at `source`.
fn parse<'a>(text: View<'a>, source: SourceId, reporter: &mut Reporter) -> Option<Parsed<'a>> {
  match text {
    View::Text(text) => parse_text(text, source, reporter).map(Parsed::Text),
    View::Bytes(bytes) => match core::str::from_utf8(bytes) {
      Ok(text) => parse_text(text, source, reporter).map(Parsed::Text),
      Err(_) => parse_bytes(bytes, source, reporter).map(Parsed::Bytes),
    },
  }
}

/// The type-system document `text` holds, or `None` once its syntax errors are reported. A blank
/// text holds a document with no item.
fn parse_text<'a>(
  text: &'a str,
  source: SourceId,
  reporter: &mut Reporter,
) -> Option<TypeSystemDocument<&'a str>> {
  if blank(text.as_bytes()) {
    return Some(TypeSystemDocument::<&'a str>::new(
      SimpleSpan::new(0, text.len()),
      Vec::new(),
    ));
  }
  let parsed: Result<TypeSystemDocument<&'a str>, GraphqlxErrors<&'a str>> =
    Parser::with_parser::<
      '_,
      GraphqlxLexer<'_, str>,
      TypeSystemDocument<&str>,
      GraphqlxErrors<&str>,
      _,
      GraphQLx,
    >(type_system_document)
    .parse_str(text);
  match parsed {
    Ok(document) => Some(document),
    Err(errors) => {
      for error in errors.iter() {
        reporter.report(syntax(error, source));
      }
      None
    }
  }
}

/// The type-system document the bytes hold, or `None` once their syntax errors are reported.
fn parse_bytes<'a>(
  bytes: &'a [u8],
  source: SourceId,
  reporter: &mut Reporter,
) -> Option<TypeSystemDocument<&'a [u8]>> {
  let parsed: Result<TypeSystemDocument<&'a [u8]>, GraphqlxErrors<&'a [u8]>> =
    Parser::with_parser::<
      '_,
      GraphqlxLexer<'_, [u8]>,
      TypeSystemDocument<&[u8]>,
      GraphqlxErrors<&[u8]>,
      _,
      GraphQLx,
    >(type_system_document)
    .parse_slice(bytes);
  match parsed {
    Ok(document) => Some(document),
    Err(errors) => {
      for error in errors.iter() {
        reporter.report(syntax(error, source));
      }
      None
    }
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

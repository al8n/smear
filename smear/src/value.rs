use std::fmt::Display;

use apollo_parser::ast::{AstNode, Value};
use codespan_reporting::diagnostic::{Diagnostic as CodespanDiagnostic, Label};

use crate::{Diagnostic, Diagnosticable, Reporter};

pub trait DiagnosticableValue: Diagnosticable<Node = Value, Error = ParseValueError> {}

impl<T: DiagnosticableValue> DiagnosticableValue for Vec<T> {}

pub enum ParseValueError {
  ParseError(Box<dyn Display>, Box<dyn AstNode>),
  UnexpectedValue(Value),
  Multiple(Vec<ParseValueError>),
}

impl Reporter for ParseValueError {
  fn report<'a, FileId>(&self, file_id: FileId) -> Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    match self {
      Self::ParseError(err, val) => {
        let syn = val.syntax();
        let range = syn.text_range();
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        CodespanDiagnostic::error()
          .with_message(err.to_string())
          .with_labels(vec![
            Label::primary(file_id, start..end).with_message(syn.text())
          ])
          .into()
      }
      Self::UnexpectedValue(val) => {
        let syn = val.syntax();
        let range = syn.text_range();
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        CodespanDiagnostic::error()
          .with_message("unexpected value")
          .with_labels(vec![
            Label::primary(file_id, start..end).with_message(syn.text())
          ])
          .into()
      }
      Self::Multiple(errors) => errors
        .iter()
        .map(|err| err.report(file_id))
        .collect::<Vec<_>>()
        .into(),
    }
  }
}

mod impls;
pub use impls::*;

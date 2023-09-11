use rowan::TextRange;

use crate::Reporter;

use super::Style;

pub enum ErrorKind {
  MissingDirective,
  UnknownDirective {
    name: String,
    did_you_mean: Option<String>,
    available_names: &'static [&'static str],
  },
  Value(crate::error::ValueError),
}

pub struct Error {
  kind: ErrorKind,
  style: Style,
  range: TextRange,
}

impl Reporter for Error {
  fn report<'a, FileId>(&self, file_id: FileId) -> crate::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    todo!()
  }
}
use std::fmt::Display;

use apollo_parser::ast::AstNode;
use rowan::TextRange;

use crate::Reporter;

use super::{Style, ValueError};

pub enum ErrorKind {
  MissingArgumentName,
  MissingArgumentValue {
    name: String,
    message: Option<String>,
  },
  MissingArgument(String),
  UnknownArgument {
    name: String,
    did_you_mean: Option<String>,
    available_names: &'static [&'static str],
  },
  Value(crate::error::ValueError),
  Invalid,
}

pub struct Error {
  kind: ErrorKind,
  style: Style,
  range: TextRange,
}

impl Error {
  pub fn missing_argument_name<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::MissingArgumentName,
      style: Style::Error,
      range: node.syntax().text_range()
    }
  }

  pub fn missing_argument_value<T: AstNode>(node: &T, name: impl Display, message: Option<impl Display>) -> Self {
    Self {
      kind: ErrorKind::MissingArgumentValue {
        name: name.to_string(),
        message: message.map(|m| m.to_string()),
      },
      style: Style::Error,
      range: node.syntax().text_range()
    }
  }

  pub fn unknown_argument_value<T: AstNode>(node: &T, name: impl Display, alts: &'static [&'static str]) -> Self {
    let name = name.to_string();
    Self {
      kind: ErrorKind::UnknownArgument {
        did_you_mean: crate::utils::did_you_mean(&name, alts),
        name: name.to_string(),
        available_names: alts,
      },
      style: Style::Error,
      range: node.syntax().text_range()
    }
  }

  pub fn invalid_value<T: AstNode>(node: &T, err: ValueError) -> Self {
    Self {
      kind: ErrorKind::Value(err),
      style: Style::Error,
      range: node.syntax().text_range()
    }
  }

  #[cold]
  pub fn invalid<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::Invalid,
      style: Style::Error,
      range: node.syntax().text_range()
    }
  }
}

impl Reporter for Error {
  fn report<'a, FileId>(&self, file_id: FileId) -> crate::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    todo!()
  }
}
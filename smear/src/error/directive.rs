use std::fmt::{self, Display};

use apollo_parser::ast::AstNode;
use rowan::TextRange;

use crate::Reporter;

use super::{Style, ValueError};

#[derive(Debug)]
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
  Multiple(Vec<Error>),
  Invalid,
}

impl ErrorKind {
  pub fn brief(&self) -> String {
    match self {
      Self::MissingArgumentName => "Missing argument name".into(),
      Self::MissingArgumentValue { .. } => "Missing argument value".into(),
      Self::MissingArgument(_) => "Missing argument".into(),
      Self::UnknownArgument { .. } => "Unknown argument".into(),
      Self::Value { .. } => "Invalid value".into(),
      Self::Multiple(_) => "Multiple errors".into(),
      Self::Invalid => "Invalid argument".into(),
    }
  }

  /// Deeply counts the number of errors this item represents.
  pub fn len(&self) -> usize {
    if let ErrorKind::Multiple(ref items) = *self {
      items.iter().map(Error::len).sum()
    } else {
      1
    }
  }
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::MissingArgumentName => write!(f, "Missing argument name"),
      Self::MissingArgumentValue { name, message } => match message {
        Some(msg) if !msg.is_empty() => write!(f, "Missing argument value for `{name}`: {msg}",),
        _ => write!(f, "Missing argument value for `{name}`"),
      },
      Self::MissingArgument(name) => {
        write!(f, "Missing argument `{}`", name)
      }
      Self::UnknownArgument {
        name,
        did_you_mean,
        available_names,
      } => {
        if let Some(did_you_mean) = did_you_mean {
          write!(
            f,
            "Unknown argument `{name}`, did you mean `{did_you_mean}`?",
          )
        } else {
          write!(
            f,
            "Unknown argument `{name}`, available arguments are: {}",
            available_names.join(", ")
          )
        }
      }
      Self::Value(e) => write!(f, "{e}"),
      Self::Invalid => write!(
        f,
        "Invalid argument, argument is missing both name and value"
      ),
      Self::Multiple(ref items) if items.len() == 1 => items[0].fmt(f),
      Self::Multiple(ref items) => {
        write!(f, "Multiple errors: (")?;
        let mut first = true;
        for item in items {
          if !first {
            write!(f, ", ")?;
          } else {
            first = false;
          }

          item.fmt(f)?;
        }

        write!(f, ")")
      }
    }
  }
}

pub struct Error {
  kind: ErrorKind,
  style: Style,
  range: TextRange,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)
  }
}

impl fmt::Debug for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{self}")
  }
}

impl std::error::Error for Error {}

impl Error {
  pub fn missing_argument_name<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::MissingArgumentName,
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn missing_argument_value<T: AstNode>(
    node: &T,
    name: impl Display,
    message: Option<impl Display>,
  ) -> Self {
    Self {
      kind: ErrorKind::MissingArgumentValue {
        name: name.to_string(),
        message: message.map(|m| m.to_string()),
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn unknown_argument_value<T: AstNode>(
    node: &T,
    name: impl Display,
    alts: &'static [&'static str],
  ) -> Self {
    let name = name.to_string();
    Self {
      kind: ErrorKind::UnknownArgument {
        did_you_mean: crate::utils::did_you_mean(&name, alts),
        name: name.to_string(),
        available_names: alts,
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn invalid_value<T: AstNode>(node: &T, err: ValueError) -> Self {
    let r = node.syntax().text_range();
    Self {
      kind: ErrorKind::Value(err),
      style: Style::Error,
      range: r,
    }
  }

  #[cold]
  pub fn invalid<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::Invalid,
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn len(&self) -> usize {
    self.kind.len()
  }
}

impl Reporter for Error {
  fn report<'a, FileId>(&self, file_id: FileId) -> crate::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    match &self.kind {
      ErrorKind::Value(e) => {
        let start = self.range.start().into();
        let end = self.range.end().into();

        let err_start = e.range().start().into();
        let err_end = e.range().end().into();
        std::iter::once(
          self
            .style
            .diagnostic()
            .with_message(self.kind.brief())
            .with_labels(vec![
              codespan_reporting::diagnostic::Label::primary(file_id, err_start..err_end),
              codespan_reporting::diagnostic::Label::secondary(file_id, start..end),
            ])
            .with_message(self.kind.to_string())
            .into(),
        )
        .chain(e.to_errors().into_iter().map(|e| e.report(file_id)))
        .collect::<Vec<_>>()
        .into()
      }
      ErrorKind::Multiple(ref items) if self.len() == 1 => items[0].report(file_id),
      ErrorKind::Multiple(ref items) => {
        let mut diags = Vec::with_capacity(items.len());
        for item in items {
          diags.push(item.report(file_id));
        }

        diags.into()
      }
      _ => self
        .style
        .diagnostic()
        .with_message(self.kind.brief())
        .with_labels(vec![codespan_reporting::diagnostic::Label::primary(
          file_id, self.range,
        )])
        .with_message(self.kind.to_string())
        .into(),
    }
  }
}

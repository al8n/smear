use std::fmt::{self, Display};

use apollo_parser::ast::AstNode;
use rowan::TextRange;

use crate::Reporter;

use super::{Style, ValueError};

#[derive(Debug)]
pub enum ErrorKind {
  MissingArgumentName,
  MissingRequiredArguments {
    directive_name: String,
    arguments: Vec<&'static str>,
  },
  DuplicatedArgument {
    directive_name: String,
    argument_name: String,
  },
  UnknownArgument {
    directive_name: String,
    name: String,
    did_you_mean: Option<String>,
    available_names: &'static [&'static str],
  },
  Value(crate::error::ValueError),
  Multiple(Vec<Error>),
  InvalidArgument,
}

impl ErrorKind {
  pub fn brief(&self) -> String {
    match self {
      Self::MissingArgumentName => "Missing argument name".into(),
      Self::MissingRequiredArguments { .. } => "Missing required argument".into(),
      Self::UnknownArgument { .. } => "Unknown argument".into(),
      Self::Value { .. } => "Invalid value".into(),
      Self::Multiple(_) => "Multiple errors".into(),
      Self::InvalidArgument => "Invalid argument".into(),
      Self::DuplicatedArgument { .. } => "Duplicated argument".into(),
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
      Self::MissingRequiredArguments {
        directive_name,
        arguments,
      } => write!(
        f,
        "Missing required argument `[{}]` in directive `@{directive_name}`",
        arguments.join(", ")
      ),
      Self::DuplicatedArgument {
        directive_name,
        argument_name,
      } => write!(
        f,
        "Duplicated argument `@{argument_name}` in directive `@{directive_name}`",
      ),
      Self::UnknownArgument {
        directive_name,
        name,
        did_you_mean,
        available_names,
      } => {
        if let Some(did_you_mean) = did_you_mean {
          write!(
            f,
            "Unknown argument `@{name}` in directive `@{directive_name}`, did you mean `@{did_you_mean}`?",
          )
        } else {
          write!(
            f,
            "Unknown argument `@{name}` in directive `@{directive_name}`, available arguments are: {}",
            available_names.join(", ")
          )
        }
      }
      Self::Value(e) => write!(f, "{e}"),
      Self::InvalidArgument => write!(
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

  pub fn unknown_argument<T: AstNode>(
    node: &T,
    directive_name: impl Display,
    name: impl Display,
    alts: &'static [&'static str],
  ) -> Self {
    let name = name.to_string();
    Self {
      kind: ErrorKind::UnknownArgument {
        directive_name: directive_name.to_string(),
        did_you_mean: crate::utils::did_you_mean(&name, alts),
        name: name.to_string(),
        available_names: alts,
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn duplicated_argument<T: AstNode>(
    node: &T,
    directive_name: impl Display,
    argument_name: impl Display,
  ) -> Self {
    Self {
      kind: ErrorKind::DuplicatedArgument {
        directive_name: directive_name.to_string(),
        argument_name: argument_name.to_string(),
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn missing_arguments<T: AstNode>(
    node: &T,
    directive_name: impl Display,
    arguments: Vec<&'static str>,
  ) -> Self {
    Self {
      kind: ErrorKind::MissingRequiredArguments {
        directive_name: directive_name.to_string(),
        arguments,
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn multiple<T: AstNode>(node: &T, errors: Vec<Error>) -> Self {
    Self {
      kind: ErrorKind::Multiple(errors),
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn invalid_argument_value<T: AstNode>(node: &T, err: ValueError) -> Self {
    let r = node.syntax().text_range();
    Self {
      kind: ErrorKind::Value(err),
      style: Style::Error,
      range: r,
    }
  }

  #[cold]
  pub fn invalid_argument<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::InvalidArgument,
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

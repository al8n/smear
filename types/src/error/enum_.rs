use std::fmt::{self, Display};

use apollo_parser::ast::AstNode;
use rowan::TextRange;

use crate::{Reporter, directive::DirectiveDescriptor};

use super::{Style, ValueError};

#[derive(Debug)]
pub enum ErrorKind {
  MissingName,
  MissingRequiredDirectives {
    enum_name: String,
    directives: Vec<&'static str>,
  },
  MissingDirectiveName,
  MismatchDirective {
    expected: &'static [&'static str],
    got: String,
  },
  DuplicatedDirective {
    source_name: String,
    directive_name: String,
  },
  UnknownDirective {
    directive_name: String,
    name: String,
    did_you_mean: Option<String>,
    available_names: &'static [&'static str],
  },
  UnknownEnumValue {
    name: String,
    did_you_mean: Option<String>,
    available_names: &'static [&'static str],
  },
  Value(crate::error::ValueError),
  Directive(crate::error::DirectiveError),
  Multiple(Vec<Error>),
  InvalidDirective,
  MissingEnumValue,
}

impl ErrorKind {
  pub fn brief(&self) -> String {
    match self {
      Self::MissingName => "Missing name".into(),
      Self::MissingDirectiveName => "Missing directive name".into(),
      Self::MissingRequiredDirectives { .. } => "Missing required argument".into(),
      Self::MissingEnumValue => "Missing enum value".into(),
      Self::MismatchDirective { .. } => "Mismatch directive".into(),
      Self::UnknownDirective { .. } => "Unknown directive".into(),
      Self::UnknownEnumValue { .. } => "Unknown enum value".into(),
      Self::Value { .. } => "Invalid value".into(),
      Self::Directive { .. } => "Invalid directive".into(),
      Self::Multiple(_) => "Multiple errors".into(),
      Self::InvalidDirective => "Invalid argument".into(),
      Self::DuplicatedDirective { .. } => "Duplicated argument".into(),
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
      Self::MissingName => write!(f, "Missing name, anonymous enum are not supported"),
      Self::MissingDirectiveName => write!(f, "Missing directive name, anonymous directive are not supported"),
      Self::MissingRequiredDirectives {
        enum_name,
        directives,
      } => write!(
        f,
        "Missing required directives `[{}]` in enum `{enum_name}`",
        directives.join(", ")
      ),
      Self::MissingEnumValue => write!(f, "Missing enum value"),
      Self::MismatchDirective { expected, got } => write!(f, "Expected one of [{}], but got `@{got}`", expected.join(", ")),
      Self::DuplicatedDirective {
        directive_name,
        source_name,
      } => write!(
        f,
        "Duplicated directive `@{directive_name}` in enum `@{source_name}`",
      ),
      Self::UnknownDirective {
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
      Self::UnknownEnumValue { name, did_you_mean, available_names } => {
        if let Some(did_you_mean) = did_you_mean {
          write!(
            f,
            "Unknown enum value `{name}`, did you mean `{did_you_mean}`?",
          )
        } else {
          write!(
            f,
            "Unknown enum value `{name}`, available values are: {}",
            available_names.join(", ")
          )
        }
      },
      Self::Value(e) => write!(f, "{e}"),
      Self::Directive(e) => write!(f, "{e}"),
      Self::InvalidDirective => write!(
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
  pub fn missing_directive_name<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::MissingDirectiveName,
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn mismatch_directive<T: AstNode>(node: &T, expected: &'static [&'static str], got: String) -> Self {
    Self {
      kind: ErrorKind::MismatchDirective { expected, got },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn missing_name<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::MissingName,
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn unknown_directive<T: AstNode>(
    node: &T,
    directive_name: impl Display,
    name: impl Display,
    alts: &'static [&'static str],
  ) -> Self {
    let name = name.to_string();
    Self {
      kind: ErrorKind::UnknownDirective {
        directive_name: directive_name.to_string(),
        did_you_mean: crate::utils::did_you_mean(&name, alts),
        name: name.to_string(),
        available_names: alts,
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn unknown_enum_value<T: AstNode>(
    node: &T,
    name: impl Display,
    alts: &'static [&'static str],
  ) -> Self {
    let name = name.to_string();
    Self {
      kind: ErrorKind::UnknownEnumValue {
        did_you_mean: crate::utils::did_you_mean(&name, alts),
        name: name.to_string(),
        available_names: alts,
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn duplicated_directive<T: AstNode>(
    node: &T,
    source_name: impl Display,
    directive_name: impl Display,
  ) -> Self {
    Self {
      kind: ErrorKind::DuplicatedDirective {
        source_name: source_name.to_string(),
        directive_name: directive_name.to_string(),
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn missing_directives<T: AstNode>(
    node: &T,
    enum_name: impl Display,
    directives: impl IntoIterator<Item = &'static DirectiveDescriptor>,
  ) -> Self {
    Self {
      kind: ErrorKind::MissingRequiredDirectives {
        enum_name: enum_name.to_string(),
        directives: directives.into_iter().map(|d| d.name()).collect(),
      },
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }

  pub fn missing_enum_value<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::MissingEnumValue,
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

  pub fn invalid_directive<T: AstNode>(node: &T, err: crate::error::DirectiveError) -> Self {
    let r = node.syntax().text_range();
    Self {
      kind: ErrorKind::Directive(err),
      style: Style::Error,
      range: r,
    }
  }

  #[cold]
  pub fn invalid_argument<T: AstNode>(node: &T) -> Self {
    Self {
      kind: ErrorKind::InvalidDirective,
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

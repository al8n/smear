use core::fmt;
use std::fmt::Display;

use apollo_parser::ast::{AstNode, Value};
use codespan_reporting::diagnostic::Label;
use rowan::TextRange;

mod kind;
pub use kind::{ErrorKind, ErrorUnknownObjectField};

use crate::Reporter;

use super::Style;

#[derive(Clone)]
pub struct Error {
  kind: ErrorKind,
  style: Style,
  range: TextRange,
}

impl Error {
  pub fn new(kind: ErrorKind, style: Style, range: TextRange) -> Self {
    Self { kind, style, range }
  }

  pub fn kind(&self) -> &ErrorKind {
    &self.kind
  }

  pub fn style(&self) -> &Style {
    &self.style
  }

  pub fn range(&self) -> TextRange {
    self.range
  }

  pub fn len(&self) -> usize {
    self.kind.len()
  }

  pub(crate) fn to_errors(&self) -> Vec<Error> {
    match &self.kind {
      ErrorKind::Multiple(errors) => errors.clone(),
      _ => vec![self.clone()],
    }
  }

  pub fn invalid_value<T: AstNode>(node: &T, msg: impl core::fmt::Display) -> Self {
    Self::new(
      ErrorKind::InvalidValue(msg.to_string()),
      Style::Error,
      node.syntax().text_range(),
    )
  }

  pub fn missing_object_field<T: AstNode>(node: &T, msg: impl core::fmt::Display) -> Self {
    Self::new(
      // TODO: add some fields to this error kind
      ErrorKind::MissingObjectField(msg.to_string()),
      Style::Error,
      node.syntax().text_range(),
    )
  }

  pub fn missing_object_field_name<T: AstNode>(node: &T) -> Self {
    Self::new(
      ErrorKind::MissingObjectFieldName,
      Style::Error,
      node.syntax().text_range(),
    )
  }

  pub fn missing_object_value<T: AstNode>(node: &T, msg: impl core::fmt::Display) -> Self {
    Self::new(
      ErrorKind::MissingObjectValue(msg.to_string()),
      Style::Error,
      node.syntax().text_range(),
    )
  }

  pub fn duplicate_object_field<T: AstNode>(node: &T, msg: impl core::fmt::Display) -> Self {
    Self::new(
      ErrorKind::DuplicateField(msg.to_string()),
      Style::Error,
      node.syntax().text_range(),
    )
  }

  pub fn unknown_object_field<T: AstNode>(node: &T, err: ErrorUnknownObjectField) -> Self {
    Self::new(
      ErrorKind::UnknownObjectField(err),
      Style::Error,
      node.syntax().text_range(),
    )
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

  pub fn unexpected_type(val: &Value) -> Self {
    let r = val.syntax().text_range();
    let val_ty = match &val {
      Value::Variable(_) => "variable",
      Value::StringValue(_) => "string",
      Value::FloatValue(_) => "float",
      Value::IntValue(_) => "int",
      Value::BooleanValue(_) => "boolean",
      Value::NullValue(_) => "null",
      Value::EnumValue(_) => "enum",
      Value::ListValue(_) => "list",
      Value::ObjectValue(_) => "object",
    };

    Self::new(
      ErrorKind::UnexpectedType(format!("Unexpected {} value", val_ty)),
      Style::Error,
      r,
    )
  }

  pub fn multiple<N: AstNode>(node: &N, errors: Vec<Self>) -> Self {
    Self {
      kind: ErrorKind::Multiple(errors),
      style: Style::Error,
      range: node.syntax().text_range(),
    }
  }
}

impl core::fmt::Display for Error {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    write!(f, "{}", self.kind)
  }
}

impl core::fmt::Debug for Error {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    write!(f, "{self}")
  }
}

impl Reporter for Error {
  fn report<'a, FileId>(&self, file_id: FileId) -> crate::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    let Self { kind, style, range } = self;

    let start: usize = range.start().into();
    let end: usize = range.end().into();

    match kind {
      ErrorKind::Custom(msg)
      | ErrorKind::DuplicateField(msg)
      | ErrorKind::MissingObjectField(msg)
      | ErrorKind::MissingObjectValue(msg)
      | ErrorKind::UnexpectedType(msg)
      | ErrorKind::InvalidValue(msg) => style
        .diagnostic()
        .with_message(kind.description())
        .with_labels(vec![Label::primary(file_id, start..end).with_message(msg)])
        .into(),
      ErrorKind::MissingObjectFieldName => style
        .diagnostic()
        .with_message(kind.description())
        .with_labels(vec![Label::primary(file_id, start..end)])
        .into(),
      ErrorKind::UnknownObjectField(msg) => {
        // TODO: optimize diagnostic report here
        style
          .diagnostic()
          .with_message(kind.description())
          .with_labels(vec![
            Label::primary(file_id, start..end).with_message(msg.name.clone()),
            match &msg.did_you_mean {
              Some(suggestion) => {
                Label::secondary(file_id, start..end).with_message(suggestion.clone())
              }
              None => Label::secondary(file_id, start..end).with_message("unknown field"),
            },
          ])
          .into()
      }
      ErrorKind::UnknownEnumValue { .. } => {
        style
        .diagnostic()
        .with_message(kind.description())
        .with_labels(vec![
          Label::primary(file_id, start..end).with_message(kind.to_string()),
        ])
        .into() 
      },
      ErrorKind::Multiple(errors) => errors
        .iter()
        .map(|err| err.report(file_id))
        .collect::<Vec<_>>()
        .into(),
    }
  }
}

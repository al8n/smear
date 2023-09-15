use apollo_parser::ast::{AstNode, Value};

use crate::{error::ValueError, Diagnosticable};

pub trait DiagnosticableValue:
  Diagnosticable<Node = Value, Error = ValueError, Descriptor = ValueDescriptor>
{
  fn parse_with_default(node: &Self::Node, default: Self) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    match node {
      Value::NullValue(_) => Ok(default),
      val => Self::parse(val),
    }
  }

  fn parse_nullable(node: &Self::Node) -> Result<Option<Self>, Self::Error>
  where
    Self: Sized,
  {
    match node {
      Value::NullValue(_) => Ok(None),
      val => Self::parse(val).map(Some),
    }
  }
}

pub trait Parser {
  fn parse_value(val: &Value) -> Result<Self, ValueError>
  where
    Self: Sized;

  fn parse_value_with_default(val: &Value, default: Self) -> Result<Self, ValueError>
  where
    Self: Sized,
  {
    match val {
      Value::NullValue(_) => Ok(default),
      val => Self::parse_value(val),
    }
  }

  fn parse_value_nullable(val: &Value) -> Result<Option<Self>, ValueError>
  where
    Self: Sized,
  {
    match val {
      Value::NullValue(_) => Ok(None),
      val => Self::parse_value(val).map(Some),
    }
  }
}

impl<T: DiagnosticableValue> Parser for T {
  fn parse_value(val: &Value) -> Result<Self, ValueError> {
    Self::parse(val)
  }
}

pub trait DiagnosticableObjectValue: DiagnosticableValue {
  fn fields() -> &'static [&'static str];

  fn did_you_mean(src: &str) -> Option<String> {
    crate::utils::did_you_mean(src, Self::fields())
  }
}

mod impls;
pub use impls::*;

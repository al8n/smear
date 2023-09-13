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

pub trait DiagnosticableObjectValue: DiagnosticableValue {
  fn fields() -> &'static [&'static str];

  fn did_you_mean(src: &str) -> Option<String> {
    crate::utils::did_you_mean(src, Self::fields())
  }
}

pub enum ValueKind {
  Scalar,
  Object(&'static [(&'static str, &'static ValueDescriptor)]),
  List(&'static ValueDescriptor),
  Optional(&'static ValueDescriptor),
  Map {
    key: &'static ValueDescriptor,
    value: &'static ValueDescriptor,
  },
  Set(&'static ValueDescriptor),
}

#[viewit::viewit(setters(skip))]
pub struct ValueDescriptor {
  name: &'static str,
  kind: &'static ValueKind,
}

mod impls;
pub use impls::*;

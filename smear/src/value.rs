use apollo_parser::ast::{AstNode, Value};

use crate::{Diagnosticable, error::ValueError};

pub trait DiagnosticableValue: Diagnosticable<Node = Value, Error = ValueError> {
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

// impl<V: DiagnosticableValue> DiagnosticableValue for Option<V> {
//   fn parse_with_default(node: &Self::Node, default: Self) -> Result<Self, Self::Error>
//   where
//     Self: Sized,
//   {
//     match node {
//       Value::NullValue(_) => Ok(default),
//       val => V::parse(val).map(Some),
//     }
//   }

//   fn parse_nullable(node: &Self::Node) -> Result<Option<Self>, Self::Error>
//   where
//     Self: Sized,
//   {
//     match node {
//       Value::NullValue(_) => Ok(None),
//       val => Option::<V>::parse(val).map(Some),
//     }
//   }
// }

pub trait DiagnosticableObjectValue: DiagnosticableValue {
  fn fields() -> &'static [&'static str];

  fn did_you_mean(src: &str) -> Option<String> {
    crate::utils::did_you_mean(src, Self::fields())
  }
}

mod impls;
pub use impls::*;

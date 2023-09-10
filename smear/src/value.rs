use apollo_parser::ast::{AstNode, Value};

use crate::Diagnosticable;

pub trait DiagnosticableValue: Diagnosticable<Node = Value, Error = Error> {
  fn nullable() -> bool {
    false
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

#[allow(clippy::len_without_is_empty)]
mod error;
pub use error::*;

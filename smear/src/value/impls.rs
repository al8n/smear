use super::*;
use core::fmt::Display;

macro_rules! impl_parse_from_str {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      paste::paste!{
        pub fn [<$parser>] (value: &apollo_parser::ast::Value) -> Result<$ty, crate::value::Error> {
          match value {
            apollo_parser::ast::Value::StringValue(val) => {
              let s: String = val.clone().into();
              s.parse()
                .map_err(|e| crate::value::Error::invalid_value(val, e))
            }
            val => Err(crate::value::Error::unexpected_type(val)),
          }
        }
      }
    )*
  };
}

macro_rules! impl_diagnostic_inner {
  ($ty:ident::$parser:ident) => {
    impl crate::Diagnosticable for $ty {
      type Error = crate::value::Error;

      type Node = apollo_parser::ast::Value;

      fn parse(node: &Self::Node) -> Result<Self, Self::Error>
      where
        Self: Sized,
      {
        $parser(node)
      }
    }

    impl crate::value::DiagnosticableValue for $ty {}
  };
}

macro_rules! impl_diagnostic {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      impl_diagnostic_inner!($ty::$parser);
    )*
  };
  (string($($ty:ident::$parser:ident ?), + $(,)?)) => {
    $(
      impl_parse_from_str!($ty::$parser);

      impl_diagnostic!($ty::$parser);
    )*
  }
}

impl_diagnostic!(
  u8::parse_number,
  u16::parse_number,
  u32::parse_number,
  u64::parse_number,
  u128::parse_number,
  usize::parse_number,
  i8::parse_number,
  i16::parse_number,
  i32::parse_number,
  i64::parse_number,
  i128::parse_number,
  f32::parse_float,
  f64::parse_float,
);

impl_diagnostic!(char::parse_char, bool::parse_boolean, String::parse_string,);

mod external;
pub use external::*;
mod builtin;
pub use builtin::*;

impl<T: DiagnosticableValue> DiagnosticableValue for Vec<T> {}

impl<T: DiagnosticableValue> DiagnosticableValue for Option<T> {
  fn nullable() -> bool {
    true
  }
}

impl<T: DiagnosticableValue> Diagnosticable for Option<T> {
  type Error = Error;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    match node {
      Value::NullValue(_) => Ok(None),
      val => <T as Diagnosticable>::parse(val).map(Some),
    }
  }
}

impl<O: DiagnosticableObjectValue> DiagnosticableObjectValue for Vec<O> {
  fn fields() -> &'static [&'static str] {
    O::fields()
  }
}

impl<O: DiagnosticableObjectValue> DiagnosticableObjectValue for Option<O> {
  fn fields() -> &'static [&'static str] {
    O::fields()
  }
}

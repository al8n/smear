use super::*;

macro_rules! impl_parse_from_str {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      paste::paste!{
        pub fn [<$parser>] (value: apollo_parser::ast::Value) -> Result<$ty, crate::value::ParseValueError> {
          match value {
            apollo_parser::ast::Value::StringValue(val) => {
              let s: String = val.clone().into();
              s.parse()
                .map_err(|e| crate::value::ParseValueError::ParseError(Box::new(e), Box::new(apollo_parser::ast::Value::StringValue(val))))
            }
            val => Err(crate::value::ParseValueError::UnexpectedValue(val)),
          }
        }
      }
    )*
  };
}

macro_rules! impl_diagnostic_inner {
  ($ty:ident::$parser:ident) => {
    impl crate::Diagnosticable for $ty {
      type Error = crate::value::ParseValueError;

      type Node = apollo_parser::ast::Value;

      fn parse(node: Self::Node) -> Result<Self, Self::Error>
      where
        Self: Sized,
      {
        $parser(node)
      }
    }

    impl crate::Diagnosticable for Option<$ty> {
      type Error = crate::value::ParseValueError;

      type Node = apollo_parser::ast::Value;

      fn parse(node: Self::Node) -> Result<Self, Self::Error>
      where
        Self: Sized,
      {
        paste::paste! {
          [<$parser _ optional>] (node)
        }
      }
    }

    impl crate::value::DiagnosticableValue for $ty {}

    impl crate::value::DiagnosticableValue for Option<$ty> {}
  };
}

macro_rules! impl_diagnostic {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      impl_diagnostic_inner!($ty::$parser);
    )*
  };
  ($($ty:ident::$parser:ident ?), + $(,)?) => {
    $(
      paste::paste! {
        pub fn [<$parser _ optional>] (src: apollo_parser::ast::Value) -> Result<Option<$ty>, crate::value::ParseValueError> {
          match src {
            apollo_parser::ast::Value::NullValue(_) => Ok(None),
            val => [<$parser>](val).map(Some),
          }
        }
      }

      impl_diagnostic_inner!($ty::$parser);
    )*
  };
  (string($($ty:ident::$parser:ident ?), + $(,)?)) => {
    $(
      impl_parse_from_str!($ty::$parser);

      impl_diagnostic!($ty::$parser?);
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

impl_diagnostic!(
  char::parse_char?,
  bool::parse_boolean?,
  String::parse_string?,
);

mod external;
pub use external::*;
mod builtin;
pub use builtin::*;

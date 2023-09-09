use std::{
  char::ParseCharError,
  fmt::Display,
  num::{ParseFloatError, ParseIntError},
  str::ParseBoolError,
  string::ParseError,
};

use apollo_parser::ast::{AstNode, Value};
use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{Diagnosticable, Reporter};

pub fn parse_boolean(val: Value) -> Result<bool, ParseValueError<ParseBoolError>> {
  match val {
    Value::BooleanValue(val) => Ok(val.true_token().is_some()),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_boolean_optional(val: Value) -> Result<Option<bool>, ParseValueError<ParseBoolError>> {
  match val {
    Value::NullValue(_) => Ok(None),
    Value::BooleanValue(val) => Ok(Some(val.true_token().is_some())),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_string(val: Value) -> Result<String, ParseValueError<ParseError>> {
  match val {
    Value::StringValue(val) => Ok(val.into()),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_string_optional(val: Value) -> Result<Option<String>, ParseValueError<ParseError>> {
  match val {
    Value::NullValue(_) => Ok(None),
    Value::StringValue(val) => Ok(Some(val.into())),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_char(val: Value) -> Result<char, ParseValueError<ParseCharError>> {
  match val {
    Value::StringValue(sval) => {
      let s: String = sval.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(sval)))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_char_optional(val: Value) -> Result<Option<char>, ParseValueError<ParseCharError>> {
  match val {
    Value::StringValue(sval) => {
      let s: String = sval.clone().into();
      s.parse()
        .map(Some)
        .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(sval)))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_number<T: std::str::FromStr<Err = ParseIntError>>(
  val: Value,
) -> Result<T, ParseValueError<ParseIntError>> {
  match val {
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_number_optional<T: std::str::FromStr<Err = ParseIntError>>(
  val: Value,
) -> Result<Option<T>, ParseValueError<ParseIntError>> {
  match val {
    Value::NullValue(_) => Ok(None),
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map(Some)
      .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_float<T: std::str::FromStr<Err = ParseFloatError>>(
  val: Value,
) -> Result<T, ParseValueError<ParseFloatError>> {
  match val {
    Value::FloatValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val))),
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_float_optional<T: std::str::FromStr<Err = ParseFloatError>>(
  val: Value,
) -> Result<Option<T>, ParseValueError<ParseFloatError>> {
  match val {
    Value::NullValue(_) => Ok(None),
    Value::FloatValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map(Some)
      .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val))),
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map(Some)
      .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub enum ParseValueError<E: Display> {
  ParseError(E, Value),
  UnexpectedValue(Value),
}

impl<E: Display> Reporter for ParseValueError<E> {
  fn report<'a, FileId>(
    &self,
    file_id: FileId,
  ) -> codespan_reporting::diagnostic::Diagnostic<FileId>
  where
    FileId: 'a + Copy + PartialEq,
  {
    match self {
      ParseValueError::ParseError(err, val) => {
        let syn = val.syntax();
        let range = syn.text_range();
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        Diagnostic::error()
          .with_message(err.to_string())
          .with_labels(vec![
            Label::primary(file_id, start..end).with_message(syn.text())
          ])
      }
      ParseValueError::UnexpectedValue(val) => {
        let syn = val.syntax();
        let range = syn.text_range();
        let start: usize = range.start().into();
        let end: usize = range.end().into();
        Diagnostic::error()
          .with_message("unexpected value")
          .with_labels(vec![
            Label::primary(file_id, start..end).with_message(syn.text())
          ])
      }
    }
  }
}

macro_rules! impl_diagnostic {
  ($($ty:ident::$parser:ident -> ?$err:ident), + $(,)?) => {
    $(
      impl Diagnosticable for $ty {
        type Error = ParseValueError<$err>;

        type Node = Value;

        fn parse(node: Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized,
        {
          $parser(node)
        }
      }

      impl Diagnosticable for Option<$ty> {
        type Error = ParseValueError<$err>;

        type Node = Value;

        fn parse(node: Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized,
        {
          paste::paste! {
            [<$parser _ optional>] (node)
          }
        }
      }
    )*
  };
}

impl_diagnostic!(
  u8::parse_number -> ?ParseIntError,
  u16::parse_number -> ?ParseIntError,
  u32::parse_number -> ?ParseIntError,
  u64::parse_number -> ?ParseIntError,
  u128::parse_number -> ?ParseIntError,
  usize::parse_number -> ?ParseIntError,
  i8::parse_number -> ?ParseIntError,
  i16::parse_number -> ?ParseIntError,
  i32::parse_number -> ?ParseIntError,
  i64::parse_number -> ?ParseIntError,
  i128::parse_number -> ?ParseIntError,
  f32::parse_float -> ?ParseFloatError,
  f64::parse_float -> ?ParseFloatError,
  char::parse_char -> ?ParseCharError,
  bool::parse_boolean -> ?ParseBoolError,
  String::parse_string -> ?ParseError,
);

#[cfg(feature = "decimal")]
#[cfg_attr(docsrs, doc(cfg(feature = "decimal")))]
pub use rust_decimal_sealed::*;

#[cfg(feature = "decimal")]
mod rust_decimal_sealed {
  use super::*;
  use rust_decimal::{Decimal, Error};

  pub fn parse_decimal(src: Value) -> Result<Decimal, ParseValueError<Error>> {
    match src {
      Value::FloatValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val)))
      }
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_decimal_optional(src: Value) -> Result<Option<Decimal>, ParseValueError<Error>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::FloatValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val)))
      }
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  impl_diagnostic!(
    Decimal::parse_decimal -> ?Error,
  );
}

#[cfg(feature = "bigdecimal")]
#[cfg_attr(docsrs, doc(cfg(feature = "bigdecimal")))]
pub use bigdecimal_sealed::*;

#[cfg(feature = "bigdecimal")]
mod bigdecimal_sealed {
  use super::*;
  use bigdecimal::{BigDecimal, ParseBigDecimalError};

  pub fn parse_bigdecimal(src: Value) -> Result<BigDecimal, ParseValueError<ParseBigDecimalError>> {
    match src {
      Value::FloatValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val)))
      }
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_bigdecimal_optional(
    src: Value,
  ) -> Result<Option<BigDecimal>, ParseValueError<ParseBigDecimalError>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::FloatValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::FloatValue(val)))
      }
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  impl_diagnostic!(
    BigDecimal::parse_bigdecimal -> ?ParseBigDecimalError,
  );
}

#[cfg(feature = "bigint")]
#[cfg_attr(docsrs, doc(cfg(feature = "bigint")))]
pub use bigint_sealed::*;

#[cfg(feature = "bigint")]
mod bigint_sealed {
  use super::*;
  use num_bigint::{BigInt, BigUint, ParseBigIntError};

  pub fn parse_bigint(src: Value) -> Result<BigInt, ParseValueError<ParseBigIntError>> {
    match src {
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_bigint_optional(
    src: Value,
  ) -> Result<Option<BigInt>, ParseValueError<ParseBigIntError>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_biguint(src: Value) -> Result<BigUint, ParseValueError<ParseBigIntError>> {
    match src {
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_biguint_optional(
    src: Value,
  ) -> Result<Option<BigUint>, ParseValueError<ParseBigIntError>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::IntValue(val) => {
        let s = val.syntax().text().to_string();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::IntValue(val)))
      }
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  impl_diagnostic!(
    BigInt::parse_bigint -> ?ParseBigIntError,
    BigUint::parse_biguint -> ?ParseBigIntError,
  );
}

#[cfg(any(feature = "humantime", feature = "chrono"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "humantime", feature = "chrono"))))]
pub mod time;

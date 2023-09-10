use std::num::{ParseFloatError, ParseIntError};

use super::*;

mod vec;
pub use vec::*;
mod hashmap;
pub use hashmap::*;
mod btreemap;
pub use btreemap::*;
mod net;
pub use net::*;
mod path;
pub use path::*;

pub fn parse_boolean(val: Value) -> Result<bool, ParseValueError> {
  match val {
    Value::BooleanValue(val) => Ok(val.true_token().is_some()),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_string(val: Value) -> Result<String, ParseValueError> {
  match val {
    Value::StringValue(val) => Ok(val.into()),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_char(val: Value) -> Result<char, ParseValueError> {
  match val {
    Value::StringValue(sval) => {
      let s: String = sval.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(sval))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_number<T: std::str::FromStr<Err = ParseIntError>>(
  val: Value,
) -> Result<T, ParseValueError> {
  match val {
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::IntValue(val)))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_number_optional<T: std::str::FromStr<Err = ParseIntError>>(
  val: Value,
) -> Result<Option<T>, ParseValueError> {
  match val {
    Value::NullValue(_) => Ok(None),
    val => parse_number(val).map(Some),
  }
}

pub fn parse_float<T: std::str::FromStr<Err = ParseFloatError>>(
  val: Value,
) -> Result<T, ParseValueError> {
  match val {
    Value::FloatValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::FloatValue(val)))),
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::IntValue(val)))),
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_float_optional<T: std::str::FromStr<Err = ParseFloatError>>(
  val: Value,
) -> Result<Option<T>, ParseValueError> {
  match val {
    Value::NullValue(_) => Ok(None),
    val => parse_float(val).map(Some),
  }
}

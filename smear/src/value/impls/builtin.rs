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

pub fn parse_boolean(val: &Value) -> Result<bool, Error> {
  match val {
    Value::BooleanValue(val) => Ok(val.true_token().is_some()),
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_string(val: &Value) -> Result<String, Error> {
  match val {
    Value::StringValue(val) => Ok(val.into()),
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_char(val: &Value) -> Result<char, Error> {
  match val {
    Value::StringValue(sval) => {
      let s: String = sval.clone().into();
      s.parse().map_err(|e| Error::invalid_value(sval, e))
    }
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_number<T: std::str::FromStr<Err = ParseIntError>>(val: &Value) -> Result<T, Error> {
  match val {
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| Error::invalid_value(val, e)),
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_number_optional<T: std::str::FromStr<Err = ParseIntError>>(
  val: &Value,
) -> Result<Option<T>, Error> {
  match val {
    Value::NullValue(_) => Ok(None),
    val => parse_number(val).map(Some),
  }
}

pub fn parse_float<T: std::str::FromStr<Err = ParseFloatError>>(val: &Value) -> Result<T, Error> {
  match val {
    Value::FloatValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| Error::invalid_value(val, e)),
    Value::IntValue(val) => val
      .syntax()
      .text()
      .to_string()
      .parse::<T>()
      .map_err(|e| Error::invalid_value(val, e)),
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_float_optional<T: std::str::FromStr<Err = ParseFloatError>>(
  val: &Value,
) -> Result<Option<T>, Error> {
  match val {
    Value::NullValue(_) => Ok(None),
    val => parse_float(val).map(Some),
  }
}

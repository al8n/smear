use super::*;
use num_bigint::{BigInt, BigUint};

pub fn parse_bigint(src: Value) -> Result<BigInt, ParseValueError> {
  match src {
    Value::IntValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::IntValue(val))))
    }
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_biguint(src: Value) -> Result<BigUint, ParseValueError> {
  match src {
    Value::IntValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::IntValue(val))))
    }
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

impl_diagnostic!(BigInt::parse_bigint?, BigUint::parse_biguint?,);

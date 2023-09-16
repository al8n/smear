use crate::value::*;
use num_bigint::{BigInt, BigUint};

pub fn parse_bigint(src: &Value) -> Result<BigInt, ValueError> {
  match src {
    Value::IntValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

pub fn parse_biguint(src: &Value) -> Result<BigUint, ValueError> {
  match src {
    Value::IntValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

impl_diagnostic_and_encodable!(BigInt::parse_bigint, BigUint::parse_biguint,);

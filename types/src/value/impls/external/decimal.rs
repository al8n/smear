use crate::value::*;
use rust_decimal::Decimal;

pub fn parse_decimal(src: &Value) -> Result<Decimal, ValueError> {
  match src {
    Value::FloatValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
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

impl_diagnostic!(Decimal::parse_decimal,);

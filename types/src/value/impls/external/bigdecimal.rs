use super::*;
use ::bigdecimal::BigDecimal;

pub fn parse_bigdecimal(src: &Value) -> Result<BigDecimal, ValueError> {
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

impl_diagnostic!(BigDecimal::parse_bigdecimal,);

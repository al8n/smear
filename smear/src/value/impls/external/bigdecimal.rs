use super::*;
use ::bigdecimal::BigDecimal;

pub fn parse_bigdecimal(src: Value) -> Result<BigDecimal, ParseValueError> {
  match src {
    Value::FloatValue(val) => {
      let s = val.syntax().text().to_string();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::FloatValue(val))))
    }
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

impl_diagnostic!(BigDecimal::parse_bigdecimal?,);

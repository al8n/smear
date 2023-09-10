use super::*;
use ::uuid::Uuid;

pub fn parse_uuid(src: Value) -> Result<Uuid, ParseValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

impl_diagnostic!(Uuid::parse_uuid?,);

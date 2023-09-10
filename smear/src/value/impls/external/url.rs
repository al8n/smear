use super::*;
use ::url::Url;

pub fn parse_url(src: Value) -> Result<Url, ParseValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

impl_diagnostic!(Url::parse_url?,);

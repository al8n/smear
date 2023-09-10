use super::*;
use ::humantime::{Duration as HumanDuration, Timestamp};
use core::time::Duration;

pub fn parse_duration(src: Value) -> Result<Duration, ParseValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse::<HumanDuration>()
        .map(Into::into)
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_timestamp(src: Value) -> Result<Timestamp, ParseValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse()
        .map_err(|e| ParseValueError::ParseError(Box::new(e), Box::new(Value::StringValue(val))))
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

impl_diagnostic!(Duration::parse_duration?, Timestamp::parse_timestamp?,);

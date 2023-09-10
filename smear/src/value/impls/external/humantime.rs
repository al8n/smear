use super::*;
use ::humantime::{Duration as HumanDuration, Timestamp};
use core::time::Duration;

pub fn parse_duration(src: &Value) -> Result<Duration, Error> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse::<HumanDuration>()
        .map(Into::into)
        .map_err(|e| Error::invalid_value(val, e))
    }
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_timestamp(src: &Value) -> Result<Timestamp, Error> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| Error::invalid_value(val, e))
    }
    val => Err(Error::unexpected_type(val)),
  }
}

impl_diagnostic!(Duration::parse_duration, Timestamp::parse_timestamp,);

use crate::value::*;

use ::humantime::{Duration as HumanDuration, Timestamp};
use core::time::Duration;

pub fn parse_duration(src: &Value) -> Result<Duration, ValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse::<HumanDuration>()
        .map(Into::into)
        .map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

fn parse_humanduration(src: &Value) -> Result<HumanDuration, ValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse::<HumanDuration>()
        .map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

impl_diagnostic_and_encodable!(Duration::parse_duration, HumanDuration::parse_humanduration);
impl_diagnostic_and_encodable!(string(Timestamp::parse_timestamp));

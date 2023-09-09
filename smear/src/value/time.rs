use super::*;

#[cfg(feature = "humantime")]
pub use humantime_sealed::*;

#[cfg(feature = "humantime")]
mod humantime_sealed {
  use super::*;
  use core::time::Duration;
  use humantime::Duration as HumanDuration;

  pub use humantime::{DurationError, Timestamp, TimestampError};

  pub fn parse_duration(src: Value) -> Result<Duration, ParseValueError<DurationError>> {
    match src {
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse::<HumanDuration>()
          .map(Into::into)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_duration_optional(
    src: Value,
  ) -> Result<Option<Duration>, ParseValueError<DurationError>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse::<HumanDuration>()
          .map(|d| Some(d.into()))
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_timestamp(src: Value) -> Result<Timestamp, ParseValueError<TimestampError>> {
    match src {
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  pub fn parse_timestamp_optional(
    src: Value,
  ) -> Result<Option<Timestamp>, ParseValueError<TimestampError>> {
    match src {
      Value::NullValue(_) => Ok(None),
      Value::StringValue(val) => {
        let s: String = val.clone().into();
        s.parse()
          .map(Some)
          .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
      }
      val => Err(ParseValueError::UnexpectedValue(val)),
    }
  }

  impl_diagnostic!(
    Duration::parse_duration -> ?DurationError,
    Timestamp::parse_timestamp -> ?TimestampError,
  );
}

#[cfg(feature = "chrono")]
pub use chrono_sealed::*;

#[cfg(feature = "chrono")]
mod chrono_sealed {
  use super::*;

  pub use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, ParseError};

  pub type Utc = DateTime<chrono::Utc>;

  pub type Local = DateTime<chrono::Local>;

  pub type FixedOffset = DateTime<chrono::FixedOffset>;

  macro_rules! impl_parse {
    ($($ty:ident::$name:ident), +$(,)?) => {
      $(
        paste::paste!{
          pub fn [<parse_ $name>] (src: Value) -> Result<$ty, ParseValueError<ParseError>> {
            match src {
              Value::StringValue(val) => {
                let s: String = val.clone().into();
                s.parse()
                  .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
              }
              val => Err(ParseValueError::UnexpectedValue(val)),
            }
          }

          pub fn [<parse_ $name _optional>] (src: Value) -> Result<Option<$ty>, ParseValueError<ParseError>> {
            match src {
              Value::StringValue(val) => {
                let s: String = val.clone().into();
                s.parse()
                  .map(Some)
                  .map_err(|e| ParseValueError::ParseError(e, Value::StringValue(val)))
              }
              val => Err(ParseValueError::UnexpectedValue(val)),
            }
          }
        }
      )*
    };
  }

  impl_parse!(
    NaiveDate::naive_date,
    NaiveDateTime::naive_date_time,
    NaiveTime::naive_time,
    Utc::utc,
    Local::local,
    FixedOffset::fixed_offset,
  );

  impl_diagnostic!(
    NaiveDate::parse_naive_date -> ?ParseError,
    NaiveDateTime::parse_naive_date_time -> ?ParseError,
    NaiveTime::parse_naive_time -> ?ParseError,
    Utc::parse_utc -> ?ParseError,
    Local::parse_local -> ?ParseError,
    FixedOffset::parse_fixed_offset -> ?ParseError,
  );
}

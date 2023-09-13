use super::*;
use ::chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};

pub type Utc = DateTime<::chrono::Utc>;

pub type Local = DateTime<::chrono::Local>;

pub type FixedOffset = DateTime<::chrono::FixedOffset>;

macro_rules! impl_parse {
  ($($ty:ident::$name:ident), +$(,)?) => {
    $(
      paste::paste!{
        pub fn [<parse_ $name>] (src: &Value) -> Result<$ty, ValueError> {
          match src {
            Value::StringValue(val) => {
              let s: String = val.clone().into();
              s.parse()
                .map_err(|e| ValueError::invalid_value(val, e))
            }
            val => Err(ValueError::unexpected_type(val)),
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
  NaiveDate::parse_naive_date,
  NaiveDateTime::parse_naive_date_time,
  NaiveTime::parse_naive_time,
  Utc::parse_utc,
  Local::parse_local,
  FixedOffset::parse_fixed_offset,
);

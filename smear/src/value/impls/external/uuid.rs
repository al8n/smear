use super::*;
use ::uuid::Uuid;

pub fn parse_uuid(src: &Value) -> Result<Uuid, Error> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| Error::invalid_value(val, e))
    }
    val => Err(Error::unexpected_type(val)),
  }
}

impl_diagnostic!(Uuid::parse_uuid);

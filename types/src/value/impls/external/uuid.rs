use ::uuid::Uuid;
use apollo_parser::ast::Value;

use crate::error::ValueError;

pub fn parse_uuid(src: &Value) -> Result<Uuid, ValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

impl_diagnostic!(Uuid::parse_uuid);

use ::url::Url;
use apollo_parser::ast::Value;

use crate::error::ValueError;

pub fn parse_url(src: &Value) -> Result<Url, ValueError> {
  match src {
    Value::StringValue(val) => {
      let s: String = val.clone().into();
      s.parse().map_err(|e| ValueError::invalid_value(val, e))
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

impl_diagnostic!(Url::parse_url);

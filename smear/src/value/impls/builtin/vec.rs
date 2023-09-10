use super::*;

pub fn parse_vec<T: DiagnosticableValue>(value: &Value) -> Result<Vec<T>, Error> {
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = Vec::new();
      for val in val.values() {
        match T::parse(&val) {
          Ok(val) => res.push(val),
          Err(err) => errors.push(err),
        }
      }
      if errors.is_empty() {
        Ok(res)
      } else {
        Err(Error::multiple(value, errors))
      }
    }
    val => Err(Error::unexpected_type(val)),
  }
}

pub fn parse_vec_optional<T: DiagnosticableValue>(value: &Value) -> Result<Option<Vec<T>>, Error> {
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_vec(val).map(Some),
  }
}

impl<V: DiagnosticableValue> Diagnosticable for Vec<V> {
  type Error = Error;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_vec(node)
  }
}

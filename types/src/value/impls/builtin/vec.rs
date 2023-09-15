use super::*;

pub fn parse_vec<T: DiagnosticableValue>(value: &Value) -> Result<Vec<T>, ValueError> {
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
        Err(ValueError::multiple(value, errors))
      }
    }
    val => Err(ValueError::unexpected_type(val)),
  }
}

pub fn parse_vec_optional<T: DiagnosticableValue>(
  value: &Value,
) -> Result<Option<Vec<T>>, ValueError> {
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_vec(val).map(Some),
  }
}

impl<V: DiagnosticableValue> Diagnosticable for Vec<V> {
  type Error = ValueError;

  type Node = Value;

  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    static DESCRIPTOR: std::sync::OnceLock<ValueDescriptor> = std::sync::OnceLock::new();
    static KIND: std::sync::OnceLock<ValueKind> = std::sync::OnceLock::new();
    DESCRIPTOR.get_or_init(|| ValueDescriptor {
      name: "List",
      kind: KIND.get_or_init(|| ValueKind::List(V::descriptor())),
    })
  }

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_vec(node)
  }
}

impl<T: DiagnosticableValue> DiagnosticableValue for Vec<T> {}

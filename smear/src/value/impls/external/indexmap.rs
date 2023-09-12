use super::*;
use ::indexmap::{IndexMap, IndexSet};

pub fn parse_indexmap<K, V>(value: &Value) -> Result<IndexMap<K, V>, ValueError>
where
  K: std::str::FromStr + core::hash::Hash + Eq,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::ObjectValue(val) => {
      let mut errors = Vec::new();
      let mut res = IndexMap::new();
      for field in val.object_fields() {
        match (field.name(), field.value()) {
          (None, None) => continue,
          (None, Some(_)) => {
            errors.push(ValueError::invalid_value(&field, "missing key"));
          }
          (Some(name), None) => {
            errors.push(ValueError::invalid_value(
              &field,
              format!("{} is missing value", name.text()),
            ));
          }
          (Some(name), Some(val)) => {
            let key =
              name.text().to_string().parse::<K>().map_err(|e| {
                ValueError::invalid_value(&field, format!("fail to parse key: {e}"))
              })?;
            match V::parse(&val) {
              Ok(val) => {
                res.insert(key, val);
              }
              Err(err) => {
                errors.push(err);
              }
            };
          }
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

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> Diagnosticable
  for IndexMap<K, V>
where
  K::Err: Display + 'static,
{
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexmap(node)
  }
}

pub fn parse_indexset<V>(value: &Value) -> Result<IndexSet<V>, ValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = IndexSet::new();
      for val in val.values() {
        match V::parse(&val) {
          Ok(val) => {
            res.insert(val);
          }
          Err(err) => {
            errors.push(err);
          }
        };
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

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for IndexSet<V> {
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexset(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for IndexMap<K, V>
where
  K::Err: Display + 'static,
{
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for IndexSet<V> {}

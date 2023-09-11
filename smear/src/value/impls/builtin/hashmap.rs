use super::*;
use std::collections::{HashMap, HashSet};

pub fn parse_hashmap<K, V>(value: &Value) -> Result<HashMap<K, V>, ValueError>
where
  K: std::str::FromStr + core::hash::Hash + Eq,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::ObjectValue(val) => {
      let mut errors = Vec::new();
      let mut res = HashMap::new();
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
            let key = name
              .text()
              .to_string()
              .parse::<K>()
              .map_err(|e| ValueError::invalid_value(&field, format!("fail to parse key: {e}")))?;
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

pub fn parse_hashmap_optional<K, V>(value: &Value) -> Result<Option<HashMap<K, V>>, ValueError>
where
  K: std::str::FromStr + core::hash::Hash + Eq,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_hashmap(val).map(Some),
  }
}

pub fn parse_hashset<V>(value: &Value) -> Result<HashSet<V>, ValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = HashSet::new();
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

pub fn parse_hashset_optional<V>(value: &Value) -> Result<Option<HashSet<V>>, ValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_hashset(val).map(Some),
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> Diagnosticable
  for HashMap<K, V>
where
  K::Err: Display + 'static,
{
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashmap(node)
  }
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for HashSet<V> {
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashset(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for HashMap<K, V>
where
  K::Err: Display + 'static,
{
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for HashSet<V> {}

use super::*;
use std::collections::{HashMap, HashSet};

pub fn parse_hashmap<K, V>(value: Value) -> Result<HashMap<K, V>, ParseValueError>
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
            errors.push(ParseValueError::ParseError(
              Box::new("missing key"),
              Box::new(field),
            ));
          }
          (Some(name), None) => {
            errors.push(ParseValueError::ParseError(
              Box::new(format!("{} is missing value", name.text())),
              Box::new(field),
            ));
          }
          (Some(name), Some(val)) => {
            let key = name.text().to_string().parse::<K>().map_err(|e| {
              ParseValueError::ParseError(
                Box::new(format!("fail to parse key: {e}")),
                Box::new(field),
              )
            })?;
            match V::parse(val) {
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
        Err(ParseValueError::Multiple(errors))
      }
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_hashmap_optional<K, V>(value: Value) -> Result<Option<HashMap<K, V>>, ParseValueError>
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

pub fn parse_hashset<V>(value: Value) -> Result<HashSet<V>, ParseValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = HashSet::new();
      for val in val.values() {
        match V::parse(val) {
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
        Err(ParseValueError::Multiple(errors))
      }
    }
    val => Err(ParseValueError::UnexpectedValue(val)),
  }
}

pub fn parse_hashset_optional<V>(value: Value) -> Result<Option<HashSet<V>>, ParseValueError>
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
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashmap(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> Diagnosticable
  for Option<HashMap<K, V>>
where
  K::Err: Display + 'static,
{
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashmap_optional(node)
  }
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for HashSet<V> {
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashset(node)
  }
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for Option<HashSet<V>> {
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_hashset_optional(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for HashMap<K, V>
where
  K::Err: Display + 'static,
{
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for Option<HashMap<K, V>>
where
  K::Err: Display + 'static,
{
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for HashSet<V> {}
impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for Option<HashSet<V>> {}

use super::*;
use ::indexmap::{IndexMap, IndexSet};

pub fn parse_indexmap<K, V>(value: Value) -> Result<IndexMap<K, V>, ParseValueError>
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

pub fn parse_indexmap_optional<K, V>(
  value: Value,
) -> Result<Option<IndexMap<K, V>>, ParseValueError>
where
  K: std::str::FromStr + core::hash::Hash + Eq,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_indexmap(val).map(Some),
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> Diagnosticable
  for IndexMap<K, V>
where
  K::Err: Display + 'static,
{
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexmap(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> Diagnosticable
  for Option<IndexMap<K, V>>
where
  K::Err: Display + 'static,
{
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexmap_optional(node)
  }
}

pub fn parse_indexset<V>(value: Value) -> Result<IndexSet<V>, ParseValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = IndexSet::new();
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

pub fn parse_indexset_optional<V>(value: Value) -> Result<Option<IndexSet<V>>, ParseValueError>
where
  V: DiagnosticableValue + core::hash::Hash + Eq,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_indexset(val).map(Some),
  }
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for IndexSet<V> {
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexset(node)
  }
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> Diagnosticable for Option<IndexSet<V>> {
  type Error = ParseValueError;

  type Node = Value;

  fn parse(node: Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexset_optional(node)
  }
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for IndexMap<K, V>
where
  K::Err: Display + 'static,
{
}

impl<K: std::str::FromStr + core::hash::Hash + Eq, V: DiagnosticableValue> DiagnosticableValue
  for Option<IndexMap<K, V>>
where
  K::Err: Display + 'static,
{
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for IndexSet<V> {}
impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for Option<IndexSet<V>> {}

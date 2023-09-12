use super::*;
use std::collections::{BTreeMap, BTreeSet};

pub fn parse_btreemap<K, V>(value: &Value) -> Result<BTreeMap<K, V>, ValueError>
where
  K: std::str::FromStr + Eq + Ord,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::ObjectValue(val) => {
      let mut errors = Vec::new();
      let mut res = BTreeMap::new();
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

pub fn parse_btreemap_optional<K, V>(value: &Value) -> Result<Option<BTreeMap<K, V>>, ValueError>
where
  K: std::str::FromStr + Eq + Ord,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_btreemap(val).map(Some),
  }
}

pub fn parse_btreeset<V>(value: &Value) -> Result<BTreeSet<V>, ValueError>
where
  V: DiagnosticableValue + Eq + Ord,
{
  match value {
    Value::ListValue(val) => {
      let mut errors = Vec::new();
      let mut res = BTreeSet::new();
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

pub fn parse_btreeset_optional<V>(value: &Value) -> Result<Option<BTreeSet<V>>, ValueError>
where
  V: DiagnosticableValue + Eq + Ord,
{
  match value {
    Value::NullValue(_) => Ok(None),
    val => parse_btreeset(val).map(Some),
  }
}

impl<K: std::str::FromStr + Eq + Ord, V: DiagnosticableValue> Diagnosticable for BTreeMap<K, V>
where
  K::Err: Display + 'static,
{
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_btreemap(node)
  }
}

impl<V: DiagnosticableValue + Eq + Ord> Diagnosticable for BTreeSet<V> {
  type Error = ValueError;

  type Node = Value;

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_btreeset(node)
  }
}

impl<K: std::str::FromStr + Eq + Ord, V: DiagnosticableValue> DiagnosticableValue for BTreeMap<K, V> where
  K::Err: Display + 'static
{
}

impl<V: DiagnosticableValue + Eq + Ord> DiagnosticableValue for BTreeSet<V> {}

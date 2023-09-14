use std::fmt::Display;

use ::indexmap::{IndexMap, IndexSet};
use apollo_parser::ast::Value;

use crate::{
  error::ValueError,
  value::{DiagnosticableValue, MapKind, SetKind, ValueDescriptor, ValueKind},
  Diagnosticable,
};

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

impl<K, V: DiagnosticableValue> Diagnosticable for IndexMap<K, V>
where
  K: std::str::FromStr + core::hash::Hash + Eq + DiagnosticableValue,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
  type Error = ValueError;

  type Node = Value;

  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    static DESCRIPTOR: std::sync::OnceLock<ValueDescriptor> = std::sync::OnceLock::new();
    static KIND: std::sync::OnceLock<ValueKind> = std::sync::OnceLock::new();
    DESCRIPTOR.get_or_init(|| ValueDescriptor {
      name: "IndexMap",
      kind: KIND.get_or_init(|| ValueKind::Map {
        kind: MapKind::IndexMap,
        key: K::descriptor(),
        value: V::descriptor(),
      }),
    })
  }

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

  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    static DESCRIPTOR: std::sync::OnceLock<ValueDescriptor> = std::sync::OnceLock::new();
    static KIND: std::sync::OnceLock<ValueKind> = std::sync::OnceLock::new();
    DESCRIPTOR.get_or_init(|| ValueDescriptor {
      name: "IndexSet",
      kind: KIND.get_or_init(|| ValueKind::Set {
        kind: SetKind::IndexSet,
        value: V::descriptor(),
      }),
    })
  }

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    parse_indexset(node)
  }
}

impl<K, V> DiagnosticableValue for IndexMap<K, V>
where
  K: std::str::FromStr + core::hash::Hash + Eq + DiagnosticableValue,
  K::Err: Display + 'static,
  V: DiagnosticableValue,
{
}

impl<V: DiagnosticableValue + core::hash::Hash + Eq> DiagnosticableValue for IndexSet<V> {}

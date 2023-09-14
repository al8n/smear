use super::*;
use core::fmt::Display;
use std::str::FromStr;

macro_rules! impl_parse_from_str {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      paste::paste!{
        pub fn [<$parser>] (value: &apollo_parser::ast::Value) -> Result<$ty, crate::error::ValueError> {
          match value {
            apollo_parser::ast::Value::StringValue(val) => {
              let s: String = val.clone().into();
              s.parse()
                .map_err(|e| crate::error::ValueError::invalid_value(val, e))
            }
            val => Err(crate::error::ValueError::unexpected_type(val)),
          }
        }
      }
    )*
  };
}

macro_rules! impl_diagnostic_inner {
  ($ty:ident::$parser:ident) => {
    impl crate::Diagnosticable for $ty {
      type Error = crate::error::ValueError;

      type Node = apollo_parser::ast::Value;

      type Descriptor = crate::value::ValueDescriptor;

      fn descriptor() -> &'static Self::Descriptor {
        const DESCRIPTOR: &crate::value::ValueDescriptor = &crate::value::ValueDescriptor {
          name: stringify!($ty!),
          kind: &crate::value::ValueKind::Scalar,
        };
        DESCRIPTOR
      }

      fn parse(node: &Self::Node) -> Result<Self, Self::Error>
      where
        Self: Sized,
      {
        $parser(node)
      }
    }

    impl crate::value::DiagnosticableValue for $ty {}
  };
}

macro_rules! impl_diagnostic {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      impl_diagnostic_inner!($ty::$parser);
    )*
  };
  (string($($ty:ident::$parser:ident ?), + $(,)?)) => {
    $(
      impl_parse_from_str!($ty::$parser);

      impl_diagnostic!($ty::$parser);
    )*
  }
}

impl_diagnostic!(
  u8::parse_number,
  u16::parse_number,
  u32::parse_number,
  u64::parse_number,
  u128::parse_number,
  usize::parse_number,
  i8::parse_number,
  i16::parse_number,
  i32::parse_number,
  i64::parse_number,
  i128::parse_number,
  f32::parse_float,
  f64::parse_float,
);

impl_diagnostic!(char::parse_char, bool::parse_boolean, String::parse_string,);

mod external;
pub use external::*;
mod builtin;
pub use builtin::*;

impl<T: DiagnosticableValue> DiagnosticableValue for Vec<T> {}

impl<T: DiagnosticableValue> Diagnosticable for Option<T> {
  type Error = ValueError;
  type Node = Value;
  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    static DESCRIPTOR: std::sync::OnceLock<ValueDescriptor> = std::sync::OnceLock::new();
    static KIND: std::sync::OnceLock<ValueKind> = std::sync::OnceLock::new();
    DESCRIPTOR.get_or_init(|| ValueDescriptor {
      name: T::descriptor().name().trim_end_matches('!'),
      kind: KIND.get_or_init(|| ValueKind::Optional(T::descriptor())),
    })
  }

  fn parse(node: &Self::Node) -> Result<Self, Self::Error> {
    match node {
      Value::NullValue(_) => Ok(None),
      node => T::parse(node).map(Some),
    }
  }
}

impl<V: DiagnosticableValue> DiagnosticableValue for Option<V> {
  fn parse_with_default(node: &Self::Node, default: Self) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    match node {
      Value::NullValue(_) => Ok(default),
      val => V::parse(val).map(Some),
    }
  }

  fn parse_nullable(node: &Self::Node) -> Result<Option<Self>, Self::Error>
  where
    Self: Sized,
  {
    match node {
      Value::NullValue(_) => Ok(None),
      val => Option::<V>::parse(val).map(Some),
    }
  }
}

impl<O: DiagnosticableObjectValue> DiagnosticableObjectValue for Vec<O> {
  fn fields() -> &'static [&'static str] {
    O::fields()
  }
}

// impl<O: DiagnosticableObjectValue> DiagnosticableObjectValue for Option<O> {
//   fn fields() -> &'static [&'static str] {
//     O::fields()
//   }
// }

pub struct UnknownMapKind(String);

#[derive(Debug, Clone, Copy)]
pub enum MapKind {
  HashMap,
  BTreeMap,
  #[cfg(feature = "indexmap")]
  IndexMap,
}

impl FromStr for MapKind {
  type Err = UnknownMapKind;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let kind = match s.trim() {
      "HashMap" | "hash" | "hashmap" | "hash_map" => Self::HashMap,
      "BTreeMap" | "btree" | "btreemap" | "btree_map" => Self::BTreeMap,
      #[cfg(feature = "indexmap")]
      "IndexMap" | "index" | "indexmap" | "index_map" => Self::IndexMap,
      val => return Err(UnknownMapKind(val.to_owned())),
    };
    Ok(kind)
  }
}

impl core::fmt::Display for MapKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

impl MapKind {
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::HashMap => "HashMap",
      Self::BTreeMap => "BTreeMap",
      #[cfg(feature = "indexmap")]
      Self::IndexMap => "IndexMap",
    }
  }
}

pub struct UnknownSetKind(String);

#[derive(Debug, Clone, Copy)]
pub enum SetKind {
  HashSet,
  BTreeSet,
  #[cfg(feature = "indexmap")]
  IndexSet,
}

impl core::fmt::Display for SetKind {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.as_str())
  }
}

impl FromStr for SetKind {
  type Err = UnknownSetKind;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let kind = match s.trim() {
      "HashSet" | "hash" | "hashset" | "hash_set" => Self::HashSet,
      "BTreeSet" | "btree" | "btreeset" | "btree_set" => Self::BTreeSet,
      #[cfg(feature = "indexmap")]
      "IndexSet" | "index" | "indexset" | "index_set" => Self::IndexSet,
      val => return Err(UnknownSetKind(val.to_owned())),
    };
    Ok(kind)
  }
}

impl SetKind {
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::HashSet => "HashSet",
      Self::BTreeSet => "BTreeSet",
      #[cfg(feature = "indexmap")]
      Self::IndexSet => "IndexSet",
    }
  }
}

#[cfg(feature = "derive")]
const _: () = {
  use darling::FromMeta;
  use syn::Meta;

  impl FromMeta for MapKind {
    fn from_meta(item: &Meta) -> darling::Result<Self> {
      match item {
        Meta::Path(_) => todo!(),
        Meta::List(_) => todo!(),
        Meta::NameValue(_) => todo!(),
      }
    }
  }

  impl FromMeta for SetKind {
    fn from_meta(item: &Meta) -> darling::Result<Self> {
      match item {
        Meta::Path(_) => todo!(),
        Meta::List(_) => todo!(),
        Meta::NameValue(_) => todo!(),
      }
    }
  }
};

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
  Scalar,
  Enum {
    variants: &'static [&'static str],
  },
  Object(&'static [(&'static str, &'static ValueDescriptor)]),
  List(&'static ValueDescriptor),
  Optional(&'static ValueDescriptor),
  Map {
    kind: MapKind,
    key: &'static ValueDescriptor,
    value: &'static ValueDescriptor,
  },
  Set {
    kind: SetKind,
    value: &'static ValueDescriptor,
  },
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Clone, Copy)]
pub struct ValueDescriptor {
  name: &'static str,
  kind: &'static ValueKind,
}
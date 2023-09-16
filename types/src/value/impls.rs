use super::*;
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

macro_rules! impl_diagnostic_and_encodable_inner {
  ($ty:ident::$parser:ident) => {
    impl crate::Diagnosticable for $ty {
      type Error = crate::error::ValueError;

      type Node = apollo_parser::ast::Value;

      type Descriptor = crate::value::ValueDescriptor;

      fn descriptor() -> &'static Self::Descriptor {
        const DESCRIPTOR: &crate::value::ValueDescriptor = &crate::value::ValueDescriptor {
          name: stringify!($ty),
          kind: &crate::value::ValueKind::Scalar,
          description: None,
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

    impl crate::Encodable for $ty {
      type SDL = apollo_encoder::ScalarDefinition;

      fn encode(&self) -> Self::SDL {
        let mut def = apollo_encoder::ScalarDefinition::new(stringify!($ty).into());
        if let Some(desc) = <Self as crate::Diagnosticable>::descriptor().description() {
          def.description(desc.to_string());
        }

        def
      }
    }
  };
}

macro_rules! impl_diagnostic_and_encodable {
  ($($ty:ident::$parser:ident), + $(,)?) => {
    $(
      impl_diagnostic_and_encodable_inner!($ty::$parser);
    )*
  };
  (string($($ty:ident::$parser:ident), + $(,)?)) => {
    $(
      impl_parse_from_str!($ty::$parser);

      impl_diagnostic_and_encodable!($ty::$parser);
    )*
  }
}

impl_diagnostic_and_encodable!(
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

impl_diagnostic_and_encodable!(char::parse_char, bool::parse_boolean, String::parse_string,);

mod external;
use apollo_encoder::Type_;
pub use external::*;
mod builtin;
pub use builtin::*;

impl<T: DiagnosticableValue> Diagnosticable for Option<T> {
  type Error = ValueError;
  type Node = Value;
  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    static DESCRIPTOR: std::sync::OnceLock<ValueDescriptor> = std::sync::OnceLock::new();
    static KIND: std::sync::OnceLock<ValueKind> = std::sync::OnceLock::new();
    DESCRIPTOR.get_or_init(|| ValueDescriptor {
      name: T::descriptor().name(),
      kind: KIND.get_or_init(|| ValueKind::Optional(T::descriptor())),
      description: T::descriptor().description(),
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
  use darling::{ast::NestedMeta, util::path_to_string, Error, FromMeta};
  use quote::{quote, ToTokens};
  use syn::{Expr, Lit};

  use crate::utils::did_you_mean;

  macro_rules! impl_from_meta {
    ($ty:ident) => {
      impl FromMeta for paste::paste!([<$ty Kind>]) {
        fn from_expr(expr: &Expr) -> darling::Result<Self> {
          let val = match expr {
            Expr::Lit(lit) => match &lit.lit {
              Lit::Str(lit) => lit.value(),
              lit => return Err(Error::unexpected_lit_type(lit)),
            }
            Expr::Path(p) => path_to_string(&p.path),
            expr => return Err(Error::unexpected_expr_type(expr)),
          };
          Self::from_string(val.as_str())
        }

        fn from_list(__outer: &[NestedMeta]) -> darling::Result<Self> {
          match __outer.len() {
            0 => Err(Error::too_few_items(1)),
            1 => {
              let val = match &__outer[0] {
                NestedMeta::Meta(ref nested) => path_to_string(nested.path()),
                NestedMeta::Lit(syn::Lit::Str(s)) => s.value(),
                NestedMeta::Lit(lit) => return Err(Error::unexpected_lit_type(lit)),
              };

              Self::from_string(&val)
            }
            _ => Err(Error::too_many_items(1)),
          }
        }

        fn from_string(lit: &str) -> darling::Result<Self> {
          const ALTS: &[&str] = &[
            "hash",
            "btree",
            #[cfg(feature = "indexmap")]
            "index",
          ];
          match lit.to_ascii_lowercase().as_str() {
            "hash" => Ok(paste::paste!(Self :: [<Hash $ty>])),
            "btree" => Ok(paste::paste!(Self :: [<BTree $ty>])),
            #[cfg(feature = "indexmap")]
            "index" => Ok(paste::paste!(Self :: [<Index $ty>])),
            __other => match did_you_mean(__other, ALTS) {
              Some(s) => Err(Error::custom(format!("Unknown value `{__other}`, did you mean `{s}`?"))),
              None => Err(Error::custom(format!("Unknown value `{__other}`. Available values are: [{}]", ALTS.join(", ")))),
            },
          }
        }
      }

      paste::paste! {
        impl ToTokens for [<$ty Kind>] {
          fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            let ts = match self {
              Self::[<Hash $ty>] => quote! { ::smear::value::[<$ty Kind>]::[<Hash $ty>] },
              Self::[<BTree $ty>] => quote! { ::smear::value::[<$ty Kind>]::[<BTree $ty>] },
              #[cfg(feature = "indexmap")]
              Self::[<Index $ty>] => quote! { ::smear::value::[<$ty Kind>]::[<Index $ty>] },
            };
            tokens.extend(ts);
          }
        }
      }
    };
  }

  impl_from_meta!(Map);
  impl_from_meta!(Set);
};

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Clone, Copy)]
pub struct FieldDescriptor {
  name: &'static str,
  ty: &'static ValueDescriptor,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
  Scalar,
  Enum {
    variants: &'static [&'static str],
  },
  Object(&'static [FieldDescriptor]),
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
  description: Option<&'static str>,
}

impl From<&ValueDescriptor> for Type_ {
  fn from(descriptor: &ValueDescriptor) -> Self {
    match descriptor.kind {
      ValueKind::Scalar => nonnull_type(descriptor),
      ValueKind::Enum { .. } => nonnull_type(descriptor),
      ValueKind::Object(_) => nonnull_type(descriptor),
      ValueKind::List(inner) => list_type(inner),
      ValueKind::Optional(inner) => named_type(inner.name),
      ValueKind::Map { .. } => nonnull_type(descriptor),
      ValueKind::Set { .. } => nonnull_type(descriptor),
    }
  }
}

fn named_type(name: &str) -> Type_ {
  Type_::NamedType {
    name: name.to_string(),
  }
}

fn nonnull_type(d: &ValueDescriptor) -> Type_ {
  Type_::NonNull {
    ty: Box::new(Type_::NamedType {
      name: d.name.to_string(),
    }),
  }
}

fn list_type(d: &ValueDescriptor) -> Type_ {
  Type_::List {
    ty: Box::new(d.into()),
  }
}

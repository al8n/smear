use std::str::FromStr;

use apollo_parser::ast::Value;

use crate::value::ValueDescriptor;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
// #[cfg_attr(feature = "derive", derive(darling::FromMeta))]
pub enum DirectiveLocation {
  Query,
  Mutation,
  Subscription,
  Field,
  FragmentDefinition,
  FragmentSpread,
  InlineFragment,
  VariableDefinition,
  Schema,
  Scalar,
  Object,
  FieldDefinition,
  ArgumentDefinition,
  Interface,
  Union,
  Enum,
  EnumValue,
  InputObject,
  InputFieldDefinition,
}

impl DirectiveLocation {
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query => "QUERY",
      Self::Mutation => "MUTATION",
      Self::Subscription => "SUBSCRIPTION",
      Self::Field => "FIELD",
      Self::FragmentDefinition => "FRAGMENT_DEFINITION",
      Self::FragmentSpread => "FRAGMENT_SPREAD",
      Self::InlineFragment => "INLINE_FRAGMENT",
      Self::VariableDefinition => "VARIABLE_DEFINITION",
      Self::Schema => "SCHEMA",
      Self::Scalar => "SCALAR",
      Self::Object => "OBJECT",
      Self::FieldDefinition => "FIELD_DEFINITION",
      Self::ArgumentDefinition => "ARGUMENT_DEFINITION",
      Self::Interface => "INTERFACE",
      Self::Union => "UNION",
      Self::Enum => "ENUM",
      Self::EnumValue => "ENUM_VALUE",
      Self::InputObject => "INPUT_OBJECT",
      Self::InputFieldDefinition => "INPUT_FIELD_DEFINITION",
    }
  }

  const fn available_locations() -> &'static [&'static str] {
    &[
      "QUERY",
      "MUTATION",
      "SUBSCRIPTION",
      "FIELD",
      "FRAGMENT_DEFINITION",
      "FRAGMENT_SPREAD",
      "INLINE_FRAGMENT",
      "VARIABLE_DEFINITION",
      "SCHEMA",
      "SCALAR",
      "OBJECT",
      "FIELD_DEFINITION",
      "ARGUMENT_DEFINITION",
      "INTERFACE",
      "UNION",
      "ENUM",
      "ENUM_VALUE",
      "INPUT_OBJECT",
      "INPUT_FIELD_DEFINITION",
    ]
  }
}

impl From<DirectiveLocation> for String {
  fn from(dir_loc: DirectiveLocation) -> Self {
    dir_loc.as_str().to_string()
  }
}

impl crate::Diagnosticable for DirectiveLocation {
  type Node = Value;
  type Error = crate::error::ValueError;
  type Descriptor = ValueDescriptor;

  fn descriptor() -> &'static Self::Descriptor {
    const DESCRIPTOR: &ValueDescriptor = &ValueDescriptor {
      name: "DirectiveLocation",
      kind: &crate::value::ValueKind::Enum {
        variants: DirectiveLocation::available_locations(),
      },
    };
    DESCRIPTOR
  }

  fn parse(node: &Self::Node) -> Result<Self, Self::Error>
  where
    Self: Sized,
  {
    if let Value::EnumValue(val) = node {
      Self::from_str(val.text().as_str())
        .map_err(|err| crate::error::ValueError::invalid_value(node, err))
    } else {
      Err(crate::error::ValueError::unexpected_type(node))
    }
  }
}

impl crate::value::DiagnosticableValue for DirectiveLocation {}

impl FromStr for DirectiveLocation {
  type Err = UnknownDirectiveLocation;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let loc = match s.trim() {
      "QUERY" | "query" => Self::Query,
      "MUTATION" | "mutation" => Self::Mutation,
      "SUBSCRIPTION" | "subscription" => Self::Subscription,
      "FIELD" | "field" => Self::Field,
      "FRAGMENT_DEFINITION" | "fragment_definition" => Self::FragmentDefinition,
      "FRAGMENT_SPREAD" | "fragment_spread" => Self::FragmentSpread,
      "INLINE_FRAGMENT" | "inline_fragment" => Self::InlineFragment,
      "VARIABLE_DEFINITION" | "variable_definition" => Self::VariableDefinition,
      "SCHEMA" | "schema" => Self::Schema,
      "SCALAR" | "scalar" => Self::Scalar,
      "OBJECT" | "object" => Self::Object,
      "FIELD_DEFINITION" | "field_definition" => Self::FieldDefinition,
      "ARGUMENT_DEFINITION" | "argument_definition" => Self::ArgumentDefinition,
      "INTERFACE" | "interface" => Self::Interface,
      "UNION" | "union" => Self::Union,
      "ENUM" | "enum" => Self::Enum,
      "ENUM_VALUE" | "enum_value" => Self::EnumValue,
      "INPUT_OBJECT" | "input_object" => Self::InputObject,
      "INPUT_FIELD_DEFINITION" | "input_field_definition" => Self::InputFieldDefinition,
      val => return Err(UnknownDirectiveLocation::new(val.to_string())),
    };
    Ok(loc)
  }
}

#[derive(Debug, Clone)]
pub struct UnknownDirectiveLocation {
  src: String,
  suggestion: Option<String>,
}

impl UnknownDirectiveLocation {
  fn new(src: String) -> Self {
    Self {
      suggestion: crate::utils::did_you_mean(
        src.as_str(),
        DirectiveLocation::available_locations(),
      ),
      src,
    }
  }
}

impl core::fmt::Display for UnknownDirectiveLocation {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match &self.suggestion {
      Some(suggestion) => write!(
        f,
        "Unknown directive location: {}, did you mean {}?",
        self.src, suggestion
      ),
      None => write!(
        f,
        "Unknown directive location: {}, available locations are `[{}]`",
        self.src,
        DirectiveLocation::available_locations().join(", ")
      ),
    }
  }
}

impl std::error::Error for UnknownDirectiveLocation {}

#[cfg(feature = "derive")]
const _: () = {
  use darling::{FromMeta, util::path_to_string, ast::NestedMeta};
  use quote::{quote, ToTokens};
  use syn::{Expr, Lit, Meta};

  impl FromMeta for DirectiveLocation {
    fn from_meta(item: &Meta) -> darling::Result<Self> {
      match item {
        Meta::Path(p) => {
          Self::from_str(path_to_string(p).as_str()).map_err(|err| darling::Error::custom(err).with_span(item))
        },
        Meta::List(value) => Self::from_list(&NestedMeta::parse_meta_list(value.tokens.clone())?[..]).map_err(|e| e.with_span(item)),
        Meta::NameValue(value) => Self::from_expr(&value.value).map_err(|e| e.with_span(item)),
      }
    }

    fn from_expr(expr: &syn::Expr) -> darling::Result<Self> {
      let s = match expr {
        Expr::Lit(lit) => match &lit.lit {
          Lit::Str(s) => s.value(),
          lit => return Err(darling::Error::unexpected_lit_type(lit)),
        },
        Expr::Path(p) => path_to_string(&p.path),
        expr => return Err(darling::Error::unexpected_expr_type(expr)),
      };
      Self::from_str(s.as_str()).map_err(darling::Error::custom)
    }

    fn from_string(value: &str) -> darling::Result<Self> {
      Self::from_str(value).map_err(|err| darling::Error::custom(err.to_string()))
    }
  }

  impl ToTokens for DirectiveLocation {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let ts = match self {
        Self::Query => quote! { ::smear::directive::DirectiveLocation::Query },
        Self::Mutation => quote! { ::smear::directive::DirectiveLocation::Mutation },
        Self::Subscription => quote! { ::smear::directive::DirectiveLocation::Subscription },
        Self::Field => quote! { ::smear::directive::DirectiveLocation::Field },
        Self::FragmentDefinition => {
          quote! { ::smear::directive::DirectiveLocation::FragmentDefinition }
        }
        Self::FragmentSpread => quote! { ::smear::directive::DirectiveLocation::FragmentSpread },
        Self::InlineFragment => quote! { ::smear::directive::DirectiveLocation::InlineFragment },
        Self::VariableDefinition => {
          quote! { ::smear::directive::DirectiveLocation::VariableDefinition }
        }
        Self::Schema => quote! { ::smear::directive::DirectiveLocation::Schema },
        Self::Scalar => quote! { ::smear::directive::DirectiveLocation::Scalar },
        Self::Object => quote! { ::smear::directive::DirectiveLocation::Object },
        Self::FieldDefinition => quote! { ::smear::directive::DirectiveLocation::FieldDefinition },
        Self::ArgumentDefinition => {
          quote! { ::smear::directive::DirectiveLocation::ArgumentDefinition }
        }
        Self::Interface => quote! { ::smear::directive::DirectiveLocation::Interface },
        Self::Union => quote! { ::smear::directive::DirectiveLocation::Union },
        Self::Enum => quote! { ::smear::directive::DirectiveLocation::Enum },
        Self::EnumValue => quote! { ::smear::directive::DirectiveLocation::EnumValue },
        Self::InputObject => quote! { ::smear::directive::DirectiveLocation::InputObject },
        Self::InputFieldDefinition => {
          quote! { ::smear::directive::DirectiveLocation::InputFieldDefinition }
        }
      };
      tokens.extend(ts);
    }
  }
};

#[cfg(feature = "derive")]
#[derive(Default)]
pub struct On {
  locations: Vec<DirectiveLocation>,
}

#[cfg(feature = "derive")]
const _: () = {
  use darling::{ast::NestedMeta, FromMeta};
  use quote::{quote, ToTokens};

  impl FromMeta for On {
    fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
      let mut errors = Vec::new();
      let mut locations = Vec::with_capacity(items.len());
      for item in items {
        match item {
          NestedMeta::Meta(meta) => {
            match DirectiveLocation::from_meta(meta) {
              Ok(val) => locations.push(val),
              Err(e) => errors.push(e),
            }
          },
          NestedMeta::Lit(lit) => {
            errors.push(darling::Error::unexpected_lit_type(lit));
          }
        }
      }

      if !errors.is_empty() {
        return Err(darling::Error::multiple(errors));
      }

      Ok(Self { locations })
    }

    fn from_word() -> darling::Result<Self> {
      Ok(Self::default())
    }
  }

  impl ToTokens for On {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
      let locations = &self.locations;
      let ts = quote! { &[#( #locations ),*] };
      tokens.extend(ts);
    }
  }
};

use darling::{ast::NestedMeta, FromDeriveInput, FromMeta};
use quote::quote;

use smear_types::value::MapKind;
use syn::{DeriveInput, Ident, Visibility};

use crate::utils::{Attributes, Ty};

pub fn map(
  kind: MapKind,
  args: proc_macro::TokenStream,
  input: proc_macro::TokenStream,
) -> syn::Result<proc_macro2::TokenStream> {
  let attr_args = NestedMeta::parse_meta_list(args.into())?;
  let input: DeriveInput = syn::parse(input)?;
  let args = MapArgs::from_list(&attr_args)?;

  Dummy::from_derive_input(&input)?;

  Map {
    name: input.ident,
    vis: input.vis,
    key: args.key,
    value: args.value,
    kind,
    attributes: args.attributes,
  }
  .derive()
}

#[derive(FromMeta)]
struct MapArgs {
  key: Ty,
  value: Ty,
  #[darling(default)]
  attributes: Attributes,
}

#[derive(darling::FromDeriveInput)]
#[darling(supports(struct_unit))]
struct Dummy {}

struct Map {
  name: Ident,
  vis: Visibility,
  key: Ty,
  value: Ty,
  kind: MapKind,

  attributes: Attributes,
}

impl Map {
  fn derive(&self) -> syn::Result<proc_macro2::TokenStream> {
    let name = &self.name;
    let name_str = name.to_string();
    let map_type = self.map_type();
    let key = &self.key;
    let value = &self.value;
    let map_kind = self.kind;
    let vis = &self.vis;
    let attributes = &self.attributes;

    Ok(quote! {
      #attributes
      #vis struct #name(#map_type);

      impl ::core::ops::Deref for #name {
        type Target = #map_type;

        fn deref(&self) -> &Self::Target {
          &self.0
        }
      }

      impl ::core::ops::DerefMut for #name {
        fn deref_mut(&mut self) -> &mut Self::Target {
          &mut self.0
        }
      }

      impl #name {
        #vis fn into_inner(self) -> #map_type {
          self.0
        }
      }

      impl ::smear::__exports::Diagnosticable for #name
      {
        type Error = ::smear::__exports::error::ValueError;

        type Node = ::smear::__exports::apollo_parser::ast::Value;

        type Descriptor = ::smear::__exports::value::ValueDescriptor;

        fn descriptor() -> &'static Self::Descriptor {
          use ::std::sync::OnceLock;

          static DESCRIPTOR: OnceLock<::smear::__exports::value::ValueDescriptor> = OnceLock::new();
          static KIND: OnceLock<::smear::__exports::value::ValueKind> = OnceLock::new();

          DESCRIPTOR.get_or_init(|| ::smear::__exports::value::ValueDescriptor {
            name: #name_str,
            kind: KIND.get_or_init(|| ::smear::__exports::value::ValueKind::Map {
              kind: #map_kind,
              key: <#key as ::smear::__exports::Diagnosticable>::descriptor(),
              value: <#value as ::smear::__exports::Diagnosticable>::descriptor(),
            }),
          })
        }

        fn parse(value: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: ::core::marker::Sized,
        {
          use ::smear::__exports::{apollo_parser::ast::{Value, AstNode}, error::ValueError, value::Parser};

          match value {
            Value::ObjectValue(val) => {
              let mut errors = ::std::vec::Vec::new();
              let mut res: #map_type = ::core::default::Default::default();
              for field in val.object_fields() {
                match (field.name(), field.value()) {
                  (::core::option::Option::None, ::core::option::Option::None) => continue,
                  (::core::option::Option::None, ::core::option::Option::Some(_)) => {
                    errors.push(ValueError::invalid_value(&field, "missing key"));
                  }
                  (::core::option::Option::Some(name), ::core::option::Option::None) => {
                    errors.push(ValueError::invalid_value(
                      &field,
                      ::std::format!("{} is missing value", name.text()),
                    ));
                  }
                  (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                    let key_str = name.text().to_string();
                    let key =
                      key_str.as_str().parse::<#key>().map_err(|e| {
                        ValueError::invalid_value(&field, ::std::format!("fail to parse key: {e}"))
                      })?;
                    match <#value as Parser>::parse_value(&val) {
                      ::core::result::Result::Ok(val) => {
                        res.insert(key, val);
                      }
                      ::core::result::Result::Err(err) => {
                        errors.push(err);
                      }
                    };
                  }
                }
              }
              if errors.is_empty() {
                ::core::result::Result::Ok(#name(res))
              } else {
                ::core::result::Result::Err(ValueError::multiple(value, errors))
              }
            }
            val => ::core::result::Result::Err(ValueError::unexpected_type(val)),
          }
        }
      }

      impl ::smear::__exports::value::DiagnosticableValue for #name {}
    })
  }

  fn map_type(&self) -> syn::Type {
    let key = &self.key;
    let value = &self.value;
    match &self.kind {
      MapKind::HashMap => {
        syn::parse_quote! { ::std::collections::HashMap<#key, #value> }
      }
      MapKind::BTreeMap => {
        syn::parse_quote! { ::std::collections::BTreeMap<#key, #value> }
      }
      #[cfg(feature = "indexmap")]
      MapKind::IndexMap => {
        syn::parse_quote! { ::smear::__exports::indexmap::IndexMap<#key, #value> }
      }
    }
  }
}

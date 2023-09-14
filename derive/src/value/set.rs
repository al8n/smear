use darling::{FromMeta, ast::NestedMeta, FromDeriveInput};
use quote::quote;

use smear_types::value::SetKind;
use syn::{DeriveInput, Visibility, Ident};

use crate::utils::{Attributes, Ty};

pub fn set(kind: SetKind, args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
  let attr_args = NestedMeta::parse_meta_list(args.into())?;
  let input: DeriveInput = syn::parse(input)?;
  let args = SetArgs::from_list(&attr_args)?;

  Dummy::from_derive_input(&input)?;

  Set {
    name: input.ident,
    vis: input.vis,
    value: args.value,
    kind,
    attributes: args.attributes,
  }.derive()
}

#[derive(FromMeta)]
struct SetArgs {
  value: Ty,
  #[darling(default)]
  attributes: Attributes,
}

#[derive(darling::FromDeriveInput)]
#[darling(supports(struct_unit))]
struct Dummy {}

struct Set {
  name: Ident,
  vis: Visibility,
  value: Ty,
  kind: SetKind,
  
  attributes: Attributes,
}

impl Set {
  fn derive(&self) -> syn::Result<proc_macro2::TokenStream> {
    let name = &self.name;
    let name_str = name.to_string();
    let set_type = self.set_type();
    let value = &self.value;
    let set_kind = self.kind;
    let vis = &self.vis;
    let attributes = &self.attributes;
  
    Ok(quote! {
      #attributes
      #vis struct #name(#set_type);
  
      impl ::core::ops::Deref for #name {
        type Target = #set_type;
  
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
        #vis fn into_inner(self) -> #set_type {
          self.0
        }
      }
  
      impl ::smear::Diagnosticable for #name
      {
        type Error = ::smear::error::ValueError;
  
        type Node = ::smear::apollo_parser::ast::Value;
  
        type Descriptor = ::smear::value::ValueDescriptor;
  
        fn descriptor() -> &'static Self::Descriptor {
          use ::std::sync::OnceLock;
  
          static DESCRIPTOR: OnceLock<::smear::value::ValueDescriptor> = OnceLock::new();
          static KIND: OnceLock<::smear::value::ValueKind> = OnceLock::new();
  
          DESCRIPTOR.get_or_init(|| ::smear::value::ValueDescriptor {
            name: #name_str,
            kind: KIND.get_or_init(|| ::smear::value::ValueKind::Set {
              kind: #set_kind,
              value: <#value as ::smear::Diagnosticable>::descriptor(),
            }),
          })
        }
  
        fn parse(value: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: Sized,
        {
          use ::smear::{apollo_parser::ast::{Value, AstNode}, error::ValueError, Diagnosticable};
  
          match value {
            Value::ListValue(val) => {
              let mut errors = ::std::vec::Vec::new();
              let mut res: #set_type = ::core::default::Default::default();
              for val in val.values() {
                match <#value as Diagnosticable>::parse(&val) {
                  ::core::result::Result::Ok(val) => {
                    res.insert(val);
                  }
                  ::core::result::Result::Err(err) => {
                    errors.push(err);
                  }
                };
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
  
      impl ::smear::value::DiagnosticableValue for #name {}
    })
  }

  fn set_type(&self) -> syn::Type {
    let value = &self.value;
    match &self.kind {
      SetKind::HashSet => {
        syn::parse_quote! { ::std::collections::HashSet<#value> }
      }
      SetKind::BTreeSet => {
        syn::parse_quote! { ::std::collections::BTreeSet<#value> }
      }
      #[cfg(feature = "indexmap")]
      SetKind::IndexSet => {
        syn::parse_quote! { ::smear::__exports::indexset::IndexSet<#value> }
      }
    }
  }
}

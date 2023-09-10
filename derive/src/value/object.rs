use darling::{
  ast::{Data, Fields},
  FromDeriveInput, FromField, FromMeta, FromVariant,
};
use quote::{format_ident, quote, ToTokens};
use syn::{DeriveInput, Expr, Ident, Path};

use crate::utils::RenameAll;

pub fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let object: Object = Object::from_derive_input(&input)?;
  let name = &object.ident;

  let mut fields_declaration = Vec::new();
  let mut fields_null_check = Vec::new();
  let mut fields_init = Vec::new();
  let mut fields_name = Vec::new();
  let field_parsing: Vec<proc_macro2::TokenStream> = match &object.data {
    Data::Struct(fields) => fields
      .iter()
      .map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_ident_dirty = format_ident!("{field_ident}_dirty");
        let field_rename = field.rename(object.rename_all);
        fields_name.push(field_rename.clone());
        let duplicated_field_error = format!("duplicated field {field_rename}");
        let missing_field_error = format!("field `{field_rename}` is required by {name}");
        let field_ty = &field.ty;

        let parser = if let Some(d) = field.default.as_ref() {
          fields_declaration.push(quote! {
            let mut #field_ident: #field_ty = #d;
            let mut #field_ident_dirty: bool = false;
          });

          fields_init.push(quote! {
            #field_ident,
          });

          let validator = match &field.validator {
            Some(p) => {
              quote! {
                match #p(&v) {
                  ::core::result::Result::Ok(_) => {
                    #field_ident = v;
                  },
                  ::core::result::Result::Err(err) => {
                    errors.push(::smear::value::Error::invalid_value(&val, err));
                  },
                }
              }
            },
            None => quote! {
              #field_ident = v;
            },
          };

          match &field.parser {
            Some(parser_path) => quote! {
              match #parser_path(val) {
                ::core::result::Result::Ok(val) => {
                  let v = ::core::option::Option::Some(val);
                  #validator
                },
                ::core::result::Result::Err(err) => {
                  errors.push(err);
                },
              }
            },
            None => quote! {
              match <#field_ty as ::smear::Diagnosticable>::parse(val) {
                ::core::result::Result::Ok(val) => {
                  #field_ident = ::core::option::Option::Some(val);
                },
                ::core::result::Result::Err(err) => {
                  errors.push(err);
                },
              }
            },
          }
        } else {
          fields_null_check.push(quote! {
            if #field_ident.is_none() && !<#field_ty as ::smear::DiagnosticableValue>::nullable() {
              errors.push(::smear::value::Error::missing_object_field(obj, #missing_field_error));
            }
          });

          fields_declaration.push(quote! {
            let mut #field_ident: ::core::option::Option<#field_ty> = ::core::option::Option::None;
          });

          fields_init.push(quote! {
            #field_ident: {
              if <#field_ty as ::smear::DiagnosticableValue>::nullable() {
                #field_ident
              } else {
                #field_ident.unwrap()
              }
            },
          });

          let validator = match &field.validator {
            Some(p) => {
              quote! {
                match #p(&v) {
                  ::core::result::Result::Ok(_) => {},
                  ::core::result::Result::Err(err) => {
                    errors.push(::smear::value::Error::invalid_value(&val, err));
                  },
                }
              }
            },
            None => quote! {
            },
          };

          match &field.parser {
            Some(parser_path) => quote! {
              match #parser_path(&val) {
                ::core::result::Result::Ok(v) => {
                  #validator
                  #field_ident = ::core::option::Option::Some(v);
                },
                ::core::result::Result::Err(err) => {
                  errors.push(err);
                },
              }
            },
            None => quote! {
              match <#field_ty as ::smear::Diagnosticable>::parse(&val) {
                ::core::result::Result::Ok(v) => {
                  #validator

                  #field_ident = ::core::option::Option::Some(v);
                },
                ::core::result::Result::Err(err) => {
                  errors.push(err);
                },
              }
            },
          }
        };

        quote! {
          #field_rename => {
            if #field_ident_dirty {
              errors.push(::smear::value::Error::duplicated_object_field(&field, #duplicated_field_error));
              continue;
            }

            #field_ident_dirty = true;

            #parser
          },
        }
      })
      .collect(),
    _ => vec![],
  };

  Ok(quote! {
    const _: () = {
      #[automatically_derived]
      impl ::smear::Diagnosticable for #name {
        type Error = ::smear::value::Error;
        type Node = ::smear::apollo_parser::ast::Value;

        fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: ::core::marker::Sized,
        {
          match node {
            ::smear::apollo_parser::ast::Value::ObjectValue(obj) => {
              #(#fields_declaration)*

              let mut errors = ::std::vec::Vec::new();

              for field in obj.object_fields() {
                match (field.name(), field.value()) {
                  (::core::option::Option::None, ::core::option::Option::None) => {},
                  (::core::option::Option::None, ::core::option::Option::Some(_)) => {
                    errors.push(::smear::value::Error::missing_object_field_name(&field));
                  },
                  (::core::option::Option::Some(name), ::core::option::Option::None) => {
                    errors.push(::smear::value::Error::missing_object_value(&field, name.text().as_str()));
                  },
                  (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                    match name.text().as_str().trim() {
                      #(#field_parsing)*
                      n => {
                        errors.push(::smear::value::Error::unknown_object_field(&field, ::smear::value::ErrorUnknownField::with_alts(n, [#(#fields_name),*])));
                      },
                    }
                  },
                }
              }

              #(#fields_null_check)*

              if !errors.is_empty() {
                return ::core::result::Result::Err(::smear::value::Error::multiple(obj, errors));
              }

              ::core::result::Result::Ok(Self {
                #(#fields_init)*
              })
            }
            val => ::core::result::Result::Err(::smear::value::Error::unexpected_type(val)),
          }
        }
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for ::core::option::Option<#name> {
        type Error = ::smear::value::Error;
        type Node = ::smear::apollo_parser::ast::Value;

        fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: ::core::marker::Sized,
        {
          match node {
            ::smear::apollo_parser::ast::Value::NullValue(_) => ::core::result::Result::Ok(None),
            val => <#name as ::smear::Diagnosticable>::parse(val).map(::core::option::Option::Some),
          }
        }
      }

      #[automatically_derived]
      impl ::smear::DiagnosticableValue for #name {}

      #[automatically_derived]
      impl ::smear::DiagnosticableObjectValue for #name {
        fn fields() -> &'static [&'static str] {
          &[#(#fields_name),*]
        }
      }

      #[automatically_derived]
      impl ::smear::DiagnosticableValue for ::core::option::Option<#name> {
        fn nullable() -> bool {
          true
        }
      }
    };
  })
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named))]
struct Object {
  ident: Ident,
  data: Data<Variant, FieldDetails>,
  rename_all: Option<RenameAll>,
}

#[derive(FromField)]
#[darling(attributes(smear))]
struct FieldDetails {
  ident: Option<Ident>,
  ty: syn::Type,
  rename: Option<String>,
  default: Option<DefaultAttribute>,
  validator: Option<Path>,
  parser: Option<Path>,
}

impl FieldDetails {
  fn rename(&self, rename_all: Option<RenameAll>) -> String {
    self.rename.clone().unwrap_or_else(|| {
      let name = self.ident.as_ref().unwrap().to_string();
      rename_all
        .map(|rename_all| rename_all.apply(&name))
        .unwrap_or(name)
    })
  }
}

#[derive(FromVariant)]
#[darling(attributes(smear))]
#[allow(dead_code)]
struct Variant {
  fields: Fields<FieldDetails>,
}

#[derive(FromMeta)]
enum DefaultAttribute {
  None,
  Expr(Expr),
  Path(Path),
}

impl ToTokens for DefaultAttribute {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match self {
      Self::None => tokens.extend(quote! { ::code::default::Default::default() }),
      Self::Expr(expr) => tokens.extend(quote! { { #expr } }),
      Self::Path(p) => tokens.extend(quote! { #p() }),
    }
  }
}

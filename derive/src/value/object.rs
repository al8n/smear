use darling::{
  ast::{Data, Fields},
  FromDeriveInput, FromField, FromVariant,
};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, DeriveInput, Generics, Ident, Path};

use crate::utils::{DefaultAttribute, PathAttribute, RenameAll};

pub fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let object: Object = Object::from_derive_input(&input)?;
  let name = &object.ident;

  if !object.generics.params.is_empty() {
    return syn::Result::Err(syn::Error::new(
      object.generics.span(),
      "structs with generics are not supported yet",
    ));
  }

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

          field.parser_tokenstream()
        } else {
          fields_null_check.push(quote! {
            if #field_ident.is_none() && !<#field_ty as ::smear::DiagnosticableValue>::nullable() {
              errors.push(::smear::error::ValueError::missing_object_field(obj, #missing_field_error));
            }
          });

          fields_declaration.push(quote! {
            let mut #field_ident: ::core::option::Option<#field_ty> = ::core::option::Option::None;
            let mut #field_ident_dirty: bool = false;
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

          field.parser_tokenstream()
        }?;

        Result::<_, syn::Error>::Ok(quote! {
          #field_rename => {
            if #field_ident_dirty {
              errors.push(::smear::error::ValueError::duplicated_object_field(&field, #duplicated_field_error));
              continue;
            }

            #field_ident_dirty = true;

            #parser
          },
        })
      })
      .collect::<Result<Vec<_>, _>>()?,
    _ => vec![],
  };

  Ok(quote! {
    const _: () = {
      #[automatically_derived]
      impl ::smear::Diagnosticable for #name {
        type Error = ::smear::error::ValueError;
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
                    errors.push(::smear::error::ValueError::missing_object_field_name(&field));
                  },
                  (::core::option::Option::Some(name), ::core::option::Option::None) => {
                    errors.push(::smear::error::ValueError::missing_object_value(&field, name.text().as_str()));
                  },
                  (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                    match name.text().as_str().trim() {
                      #(#field_parsing)*
                      n => {
                        errors.push(::smear::error::ValueError::unknown_object_field(&field, ::smear::error::ErrorUnknownObjectField::with_alts(n, [#(#fields_name),*])));
                      },
                    }
                  },
                }
              }

              #(#fields_null_check)*

              if !errors.is_empty() {
                return ::core::result::Result::Err(::smear::error::ValueError::multiple(obj, errors));
              }

              ::core::result::Result::Ok(Self {
                #(#fields_init)*
              })
            }
            val => ::core::result::Result::Err(::smear::error::ValueError::unexpected_type(val)),
          }
        }
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for ::core::option::Option<#name> {
        type Error = ::smear::error::ValueError;
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

#[derive(FromVariant)]
#[darling(attributes(smear))]
#[allow(dead_code)]
struct Variant {
  fields: Fields<FieldDetails>,
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named))]
struct Object {
  ident: Ident,
  generics: Generics,
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
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
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

  fn validator_tokenstream(&self) -> syn::Result<proc_macro2::TokenStream> {
    let field_ident = self.ident.as_ref().unwrap();
    let assign: proc_macro2::TokenStream = if self.default.is_some() {
      quote! {
        #field_ident = v;
      }
    } else {
      quote! {
        #field_ident = ::core::option::Option::Some(v);
      }
    };

    let handle_path = |p: &syn::Path| {
      quote! {
        match #p(&v) {
          ::core::result::Result::Ok(_) => {
            #assign
          },
          ::core::result::Result::Err(err) => {
            errors.push(::smear::error::ValueError::invalid_value(&val, err));
          },
        }
      }
    };

    Ok(match &self.validator {
      PathAttribute::None => quote! {
        #assign
      },
      PathAttribute::Path(p) => handle_path(p),
      PathAttribute::Str(p) => {
        let p = syn::parse_str::<Path>(p)?;
        handle_path(&p)
      }
    })
  }

  fn parser_tokenstream(&self) -> syn::Result<proc_macro2::TokenStream> {
    let field_ty = &self.ty;
    let validator = self.validator_tokenstream()?;
    let parser = self
      .parser
      .to_token_stream_with_default(quote! {&val}, || {
        Ok(quote!(<#field_ty as ::smear::Diagnosticable>::parse(&val)))
      })?;

    Ok(quote! {
      match #parser {
        ::core::result::Result::Ok(v) => {
          #validator
        },
        ::core::result::Result::Err(err) => {
          errors.push(err);
        },
      }
    })
  }
}

mod field;
pub(crate) use field::ArgumentField;
mod struct_;
pub(crate) use struct_::Argument;

use darling::FromDeriveInput;
use heck::ToPascalCase;
use indexmap::IndexSet;
use quote::{format_ident, quote};
use syn::{Data, Ident, Visibility};

use crate::utils::{Aliases, DefaultAttribute, Long, Optional, PathAttribute, RenameAll, Short};

pub(crate) fn derive(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let argument = Argument::from_derive_input(&input)?;
  match input.data {
    Data::Struct(data) => match data.fields {
      syn::Fields::Named(_) => unreachable!("should not be able to reach this branch"),
      syn::Fields::Unnamed(_) => argument.generate(),
      syn::Fields::Unit => argument.generate_unit(),
    },
    _ => unreachable!("should not be able to reach this branch"),
  }
}

struct ArgumentCodegen {
  ident: Ident,
  vis: Visibility,
  ty: syn::Type,
  short: Short,
  long: Long,
  aliases: Aliases,
  optional: Option<Optional>,
  default: Option<DefaultAttribute>,
  validator: PathAttribute,
  parser: PathAttribute,
}

impl ArgumentCodegen {
  /// Returns the name of the field as it should appear in the SDL.
  pub(crate) fn sdl_name(&self, rename_all: Option<RenameAll>) -> String {
    self.long.0.clone().unwrap_or_else(|| {
      let name = self.ident.to_string();
      rename_all
        .map(|rename_all| rename_all.apply(&name))
        .unwrap_or(name)
    })
  }

  /// Returns the name of the argument struct in sdl side.
  pub(crate) fn sdl_argument_struct_name(
    &self,
    parent_name: &str,
    rename_all: Option<RenameAll>,
  ) -> Ident {
    let name = self.sdl_name(rename_all);
    format_ident!("{}{}Argument", parent_name, name.to_pascal_case())
  }

  pub(crate) fn validate_names(&self, rename_all: Option<RenameAll>) -> syn::Result<()> {
    let name = self.sdl_name(rename_all);
    self.short.validate(&name)?;
    self.long.validate(&name)?;
    Ok(())
  }

  pub(crate) fn short(&self, rename_all: Option<RenameAll>) -> Option<char> {
    self.short.0.as_ref().map(|s| {
      s.unwrap_or_else(|| {
        self
          .sdl_name(rename_all)
          .chars()
          .next()
          .unwrap()
          .to_ascii_lowercase()
      })
    })
  }

  pub(crate) fn possible_names(&self, rename_all: Option<RenameAll>) -> Vec<String> {
    let mut suggestions = IndexSet::new();

    suggestions.insert(self.sdl_name(rename_all));

    if let Some(short) = self.short(rename_all) {
      suggestions.insert(short.to_string());
    }

    suggestions.extend(self.aliases.names.iter().cloned());
    suggestions.into_iter().collect()
  }

  pub(crate) fn generate_unit(
    &self,
    parent_name: &str,
    rename_all: Option<RenameAll>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    self.validate_names(rename_all)?;
    if !self.parser.is_none() {
      return Err(syn::Error::new_spanned(
        &self.ident,
        "parser attribute is not allowed on unit structs",
      ));
    }

    if !self.validator.is_none() {
      return Err(syn::Error::new_spanned(
        &self.ident,
        "validator attribute is not allowed on unit structs",
      ));
    }

    let vis = &self.vis;
    let possible_names = self.possible_names(rename_all);
    let short = match self.short(rename_all) {
      Some(short) => quote!(::core::option::Option::Some(#short)),
      None => quote!(::core::option::Option::None),
    };
    let long = self.sdl_name(rename_all);
    let aliases = &self.aliases.names;
    let struct_name = self.sdl_argument_struct_name(parent_name, rename_all);

    let (default, ty, parser) = match self.optional {
      Some(optional) if optional.is_optional() => {
        let default = match &self.default {
          Some(d) => quote!(#d),
          None => quote!(Self(::core::option::Option::None)),
        };
        let ty = quote!(::core::option::Option<bool>);
        let parser = quote!(::core::option::Option::Some(val.true_token().is_some()));

        (default, ty, parser)
      }
      _ => {
        let default = match &self.default {
          Some(d) => quote!(#d),
          None => quote!(Self(false)),
        };
        let ty = quote!(bool);
        let parser = quote!(val.true_token().is_some());

        (default, ty, parser)
      }
    };

    Ok(quote! {
      #[derive(
        ::core::fmt::Debug,
        ::core::clone::Clone,
        ::core::marker::Copy,
        ::core::cmp::PartialEq,
        ::core::cmp::Eq,
        ::core::cmp::PartialOrd,
        ::core::cmp::Ord,
        ::core::hash::Hash,
        ::smear::derive_more::From,
        ::smear::derive_more::Into,
        ::smear::derive_more::AsRef,
        ::smear::derive_more::AsMut,
      )]
      #[repr(transparent)]
      #[automatically_derived]
      #vis struct #struct_name(#ty);

      #[automatically_derived]
      impl ::core::default::Default for #struct_name {
        fn default() -> Self {
          #default
        }
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for #struct_name {
        type Error = ::smear::error::ArgumentError;
        type Node = ::smear::apollo_parser::ast::Argument;

        fn parse(arg: &Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized
        {
          match arg {
            ::smear::apollo_parser::ast::Value::NullValue(_) => ::core::result::Result::Ok(#default),
            ::smear::apollo_parser::ast::Value::BooleanValue(val) => ::core::result::Result::Ok(#parser),
            val => ::core::result::Result::Err(::smear::error::ArgumentError::invalid_value(&arg, err)),
          }
        }
      }

      #[automatically_derived]
      impl ::smear::NamedDiagnosticable for #struct_name {
        fn possible_names() -> &'static [&'static str] {
          &[#(#possible_names),*]
        }

        fn short() -> ::core::option::Option<char> {
          #short
        }

        fn long() -> &'static str {
          #long
        }

        fn aliases() -> &'static [&'static str] {
          &[#(#aliases),*]
        }
      }

      #[automatically_derived]
      impl ::smear::DiagnosticableArgument for #struct_name {}
    })
  }

  pub(crate) fn generate(
    &self,
    parent_name: &str,
    rename_all: Option<RenameAll>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    self.validate_names(rename_all)?;
    let vis = &self.vis;
    let possible_names = self.possible_names(rename_all);
    let short = match self.short(rename_all) {
      Some(short) => quote!(::core::option::Option::Some(#short)),
      None => quote!(::core::option::Option::None),
    };
    let long = self.sdl_name(rename_all);
    let aliases = &self.aliases.names;

    let validator = self.validator.to_token_stream_with_default_and_custom(|| Ok(quote!(Self(val))), |p| {
      Ok(quote! {#p(&val).map(Self).map_err(|err| ::smear::error::ArgumentError::invalid_value(&arg, err))})
    })?;

    let ty = match self.optional {
      Some(optional) if optional.is_optional() => {
        let ty = &self.ty;
        quote!(::core::option::Option<#ty>)
      }
      _ => {
        let ty = &self.ty;
        quote!(#ty)
      }
    };

    let (parser, missing_value_branch) = match self.optional {
      Some(optional) if optional.is_optional() => match &self.default {
        Some(default) => {
          let parser = self
            .parser
            .to_token_stream_with_default(quote! {&val}, || {
              Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse_with_default(&val, #default)))
            })?;
          let missing_value_branch = quote! {::core::result::Result::Ok(Self(#default))};
          (parser, missing_value_branch)
        }
        None => {
          let parser = self
            .parser
            .to_token_stream_with_default(quote! {&val}, || {
              Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse_optional(&val)))
            })?;
          let missing_value_branch =
            quote! {::core::result::Result::Ok(Self(::core::option::Option::None))};
          (parser, missing_value_branch)
        }
      },
      _ => match &self.default {
        Some(default) => {
          let parser = self
            .parser
            .to_token_stream_with_default(quote! {&val}, || {
              Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse_with_default(&val, #default)))
            })?;
          let missing_value_branch = quote! {
            ::core::result::Result::Ok(Self(#default))
          };
          (parser, missing_value_branch)
        }
        None => {
          let parser = self
            .parser
            .to_token_stream_with_default(quote! {&val}, || {
              Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse(&val)))
            })?;
          let missing_value_branch = quote! {::core::result::Result::Err(::smear::error::ArgumentError::missing_argument_value(&arg, name.text().to_string(), ::core::option::Option::None))};
          (parser, missing_value_branch)
        }
      },
    };

    let struct_name = self.sdl_argument_struct_name(parent_name, rename_all);
    Ok(quote! {
      #vis #struct_name(#vis #ty);

      #[automatically_derived]
      impl ::smear::Diagnosticable for #struct_name {
        type Error = ::smear::error::ArgumentError;
        type Node = ::smear::apollo_parser::ast::Argument;

        fn parse(arg: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: Sized
        {
          use ::smear::apollo_parser::ast::AstNode;

          match (arg.name(), arg.value()) {
            (::core::option::Option::None, ::core::option::Option::None) => ::core::result::Result::Err(::smear::error::ArgumentError::invalid(&arg)),
            (::core::option::Option::None, ::core::option::Option::Some(_)) => {
              ::core::result::Result::Err(::smear::error::ArgumentError::missing_argument_name(&arg))
            },
            (::core::option::Option::Some(name), ::core::option::Option::None) => {
              match name.text().as_str().trim() {
                #(#possible_names)|* => {
                  #missing_value_branch
                },
                name => {
                  ::core::result::Result::Err(::smear::error::ArgumentError::unknown_argument(&arg, name, &[#(#possible_names),*]))
                }
              }
            },
            (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
              match name.text().as_str().trim() {
                #(#possible_names)|* => {
                  match #parser {
                    ::core::result::Result::Ok(val) => {
                      #validator
                    }
                    ::core::result::Result::Err(err) => {
                      ::core::result::Result::Err(::smear::error::ArgumentError::invalid_value(&arg, err))
                    }
                  }
                },
                name => {
                  ::core::result::Result::Err(::smear::error::ArgumentError::unknown_argument(&arg, name, &[#(#possible_names),*]))
                }
              }
            },
          }
        }
      }

      #[automatically_derived]
      impl ::smear::NamedDiagnosticable for #struct_name {
        fn possible_names() -> &'static [&'static str] {
          &[#(#possible_names),*]
        }

        fn short() -> ::core::option::Option<char> {
          #short
        }

        fn long() -> &'static str {
          #long
        }

        fn aliases() -> &'static [&'static str] {
          &[#(#aliases),*]
        }
      }

      #[automatically_derived]
      impl ::smear::DiagnosticableArgument for #struct_name {}
    })
  }
}

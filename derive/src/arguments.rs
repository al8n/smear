
mod field;
pub(crate) use field::ArgumentField;
mod struct_;
pub(crate) use struct_::Argument;

use darling::FromDeriveInput;
use heck::ToPascalCase;
use indexmap::IndexSet;
use quote::{quote, format_ident};
use syn::{Ident, Visibility, Data};

use crate::utils::{Short, Long, Aliases, DefaultAttribute, PathAttribute, RenameAll, Optional};

pub(crate) fn derive(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let argument  = Argument::from_derive_input(&input)?;
  match input.data {
    Data::Struct(data) => {
      match data.fields {
        syn::Fields::Named(_) => unreachable!("should not be able to reach this branch"),
        syn::Fields::Unnamed(_) => argument.generate(),
        syn::Fields::Unit => argument.generate_unit(),
      }
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
  pub(crate) fn ty(&self) -> &syn::Type {
    &self.ty
  }

  pub(crate) fn vis(&self) -> &Visibility {
    &self.vis
  }

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
  pub(crate) fn sdl_argument_struct_name(&self, parent_name: &str, rename_all: Option<RenameAll>) -> Ident {
    let name = self.sdl_name(rename_all);
    format_ident!("{}{}Argument", parent_name, name.to_pascal_case())
  }

  pub(crate) fn validate(&self, rename_all: Option<RenameAll>) -> syn::Result<()> {
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

  pub(crate) fn generate(&self, parent_name: &str, rename_all: Option<RenameAll>) -> syn::Result<proc_macro2::TokenStream> {
    self.validate(rename_all)?;

    let ty = &self.ty;
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

    let parser = match &self.default {
      Some(default) => self
        .parser
        .to_token_stream_with_default(quote! {&val}, || {
          Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse_with_default(&val, #default)))
        })?,
      None => self
        .parser
        .to_token_stream_with_default(quote! {&val}, || {
          Ok(quote!(<#ty as ::smear::DiagnosticableValue>::parse(&val)))
        })?,
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
              ::core::result::Result::Err(::smear::error::ArgumentError::missing_argument_value(&arg, name.text().to_string(), ::core::option::Option::None))
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
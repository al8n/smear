use darling::{
  ast::{Data, Fields, Style},
  FromDeriveInput, FromVariant,
};
use heck::{ToLowerCamelCase, ToPascalCase};
use indexmap::IndexSet;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use smear_types::directive::On;
use syn::{spanned::Spanned, DeriveInput, Generics, Ident, Visibility};

use crate::utils::{
  Aliases, Attributes, DefaultAttribute, Deprecated, Description, Long, RenameAll, Short,
};

mod argument;
use argument::Argument;

pub fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let obj = Directive::from_derive_input(&input)?;
  obj.validate_sdl_variable_name()?;

  if !obj.generics.params.is_empty() {
    return syn::Result::Err(syn::Error::new(
      obj.generics.span(),
      "structs with generics are not supported yet",
    ));
  }

  let token = match &obj.data {
    Data::Enum(_) => unreachable!("currently only structs are supported"),
    Data::Struct(data) => match data.style {
      Style::Tuple => unreachable!("currently only named structs and unit structs are supported"),
      Style::Struct => {
        if data.fields.is_empty() {
          obj.generate_unit()?
        } else {
          obj.generate_struct(data)?
        }
      }
      Style::Unit => obj.generate_unit()?,
    },
  };

  Ok(token)
}

#[derive(FromVariant)]
#[darling(attributes(smear))]
#[allow(dead_code)]
struct Variant {
  fields: Fields<Argument>,
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named, struct_unit))]
struct Directive {
  ident: Ident,
  vis: Visibility,
  generics: Generics,
  #[darling(default)]
  description: Description,
  #[darling(default)]
  deprecated: Deprecated,
  #[darling(default)]
  attributes: Attributes,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
  on: On,
  default: Option<DefaultAttribute>,
  data: Data<Variant, Argument>,
  rename_all: Option<RenameAll>,
}

impl Directive {
  /// Returns the name of the directive as it should be used in the SDL.
  ///
  /// e.g.
  /// ```graphql
  /// type BarDirective { arg1: String, arg2: Int }
  ///
  /// type FooStruct @bar {
  ///   field1: String,
  ///   field2: Int,
  /// }
  /// ```
  fn sdl_variable_name(&self) -> String {
    self
      .long
      .0
      .clone()
      .unwrap_or_else(|| self.ident.to_string().to_lower_camel_case())
  }

  /// Returns the name of the argument struct in sdl side. This fn is helpful to generate the rules schema
  fn sdl_directive_struct_name(&self) -> Ident {
    let name = self.sdl_variable_name().to_pascal_case();
    format_ident!("{name}Directive")
  }

  fn validate_sdl_variable_name(&self) -> syn::Result<()> {
    let name = self.sdl_variable_name();
    self.short.validate(&name)?;
    self.long.validate(&name)?;
    Ok(())
  }

  fn short(&self) -> Option<char> {
    self.short.0.as_ref().map(|s| {
      s.unwrap_or_else(|| {
        self
          .sdl_variable_name()
          .chars()
          .next()
          .unwrap()
          .to_ascii_lowercase()
      })
    })
  }

  fn possible_names(&self) -> Vec<String> {
    let mut suggestions = IndexSet::new();

    suggestions.insert(self.sdl_variable_name());

    if let Some(short) = self.short() {
      suggestions.insert(short.to_string());
    }

    suggestions.extend(self.aliases.names.iter().cloned());
    suggestions.into_iter().collect()
  }

  fn possible_argument_names(&self, fields: &Fields<Argument>) -> Vec<String> {
    let mut names = IndexSet::new();
    for field in fields.iter() {
      names.extend(field.possible_names(self.rename_all));
    }
    names.into_iter().collect()
  }

  fn generate_struct(&self, fields: &Fields<Argument>) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = self.sdl_directive_struct_name();
    let parse_struct_name = format_ident!("{}Parser", struct_name);
    let possible_names = self.possible_names();
    let available_argument_names = self.possible_argument_names(fields);
    let short = match self.short() {
      Some(short) => quote!(::core::option::Option::Some(#short)),
      None => quote!(::core::option::Option::None),
    };
    let long = self.sdl_variable_name();
    let aliases = &self.aliases.names;
    let vis = &self.vis;

    let mut helper = Helper::default();
    for field in fields.iter() {
      field.to_tokens(self.rename_all, &mut helper)?;
    }

    let Helper {
      struct_fields_definitions,
      parse_helper_struct_fields_definitions,
      parse_helper_struct_fields_default,
      missing_argument_value_handlers,
      argument_handlers,
      required_arguments_name,
      dirty_checks,
      dirty_definitions,
      converts,
      available_arguments,
      required_arguments,
      optional_arguments,
    } = helper;
    let len_available_arguments = available_arguments.len();
    let len_required_arguments = required_arguments.len();
    let len_optional_arguments = optional_arguments.len();

    let empty_branch = match &self.default {
      Some(default) => quote!(::core::result::Result::Ok(#default)),
      None => {
        quote!(::core::result::Result::Err(::smear::error::DirectiveError::missing_arguments(directive, directive_name, [#(#required_arguments_name),*].into_iter())))
      }
    };

    let inherit_impls = Self::generate_inherit_impls();
    let on = &self.on;
    let desc = self.description.to_tokens();
    let deprecated = &self.deprecated;

    let attrs = &self.attributes;
    Ok(quote! {
      #attrs
      #[automatically_derived]
      #vis struct #struct_name {
        #(#struct_fields_definitions)*
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for #struct_name {
        type Error = ::smear::error::DirectiveError;
        type Node = ::smear::apollo_parser::ast::Directive;
        type Descriptor = ::smear::directive::DirectiveDescriptor;

        fn parse(directive: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: Sized
        {
          struct #parse_struct_name {
            #(#parse_helper_struct_fields_definitions)*
          }

          impl ::core::default::Default for #parse_struct_name {
            fn default() -> Self {
              Self {
                #(#parse_helper_struct_fields_default)*
              }
            }
          }

          impl ::core::convert::From<#parse_struct_name> for #struct_name {
            fn from(parser: #parse_struct_name) -> Self {
              Self {
                #(#converts),*
              }
            }
          }

          let directive_name = directive.name().map(|n| n.text().to_string()).unwrap_or_default();
          if let ::core::option::Option::Some(args) = directive.arguments() {
            let mut parser = #parse_struct_name::default();
            let mut errors = ::std::vec::Vec::new();
            let mut missing_arguments = ::std::vec::Vec::new();

            #(#dirty_definitions)*

            for arg in args.arguments() {
              match (arg.name(), arg.value()) {
                (::core::option::Option::None, ::core::option::Option::None) => {
                  errors.push(::smear::error::DirectiveError::invalid_argument(&arg));
                },
                (::core::option::Option::None, ::core::option::Option::Some(_)) => {
                  errors.push(::smear::error::DirectiveError::missing_argument_name(&arg));
                },
                (::core::option::Option::Some(name), ::core::option::Option::None) => {
                  let name_text = name.text();
                  let name_str = name_text.as_str().trim();
                  match name_str {
                    #(#missing_argument_value_handlers)*
                    name => {
                      errors.push(::smear::error::DirectiveError::unknown_argument(&arg, directive_name.clone(), name_str, &[#(#available_argument_names),*]));
                    }
                  }
                },
                (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                  let name_text = name.text();
                  let name_str = name_text.as_str().trim();
                  match name_str {
                    #(#argument_handlers)*
                    name => {
                      errors.push(::smear::error::DirectiveError::unknown_argument(&arg, directive_name.clone(), name, &[#(#available_argument_names),*]));
                    }
                  }
                },
              }
            }

            #(#dirty_checks)*

            if !missing_arguments.is_empty() {
              errors.push(::smear::error::DirectiveError::missing_arguments(directive, directive_name, missing_arguments.into_iter()));
            }

            if !errors.is_empty() {
              return ::core::result::Result::Err(::smear::error::DirectiveError::multiple(directive, errors));
            }

            ::core::result::Result::Ok(::core::convert::From::from(parser))
          } else {
            #empty_branch
          }
        }

        fn descriptor() -> &'static Self::Descriptor {
          use ::smear::__exports::once_cell::sync::Lazy;
          static AVAILABLE_ARGUMENTS: Lazy<[::smear::directive::ArgumentDescriptor; #len_available_arguments]> = Lazy::new(|| [
            #(#available_arguments),*
          ]);
          static REQUIRED_ARGUMENTS: Lazy<[::smear::directive::ArgumentDescriptor; #len_required_arguments]> = Lazy::new(|| [
            #(#required_arguments),*
          ]);
          static OPTIONAL_ARGUMENTS: Lazy<[::smear::directive::ArgumentDescriptor; #len_optional_arguments]> = Lazy::new(|| [
            #(#optional_arguments),*
          ]);

          static DESCRIPTOR: ::std::sync::OnceLock<::smear::directive::DirectiveDescriptor> = ::std::sync::OnceLock::new();
          DESCRIPTOR.get_or_init(|| {
            ::smear::directive::DirectiveDescriptor {
              name: #long,
              short: #short,
              aliases: &[#(#aliases),*],
              available_names: &[#(#possible_names),*],
              locations: #on,
              description: #desc,
              deprecated: #deprecated,
              arguments: ::smear::directive::ArgumentsDescriptor {
                available_arguments: &*AVAILABLE_ARGUMENTS,
                required_arguments: &*REQUIRED_ARGUMENTS,
                optional_arguments: &*OPTIONAL_ARGUMENTS,
              },
            }
          })
        }
      }

      #inherit_impls
    })
  }

  fn generate_unit(&self) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = self.sdl_directive_struct_name();
    let possible_names = self.possible_names();
    let short = match self.short() {
      Some(short) => quote!(::core::option::Option::Some(#short)),
      None => quote!(::core::option::Option::None),
    };
    let long = self.sdl_variable_name();
    let aliases = &self.aliases.names;
    let vis = &self.vis;
    let on = &self.on;
    let inherit_impls = Self::generate_inherit_impls();
    let desc = self.description.to_tokens();
    let deprecated = &self.deprecated;
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
      #vis struct #struct_name(bool);

      #[automatically_derived]
      impl ::core::default::Default for #struct_name {
        fn default() -> Self {
          Self(false)
        }
      }

      #[automatically_derived]
      impl ::core::cmp::PartialEq<bool> for #struct_name {
        fn eq(&self, other: &bool) -> bool {
          self.0 == *other
        }
      }

      #[automatically_derived]
      impl ::core::cmp::PartialEq<#struct_name> for bool {
        fn eq(&self, other: &#struct_name) -> bool {
          other.0 == *self
        }
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for #struct_name {
        type Error = ::smear::error::DirectiveError;
        type Node = ::smear::apollo_parser::ast::Directive;
        type Descriptor = ::smear::directive::DirectiveDescriptor;

        fn descriptor() -> &'static Self::Descriptor {
          const DESCRIPTOR: &::smear::directive::DirectiveDescriptor = &::smear::directive::DirectiveDescriptor {
            name: #long,
            short: #short,
            aliases: &[#(#aliases),*],
            description: #desc,
            deprecated: #deprecated,
            available_names: &[#(#possible_names),*],
            locations: #on,
            available_arguments: &::smear::directive::ArgumentsDescriptor {
              available_arguments: &[],
              required_arguments: &[],
              optional_arguments: &[],
            }
          };
          DESCRIPTOR
        }

        fn parse(node: &Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized
        {
          use ::smear::apollo_parser::ast::AstNode;

          if let ::core::option::Option::Some(args) = node.arguments() {
            let mut errors = ::std::vec::Vec::new();
            let name = directive.name().map(|n| n.text().to_string()).unwrap_or_default();
            for arg in args.arguments() {
              errors.push(::smear::error::DirectiveError::unknown_argument(&arg, name.clone(), arg.name().map(|n| n.text().to_string()).unwrap_or_default(), &[]));
            }
            if !errors.is_empty() {
              return ::core::result::Result::Err(::smear::error::DirectiveError::multiple(node, errors));
            }
          }

          ::core::result::Result::Ok(Self(true))
        }
      }

      #[automatically_derived]
      impl #struct_name {
        /// Returns the value of the directive.
        #vis const fn get(&self) -> bool {
          self.0
        }

        /// Sets the value of the directive.
        #vis fn set(&mut self, value: bool) {
          self.0 = value;
        }

        /// Returns `Some(t)` if is `true`, or `None` otherwise.
        /// Arguments passed to `then_some` are eagerly evaluated;
        /// if you are passing the result of a function call,
        /// it is recommended to use `then`, which is lazily evaluated.
        #vis fn then_some<T>(self, t: T) -> ::core::option::Option<T> {
          self.0.then_some(t)
        }

        /// Returns `Some(f())` if is true, or `None` otherwise.
        #vis fn then<T, F>(self, f: F) -> ::core::option::Option<T>
        where
          F: ::core::ops::FnOnce() -> T,
        {
          self.0.then(f)
        }
      }

      #inherit_impls
    })
  }

  fn generate_inherit_impls() -> TokenStream {
    quote! {}
  }
}

#[derive(Default)]
struct Helper {
  struct_fields_definitions: Vec<TokenStream>,
  parse_helper_struct_fields_definitions: Vec<TokenStream>,
  parse_helper_struct_fields_default: Vec<TokenStream>,
  missing_argument_value_handlers: Vec<TokenStream>,
  argument_handlers: Vec<TokenStream>,
  required_arguments_name: Vec<String>,
  dirty_definitions: Vec<TokenStream>,
  dirty_checks: Vec<TokenStream>,
  converts: Vec<TokenStream>,
  available_arguments: Vec<TokenStream>,
  required_arguments: Vec<TokenStream>,
  optional_arguments: Vec<TokenStream>,
}

use darling::{
  ast::{Data, Fields, Style},
  FromDeriveInput, FromVariant,
};
use heck::{ToLowerCamelCase, ToPascalCase};
use indexmap::IndexSet;
use quote::{format_ident, quote};
use syn::{spanned::Spanned, DeriveInput, Generics, Ident, Visibility};

use crate::{
  arguments::ArgumentField,
  utils::{Aliases, Attributes, DefaultAttribute, Long, RenameAll, Short},
};

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
  fields: Fields<ArgumentField>,
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named, struct_unit))]
struct Directive {
  ident: Ident,
  vis: Visibility,
  generics: Generics,
  #[darling(default)]
  attributes: Attributes,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
  default: Option<DefaultAttribute>,
  data: Data<Variant, ArgumentField>,
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

  fn possible_argument_names(&self, fields: &Fields<ArgumentField>) -> Vec<String> {
    let mut names = IndexSet::new();
    for field in fields.iter() {
      names.extend(field.possible_names(self.rename_all));
    }
    names.into_iter().collect()
  }

  fn generate_struct(
    &self,
    fields: &Fields<ArgumentField>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = format_ident!("{}Directive", self.sdl_directive_struct_name());
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

    let mut struct_fields_definitions = Vec::new();
    let mut parse_helper_struct_fields_definitions = Vec::new();
    let mut parse_helper_struct_fields_default = Vec::new();
    let mut missing_argument_value_handlers = Vec::new();
    let mut argument_handlers = Vec::new();
    let mut required_arguments_name = Vec::new();
    let mut dirty_checks = Vec::new();
    let mut converts = Vec::new();
    for field in fields.iter() {
      let field_name = field.sdl_name(self.rename_all);
      let field_name_ident = format_ident!("{field_name}");
      let dirty = format_ident!("{field_name_ident}_dirty");
      let field_vis = field.vis();
      let field_attrs = field.attributes();
      let field_possible_names = field.possible_names(self.rename_all);
      let field_parser = field.parser();
      let field_validator = field.validator();
      let optional = field.optional();
      let default_attr = field.default();

      let field_ty = if optional {
        let ty = field.ty();
        quote!(::core::option::Option<#ty>)
      } else {
        required_arguments_name.push(field_name.clone());
        let ty = field.ty();
        quote!(#ty)
      };

      let (parse_helper_field_ty, default_parse_helper_field, parser, validator, converter) =
        match (optional, default_attr) {
          (true, None) => {
            let parser_fn = match field_parser.path()? {
              Some(p) => quote!(#p(&val)),
              None => quote!(<#field_ty as ::smear::DiagnosticableValue>::parse_optional(&val)),
            };
            let parser = quote! {
              match #parser_fn {
                ::core::result::Result::Ok(parsed) => {
                  parser.#field_name_ident = parsed;
                }
                ::core::result::Result::Err(err) => {
                  errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  continue;
                }
              }
            };
            let validator = match field_validator.path()? {
              Some(p) => quote! {
                if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
                  if let ::core::result::Result::Err(e) = #p(parsed) {
                    errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  }
                }
              },
              None => quote!(),
            };

            (
              quote!(::core::option::Option<#field_ty>),
              quote!(::core::option::Option::None),
              parser,
              validator,
              quote!(#field_name_ident: parser.#field_name_ident),
            )
          }
          (true, Some(default)) => {
            let parser_fn = match field_parser.path()? {
              Some(p) => quote!(#p(&val)),
              None => quote!(<#field_ty as ::smear::DiagnosticableValue>::parse_optional(&val)),
            };
            let parser = quote! {
              match #parser_fn {
                ::core::result::Result::Ok(parsed) => {
                  parser.#field_name_ident = parsed;
                }
                ::core::result::Result::Err(err) => {
                  errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  continue;
                }
              }
            };
            let validator = match field_validator.path()? {
              Some(p) => quote! {
                if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
                  if let ::core::result::Result::Err(e) = #p(parsed) {
                    errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  }
                }
              },
              None => quote!(),
            };

            (
              quote!(::core::option::Option<#field_ty>),
              quote!(::core::option::Option::Some(#default)),
              parser,
              validator,
              quote!(#field_name_ident: parser.#field_name_ident),
            )
          }
          (false, None) => {
            let parser_fn = match field_parser.path()? {
              Some(p) => quote!(#p(&val)),
              None => quote!(<#field_ty as ::smear::DiagnosticableValue>::parse(&val)),
            };
            let parser = quote! {
              match #parser_fn {
                ::core::result::Result::Ok(parsed) => {
                  parser.#field_name_ident = ::core::option::Option::Some(parsed);
                }
                ::core::result::Result::Err(err) => {
                  errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  continue;
                }
              }
            };
            let validator = match field_validator.path()? {
              Some(p) => quote! {
                if let ::core::option::Option::Some(ref parsed) = parser.#field_name_ident {
                  if let ::core::result::Result::Err(e) = #p(parsed) {
                    errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  }
                }
              },
              None => quote! {},
            };
            (
              quote!(::core::option::Option<#field_ty>),
              quote!(::core::option::Option::None),
              parser,
              validator,
              quote!(#field_name_ident: parser.#field_name_ident.unwrap()),
            )
          }
          (false, Some(default)) => {
            let parser_fn = match field_parser.path()? {
              Some(p) => quote!(#p(&val)),
              None => quote!(<#field_ty as ::smear::DiagnosticableValue>::parse_optional(&val)),
            };
            let parser = quote! {
              match #parser_fn {
                ::core::result::Result::Ok(parsed) => {
                  parser.#field_name_ident = parsed;
                }
                ::core::result::Result::Err(err) => {
                  errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                  continue;
                }
              }
            };
            let validator = match field_validator.path()? {
              Some(p) => quote! {
                if let ::core::result::Result::Err(e) = #p(&parser.#field_name_ident) {
                  errors.push(::smear::error::DirectiveError::invalid_argument_value(&arg, err));
                }
              },
              None => quote!(),
            };
            (
              quote!(#field_ty),
              quote!(#default),
              parser,
              validator,
              quote!(#field_name_ident: parser.#field_name_ident),
            )
          }
        };
      struct_fields_definitions.push(quote!(
        #field_attrs
        #field_vis #field_name_ident: #field_ty,
      ));
      parse_helper_struct_fields_definitions.push(quote!(
        #field_name_ident: #parse_helper_field_ty,
      ));
      parse_helper_struct_fields_default.push(quote!(
        #field_name_ident: #default_parse_helper_field,
      ));
      argument_handlers.push(quote! {
        #(#field_possible_names)|* => {
          if #dirty {
            errors.push(::smear::error::DirectiveError::duplicated_argument(&arg));
            continue;
          }
          #dirty = true;

          #parser

          #validator
        },
      });
      missing_argument_value_handlers.push(quote! {
        #(#field_possible_names)|* => {
          if !#dirty {
            #dirty = true;
          } else {
            errors.push(::smear::error::DirectiveError::duplicated_argument(&arg));
          }
        },
      });
      converts.push(converter);
      if !optional && default_attr.is_none() {
        dirty_checks.push(quote! {
          if !#dirty {
            errors.push(::smear::error::DirectiveError::missing_required_argument(&arg));
          }
        });
      }
    }

    let empty_branch = match &self.default {
      Some(default) => quote!(::core::result::Result::Ok(#default)),
      None => {
        quote!(::core::result::Result::Err(::smear::error::DirectiveError::missing_arguments(&[#(#required_arguments_name),*])))
      }
    };

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

          if let ::core::option::Option::Some(args) = directive.arguments() {
            let mut parser = #parse_struct_name::default();
            let mut errors = ::std::vec::Vec::new();
            for arg in args.arguments() {
              let arg_name = arg.name();
              let arg_value = arg.value();
              match (arg.name(), arg.value()) {
                (::core::option::Option::None, ::core::option::Option::None) => {
                  errors.push(::smear::error::ArgumentError::invalid(&arg));
                },
                (::core::option::Option::None, ::core::option::Option::Some(_)) => {
                  errors.push(::smear::error::DirectiveError::missing_argument_name(&arg));
                },
                (::core::option::Option::Some(name), ::core::option::Option::None) => {
                  let name_str = name.text().as_str().trim();
                  match name_str {
                    #(#missing_argument_value_handlers)*
                    name => {
                      errors.push(::smear::error::ArgumentError::unknown_argument(&arg, name_str, &[#(#available_argument_names),*]));
                    }
                  }
                },
                (::core::option::Option::Some(name), ::core::option::Option::Some(val)) => {
                  let name_str = name.text().as_str().trim();
                  match name_str {
                    #(#argument_handlers)*
                    name => {
                      ::core::result::Result::Err(::smear::error::ArgumentError::unknown_argument(&arg, name, &[#(#available_argument_names),*]))
                    }
                  }
                },
              }
            }

            #(#dirty_checks)*

            if !errors.is_empty() {
              return ::core::result::Result::Err(::smear::error::DirectiveError::multiple(&directive, errors));
            }

            ::core::result::Result::Ok(::core::convert::From::from(parser))
          } else {
            #empty_branch
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
      impl ::smear::DiagnosticableDirective for #struct_name {
        fn available_argument_names() -> &'static [&'static str] {
          &[#(#available_argument_names),*]
        }
      }

    })
  }

  fn generate_unit(&self) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = format_ident!("{}Directive", self.sdl_directive_struct_name());
    let possible_names = self.possible_names();
    let short = match self.short() {
      Some(short) => quote!(::core::option::Option::Some(#short)),
      None => quote!(::core::option::Option::None),
    };
    let long = self.sdl_variable_name();
    let aliases = &self.aliases.names;
    let vis = &self.vis;

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

        fn parse(node: &Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized
        {
          if let ::core::option::Option::Some(args) = node.arguments() {
            ::core::result::Result::Err(::smear::error::DirectiveError::unexpected_arguments(&args))
          } else {
            ::core::result::Result::Ok(Self(true))
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
    })
  }
}

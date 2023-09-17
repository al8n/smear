use darling::{ast::Data, FromDeriveInput, FromVariant};
use heck::ToShoutySnakeCase;
use quote::{format_ident, quote};
use syn::{Ident, Type, Visibility};

use crate::utils::{Deprecated, Description, Directive, PathAttribute, RenameAll, Directives, ProcessedDirectives, SafeIdent};

pub(crate) fn derive(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let enum_ = Enum::from_derive_input(&input)?;

  enum_.derive()
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(enum_unit))]
struct Enum {
  ident: Ident,
  name: Option<String>,
  vis: Visibility,
  #[darling(default = "RenameAll::graphql_enum_style")]
  rename_all: RenameAll,
  data: Data<Variant, ()>,
  #[darling(default)]
  description: Description,
  #[darling(default)]
  deprecated: Deprecated,
  #[darling(default, multiple)]
  directives: Vec<Directive>,
  #[darling(default, multiple)]
  variant_directives: Vec<Directive>,
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
}

impl Enum {
  fn sdl_name(&self) -> String {
    self.name.clone().unwrap_or_else(|| self.ident.to_string())
  }

  fn enum_value_struct_name(&self) -> Ident {
    format_ident!("{}EnumValue", self.sdl_name())
  }

  fn enum_value(&self) -> proc_macro2::TokenStream {
    let name = self.enum_value_struct_name();
    let name_str = name.to_string();
    let vis = &self.vis;
    let variants = self.data.as_ref().take_enum().unwrap();

    let from_str = variants.iter().map(|v| {
      let sdl_name = v.sdl_name(self.rename_all);
      let ident = SafeIdent::from(sdl_name.as_str());
      quote!(#sdl_name => ::core::result::Result::Ok(#name::#ident))
    }).collect::<Vec<_>>();
    let as_str = variants.iter().map(|v| {
      let sdl_name = v.sdl_name(self.rename_all);
      let ident = SafeIdent::from(sdl_name.as_str());
      quote!(#name::#ident => #sdl_name)
    });
    let variants_str = variants.iter().map(|v| {
      let sdl_name = v.sdl_name(self.rename_all);
      quote!(#sdl_name)
    });
    let variants = variants.iter().map(|v| {
      SafeIdent::from(v.sdl_name(self.rename_all).as_str())
    });

    quote! {
      #[derive(::core::marker::Copy, ::core::clone::Clone)]
      #vis enum #name {
        #(#variants),*
      }

      impl ::core::str::FromStr for #name {
        type Err = &'static str;

        fn from_str(src: &str) -> ::core::result::Result::<Self, Self::Err> {
          match src.trim() {
            #(#from_str),*,
            src => ::core::result::Result::Err("Unknown enum value"),
          }
        }
      }

      impl ::core::fmt::Display for #name {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
          ::core::fmt::Display::fmt(self.as_str(), f)
        }
      }

      impl ::smear::__exports::Diagnosticable for #name {
        type Error = ::smear::__exports::value::ValueError;
        type Node = ::smear::__exports::apollo_parser::ast::Value;
        type Descriptor = ::smear::__exports::definition::ValueDescriptor;

        fn descriptor() -> &'static Self::Descriptor {
          static DESCRIPTOR: ::smear::__exports::definition::ValueDescriptor =
            ::smear::__exports::value::ValueDescriptor {
              name: #name_str,
              description: ::core::option::Option::None,
              kind: ::smear::__exports::value::ValueKind::Enum {
                variants: Self::VARIANTS_STR,
              },
            };
          DESCRIPTOR
        }

        fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: ::core::marker::Sized,
        {
          match val {
            ::smear::__exports::value::Value::EnumValue(ev) => {
              let src = ev.text();
              match src.as_str() {
                #(#from_str),*,
                src => ::core::result::Result::Err(::smear::__exports::value::ValueError::unknown_enum_value(val, src, Self::VARIANTS_STR)),
              }
            }
            val => ::core::result::Result::Err(::smear::__exports::value::ValueError::unexpected_type(val)),
          }
        }
      }

      impl ::smear::__exports::value::Parser for #name {
        fn parse_value(val: &::smear::__exports::value::Value) -> ::core::fmt::Result<Self, ::smear::__exports::value::ValueError>
        where
          Self: ::core::marker::Sized
        {
          <Self as ::smear::__exports::Diagnosticable>::parse(val)
        }
      }

      impl #name {
        #vis const VARIANTS_STR: &'static [&'static str] = &[
          #(#variants_str),*
        ];

        #vis const fn as_str(&self) -> &'static str {
          match self {
            #(#as_str),*
          }
        }
      }
    }
  }

  fn derive(&self) -> syn::Result<proc_macro2::TokenStream> {
    let enum_name = &self.ident;
    let sdl_name = self.sdl_name();
    let enum_struct_name = format_ident!("{}EnumDefinition", sdl_name);
    let enum_values_name = format_ident!("{}EnumValuesDefinition", sdl_name);
    let enum_value_name = self.enum_value_struct_name();
    let vis = &self.vis;
    let description = &self.description;
    let deprecated = &self.deprecated;
    let error_ty = quote!(EnumError);
    let processed = Directives::new(self
      .directives
      .iter())
      .processed(&error_ty);
    let handlers = processed.directive_handlers(&error_ty);
    let ProcessedDirectives {
      fields,
      dirty_check_variable_names,
      null_checks,
      converts,
      parser_fields,
      parser_field_default,
      required_directives_descriptors,
      optional_directives_descriptors,
      available_directives_descriptors,
      ..
    } = &processed;

    let dirty_check_variables = dirty_check_variable_names.iter().map(|n| quote!(let mut #n = false;));
    let num_directives = available_directives_descriptors.len();
    let num_required_directives = required_directives_descriptors.len(); 
    let num_optional_directives = optional_directives_descriptors.len(); 

    let variants = self.data.as_ref().take_enum().unwrap();
    let num_variants = variants.len();

    let variant_directive_descriptors = variants.iter().map(|variant| {
      variant.directives_descriptors_definition(self.rename_all, &self.variant_directives)
    });
    let variant_descriptors = variants
      .iter()
      .map(|variant| variant.descriptor(self.rename_all));
    let parser = match self.parser.path() {
      Some(path) => quote!(#path(node)),
      None => quote!(__parser(node)),
    };
    let validator = match self.validator.path() {
      Some(path) => quote!(#path(&parsed)),
      None => quote!(::core::result::Result::Ok(())),
    };

    let empty_directives = if num_required_directives == 0 {
      quote!()
    } else {
      quote!(return ::core::result::Result::Err(::smear::__exports::error::EnumError::missing_directives(node, name, descriptor.required_directives()));)
    };

    Ok(quote! {
      #vis enum #enum_values_name {
        
      }

      #[::smear::__exports::viewit::viewit(vis_all = "", setters(skip), getters(style = "ref"))]
      #vis struct #enum_struct_name {
        #(#fields),*
        value: #enum_values_name,
      }

      const _: () = {
        fn __parser(node: &::smear::__exports::apollo_parser::EnumDefinition) -> ::core::result::Result<#enum_struct_name, ::smear::__exports::error::EnumError> {
          let descriptor = <#enum_struct_name as ::smear::__exports::Diagnosticable>::descriptor();
          
          struct Parser {
            #(#parser_fields),*
            value: ::core::option::Option<#enum_values_name>,
          }

          impl ::core::default::Default for Parser {
            fn default() -> Self {
              Self {
                #(#parser_field_default),*
                value: ::core::option::Option::None,
              }
            }
          }

          impl ::core::convert::From<Parser> for #enum_struct_name {
            fn from(parser: Parser) -> #enum_struct_name {
              #enum_struct_name {
                #(#converts),*
                value: parser.value.unwrap(),
              }
            }
          }

          let mut parser: Parser = ::core::default::Default::default();
          let mut errors = ::std::vec::Vec::new();
          match node.name() {
            ::core::option::Option::Some(name) => {
              let name = name.text().to_string();
              match node.directives() {
                ::core::option::Option::Some(directives) => {
                  #(#dirty_check_variables)*

                  #handlers
                }
                ::core::option::Option::None => {
                  #empty_directives
                }
              }

              #(#null_checks)*

              for values in node.enum_values_definition() {
                for value in values.enum_value_definitions() {
                  if let ::core::option::Option::Some(enum_val) = x.enum_value() {
                    let enum_val_txt = enum_val.text();
                    match enum_val_txt.as_str() {
                      // #(#variants),*,
                      src => {
                        errors.push(::smear::__exports::error::EnumError::unknown_enum_value(&value, src, #enum_value_name::VARIANTS_STR));
                      }
                    }
                  } else {
                    errors.push(::smear::__exports::error::EnumError::missing_enum_value(value)); 
                  }
                }
              }
            },
            ::core::option::Option::None => ::core::result::Result::Err(::smear::__exports::error::EnumError::missing_name(node)),
          }
        }

        impl ::smear::__exports::Diagnosticable for #enum_struct_name {
          type Node = ::smear::__exports::apollo_parser::ast::EnumDefinition;
          type Error = ::smear::__exports::error::EnumError;
          type Descriptor = ::smear::__exports::definition::EnumDescriptor;

          fn descriptor() -> &'static Self::Descriptor {
            use ::smear::__exports::once_cell::sync::Lazy;

            static DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_directives]> = Lazy::new(|| [
              #(#available_directives_descriptors),*
            ]);
            static REQUIRED_DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_required_directives]> = Lazy::new(|| [
              #(#required_directives_descriptors),*
            ]);
            static OPTIONAL_DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_optional_directives]> = Lazy::new(|| [
              #(#optional_directives_descriptors),*
            ]);

            static VARIANTS: Lazy<[&'static ::smear::__exports::definition::EnumValueDescriptor; #num_variants]> = Lazy::new(|| {
              #(#variant_directive_descriptors)*

              [
                #(#variant_descriptors),*
              ]
            });
            static DESCRIPTOR: Lazy<::smear::__exports::definition::EnumDescriptor> = Lazy::new(|| {
              ::smear::__exports::definition::EnumDescriptor {
                name: #sdl_name,
                description: #description,
                deprecated: #deprecated,
                available_directives: &*DIRECTIVES,
                required_directives: &*REQUIRED_DIRECTIVES,
                optional_directives: &*OPTIONAL_DIRECTIVES,
                variants: &*VARIANTS,
              }
            });
            &*DESCRIPTOR
          }

          fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
          where
            Self: ::core::marker::Sized,
          {
            match #parser {
              ::core::result::Result::Ok(parsed) => #validator.map(|_| parsed),
              ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
            }
          }
        }

        impl ::smear::__exports::Encodable for #enum_struct_name {
          type SDL = ::smear::__exports::apollo_encoder::EnumDefinition;

          fn encode() -> Self::SDL {
            ::core::convert::From::from(<Self as ::smear::__exports::Diagnosticable>::descriptor())
          }
        }
      };
    })
  }
}

#[derive(FromVariant)]
#[darling(attributes(smear))]
struct Variant {
  ident: Ident,
  name: Option<String>,
  #[darling(default)]
  description: Description,
  #[darling(default)]
  deprecated: Deprecated,
  #[darling(default, multiple)]
  directives: Vec<Directive>,
}

impl Variant {
  fn sdl_name(&self, rename_all: RenameAll) -> String {
    self
      .name
      .clone()
      .unwrap_or_else(|| rename_all.apply(&self.ident.to_string()))
  }

  fn directives_descriptors_name(&self, rename_all: RenameAll) -> Ident {
    format_ident!(
      "VARIANT_{}_DIRECTIVES",
      self.sdl_name(rename_all).to_shouty_snake_case()
    )
  }

  fn directives_descriptors_definition(
    &self,
    rename_all: RenameAll,
    common_directives: &[Directive],
  ) -> proc_macro2::TokenStream {
    let num_directives = self.directives.len() + common_directives.len();
    let directives = self
      .directives
      .iter()
      .chain(common_directives.iter())
      .map(|d| {
        let ty = &d.ty;
        quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor())
      });
    let static_directives_name = self.directives_descriptors_name(rename_all);
    quote! {
      static #static_directives_name: Lazy<[::smear::__exports::directive::DirectiveDescriptor; #num_directives]> = Lazy::new(|| [
        #(#directives),*
      ]);
    }
  }

  fn descriptor(&self, rename_all: RenameAll) -> proc_macro2::TokenStream {
    let name = self.sdl_name(rename_all);
    let description = &self.description;
    let deprecated = &self.deprecated;
    let directives_name = self.directives_descriptors_name(rename_all);
    quote! {
      ::smear::__exports::definition::EnumValueDescriptor {
        name: #name,
        description: #description,
        deprecated: #deprecated,
        directives: &*#directives_name,
      }
    }
  }

  fn derive(
    &self,
    vis: &Visibility,
    enum_name: &Ident,
    suffix: &Ident,
    rename_all: RenameAll,
    common_directives: &[Directive], 
  ) -> proc_macro2::TokenStream {
    let sdl_name = self.sdl_name(rename_all);
    let struct_name = format_ident!("{}{}", sdl_name, suffix);
    let description = &self.description;
    let deprecated = &self.deprecated;
    let error_ty = quote!(EnumError);
    let processed = Directives::new(self
      .directives
      .iter()
      .chain(common_directives.iter()))
      .processed(&error_ty);
    let handlers = processed.directive_handlers(&error_ty);
    let ProcessedDirectives {
      fields,
      dirty_check_variable_names,
      null_checks,
      converts,
      parser_fields,
      parser_field_default,
      required_directives_descriptors,
      optional_directives_descriptors,
      available_directives_descriptors,
      ..
    } = &processed;

    let dirty_check_variables = dirty_check_variable_names.iter().map(|n| quote!(let mut #n = false;));
    let num_directives = available_directives_descriptors.len();
    let num_required_directives = required_directives_descriptors.len(); 
    let num_optional_directives = optional_directives_descriptors.len();

    let empty_directives = if num_required_directives == 0 {
      quote!()
    } else {
      quote!(return ::core::result::Result::Err(::smear::__exports::error::EnumError::missing_directives(node, name, descriptor.required_directives()));)
    };

    quote! {
      #vis struct #struct_name {
        #(#fields),*
        value: #enum_name,
      }

      impl #struct_name {
        #vis fn value(&self) -> #enum_name {
          self.value
        }
      }

      impl ::smear::__exports::Diagnosticable for #struct_name {
        type Node = ::smear::__exports::apollo_parser::ast::EnumValueDefinition;
        type Error = ::smear::__exports::error::EnumValueError;
        type Descriptor = ::smear::__exports::definition::EnumValueDescriptor;

        fn descriptor() -> &'static Self::Descriptor {
          use ::smear::__exports::once_cell::sync::Lazy;

          static DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_directives]> = Lazy::new(|| [
            #(#available_directives_descriptors),*
          ]);
          static REQUIRED_DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_required_directives]> = Lazy::new(|| [
            #(#required_directives_descriptors),*
          ]);
          static OPTIONAL_DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_optional_directives]> = Lazy::new(|| [
            #(#optional_directives_descriptors),*
          ]);
 
          static DESCRIPTOR: Lazy<::smear::__exports::definition::EnumDescriptor> = Lazy::new(|| {
            ::smear::__exports::definition::EnumDescriptor {
              name: #sdl_name,
              description: #description,
              deprecated: #deprecated,
              available_directives: &*DIRECTIVES,
              required_directives: &*REQUIRED_DIRECTIVES,
              optional_directives: &*OPTIONAL_DIRECTIVES,
            }
          });
          &*DESCRIPTOR
        }

        fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
        where
          Self: ::core::marker::Sized,
        {
          struct Parser {
            #(#parser_fields),*
            value: ::core::option::Option<#enum_name>,
          }

          impl ::core::default::Default for Parser {
            fn default() -> Self {
              Self {
                #(#parser_field_default),*
                value: ::core::option::Option::None,
              }
            }
          }

          impl ::core::convert::From<Parser> for #struct_name {
            fn from(parser: Parser) -> #struct_name {
              #struct_name {
                #(#converts),*
                value: parser.value.unwrap(),
              }
            }
          }

          let descriptor = <#struct_name as ::smear::__exports::Diagnosticable>::descriptor();
          let mut parser: Parser = ::core::default::Default::default();
          let mut errors = ::std::vec::Vec::new();
          match node.name() {
            ::core::option::Option::Some(name) => {
              let name = name.text().to_string();
              match node.directives() {
                ::core::option::Option::Some(directives) => {
                  #(#dirty_check_variables)*

                  #handlers
                }
                ::core::option::Option::None => {
                  #empty_directives
                }
              }

              #(#null_checks)*

              if ::core::option::Option::Some(value) = node.value() {
                let value_txt = value.text();
                match <#enum_name as ::core::str::FromStr>::from_str(value_txt.as_str()) {
                  ::core::result::Result::Ok(value) => {
                    parser.value = ::core::option::Option::Some(value);
                  }
                  ::core::result::Result::Err(_) => {
                    errors.push(::smear::__exports::error::EnumError::unknown_enum_value(node, value_txt.as_str(), #enum_name::VARIANTS_STR));
                  }
                }
              } else {
                errors.push(::smear::__exports::error::EnumError::missing_enum_value(node));
              }

              if !errors.is_empty() {
                return ::core::result::Result::Err(::smear::__exports::error::EnumError::multiple(node, errors));
              }

              ::core::result::Result::Ok(::core::convert::From::from(parser))
            },
            ::core::option::Option::None => ::core::result::Result::Err(::smear::__exports::error::EnumError::missing_name(node)),
          }
        }
      }
    }
  }
}

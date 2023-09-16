use darling::{ast::Data, FromDeriveInput, FromVariant};
use heck::ToShoutySnakeCase;
use quote::{format_ident, quote};
use syn::{Ident, Type};

use crate::utils::{Deprecated, Description, Directives, PathAttribute, RenameAll};

pub(crate) fn derive(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let enum_ = Enum::from_derive_input(&input)?;

  enum_.derive()
}

#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(enum_unit))]
struct Enum {
  ident: Ident,
  name: Option<String>,
  #[darling(default = "RenameAll::graphql_enum_style")]
  rename_all: RenameAll,
  data: Data<Variant, ()>,
  #[darling(default)]
  description: Description,
  #[darling(default)]
  deprecated: Deprecated,
  #[darling(default)]
  directives: Directives,
  #[darling(default)]
  variant_directives: Directives,
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
}

impl Enum {
  fn sdl_name(&self) -> String {
    self.name.clone().unwrap_or_else(|| self.ident.to_string())
  }

  fn derive(&self) -> syn::Result<proc_macro2::TokenStream> {
    let enum_name = &self.ident;
    let sdl_name = self.sdl_name();
    let description = &self.description;
    let deprecated = &self.deprecated;
    let num_directives = self.directives.directives.len();
    let directives = self
      .directives
      .directives
      .iter()
      .map(|ty| quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor()));
    let variants = self.data.as_ref().take_enum().unwrap();
    let num_variants = variants.len();

    let variant_directive_descriptors = variants.iter().map(|variant| {
      variant
        .directives_descriptors_definition(self.rename_all, &self.variant_directives.directives)
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

    Ok(quote! {
      const _: () = {
        fn __parser(node: &::smear::__exports::apollo_parser::EnumDefinition) -> ::core::result::Result<#enum_name, ::smear::__exports::error::EnumError> {
          todo!()
        }

        impl ::smear::__exports::Diagnosticable for #enum_name {
          type Node = ::smear::__exports::apollo_parser::EnumDefinition;
          type Error = ::smear::__exports::error::EnumError;
          type Descriptor = ::smear::__exports::definition::EnumDescriptor;

          fn descriptor() -> &'static Self::Descriptor {
            use ::smear::__exports::once_cell::sync::Lazy;

            static DIRECTIVES: Lazy<[&'static ::smear::__exports::directive::DirectiveDescriptor; #num_directives]> = Lazy::new(|| [
              #(#directives),*
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
                directives: &*DIRECTIVES,
                variants: &*VARIANTS,
              }
            });
            &*DESCRIPTOR
          }

          fn parse(node: &Self::Node) -> ::core::result::Result<Self, Self::Error>
          where
            Self: Sized,
          {
            match #parser {
              ::core::result::Result::Ok(parsed) => #validator.map(|_| parsed),
              ::core::result::Result::Err(err) => ::core::result::Result::Err(err),
            }
          }
        }

        impl ::smear::__exports::Encodable for #enum_name {
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
  #[darling(default)]
  directives: Directives,
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
    common_directives: &[Type],
  ) -> proc_macro2::TokenStream {
    let num_directives = self.directives.directives.len() + common_directives.len();
    let directives = self
      .directives
      .directives
      .iter()
      .chain(common_directives.iter())
      .map(|ty| quote!(<#ty as ::smear::__exports::Diagnosticable>::descriptor()));
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
}

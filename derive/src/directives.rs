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
  utils::{Aliases, Long, RenameAll, Short},
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
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
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
    let diagnostic_name = format_ident!("{}Diagnostic", struct_name);
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
    let mut arguments = Vec::new();
    for field in fields.iter() {
      let field_name = field.sdl_name(self.rename_all);
      let field_name_ident = format_ident!("{field_name}");
      let field_ty = field.ty();
      let field_vis = field.vis();
      arguments.push(field.generate(&long, self.rename_all)?);
      struct_fields_definitions
        .push(quote!(#field_vis #field_name_ident: ::core::option::Option<#field_ty>));
    }

    Ok(quote! {
      #[repr(transparent)]
      #[automatically_derived]
      #vis struct #struct_name {
        #(#struct_fields_definitions),*
      }

      #[automatically_derived]
      impl ::smear::Diagnosticable for #struct_name {
        type Error = #diagnostic_name;
        type Node = ::smear::apollo_parser::ast::Directive;

        fn parse(node: &Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized
        {
          if let ::core::option::Option::Some(args) = node.arguments() {
            ::core::result::Result::Err(#diagnostic_name(args))
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
      impl ::smear::DiagnosticableDirective for #struct_name {
        fn available_argument_names() -> &'static [&'static str] {
          &[#(#available_argument_names),*]
        }
      }

    })
  }

  fn generate_unit(&self) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = format_ident!("{}Directive", self.sdl_directive_struct_name());
    let diagnostic_name = format_ident!("{}Diagnostic", struct_name);
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
        type Error = #diagnostic_name;
        type Node = ::smear::apollo_parser::ast::Directive;

        fn parse(node: &Self::Node) -> Result<Self, Self::Error>
        where
          Self: Sized
        {
          if let ::core::option::Option::Some(args) = node.arguments() {
            ::core::result::Result::Err(#diagnostic_name(args))
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

      #[derive(
        ::core::fmt::Debug,
        ::core::clone::Clone,
      )]
      #[repr(transparent)]
      #[automatically_derived]
      #vis struct #diagnostic_name(::smear::apollo_parser::ast::Arguments);

      #[automatically_derived]
      impl ::smear::Reporter for #diagnostic_name {
        fn report<'a, FileId>(&self, file_id: FileId) -> ::smear::Diagnostic<FileId>
        where
          FileId: 'a + ::core::marker::Copy + ::core::cmp::PartialEq
        {
          use ::smear::apollo_parser::ast::AstNode;

          let syn = self.0.syntax();
          let range = syn.text_range();
          let start: usize = range.start().into();
          let end: usize = range.end().into();

          ::smear::codespan_reporting::diagnostic::Diagnostic::error()
            .with_message("unexpected arguments")
            .with_labels(vec![
              ::smear::codespan_reporting::diagnostic::Label::primary(file_id, start..end)
                .with_message(syn.text()),
            ])
            .into()
        }
      }
    })
  }
}

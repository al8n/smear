use darling::FromDeriveInput;
use indexmap::IndexSet;
use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{DeriveInput, Visibility};

use crate::utils::{Aliases, Long, Short};

pub fn derive(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
  let opts = match Options::from_derive_input(&input) {
    Ok(v) => v,
    Err(e) => {
      return Err(e.into());
    }
  };

  opts.validate()?;

  let struct_name = format_ident!("{}Directive", opts.directive_struct_name());
  let diagnostic_name = format_ident!("{}Diagnostic", struct_name);
  let vis = &opts.vis;
  let possible_names = opts.possible_names();
  let short = match opts.short() {
    Some(short) => quote!(::core::option::Option::Some(#short)),
    None => quote!(::core::option::Option::None),
  };
  let long = opts.long();
  let aliases = &opts.aliases.names;

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
      ::smear::derive_more::Not,
      ::smear::derive_more::Display,
      ::smear::derive_more::BitAnd,
      ::smear::derive_more::BitOr,
      ::smear::derive_more::BitXor,
      ::smear::derive_more::BitAndAssign,
      ::smear::derive_more::BitOrAssign,
      ::smear::derive_more::BitXorAssign,
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

      fn parse(node: Self::Node) -> Result<Self, Self::Error>
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

      fn short() -> Option<char> {
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

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(smear))]
struct Options {
  ident: Ident,
  vis: Visibility,
  rename: Option<String>,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
}

impl Options {
  fn self_name(&self) -> String {
    self.ident.to_string().to_ascii_lowercase()
  }

  fn directive_struct_name(&self) -> String {
    self
      .rename
      .clone()
      .unwrap_or_else(|| self.ident.to_string())
  }

  fn validate(&self) -> syn::Result<()> {
    let name = self.self_name();
    self.short.validate(&name)?;
    self.long.validate(&name)?;
    Ok(())
  }

  fn long(&self) -> String {
    self.long.0.clone().unwrap_or(self.self_name())
  }

  fn short(&self) -> Option<char> {
    self.short.0.as_ref().map(|s| {
      s.unwrap_or_else(|| {
        self
          .self_name()
          .chars()
          .next()
          .unwrap()
          .to_ascii_lowercase()
      })
    })
  }

  fn possible_names(&self) -> Vec<String> {
    let mut suggestions = IndexSet::new();

    suggestions.insert(self.long());

    if let Some(short) = self.short() {
      suggestions.insert(short.to_string());
    }

    suggestions.extend(self.aliases.names.iter().cloned());
    suggestions.into_iter().collect()
  }
}

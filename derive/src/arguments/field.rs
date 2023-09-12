use darling::FromField;
use indexmap::IndexSet;
use syn::{Ident, Visibility};

use crate::utils::{Aliases, DefaultAttribute, Long, Optional, PathAttribute, RenameAll, Short};

#[derive(FromField)]
#[darling(attributes(smear))]
pub(crate) struct ArgumentField {
  ident: Option<Ident>,
  vis: Visibility,
  ty: syn::Type,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
  optional: Option<Optional>,
  default: Option<DefaultAttribute>,
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
}

impl ArgumentField {
  pub(crate) fn ty(&self) -> &syn::Type {
    &self.ty
  }

  pub(crate) fn vis(&self) -> &Visibility {
    &self.vis
  }

  /// Returns the name of the field as it should appear in the SDL.
  pub(crate) fn sdl_name(&self, rename_all: Option<RenameAll>) -> String {
    self.long.0.clone().unwrap_or_else(|| {
      let name = self.ident.as_ref().unwrap().to_string();
      rename_all
        .map(|rename_all| rename_all.apply(&name))
        .unwrap_or(name)
    })
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

  pub(crate) fn generate(
    &self,
    parent: &str,
    rename_all: Option<RenameAll>,
  ) -> syn::Result<proc_macro2::TokenStream> {
    super::ArgumentCodegen {
      ident: self.ident.clone().unwrap(),
      vis: self.vis.clone(),
      ty: self.ty.clone(),
      short: self.short.clone(),
      long: self.long.clone(),
      aliases: self.aliases.clone(),
      optional: self.optional,
      default: self.default.clone(),
      validator: self.validator.clone(),
      parser: self.parser.clone(),
    }
    .generate(parent, rename_all)
  }
}

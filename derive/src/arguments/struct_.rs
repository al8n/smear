use darling::FromDeriveInput;

use quote::quote;
use syn::{Ident, Visibility, Generics, Type};

use crate::utils::{Short, Long, Aliases, DefaultAttribute, PathAttribute, Optional};


#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_newtype, struct_unit))]
pub(crate) struct Argument {
  ident: Ident,
  generics: Generics,
  ty: Type,
  vis: Visibility,
  #[darling(default)]
  short: Short,
  #[darling(default)]
  long: Long,
  #[darling(default)]
  aliases: Aliases,
  default: Option<DefaultAttribute>,
  optional: Option<Optional>,
  #[darling(default)]
  validator: PathAttribute,
  #[darling(default)]
  parser: PathAttribute,
}

impl Argument {
  pub(crate) fn generate_unit(&self) -> syn::Result<proc_macro2::TokenStream> {
    Ok(quote! {
      
    })
  }

  pub(crate) fn generate(&self) -> syn::Result<proc_macro2::TokenStream> {
    super::ArgumentCodegen {
      ident: self.ident.clone(),
      vis: self.vis.clone(),
      ty: self.ty.clone(),
      short: self.short.clone(),
      long: self.long.clone(),
      aliases: self.aliases.clone(),
      optional: self.optional.clone(),
      default: self.default.clone(),
      validator: self.validator.clone(),
      parser: self.parser.clone(),
    }.generate("", None)
  }
}
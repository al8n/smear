use darling::FromDeriveInput;

use syn::{Ident, Visibility, Generics};

use crate::utils::{Short, Long, Aliases, DefaultAttribute, PathAttribute, Optional};


#[derive(FromDeriveInput)]
#[darling(attributes(smear), supports(struct_named, struct_newtype, struct_unit))]
pub(crate) struct Argument {
  ident: Ident,
  generics: Generics,
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

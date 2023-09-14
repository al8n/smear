use darling::FromMeta;
use quote::ToTokens;
use syn::{parse::Parser, Attribute};

#[derive(Default, Clone)]
pub(crate) struct Attributes {
  pub(crate) attrs: Vec<Attribute>,
}

impl core::fmt::Debug for Attributes {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let attrs = &self.attrs;
    for attr in attrs.iter() {
      quote::quote!(#attr).to_string().fmt(f)?;
    }
    Ok(())
  }
}

impl From<Vec<Attribute>> for Attributes {
  fn from(attrs: Vec<Attribute>) -> Self {
    Self { attrs }
  }
}

impl FromMeta for Attributes {
  fn from_list(items: &[darling::ast::NestedMeta]) -> darling::Result<Self> {
    let mut attrs = Vec::with_capacity(items.len());
    let mut errors = vec![];
    for n in items {
      match Attribute::parse_outer.parse2(quote::quote! { #[#n] }) {
        Ok(attr) => attrs.extend(attr),
        Err(e) => errors.push(e.into()),
      }
    }

    if !errors.is_empty() {
      return Err(darling::Error::multiple(errors));
    }
    Ok(Attributes { attrs })
  }
}

impl ToTokens for Attributes {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    for attr in self.attrs.iter() {
      tokens.extend(quote::quote!(#attr));
    }
  }
}

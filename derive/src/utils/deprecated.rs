use darling::FromMeta;
use quote::{quote, ToTokens};

#[derive(Default)]
pub struct Deprecated(Option<DeprecatedBody>);

impl FromMeta for Deprecated {
  fn from_word() -> darling::Result<Self> {
    Ok(Self(Some(DeprecatedBody::default())))
  }

  fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
    match item {
      syn::Meta::Path(_) => Self::from_word(),
      meta => DeprecatedBody::from_meta(meta).map(|b| Self(Some(b))),
    }
  }
}

impl ToTokens for Deprecated {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    match self.0.as_ref() {
      Some(d) => d.to_tokens(tokens),
      None => tokens.extend(quote!(::core::option::Option::None)),
    }
  }
}

#[viewit::viewit]
#[derive(Default, FromMeta)]
pub struct DeprecatedBody {
  reason: Option<String>,
  version: Option<String>,
  suggestion: Option<String>,
}

impl ToTokens for DeprecatedBody {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    let reason = if let Some(val) = self.reason.as_ref() {
      quote!(::core::option::Option::Some(#val))
    } else {
      quote!(::core::option::Option::None)
    };

    let version = if let Some(val) = self.version.as_ref() {
      quote!(::core::option::Option::Some(#val))
    } else {
      quote!(::core::option::Option::None)
    };

    let suggestion = if let Some(val) = self.suggestion.as_ref() {
      quote!(::core::option::Option::Some(#val))
    } else {
      quote!(::core::option::Option::None)
    };

    tokens.extend(quote! {
      ::core::option::Option::Some(::smear::__exports::Deprecated::new(#version, #reason, #suggestion))
    });
  }
}

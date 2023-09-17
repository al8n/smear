use darling::{ast::NestedMeta, Error, FromMeta};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{Expr, Lit, Ident, spanned::Spanned};

use super::{DisplayPath, SafeIdent};

pub(crate) struct Ty {
  pub(crate) ty: syn::Type,
}

impl Ty {
  pub(crate) fn to_rust_ident(&self) -> SafeIdent {
    // Convert the type to a token stream.
    let mut token_stream = TokenStream::new();
    self.ty.to_tokens(&mut token_stream);
    
    // Convert the token stream to a string and replace :: with _
    let modified_str = token_stream.to_string().replace("::", "_");
    
    // Convert the modified string to Ident.
    SafeIdent::from(modified_str.trim_start_matches('_'))
  }
}

impl FromMeta for Ty {
  fn from_expr(expr: &Expr) -> darling::Result<Self> {
    let val = match expr {
      Expr::Lit(lit) => match &lit.lit {
        Lit::Str(lit) => lit.value(),
        lit => return Err(Error::unexpected_lit_type(lit)),
      },
      Expr::Path(p) => DisplayPath(&p.path).to_string(),
      expr => return Err(Error::unexpected_expr_type(expr)),
    };
    Self::from_string(val.as_str())
  }

  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    match items.len() {
      0 => Err(Error::too_few_items(1)),
      1 => {
        let val = match &items[0] {
          NestedMeta::Meta(ref nested) => DisplayPath(nested.path()).to_string(),
          NestedMeta::Lit(syn::Lit::Str(s)) => s.value(),
          NestedMeta::Lit(lit) => return Err(Error::unexpected_lit_type(lit)),
        };
        Self::from_string(&val)
      }
      _ => Err(Error::too_many_items(1)),
    }
  }

  fn from_string(value: &str) -> darling::Result<Self> {
    syn::parse_str(value)
      .map(|ty| Ty { ty })
      .map_err(Into::into)
  }
}

impl ToTokens for Ty {
  fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
    self.ty.to_tokens(tokens)
  }
}

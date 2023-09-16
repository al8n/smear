use darling::{ast::NestedMeta, Error, FromMeta};
use quote::quote;
use syn::{
  parse::{Parse, Parser},
  Expr, Lit, Type,
};

#[derive(Default)]
pub(crate) struct Directives {
  pub(crate) directives: Vec<Type>,
}

impl FromMeta for Directives {
  fn from_expr(expr: &Expr) -> darling::Result<Self> {
    let mut errors = Error::accumulator();
    match expr {
      Expr::Array(array) => {
        let mut directives = Vec::with_capacity(array.elems.len());
        for expr in &array.elems {
          if let Some(ty) = errors.handle(Type::from_expr(expr)) {
            directives.push(ty);
          }
        }
        Ok(Self { directives })
      }
      Expr::Path(p) => match Type::parse.parse2(quote!(#p)) {
        Ok(ty) => Ok(Self {
          directives: vec![ty],
        }),
        Err(e) => Err(e.into()),
      },
      Expr::Lit(lit) => {
        if let Lit::Str(s) = &lit.lit {
          match syn::parse_str(s.value().as_str()) {
            Ok(ty) => Ok(Self {
              directives: vec![ty],
            }),
            Err(e) => Err(e.into()),
          }
        } else {
          Err(Error::unexpected_lit_type(&lit.lit))
        }
      }
      expr => Err(Error::unexpected_expr_type(expr)),
    }
  }

  fn from_list(items: &[NestedMeta]) -> darling::Result<Self> {
    let mut errors = Error::accumulator();
    let mut directives = Vec::new();
    for item in items {
      match item {
        NestedMeta::Meta(inner) => {
          if let Some(ty) = errors.handle(Type::from_meta(inner)) {
            directives.push(ty);
          }
        }
        NestedMeta::Lit(inner) => {
          if let Lit::Str(s) = inner {
            if let Some(ty) = errors.handle(syn::parse_str(s.value().as_str()).map_err(Into::into))
            {
              directives.push(ty);
            }
          } else {
            errors.push(Error::unexpected_lit_type(inner));
          }
        }
      }
    }

    errors.finish()?;
    Ok(Self { directives })
  }
}

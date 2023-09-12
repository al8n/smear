use darling::FromMeta;
use syn::Path;

#[derive(FromMeta, Default, Clone)]
pub(crate) enum PathAttribute {
  #[default]
  None,
  Str(String),
  Path(Path),
}

impl PathAttribute {
  pub(crate) fn path(&self) -> syn::Result<Option<Path>> {
    match self {
      Self::None => Ok(None),
      Self::Str(s) => syn::parse_str::<Path>(s).map(Some),
      Self::Path(p) => Ok(Some(p.clone())),
    }
  }
}

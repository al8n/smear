
#[derive(darling::FromMeta, Clone)]
pub(crate) enum Optional {
  None,
  Bool(bool),
}

impl Optional {
  pub(crate) fn is_optional(&self) -> bool {
    match self {
      Self::None => true,
      Self::Bool(b) => *b,
    }
  }
}
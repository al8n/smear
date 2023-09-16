use darling::FromMeta;

#[derive(Default, Clone, Copy)]
pub(crate) struct Optional(Option<bool>);

impl Optional {
  pub(crate) fn is_optional(&self) -> bool {
    self.0.unwrap_or(false)
  }
}

impl FromMeta for Optional {
  fn from_word() -> darling::Result<Self> {
    Ok(Self(Some(true)))
  }

  fn from_bool(value: bool) -> darling::Result<Self> {
    Ok(Self(Some(value)))
  }
}
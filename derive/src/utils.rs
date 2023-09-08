// Utility function to check if a character is valid
pub(crate) fn is_valid_char(ch: char) -> bool {
  ch.is_ascii_alphabetic() || ch == '_' || ch.is_ascii_digit()
}

pub(crate) struct DisplayPath<'a>(pub &'a syn::Path);

impl<'a> core::fmt::Display for DisplayPath<'a> {
  fn fmt(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
    for (i, segment) in self.0.segments.iter().enumerate() {
      if i > 0 || self.0.leading_colon.is_some() {
        formatter.write_str("::")?;
      }
      write!(formatter, "{}", segment.ident)?;
    }
    Ok(())
  }
}

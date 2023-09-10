use super::*;

// TODO: cleanup variant fields, rather than just leave a String there.
#[non_exhaustive]
pub enum ErrorKind {
  /// An arbitrary error message.
  Custom(String),
  DuplicateField(String),
  MissingObjectFieldName,
  MissingObjectField(String),
  UnknownObjectField(ErrorUnknownField),
  MissingObjectValue(String),
  UnexpectedType(String),
  InvalidValue(String),
  /// A set of errors.
  Multiple(Vec<Error>),
}

impl ErrorKind {
  pub fn description(&self) -> &str {
    use self::ErrorKind::*;

    match *self {
      Custom(ref s) => s,
      DuplicateField(_) => "Duplicate field",
      UnknownObjectField(_) => "Unknown object field",
      MissingObjectValue(_) => "Missing object value",
      MissingObjectFieldName => "Missing object field name",
      MissingObjectField(_) => "Missing object field",
      InvalidValue(_) => "Invalid value",
      UnexpectedType(_) => "Unexpected value type",
      Multiple(_) => "Multiple errors",
    }
  }

  /// Deeply counts the number of errors this item represents.
  pub fn len(&self) -> usize {
    if let ErrorKind::Multiple(ref items) = *self {
      items.iter().map(Error::len).sum()
    } else {
      1
    }
  }
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use self::ErrorKind::*;

    match *self {
      Custom(ref s) => s.fmt(f),
      DuplicateField(ref field) => write!(f, "Duplicate field `{field}`"),
      MissingObjectFieldName => write!(f, "Missing field name"),
      MissingObjectField(ref field) => write!(f, "Missing object field `{field}`"),
      UnknownObjectField(ref field) => field.fmt(f),
      UnexpectedType(ref ty) => write!(f, "Unexpected type `{ty}`"),
      MissingObjectValue(ref field) => write!(f, "Missing value for field `{field}`"),
      InvalidValue(ref e) => write!(f, "Invalid value: `{e}`"),
      Multiple(ref items) if items.len() == 1 => items[0].fmt(f),
      Multiple(ref items) => {
        write!(f, "Multiple errors: (")?;
        let mut first = true;
        for item in items {
          if !first {
            write!(f, ", ")?;
          } else {
            first = false;
          }

          item.fmt(f)?;
        }

        write!(f, ")")
      }
    }
  }
}

impl From<ErrorUnknownField> for ErrorKind {
  fn from(err: ErrorUnknownField) -> Self {
    ErrorKind::UnknownObjectField(err)
  }
}

/// An error for an unknown field, with a possible "did-you-mean" suggestion to get
/// the user back on the right track.
#[derive(Clone, Debug)]
// Don't want to publicly commit to ErrorKind supporting equality yet, but
// not having it makes testing very difficult.
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct ErrorUnknownField {
  pub(super) name: String,
  pub(super) did_you_mean: Option<String>,
}

impl ErrorUnknownField {
  pub fn new<I: Into<String>>(name: I, did_you_mean: Option<String>) -> Self {
    ErrorUnknownField {
      name: name.into(),
      did_you_mean,
    }
  }

  pub fn with_alts<'a, T, I>(field: &str, alternates: I) -> Self
  where
    T: AsRef<str> + 'a,
    I: IntoIterator<Item = &'a T>,
  {
    ErrorUnknownField::new(field, crate::utils::did_you_mean(field, alternates))
  }
}

impl From<String> for ErrorUnknownField {
  fn from(name: String) -> Self {
    ErrorUnknownField::new(name, None)
  }
}

impl<'a> From<&'a str> for ErrorUnknownField {
  fn from(name: &'a str) -> Self {
    ErrorUnknownField::new(name, None)
  }
}

impl fmt::Display for ErrorUnknownField {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Unknown field: `{}`", self.name)?;

    if let Some(ref did_you_mean) = self.did_you_mean {
      write!(f, ". Did you mean `{}`?", did_you_mean)?;
    }

    Ok(())
  }
}

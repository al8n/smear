//! The texts a source file may hold, and how the analysis reads each one.

pub(crate) use private::View;

/// A source file's text: any representation the GraphQLx syntactic parser reads.
///
/// The analysis is generic over the caller's text type at its entrance and nowhere after it. Each
/// file is borrowed for the length of one call, read once as text or as bytes, and nothing of it
/// is kept: the analysis owns every spelling it answers with, so the caller may drop or reuse a
/// text as soon as the call returns.
///
/// | type | read as | feature |
/// |---|---|---|
/// | `&str` | text | |
/// | `&[u8]` | bytes | |
/// | `&bstr::BStr` | bytes | `bstr_1` |
/// | `bytes::Bytes` | bytes | `bytes_1` |
/// | `hipstr::HipStr<'_>` | text | `hipstr_0_8` |
/// | `hipstr::HipByt<'_>` | bytes | `hipstr_0_8` |
/// | `smol_bytes::shared::Bytes`, `smol_bytes::compact::Bytes` | bytes | `smol-bytes_0_2` |
/// | `smol_bytes::Utf8Bytes`, `smol_bytes::compact::Utf8Bytes` | text | `smol-bytes_0_2` |
///
/// # Bytes that are text
///
/// A byte text that is valid UTF-8 is parsed as text, exactly as a `&str` holding the same bytes
/// would be, so one package analyzed as `&str` and as `&[u8]` gives the same diagnostics. Only a
/// byte text that is not UTF-8 is parsed as bytes, and such a text always carries a syntax
/// diagnostic of its own: the GraphQLx lexer refuses a byte that is not UTF-8 wherever it stands.
///
/// # Sealed
///
/// The trait has no public method, and only this crate implements it. A text type added later is
/// one more impl here, which breaks no caller.
pub trait SourceText: private::Sealed {}

/// The half of [`SourceText`] the analysis calls, kept out of reach of every other crate.
mod private {
  /// How the analysis borrows one text.
  ///
  /// The supertrait that seals [`SourceText`](super::SourceText): this module is private, so no
  /// other crate can name it, implement it or call it.
  pub trait Sealed {
    /// The text, borrowed as the parser will read it.
    fn view(&self) -> View<'_>;
  }

  /// A source text, borrowed as text or as bytes, with nothing copied.
  #[derive(Debug, Clone, Copy)]
  pub enum View<'a> {
    /// The text's type makes it UTF-8.
    Text(&'a str),
    /// The text is bytes, which may or may not be UTF-8.
    Bytes(&'a [u8]),
  }

  impl<'a> View<'a> {
    /// The text's bytes, whichever way it was borrowed.
    #[inline]
    pub(crate) fn bytes(self) -> &'a [u8] {
      match self {
        Self::Text(text) => text.as_bytes(),
        Self::Bytes(bytes) => bytes,
      }
    }

    /// The text as `str`, or `None` for bytes that are not UTF-8.
    #[inline]
    pub(crate) fn as_text(self) -> Option<&'a str> {
      match self {
        Self::Text(text) => Some(text),
        Self::Bytes(bytes) => core::str::from_utf8(bytes).ok(),
      }
    }
  }
}

impl SourceText for &str {}

impl private::Sealed for &str {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Text(self)
  }
}

impl SourceText for &[u8] {}

impl private::Sealed for &[u8] {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(self)
  }
}

#[cfg(feature = "bstr_1")]
#[cfg_attr(docsrs, doc(cfg(feature = "bstr_1")))]
impl SourceText for &bstr_1::BStr {}

#[cfg(feature = "bstr_1")]
impl private::Sealed for &bstr_1::BStr {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(<bstr_1::BStr as AsRef<[u8]>>::as_ref(self))
  }
}

#[cfg(feature = "bytes_1")]
#[cfg_attr(docsrs, doc(cfg(feature = "bytes_1")))]
impl SourceText for bytes_1::Bytes {}

#[cfg(feature = "bytes_1")]
impl private::Sealed for bytes_1::Bytes {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(&self[..])
  }
}

#[cfg(feature = "hipstr_0_8")]
#[cfg_attr(docsrs, doc(cfg(feature = "hipstr_0_8")))]
impl SourceText for hipstr_0_8::HipStr<'_> {}

#[cfg(feature = "hipstr_0_8")]
impl private::Sealed for hipstr_0_8::HipStr<'_> {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Text(self.as_str())
  }
}

#[cfg(feature = "hipstr_0_8")]
#[cfg_attr(docsrs, doc(cfg(feature = "hipstr_0_8")))]
impl SourceText for hipstr_0_8::HipByt<'_> {}

#[cfg(feature = "hipstr_0_8")]
impl private::Sealed for hipstr_0_8::HipByt<'_> {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(&self[..])
  }
}

#[cfg(feature = "smol-bytes_0_2")]
#[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
impl SourceText for smol_bytes_0_2::shared::Bytes {}

#[cfg(feature = "smol-bytes_0_2")]
impl private::Sealed for smol_bytes_0_2::shared::Bytes {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(&self[..])
  }
}

#[cfg(feature = "smol-bytes_0_2")]
#[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
impl SourceText for smol_bytes_0_2::compact::Bytes {}

#[cfg(feature = "smol-bytes_0_2")]
impl private::Sealed for smol_bytes_0_2::compact::Bytes {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Bytes(&self[..])
  }
}

#[cfg(feature = "smol-bytes_0_2")]
#[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
impl SourceText for smol_bytes_0_2::Utf8Bytes {}

#[cfg(feature = "smol-bytes_0_2")]
impl private::Sealed for smol_bytes_0_2::Utf8Bytes {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Text(self.as_str())
  }
}

#[cfg(feature = "smol-bytes_0_2")]
#[cfg_attr(docsrs, doc(cfg(feature = "smol-bytes_0_2")))]
impl SourceText for smol_bytes_0_2::compact::Utf8Bytes {}

#[cfg(feature = "smol-bytes_0_2")]
impl private::Sealed for smol_bytes_0_2::compact::Utf8Bytes {
  #[inline]
  fn view(&self) -> View<'_> {
    View::Text(self.as_str())
  }
}

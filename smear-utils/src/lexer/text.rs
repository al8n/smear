use core::ops::{Bound, RangeBounds};

/// A helper struct for unifying display of text.
pub struct DisplayText<'a, T>(&'a T);

impl<'a, T> From<&'a T> for DisplayText<'a, T> {
  #[inline(always)]
  fn from(t: &'a T) -> Self {
    Self(t)
  }
}

impl<'a, T: Text<'a>> core::fmt::Display for DisplayText<'_, T> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.0.format(f)
  }
}

/// The source text, e.g. `&[u8]`, `&str`
pub trait Text<'a>: Clone + 'a {
  /// Returns a subslice of the source with the given range.
  fn slice(&self, range: impl RangeBounds<usize>) -> Self;

  /// Returns the length of the source.
  fn len(&self) -> usize;

  /// Returns `true` if the source is empty.
  #[inline(always)]
  fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Format the text to the given formatter
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result;
}

/// An extension trait for [`Text`]
pub trait TextExt<'a>: Text<'a> {
  /// Returns a wrapper which implement `Display`.
  fn display(&self) -> DisplayText<'_, Self> {
    DisplayText(self)
  }
}

macro_rules! slice {
  ($this:ident($range:ident)) => {{
    let len = $this.len();

    let begin = match $range.start_bound() {
      Bound::Included(&n) => n,
      Bound::Excluded(&n) => n.checked_add(1).expect("out of range"),
      Bound::Unbounded => 0,
    };

    let end = match $range.end_bound() {
      Bound::Included(&n) => n.checked_add(1).expect("out of range"),
      Bound::Excluded(&n) => n,
      Bound::Unbounded => len,
    };

    &$this[begin..end]
  }};
}

#[inline]
fn next_char_boundary(s: &str, mut i: usize) -> usize {
  if i >= s.len() {
    return s.len();
  }
  while !s.is_char_boundary(i) {
    i += 1;
  }
  i
}

#[inline]
fn prev_char_boundary(s: &str, mut i: usize) -> usize {
  if i >= s.len() {
    return s.len();
  }
  while i > 0 && !s.is_char_boundary(i) {
    i -= 1;
  }
  i
}

impl<'a> Text<'a> for &'a str {
  #[inline(always)]
  fn len(&self) -> usize {
    <str>::len(self)
  }

  #[inline]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    let len = self.len();
    let begin = match range.start_bound() {
      Bound::Included(&n) => n,
      Bound::Excluded(&n) => n.saturating_add(1),
      Bound::Unbounded => 0,
    }
    .min(len);

    let end = match range.end_bound() {
      Bound::Included(&n) => n.saturating_add(1),
      Bound::Excluded(&n) => n,
      Bound::Unbounded => len,
    }
    .min(len);

    let begin = next_char_boundary(self, begin);
    let end = prev_char_boundary(self, end);

    if begin >= end {
      ""
    } else {
      unsafe { self.get_unchecked(begin..end) }
    }
  }

  #[inline]
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    <str as core::fmt::Display>::fmt(self, fmt)
  }
}

impl<'a> Text<'a> for &'a [u8] {
  #[inline(always)]
  fn slice(&self, range: impl RangeBounds<usize>) -> Self {
    slice!(self(range))
  }

  #[inline(always)]
  fn len(&self) -> usize {
    <[u8]>::len(self)
  }

  #[inline(always)]
  #[cfg(not(feature = "bstr"))]
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    <[u8] as core::fmt::Debug>::fmt(self, fmt)
  }

  #[inline(always)]
  #[cfg(feature = "bstr")]
  fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    use bstr::BStr;

    <BStr as core::fmt::Display>::fmt(BStr::new(self), fmt)
  }
}

#[cfg(feature = "bstr")]
const _: () = {
  use bstr::BStr;

  impl<'a> Text<'a> for &'a BStr {
    #[inline]
    fn slice(&self, range: impl RangeBounds<usize>) -> Self {
      slice!(self(range))
    }

    #[inline]
    fn len(&self) -> usize {
      <[u8]>::len(self)
    }

    #[inline]
    fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      <BStr as core::fmt::Display>::fmt(self, fmt)
    }
  }
};

#[cfg(feature = "bytes")]
const _: () = {
  use bytes::Bytes;

  impl Text<'_> for Bytes {
    #[inline]
    fn slice(&self, range: impl RangeBounds<usize>) -> Self {
      Bytes::slice(self, range)
    }

    #[inline]
    fn len(&self) -> usize {
      Bytes::len(self)
    }

    #[inline]
    fn format(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      <&[u8] as Text<'_>>::format(&self.as_ref(), fmt)
    }
  }
};

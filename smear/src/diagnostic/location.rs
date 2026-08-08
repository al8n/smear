use tokora::SimpleSpan;

/// A position in one of the inputs a diagnostic was produced from.
///
/// # Two halves, because a build reads more than one document
///
/// `source` names which input the span belongs to, as an index into whatever list the producer
/// was given: a schema build accepts several documents and numbers them in the order it received
/// them, and a single-input producer answers `0`. A byte offset with no input to index into is
/// not a position, which is why the two travel together rather than the span alone.
///
/// # [`entire`](Location::entire) is the honest answer for an input with no positions
///
/// Not every diagnostic comes from text a human wrote. A draft §4 introspection response is a
/// machine-generated JSON blob, and a byte offset into it would point at nothing anybody can
/// edit — a lie with a unit attached. Such a producer answers [`Location::entire`], whose
/// [`span`](Location::span) is `None`: the diagnostic is about that input as a whole, and a
/// renderer is told so rather than being handed a fabricated `0..len`.
///
/// **This is an exception with a named holder, not a default.** Its only user in this crate is
/// the introspection door's `ResponseError`, whose own documentation says why it has no span. A
/// family that *has* positions and answers `entire` because plumbing them through was
/// inconvenient has thrown away the thing a renderer exists to show.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location {
  source: u32,
  span: Option<SimpleSpan>,
}

impl Location {
  /// A position in `source`.
  #[inline]
  pub const fn new(source: u32, span: SimpleSpan) -> Self {
    Self {
      source,
      span: Some(span),
    }
  }

  /// The whole of `source`, for an input with no positions to point at.
  ///
  /// Read the type's documentation before reaching for this: it is a documented exception, and
  /// answering it from a family that has spans discards them silently.
  #[inline]
  pub const fn entire(source: u32) -> Self {
    Self { source, span: None }
  }

  /// Returns which input the location belongs to.
  #[inline]
  pub const fn source(&self) -> u32 {
    self.source
  }

  /// Returns the byte range, or `None` when the location is the whole input.
  #[inline]
  pub const fn span(&self) -> Option<SimpleSpan> {
    self.span
  }

  /// Returns whether the location is the whole input rather than a range within it.
  #[inline]
  pub const fn is_entire(&self) -> bool {
    self.span.is_none()
  }
}

/// A secondary position with a phrase explaining what is there.
///
/// "declared here", "first defined here", "the other selection" — the second half of a diagnostic
/// that is about a *relationship* between two places. The text is `&'static str` on purpose:
/// everything variable about a diagnostic belongs in the lazily rendered message, so that reading
/// the labels off a diagnostic costs no allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
  location: Location,
  text: &'static str,
}

impl Label {
  /// Attaches `text` to `location`.
  #[inline]
  pub const fn new(location: Location, text: &'static str) -> Self {
    Self { location, text }
  }

  /// Returns where the label points.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }

  /// Returns what the label says.
  #[inline]
  pub const fn text(&self) -> &'static str {
    self.text
  }
}

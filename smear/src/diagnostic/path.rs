/// One step of a draft §7.1.2 response path.
///
/// # This is a coordinate in the response tree, not in any source text
///
/// A [`Location`](super::Location) says where in a *document* something is; a path says which
/// field of which element of the *result* an execution error belongs to, so that a client can
/// line the error up with the `null` it is holding. The two never substitute for one another,
/// and a producer that has only one of them answers zero of the other rather than converting.
///
/// The draft's own shape: "path entries … should be strings for Object fields, and 0-indexed
/// integers for List entries". A field name is borrowed from the operation, which is why the
/// segment carries a lifetime rather than owning its text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathSegment<'a> {
  /// A response key — a field's alias where it has one, otherwise its name.
  Field(&'a str),
  /// A zero-based index into a list.
  Index(u32),
}

impl core::fmt::Display for PathSegment<'_> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Field(name) => f.write_str(name),
      Self::Index(index) => write!(f, "{index}"),
    }
  }
}

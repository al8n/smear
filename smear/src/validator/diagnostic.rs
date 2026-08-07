//! What a rule says when it fires: a plain value, rendered only on demand.
//!
//! The two claims this module is built around — that nothing is formatted on the validation path,
//! and that a diagnostic's escape class is decided by the document's source type — are stated on
//! [`Diagnostic`] itself, where a reader of the published documentation will find them. This
//! module is private and its header is not published.

use tokora::SimpleSpan;

use super::{
  Rule,
  schema::{DirectiveLocation, PackedType, RootOperation, Schema, Sym},
};

/// The schema-side half of a diagnostic: what the rule expected, in the schema's own vocabulary.
///
/// Everything here is an integer. Names are [`Sym`]s and type references are [`PackedType`]s, so
/// the whole value is `Copy` and renders only against the [`Schema`] that issued them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Context {
  /// The rule's spans and subject say everything.
  None,
  /// A root operation slot the schema does not fill.
  Root(RootOperation),
  /// A schema type — the type in scope, the type a condition names, the input object a literal is
  /// checked against.
  Type(Sym),
  /// A schema-side member of a schema-side owner: an argument of a field or directive, or a field
  /// of an input object.
  Member {
    /// The owning field, directive or input object type.
    owner: Sym,
    /// The argument or input field.
    name: Sym,
  },
  /// The grammar location a directive was written at.
  Location(DirectiveLocation),
  /// The type expected in the position.
  Expected(PackedType),
  /// A variable usage: the variable's declared type against the type its position expects.
  Usage {
    /// The type the operation declared the variable with.
    variable: PackedType,
    /// The type the position expects.
    expected: PackedType,
  },
  /// A plain count — how many root fields a subscription collected, for instance.
  Count(u32),
}

/// One reason a document is invalid.
///
/// # Nothing is formatted until somebody asks
///
/// A `Diagnostic` is data — a [`Rule`], one or two byte spans, the document's own spelling of the
/// offending name, and a small [`Context`] of interned schema symbols. No string is built and no
/// allocation happens, so a server that only needs "is this document valid" pays for none of it.
/// Rendering is [`Diagnostic::display`], against the schema the diagnostic came from.
///
/// # The lifetime contract `S` carries
///
/// A `Diagnostic<S>` has exactly `S`'s lifetime class, and that is a contract rather than a note:
///
/// - **Borrowed sources** (`&'src str`, `&'src [u8]`) make it **non-escaping**. It must be
///   rendered or dropped before the request buffer's life ends, and must never cross an FFI
///   boundary raw. This is the right default for a Sans-I/O server that renders within the
///   request.
/// - **Refcounted sources** (`smol-bytes`, `bytes`, `hipstr`'s owned states) make it **escaping**:
///   `'static`, cheap to clone, and safe to hand to a caller with its own lifetime discipline.
///
/// Choose the source type by whether diagnostics escape, not by what looks cheapest. `&[u8]` is
/// the cheapest-looking choice and is a use-after-free the moment a foreign caller holds a
/// diagnostic past the buffer.
///
/// # A [`Sym`] is meaningless without its schema
///
/// [`Context`] carries interned symbols, which are indices into one particular [`Schema`]. An
/// escaping diagnostic must therefore either be rendered eagerly into owned text or travel with a
/// handle to the schema that produced it; a raw symbol on its own is an integer with no meaning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostic<S> {
  rule: Rule,
  span: SimpleSpan,
  related: Option<SimpleSpan>,
  subject: Option<S>,
  context: Context,
}

impl<S> Diagnostic<S> {
  /// Creates a diagnostic pointing at one position, with no subject and no context.
  #[inline]
  pub(crate) const fn new(rule: Rule, span: SimpleSpan) -> Self {
    Self {
      rule,
      span,
      related: None,
      subject: None,
      context: Context::None,
    }
  }

  /// Attaches the document's own spelling of the offending name.
  #[inline]
  pub(crate) fn subject(mut self, subject: S) -> Self {
    self.subject = Some(subject);
    self
  }

  /// Attaches the schema-side context.
  #[inline]
  pub(crate) const fn context(mut self, context: Context) -> Self {
    self.context = context;
    self
  }

  /// Points at a second, related position — the first of two duplicates, say.
  #[inline]
  pub(crate) const fn related(mut self, span: SimpleSpan) -> Self {
    self.related = Some(span);
    self
  }

  /// Returns the rule that fired.
  #[inline]
  pub const fn rule(&self) -> Rule {
    self.rule
  }

  /// Returns the position the rule refused.
  #[inline]
  pub const fn span(&self) -> SimpleSpan {
    self.span
  }

  /// Returns a second position the diagnostic refers to, when there is one.
  #[inline]
  pub const fn related_span(&self) -> Option<SimpleSpan> {
    self.related
  }

  /// Returns the document's own spelling of the offending name, when the rule named one.
  #[inline]
  pub const fn subject_source(&self) -> Option<&S> {
    self.subject.as_ref()
  }

  /// Returns the schema-side context.
  #[inline]
  pub const fn context_of(&self) -> Context {
    self.context
  }

  /// Renders the diagnostic against the schema that produced it.
  ///
  /// This is the only place a diagnostic is turned into text, and it is a caller's decision: the
  /// validation path itself formats nothing. Passing a *different* schema than the one validation
  /// ran against renders nonsense — a [`Sym`] belongs to one schema.
  #[inline]
  pub const fn display<'a>(&'a self, schema: &'a Schema) -> DiagnosticDisplay<'a, S> {
    DiagnosticDisplay {
      diagnostic: self,
      schema,
    }
  }
}

/// The rendering of a [`Diagnostic`] against its schema, from [`Diagnostic::display`].
#[derive(Debug, Clone, Copy)]
pub struct DiagnosticDisplay<'a, S> {
  diagnostic: &'a Diagnostic<S>,
  schema: &'a Schema,
}

impl<S> core::fmt::Display for DiagnosticDisplay<'_, S>
where
  S: AsRef<[u8]>,
{
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    let Diagnostic {
      rule,
      span,
      subject,
      context,
      ..
    } = self.diagnostic;
    write!(f, "{rule}: ")?;
    match subject {
      Some(subject) => {
        f.write_str("`")?;
        write_lossy(f, subject.as_ref())?;
        f.write_str("`")?;
      }
      None => f.write_str("<here>")?,
    }
    match context {
      Context::None => {}
      Context::Root(operation) => write!(f, ", no `{operation}` root operation type")?,
      Context::Type(sym) => write!(f, " on type `{}`", self.schema.name(*sym))?,
      Context::Member { owner, name } => write!(
        f,
        " of `{}.{}`",
        self.schema.name(*owner),
        self.schema.name(*name)
      )?,
      Context::Location(location) => write!(f, " at {location}")?,
      Context::Expected(ty) => {
        f.write_str(", expected `")?;
        ty.write(f, self.schema.name(ty.base()))?;
        f.write_str("`")?;
      }
      Context::Usage { variable, expected } => {
        f.write_str(", declared `")?;
        variable.write(f, self.schema.name(variable.base()))?;
        f.write_str("` used where `")?;
        expected.write(f, self.schema.name(expected.base()))?;
        f.write_str("` is expected")?;
      }
      Context::Count(count) => write!(f, ", found {count}")?,
    }
    write!(f, " ({}..{})", span.start(), span.end())
  }
}

/// Writes possibly-non-UTF-8 source bytes without allocating.
///
/// A document's source slice type may be `&[u8]`, so a name is not guaranteed to be text. Valid
/// runs are written as they are and invalid bytes become the replacement character, which is what
/// `String::from_utf8_lossy` would produce — except that this builds no `String`, and the hot path
/// this module exists to keep allocation-free stays that way even while rendering.
fn write_lossy(f: &mut core::fmt::Formatter<'_>, mut bytes: &[u8]) -> core::fmt::Result {
  loop {
    match core::str::from_utf8(bytes) {
      Ok(text) => return f.write_str(text),
      Err(error) => {
        let valid = error.valid_up_to();
        // `valid_up_to` is a UTF-8 boundary by definition, so this cannot fail.
        if let Ok(text) = core::str::from_utf8(&bytes[..valid]) {
          f.write_str(text)?;
        }
        f.write_str("\u{FFFD}")?;
        match error.error_len() {
          Some(len) => bytes = &bytes[valid + len..],
          // The trailing bytes are an incomplete sequence; one replacement covers them.
          None => return Ok(()),
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::write_lossy;
  use std::{format, string::String};

  struct Lossy<'a>(&'a [u8]);

  impl core::fmt::Display for Lossy<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
      write_lossy(f, self.0)
    }
  }

  #[test]
  fn lossy_matches_from_utf8_lossy() {
    for bytes in [
      &b"plain"[..],
      &b""[..],
      &[b'a', 0xff, b'b'][..],
      &[0xff, 0xfe][..],
      &[b'x', 0xe2, 0x82][..],
      "héllo".as_bytes(),
    ] {
      assert_eq!(
        format!("{}", Lossy(bytes)),
        String::from_utf8_lossy(bytes),
        "lossy rendering of {bytes:?} diverges"
      );
    }
  }
}

/// A machine identifier for one diagnostic, stable across every rewording of its message.
///
/// # This, and not the message, is the contract
///
/// [`Display`](core::fmt::Display) text is prose: it is written for a human, it gets clearer over
/// time, and a consumer that pattern-matches on it is broken by the next improvement. A `Code` is
/// the thing that does not move. A rule that gains a better sentence keeps the code it had, and a
/// consumer keying off it — an editor suppressing a class of warning, a CI job counting one kind
/// of refusal, an LSP client mapping to a quick-fix — keeps working.
///
/// # One flat namespace across every family
///
/// A renderer sees schema refusals, executable-validation diagnostics and introspection-door
/// failures in the same pass, so their identifiers have to be comparable without knowing which
/// enum each came from. That is why a code is a string in a single namespace,
/// `smear::<family>::<rule>`, rather than a per-family integer or the kind enum's own spelling:
///
/// - `smear::schema::duplicate-type-name`
/// - `smear::validation::field-selection-merging`
/// - `smear::introspection::malformed-json`
///
/// Strings rather than integers because they are self-describing — a code in a log needs no
/// registry to decode — and because both of the machine consumers this serves take one: LSP's
/// `Diagnostic.code` is `integer | string`, and a §7.1.2 `errors[].extensions` entry is JSON.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Code(&'static str);

impl Code {
  /// Wraps a static identifier.
  #[inline]
  pub const fn new(code: &'static str) -> Self {
    Self(code)
  }

  /// Returns the identifier.
  #[inline]
  pub const fn as_str(&self) -> &'static str {
    self.0
  }
}

impl core::fmt::Display for Code {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.write_str(self.0)
  }
}

impl AsRef<str> for Code {
  #[inline]
  fn as_ref(&self) -> &str {
    self.0
  }
}

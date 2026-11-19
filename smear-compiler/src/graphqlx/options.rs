//! The switches that relax or tighten what an analysis accepts.

/// The switches an analysis runs with: each relaxes or tightens one strictness of the language,
/// and each is off by default, so [`Options::new`] is the language as this crate's documentation
/// describes it.
///
/// A switch changes which programs are accepted, and never what a program accepted both ways
/// means. One that relaxes accepts more: a program that analyzes as ok with it off analyzes to the
/// same model with it on. One that tightens refuses more: a program that analyzes as ok with it on
/// analyzes to the same model with it off.
///
/// A switch is the analysis's, not a package's: one setting holds for every package analyzed
/// together, so a dependency that needs one makes each program that uses it turn it on. A program
/// says so where it is built, in its manifest or its tool's configuration, and the analysis
/// records what it ran under, [`Analysis::options`](crate::graphqlx::Analysis::options).
///
/// [`Limits`](crate::graphqlx::Limits) is the other half of an analysis's configuration: it bounds
/// what the analysis spends and the shapes it admits, where these switches relax or tighten what
/// the language accepts.
///
/// # Building one
///
/// [`Options::new`] and [`Options::default`] give every switch off, and each `with_*` setter turns
/// one on or off. Outside code builds one through those alone, so a switch added later, with its
/// getter and its setter, breaks no caller. A `const` works too:
///
/// ```
/// use smear_compiler::graphqlx::Options;
///
/// const OPTIONS: Options = Options::new();
///
/// assert_eq!(OPTIONS, Options::default());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Options {}

impl Default for Options {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl Options {
  /// Every switch off: the language as this crate's documentation describes it.
  ///
  /// [`Options::default`] returns the same value; this one can start a `const` item.
  #[inline]
  pub const fn new() -> Self {
    Self {}
  }
}

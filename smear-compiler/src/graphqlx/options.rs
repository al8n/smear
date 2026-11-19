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
/// | Switch | Kind | Off, by default | On |
/// |---|---|---|---|
/// | [`extern_collections_as_written`](Self::extern_collections_as_written) | relaxes | A collection takes its own literal or `null`, an extern one as any other; any other literal is `smear::graphqlx::collection-literal` | An extern collection keeps a literal that is neither `set!{…}`, `map!{…}` nor `null` as written |
///
/// [`Limits`](crate::graphqlx::Limits) is the other half of an analysis's configuration: it bounds
/// what the analysis spends and the shapes it admits, where these switches relax or tighten what
/// the language accepts.
///
/// # Building one
///
/// [`Options::new`] and [`Options::default`] give every switch off, and each `with_*` setter turns
/// one on or off. The fields are private, so a switch added later, with its getter and its setter,
/// breaks no caller. A `const` works too:
///
/// ```
/// use smear_compiler::graphqlx::Options;
///
/// const MINE: Options = Options::new().with_extern_collections_as_written(true);
///
/// assert!(MINE.extern_collections_as_written());
/// assert_eq!(Options::new(), Options::default());
/// assert!(!Options::new().extern_collections_as_written());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Options {
  extern_collections_as_written: bool,
}

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
    Self {
      extern_collections_as_written: false,
    }
  }

  /// Whether an extern collection keeps a literal other than its own as written, as an extern
  /// scalar keeps its value. An extern collection is a scalar its own package makes a set or a map
  /// with `core`'s `@set` or `@map`, and to which its own package applies `core`'s `@extern`, in
  /// its definition or in an extension of its own.
  ///
  /// Off, every collection takes its own literal and `null` alone, an extern one as any other:
  /// `set!{…}` for a set and `map!{…}` for a map, each element, key and value coerced, and any
  /// other literal is `smear::graphqlx::collection-literal`. On, an extern collection still takes
  /// its own literal so, and `null` as before. Any other literal but the other collection's,
  /// `[1, 2]` or `"a,b"` for an `IndexSet<i32>`, is kept as written, a
  /// [`ValueKind::Literal`](crate::graphqlx::ValueKind::Literal) for its host type to read, and
  /// nothing inside it is coerced or compared. A `map!{…}` for a set, and a `set!{…}` for a map,
  /// stay `collection-literal`. A collection to which its own package applies no `@extern` is
  /// strict whatever the switch: the built-in library's `HashSet` and `BTreeMap` among them, even
  /// where the schema gives one a host type with `@repr(for: …)`.
  #[inline]
  pub const fn extern_collections_as_written(&self) -> bool {
    self.extern_collections_as_written
  }

  /// The same options with [`extern_collections_as_written`](Self::extern_collections_as_written)
  /// set to `allow`.
  #[inline]
  #[must_use]
  pub const fn with_extern_collections_as_written(mut self, allow: bool) -> Self {
    self.extern_collections_as_written = allow;
    self
  }
}

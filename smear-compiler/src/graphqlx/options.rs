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
/// | [`judged_metadata`](Self::judged_metadata) | tightens | The tier rule judges no compile-time metadata: the types a directive definition writes, every application's type arguments, written or left to a default, and the definitions `Def::is_metadata` marks | The tier rule judges them as it judges any other use, `@heap` on a directive's argument included, and never a value |
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
/// const MINE: Options = Options::new()
///   .with_extern_collections_as_written(true)
///   .with_judged_metadata(true);
///
/// assert!(MINE.extern_collections_as_written() && MINE.judged_metadata());
/// assert_eq!(Options::new(), Options::default());
/// assert!(!Options::new().extern_collections_as_written());
/// assert!(!Options::new().judged_metadata());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Options {
  extern_collections_as_written: bool,
  judged_metadata: bool,
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
      judged_metadata: false,
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

  /// Whether the tier rule judges compile-time metadata too, as it judges every other use.
  /// Compile-time metadata is every type a directive definition writes, its arguments' types and
  /// its type parameters' defaults and bounds; every type argument of an application of any
  /// directive, the written ones and those its directive's defaults fill; and every definition
  /// [`Def::is_metadata`](crate::graphqlx::Def::is_metadata) marks.
  ///
  /// Off, none of it counts toward a `smear::graphqlx::tier` report: a package declared at `core`
  /// may declare a directive whose argument is a `string`, apply another package's directive with
  /// a `string` type argument, written or left to its default, and declare an input object of
  /// `string` fields that `Def::is_metadata` marks. On, the tier rule judges each of them as it
  /// judges any other use, each where it is written: a list; another package's definition that
  /// needs more; `core`'s `@heap`, on a directive's argument too; an extern's `tier`; and an
  /// application of another package's generic directive whose left-out type arguments' defaults
  /// need more, at its path, labelled at the default.
  ///
  /// It judges types and never a value: an application of another package's directive is accepted
  /// whatever types that directive's arguments take, the strings it writes among them.
  ///
  /// It changes what the rule reports and nothing the model answers but the `has_errors()` of what
  /// those reports mark: [`Def::is_metadata`](crate::graphqlx::Def::is_metadata) and
  /// [`Def::tier`](crate::graphqlx::Def::tier) answer the program's own facts whatever it says.
  /// Under it, in an analysis that is ok, every definition's tier is at most its package's,
  /// GraphQL's meta-types aside and metadata included: what a backend that emits every definition
  /// at its package's tier needs.
  #[inline]
  pub const fn judged_metadata(&self) -> bool {
    self.judged_metadata
  }

  /// The same options with [`judged_metadata`](Self::judged_metadata) set to `judge`.
  #[inline]
  #[must_use]
  pub const fn with_judged_metadata(mut self, judge: bool) -> Self {
    self.judged_metadata = judge;
    self
  }
}

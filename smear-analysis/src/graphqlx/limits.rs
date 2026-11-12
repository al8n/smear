//! The bounds an analysis runs under, so that untrusted input costs a bounded amount of work.

/// See [`Limits::max_definitions`].
const DEFAULT_DEFINITIONS: u32 = 1 << 20;
/// See [`Limits::max_module_depth`].
const DEFAULT_MODULE_DEPTH: u32 = 64;
/// See [`Limits::max_path_segments`].
const DEFAULT_PATH_SEGMENTS: u32 = 64;
/// See [`Limits::max_use_depth`].
const DEFAULT_USE_DEPTH: u32 = 32;
/// See [`Limits::max_glob_iterations`].
const DEFAULT_GLOB_ITERATIONS: u32 = 1 << 10;
/// See [`Limits::max_generic_depth`].
const DEFAULT_GENERIC_DEPTH: u32 = 64;
/// See [`Limits::max_instantiations`].
const DEFAULT_INSTANTIATIONS: u32 = 1 << 16;
/// See [`Limits::max_source_bytes`].
const DEFAULT_SOURCE_BYTES: u32 = 1 << 26;
/// See [`Limits::max_diagnostics`].
const DEFAULT_DIAGNOSTICS: u32 = 16_384;
/// See [`Limits::max_near_miss_comparisons`].
const DEFAULT_NEAR_MISS_COMPARISONS: u32 = 1 << 20;

/// The bounds one analysis runs under.
///
/// A package set is untrusted input, and the analysis bounds every resource such input could
/// otherwise grow without limit: the number of definitions, how deep modules, `use` trees and type
/// arguments nest, how long a path is, how many rounds the glob fixpoint runs, how many
/// instantiations it records, and how many bytes of source it reads. A bound that is passed is
/// reported as `smear::graphqlx::resource-limit`, never as a panic. Two bounds are not refusals:
/// the diagnostic ceiling and the near-miss budget limit what the analysis says, not what it
/// accepts.
///
/// Every bound is a count, and `0` admits none of what it counts, except that the built-in library
/// is always declared whole: see [`Limits::max_definitions`].
///
/// # Building one
///
/// [`Limits::new`] and [`Limits::default`] give the defaults, and each `with_*` setter replaces one
/// bound. The fields are private so that a bound added later, with its getter and its setter,
/// breaks no caller. A `const` works too:
///
/// ```
/// use smear_analysis::graphqlx::Limits;
///
/// const LIMITS: Limits = Limits::new().with_max_diagnostics(100);
///
/// assert_eq!(LIMITS.max_diagnostics(), 100);
/// assert_eq!(LIMITS.max_source_bytes(), Limits::new().max_source_bytes());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Limits {
  max_definitions: u32,
  max_module_depth: u32,
  max_path_segments: u32,
  max_use_depth: u32,
  max_glob_iterations: u32,
  max_generic_depth: u32,
  max_instantiations: u32,
  max_source_bytes: u32,
  max_diagnostics: u32,
  max_near_miss_comparisons: u32,
}

impl Default for Limits {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl Limits {
  /// The default bounds.
  ///
  /// [`Limits::default`] returns the same value; this one can start a `const` item.
  #[inline]
  pub const fn new() -> Self {
    Self {
      max_definitions: DEFAULT_DEFINITIONS,
      max_module_depth: DEFAULT_MODULE_DEPTH,
      max_path_segments: DEFAULT_PATH_SEGMENTS,
      max_use_depth: DEFAULT_USE_DEPTH,
      max_glob_iterations: DEFAULT_GLOB_ITERATIONS,
      max_generic_depth: DEFAULT_GENERIC_DEPTH,
      max_instantiations: DEFAULT_INSTANTIATIONS,
      max_source_bytes: DEFAULT_SOURCE_BYTES,
      max_diagnostics: DEFAULT_DIAGNOSTICS,
      max_near_miss_comparisons: DEFAULT_NEAR_MISS_COMPARISONS,
    }
  }

  /// How many definitions the model may hold, over every package. Every definition the model would
  /// hold counts, the built-in library's own included: each type and directive a file declares, and
  /// each primitive `core` makes for a width a package writes. A root package's definition that
  /// replaces one of GraphQL's built-ins takes that built-in's definition and adds none, unless the
  /// built-in is a primitive that no package writes, which it makes.
  ///
  /// The built-in library is always declared whole, so a limit below its own count admits no
  /// supplied definition at all. The first definition a supplied package would add past the limit,
  /// a type, a directive or a primitive it writes, is reported once, where it is named, and nothing
  /// after it is collected: no item of the rest of its file, nor of any file after it.
  ///
  /// The default is 2^20. A hand-written schema declares thousands of definitions, not a million,
  /// and the bound is what stops a generated or hostile package set from growing the model's
  /// tables without limit before any rule has run.
  #[inline]
  pub const fn max_definitions(&self) -> u32 {
    self.max_definitions
  }

  /// How deep a module may sit below its package's root module, each directory and each inline
  /// `mod` counting one level: the files of `media/vision/` are a module two deep, and a `mod`
  /// written in one of them declares a module three deep.
  ///
  /// A directory deeper than this is reported once, at its package's package source, and the
  /// files under it are parsed but place nothing in any module; a `mod` deeper than this is
  /// reported where it names its module, and nothing inside it is collected.
  ///
  /// The default is 64. A source tree is rarely ten directories deep; the module tree is walked
  /// with an explicit stack, so the bound limits that stack's growth rather than protecting the
  /// native one, and an input deeper than it is a generated one.
  #[inline]
  pub const fn max_module_depth(&self) -> u32 {
    self.max_module_depth
  }

  /// How many segments one written path may have: `^::geo::shapes::Point` has three.
  ///
  /// The default is 64. A path names a package, a few modules and an item; resolution walks each
  /// path segment by segment, so the bound limits that walk for a path no author writes.
  #[inline]
  pub const fn max_path_segments(&self) -> u32 {
    self.max_path_segments
  }

  /// How deeply one `use` tree may nest its braces: `use a::{b::{c}}` is two deep.
  ///
  /// The default is 32. The parser's own bracket ceiling (24 at its default) refuses a deeper
  /// tree first, so this bound decides only for a tree the parser was allowed to read deeper.
  #[inline]
  pub const fn max_use_depth(&self) -> u32 {
    self.max_use_depth
  }

  /// How many rounds the glob fixpoint may run, each round resolving the `use` items the previous
  /// round's globs made resolvable.
  ///
  /// The default is 2^10. The fixpoint needs at most one round per `use` item that waits on
  /// another, and a real program needs a handful; a chain of a thousand modules each globbing the
  /// next is a generated one, and stopping it bounds the fixpoint's work.
  #[inline]
  pub const fn max_glob_iterations(&self) -> u32 {
    self.max_glob_iterations
  }

  /// How deeply type arguments may nest in one type once substitution has filled them:
  /// `Vec<Map<string, Vec<i32>>>` is three deep.
  ///
  /// The default is 64. A written type is already bounded by the parser's bracket ceiling (24 at
  /// its default); substitution is what can go deeper, since `interface A<X> implements B<Box<X>>`
  /// wraps once more per level, and this bound stops that growth.
  #[inline]
  pub const fn max_generic_depth(&self) -> u32 {
    self.max_generic_depth
  }

  /// How many distinct instantiations of generic definitions the analysis may record, those the
  /// program writes and those substitution derives together.
  ///
  /// The default is 2^16. A program writes hundreds of instantiations; derived ones can double per
  /// level of an `implements` chain, and this bound is what keeps that doubling finite.
  #[inline]
  pub const fn max_instantiations(&self) -> u32 {
    self.max_instantiations
  }

  /// How many bytes of source text the supplied packages may hold together. The built-in
  /// packages' own text is smear's and does not count.
  ///
  /// Past it, the package set is refused at the root package and no supplied file is parsed.
  ///
  /// The default is 2^26, 64 MiB: far more than any package set written by hand, and the bound on
  /// everything the analysis reads. Because the bound is a `u32`, every byte offset into an
  /// accepted package set fits one.
  #[inline]
  pub const fn max_source_bytes(&self) -> u32 {
    self.max_source_bytes
  }

  /// How many diagnostics one analysis keeps: the least of them, in the order its diagnostics
  /// iterate.
  ///
  /// Past it the greater ones are dropped, the list says it was truncated, and every error is
  /// still counted, so no dropped error changes the verdict. Which are kept depends on nothing but
  /// the diagnostics themselves, not on the order they were reported in.
  ///
  /// The default is 16 384, smear-schema's ceiling on each of its two quadratic lists: more than
  /// an author reads, and a bound on memory against an input built to be refused many times over.
  #[inline]
  pub const fn max_diagnostics(&self) -> u32 {
    self.max_diagnostics
  }

  /// How many name comparisons the analysis may spend, over the whole analysis, looking for a "did
  /// you mean" suggestion for names that do not resolve.
  ///
  /// Past it, a name that does not resolve is still reported, without a suggestion. The search
  /// compares each unresolved name with every name visible where it was written, which is
  /// quadratic over the program; this bound makes it linear in the worst case.
  ///
  /// The default is 2^20: a program with a thousand unresolved names among a thousand visible
  /// ones still gets every suggestion, and an honest program has a handful of unresolved names.
  #[inline]
  pub const fn max_near_miss_comparisons(&self) -> u32 {
    self.max_near_miss_comparisons
  }

  /// The same limits with `max` as their [`max_definitions`](Self::max_definitions) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_definitions(mut self, max: u32) -> Self {
    self.max_definitions = max;
    self
  }

  /// The same limits with `max` as their [`max_module_depth`](Self::max_module_depth) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_module_depth(mut self, max: u32) -> Self {
    self.max_module_depth = max;
    self
  }

  /// The same limits with `max` as their [`max_path_segments`](Self::max_path_segments) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_path_segments(mut self, max: u32) -> Self {
    self.max_path_segments = max;
    self
  }

  /// The same limits with `max` as their [`max_use_depth`](Self::max_use_depth) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_use_depth(mut self, max: u32) -> Self {
    self.max_use_depth = max;
    self
  }

  /// The same limits with `max` as their [`max_glob_iterations`](Self::max_glob_iterations) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_glob_iterations(mut self, max: u32) -> Self {
    self.max_glob_iterations = max;
    self
  }

  /// The same limits with `max` as their [`max_generic_depth`](Self::max_generic_depth) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_generic_depth(mut self, max: u32) -> Self {
    self.max_generic_depth = max;
    self
  }

  /// The same limits with `max` as their [`max_instantiations`](Self::max_instantiations) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_instantiations(mut self, max: u32) -> Self {
    self.max_instantiations = max;
    self
  }

  /// The same limits with `max` as their [`max_source_bytes`](Self::max_source_bytes) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_source_bytes(mut self, max: u32) -> Self {
    self.max_source_bytes = max;
    self
  }

  /// The same limits with `max` as their [`max_diagnostics`](Self::max_diagnostics) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_diagnostics(mut self, max: u32) -> Self {
    self.max_diagnostics = max;
    self
  }

  /// The same limits with `max` as their
  /// [`max_near_miss_comparisons`](Self::max_near_miss_comparisons) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_near_miss_comparisons(mut self, max: u32) -> Self {
    self.max_near_miss_comparisons = max;
    self
  }
}

//! The bounds an analysis runs under, so that untrusted input costs a bounded amount of work.

/// See [`Limits::max_definitions`].
const DEFAULT_DEFINITIONS: u32 = 1 << 20;
/// See [`Limits::max_modules`].
const DEFAULT_MODULES: u32 = 1 << 16;
/// See [`Limits::max_module_depth`].
const DEFAULT_MODULE_DEPTH: u32 = 64;
/// See [`Limits::max_path_segments`].
const DEFAULT_PATH_SEGMENTS: u32 = 64;
/// See [`Limits::max_use_depth`].
const DEFAULT_USE_DEPTH: u32 = 32;
/// See [`Limits::max_glob_iterations`].
const DEFAULT_GLOB_ITERATIONS: u32 = 1 << 10;
/// See [`Limits::max_globs`].
const DEFAULT_GLOBS: u32 = 64;
/// See [`Limits::max_generic_depth`].
const DEFAULT_GENERIC_DEPTH: u32 = 64;
/// See [`Limits::max_instantiations`].
const DEFAULT_INSTANTIATIONS: u32 = 1 << 16;
/// See [`Limits::max_substitutions`].
const DEFAULT_SUBSTITUTIONS: u32 = 1 << 20;
/// See [`Limits::max_bound_steps`].
const DEFAULT_BOUND_STEPS: u32 = 1 << 20;
/// See [`Limits::max_value_steps`].
const DEFAULT_VALUE_STEPS: u32 = 1 << 20;
/// See [`Limits::max_implementation_steps`].
const DEFAULT_IMPLEMENTATION_STEPS: u32 = 1 << 20;
/// See [`Limits::max_source_bytes`].
const DEFAULT_SOURCE_BYTES: u32 = 1 << 26;
/// See [`Limits::max_diagnostics`].
const DEFAULT_DIAGNOSTICS: u32 = 16_384;
/// See [`Limits::max_near_miss_comparisons`].
const DEFAULT_NEAR_MISS_COMPARISONS: u32 = 1 << 20;

// The lossless parser's default ceiling on the diagnostics a parse keeps is at least twice this
// analysis's default ceiling on diagnostics, so that a default analysis through the lossless layer
// is handed a record for every error its list can show, and a recovery hole beside each. Neither
// default moves without the other.
const _: () = assert!(
  smear_lexer::limits::LosslessLimits::new().max_diagnostics()
    >= 2 * (DEFAULT_DIAGNOSTICS as usize),
  "the lossless parser's default ceiling on kept diagnostics must be at least twice the \
   analysis's default ceiling on diagnostics"
);

/// The bounds one analysis runs under.
///
/// A package set is untrusted input, and these bounds hold what the analysis builds and spends
/// once it has numbered the package set: the number of definitions and of modules, how deep
/// modules, `use` trees and types nest, how many segments a path a scope resolves has, how many
/// rounds the glob fixpoint runs, how many modules one module's globs name, how many instantiations
/// of generic definitions it interns, how many steps of substitution it takes, how many steps
/// judging bounds takes, how many steps completing values takes, how many steps checking
/// implementations takes, and how many bytes the supplied texts hold. A bound that is passed stops
/// that work, is reported as `smear::graphqlx::resource-limit`, and the analysis goes on. Two
/// bounds are not refusals: the diagnostic ceiling and the near-miss budget limit what the analysis
/// says, not what it accepts.
///
/// Before any bound applies, the diagnostic ceiling aside, the analysis reads what the caller
/// supplied, to number and order it. It sorts each package's files by path, and orders two files
/// that share a path, which the package set refuses, by their texts, so that no id depends on the
/// order the caller passed them in. It sorts the dependencies by name, and orders two that share a
/// name, which the package set refuses too, by everything they hold, their files' texts included.
/// It sorts each package's declared dependencies, copies every package's name and every file's
/// path, checks each name and each path, and resolves each declared dependency; only then does it
/// sum the texts' lengths against [`max_source_bytes`](Limits::max_source_bytes). That work grows
/// with everything the caller supplies, texts included, and only the caller can bound it.
///
/// The parser's own ceilings are not among these bounds: how deep brackets nest, how many tokens a
/// parse lexes, and how many diagnostics it keeps. They belong to the parser value:
/// [`analyze`](crate::graphqlx::analyze) reads every file with the syntactic layer's defaults, and
/// [`analyze_with`](crate::graphqlx::analyze_with) takes a layer that holds its own limits,
/// `smear-lexer`'s `SyntacticLimits` or `LosslessLimits`, whose ceilings are `usize`, the lexer's
/// width, where these bounds are `u32`.
///
/// Every bound is a count, and `0` admits none of what it counts, except that the built-in library
/// is always declared whole and every package has its root module, and that the built-in library's
/// types are built whatever the generic depth, instantiation and substitution bounds are, its
/// bounds cost nothing to judge, since it writes no instantiation of a definition with a bound,
/// its own values cost nothing to complete, where it writes them and in a copy made for an
/// application it writes, and its implementations cost nothing to check, since it writes no
/// `implements` entry and no union: see
/// [`Limits::max_definitions`], [`Limits::max_modules`], [`Limits::max_generic_depth`],
/// [`Limits::max_instantiations`], [`Limits::max_substitutions`], [`Limits::max_bound_steps`],
/// [`Limits::max_value_steps`] and [`Limits::max_implementation_steps`].
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
  max_modules: u32,
  max_module_depth: u32,
  max_path_segments: u32,
  max_use_depth: u32,
  max_glob_iterations: u32,
  max_globs: u32,
  max_generic_depth: u32,
  max_instantiations: u32,
  max_substitutions: u32,
  max_bound_steps: u32,
  max_value_steps: u32,
  max_implementation_steps: u32,
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
      max_modules: DEFAULT_MODULES,
      max_module_depth: DEFAULT_MODULE_DEPTH,
      max_path_segments: DEFAULT_PATH_SEGMENTS,
      max_use_depth: DEFAULT_USE_DEPTH,
      max_glob_iterations: DEFAULT_GLOB_ITERATIONS,
      max_globs: DEFAULT_GLOBS,
      max_generic_depth: DEFAULT_GENERIC_DEPTH,
      max_instantiations: DEFAULT_INSTANTIATIONS,
      max_substitutions: DEFAULT_SUBSTITUTIONS,
      max_bound_steps: DEFAULT_BOUND_STEPS,
      max_value_steps: DEFAULT_VALUE_STEPS,
      max_implementation_steps: DEFAULT_IMPLEMENTATION_STEPS,
      max_source_bytes: DEFAULT_SOURCE_BYTES,
      max_diagnostics: DEFAULT_DIAGNOSTICS,
      max_near_miss_comparisons: DEFAULT_NEAR_MISS_COMPARISONS,
    }
  }

  /// How many definitions the model may hold, over every package. Every definition the model would
  /// hold counts, the built-in library's own included: each type and directive a file declares, and
  /// each primitive `core` makes for a width a package writes. A root package's definition that
  /// replaces one of GraphQL's built-ins takes that built-in's definition and adds none, unless the
  /// built-in is a primitive no package has written yet, which it makes.
  ///
  /// The built-in library is always declared whole, so a limit below its own count admits no
  /// supplied definition beyond one that replaces a GraphQL built-in the model already holds, which
  /// adds none. The first definition a supplied package would add past the limit, a type, a
  /// directive or a primitive it writes, is reported once, where it is named, and nothing after it
  /// is collected: no item of the rest of its file, nor of any file after it.
  ///
  /// The default is 2^20. A hand-written schema declares thousands of definitions, not a million,
  /// and the bound caps a generated or hostile package set at that many before any rule has run.
  #[inline]
  pub const fn max_definitions(&self) -> u32 {
    self.max_definitions
  }

  /// How many modules the model may hold, over every package. Every module the model would hold
  /// counts: each package's root module, each directory module of a `Directory` package, and the
  /// module of each inline `mod` that declares one its package does not have yet. A `mod` of a
  /// module the package already has adds none.
  ///
  /// The built-in packages' modules and every package's root module are always held, so a limit
  /// below their count admits no other module. The first module a supplied package's directory or
  /// `mod` would add past the limit is reported once: a `mod` where it names its module, a directory
  /// at its package's package source. No module is added after it: the files under a refused
  /// directory are parsed but placed in no module, and nothing inside a refused `mod` is collected,
  /// as past [`max_module_depth`](Self::max_module_depth). What sits in a module already held is
  /// still collected.
  ///
  /// The default is 2^16. The model keeps each module's path, one name per segment, so this bound
  /// and [`max_module_depth`](Self::max_module_depth) together bound the model's path table: it
  /// holds at most their product of names.
  #[inline]
  pub const fn max_modules(&self) -> u32 {
    self.max_modules
  }

  /// How deep a module may sit below its package's root module, each directory and each inline
  /// `mod` counting one level: the files of `media/vision/` are a module two deep, and a `mod`
  /// written in one of them declares a module three deep.
  ///
  /// An inline `mod` opens a brace, so at the parser's default bracket ceiling, 24, the parser
  /// refuses inline modules nested deeper than that before this bound can: it decides for directory
  /// modules, and for a parser value given a deeper ceiling.
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

  /// How many segments one path may have where a scope resolves it: a type's, a directive's, a
  /// `use` item's, an extension's target, a root and a type value, `^::geo::shapes::Point` having
  /// three. An enum value's segments but its last are resolved, so `Color::RED` has one, and a bare
  /// enum value, looked up in its enum, none. The built-in library's paths count as any other's.
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

  /// How many rounds the glob fixpoint of one module's `use` items may run, each round resolving
  /// the items that the names and the globs of the round before made resolvable. A round looks up
  /// only those items, so the bound limits how many rounds there are, not what one costs.
  ///
  /// A chain needs a round a link, with or without a glob in it: a module that globs a module that
  /// globs the next, or `use` items that each rename the one before by a path of its head alone,
  /// `use A1 as A2`, which waits until both namespaces have its name. Either chain longer than the
  /// bound is reported once for its module, at the head the next round would resolve, and the items
  /// left are reported no further.
  ///
  /// The default is 2^10. The fixpoint needs at most one round per `use` item that waits on
  /// another, and a real program needs a handful; a chain a thousand long is a generated one.
  #[inline]
  pub const fn max_glob_iterations(&self) -> u32 {
    self.max_glob_iterations
  }

  /// How many modules the glob `use` items of one module may name: `use ~::a::*` and
  /// `use ^::geo::*` name two, and another glob of a module already globbed names none more.
  ///
  /// A name looked up in a module's scope is searched for in each module its globs name, and so is
  /// a near miss of a name that does not resolve, so this bound is what one lookup looks at. The
  /// first glob that would name one module more is reported once for its module, where it is
  /// written, and gives nothing; a later glob of yet another module gives nothing either,
  /// unreported, and one of a module already globbed is still accepted. Which glob is refused
  /// depends on the program alone: a module's `use` items resolve in an order that the order the
  /// files and the dependencies are given in does not change.
  ///
  /// The default is 64. A module globs a handful of modules; one that globs more is generated, and
  /// every lookup in its scope would pay for each.
  #[inline]
  pub const fn max_globs(&self) -> u32 {
    self.max_globs
  }

  /// How deeply one type may nest once substitution has filled it: a type argument and a list's
  /// element each nest one level, so `Vec<Map<string, Vec<i32>>>` is three deep, and `[[i32]]` is
  /// two.
  ///
  /// A type deeper than this is reported at each occurrence that would have it, which is then the
  /// unresolved type, and at each directive application whose type would, at its path: an
  /// application is typed as an occurrence of its directive, a level above its type arguments, so
  /// `@d<[Int]>` is two deep. But a bound put under an instantiation's arguments that would nest
  /// deeper than this is reported at each occurrence of the instantiation, where it is written, and
  /// the occurrence keeps its type; and a type that checking an implementation reads under an
  /// `implements` entry's type arguments that would nest deeper stops the check of its definition,
  /// reported once, at the entry it was checking, and the comparing of an instantiation's entries
  /// and members, reported at each occurrence of it. The built-in library's types are built
  /// whatever the bound, so `0`, which refuses every list and every type with an argument a
  /// supplied package writes, reports nothing inside it.
  ///
  /// The default is 64. A written type is already bounded by the parser's bracket ceiling (24 at
  /// its default); substitution is what can go deeper, since a default that wraps a parameter once
  /// more, or `interface A<X> implements B<Box<X>>`, nests once more per level, and this bound
  /// stops that growth.
  #[inline]
  pub const fn max_generic_depth(&self) -> u32 {
    self.max_generic_depth
  }

  /// How many distinct instantiations of generic definitions the analysis may intern: each
  /// definition of at least one type parameter with each list of arguments, those the program
  /// writes, those filling its defaults derives, those a bound derives when it is put under an
  /// instantiation's arguments, each directive application's type, its directive with its
  /// complete type arguments, those a member's type rebuilt under type arguments derives, and
  /// those checking an implementation derives, reading a type under an `implements` entry's type
  /// arguments, together, a type that names a type parameter included. An instantiation written
  /// nullable and non-null counts once.
  ///
  /// Past it, each occurrence that would need one more is reported where it is written, and is the
  /// unresolved type; one already interned is still built. An application that would need one
  /// more is reported at its path, and its type arguments are not completed. A value whose type
  /// under type arguments would need one more is reported as
  /// [`max_value_steps`](Self::max_value_steps) says of a type that cannot be built. An occurrence
  /// whose bounds, put under its arguments, would need one more is reported where it is written
  /// too, and keeps its type; a definition whose implementation's check would need one more is
  /// reported once, at the `implements` entry it was checking; and an instantiation whose entries
  /// and members, compared under its arguments, would need one more is reported at each
  /// occurrence of it. The built-in library's are not
  /// counted.
  ///
  /// The default is 2^16. A program writes hundreds of instantiations; derived ones can double per
  /// level of a chain of defaults, or of an `implements` chain, and this bound is what keeps that
  /// doubling finite.
  #[inline]
  pub const fn max_instantiations(&self) -> u32 {
    self.max_instantiations
  }

  /// How many steps of substitution the analysis may take, over the whole analysis.
  ///
  /// Substitution puts types in place of a generic definition's type parameters. Filling the type
  /// arguments a use leaves out is substitution, a directive application's left-out type
  /// arguments included: a set of written arguments filled for the first time takes one step for
  /// each argument left out, and one for each type occurrence of a default that the filling walks,
  /// whether the type that occurrence yields is new or not. A set of arguments filled once is not
  /// filled again, whichever use asks for it. Judging a bound puts an instantiation's arguments in
  /// place of the parameters the bound names too, and draws on
  /// [`max_bound_steps`](Self::max_bound_steps), never on this bound; and checking an
  /// implementation puts an `implements` entry's type arguments in place of an interface's
  /// parameters, and draws on [`max_implementation_steps`](Self::max_implementation_steps), never
  /// on this bound.
  ///
  /// Past it, each type occurrence whose type would take one more step is reported where it is
  /// written, and its type is the unresolved one; an application is reported at its path, and its
  /// type arguments are not completed.
  ///
  /// The default is 2^20: sixteen steps for each of the instantiations
  /// [`max_instantiations`](Self::max_instantiations) admits at its default, which no program
  /// written by hand approaches. An instantiation is counted once however many steps deriving it
  /// takes, so this is the bound on that work: a default of many types, filled for many distinct
  /// uses, is walked once for each.
  #[inline]
  pub const fn max_substitutions(&self) -> u32 {
    self.max_substitutions
  }

  /// How many steps the analysis may take to judge bounds, over the whole analysis.
  ///
  /// The bounds of each instantiation the program writes of a definition with a bound are judged
  /// once, whatever nullness the instantiation is written with, and so are a directive's bounds
  /// once for each distinct type its applications have. A step is one of: a bound taken
  /// under the instantiation's arguments; a node of a bound's interface rebuilt with those
  /// arguments in place of the definition's type parameters; an `implements` entry, or a type
  /// parameter's bound, looked at; and a pair of types, or an argument, compared. What a type
  /// argument takes against one interface type is taken once, whichever instantiation asks it.
  ///
  /// Past it, each occurrence of an instantiation whose bounds could not be judged within it is
  /// reported where it is written, and keeps its type, and each such application is reported at
  /// its path. Every bound takes a step, so `0` reports each written instantiation of a definition
  /// with a bound, and each application of a directive with one whose type arguments complete, a
  /// `HashMap` among them.
  ///
  /// The default is 2^20: sixteen steps for each of the instantiations
  /// [`max_instantiations`](Self::max_instantiations) admits at its default. `Sorted<User>`, of
  /// `type Sorted<T> where T: Comparable<T>` and a `User` that implements two interfaces, takes
  /// five: one for the bound, two to put `User` into `Comparable<T>`, and two for `User`'s
  /// `implements` entries.
  #[inline]
  pub const fn max_bound_steps(&self) -> u32 {
    self.max_bound_steps
  }

  /// How many steps the analysis may take to complete values beyond what is written, over the
  /// whole analysis.
  ///
  /// A written value takes none, but for the conversion of a long decimal integer below:
  /// [`max_source_bytes`](Self::max_source_bytes) bounds the rest of what is written. A step is
  /// one of: a value of a copy of a default, which fills an argument an application leaves out or
  /// a field an object literal leaves out, each value nested in the default counting one, the
  /// copies of the defaults its own object literals leave out among them; a read, while a copy is
  /// counted, of the count of each default one of its object literals leaves out, and a read of a
  /// count made already, when a later copy of its default is asked for, each spent whether or not
  /// the copy it was read for is made; a default left out whose copy would hold a copy of itself,
  /// which is not made; a list that a written value stands for where a list is expected, one for
  /// each level of list between the value and its type, as `tags: [[Int]] = 1` stands for a list
  /// of a list of `1`; a required argument an application leaves out, and a required field an
  /// object literal leaves out, each found left out, reported where it is written and not in a
  /// count; a node a rebuild of a member's type
  /// under type arguments looks at, its root included, once for each written type and
  /// instantiation; a node of a count made under a type its default is not written against, and an
  /// identity of a set's element or of a map's key that such a count renders to search them for
  /// one held twice, a step for each 64 bytes of it or part, once for each literal and the type its
  /// identities are read against; a field an object literal of a default writes, one its input
  /// object does not declare included, the first time the default's count reads the literal
  /// against that input object; an argument that may be required, a bare nullable type parameter
  /// of its directive, that an application with complete type arguments leaves out, and a field
  /// that may be required that a written object literal of a generic input object leaves out; a
  /// pair of a closed instantiation, of a type the program writes or of a generic directive an
  /// application of it makes, and what its definition records, checked under the instantiation's
  /// arguments: a value whose type names its type parameters, or an argument an application in it
  /// leaves out whose type is one of them; and converting a decimal integer of more
  /// than 38 significant digits, the most `u128` arithmetic reads whatever they spell, to its
  /// value, charged before the conversion as an upper bound of its work:
  /// `ceil(d / 9) × (d × 3322 / 32000 + 2)` steps for `d` digits, its chunks of nine digits times
  /// the 32-bit limbs of its value. A hexadecimal, octal or binary integer converts in one pass and
  /// takes none. The built-in library's own values take none: its defaults where it writes them,
  /// and a copy made for an application it writes. A copy of one of its defaults made for an
  /// application a supplied package writes, a bare `@deprecated` among them, is charged.
  ///
  /// A type that cannot be built under type arguments, past this bound, past
  /// [`max_generic_depth`](Self::max_generic_depth) or past
  /// [`max_instantiations`](Self::max_instantiations), makes a written value one unresolved row,
  /// `resource-limit` at it, and starves an application that leaves out an argument of that type,
  /// the report naming the limit.
  ///
  /// Past it, a written value whose lists cannot all be paid for is `resource-limit` where it is
  /// written, and the level of list that could not be paid for is its one unresolved row, under
  /// the levels that were: with one step left, `a: [[Int]] = 1` is a list of one unresolved value.
  /// An integer whose conversion cannot be paid for is `resource-limit` where it is written, and
  /// unresolved. An application whose next argument left out cannot be paid for is
  /// `resource-limit` at its path, once, and is filled no further: what it writes is still
  /// coerced, an argument the budget kept from being filled is absent, a required one it kept from
  /// being reported is not reported, and a copy is made whole or not at all. An object literal
  /// whose next required field, or next default, left out cannot be paid for is starved likewise:
  /// `resource-limit` at the literal, once; what it writes is still made, what was filled stays,
  /// and none of the rest of its required fields and defaults is read. Its required fields are
  /// asked for before its defaults. At `0`, no default fills what a supplied package leaves out, no
  /// value it writes stands for a list, and no decimal integer of more than 38 significant digits
  /// is read: a decimal `u128::MAX` is `resource-limit`.
  ///
  /// The default is 2^20: a read and one copied value for each of half a million applications, so
  /// a program of a hundred thousand applications that each leave four defaults of one value out
  /// stays inside it. The longest decimal integer it converts, with nothing else spent, has 9 526
  /// digits, charged 1 048 410 steps; a longer one is written in hexadecimal, or read under a
  /// higher limit. Defaults that fill defaults multiply: input objects that each default two fields
  /// to `{}` of the next fill twice as many values at each level, and nineteen of them, a valid
  /// GraphQL document, pass the default where their own defaults are written.
  #[inline]
  pub const fn max_value_steps(&self) -> u32 {
    self.max_value_steps
  }

  /// How many steps the analysis may take to check implementations, over the whole analysis:
  /// that each definition implements the interfaces its own package's `implements` entries name,
  /// markers alone where it is no object and no interface, under the type arguments each entry
  /// writes, and what those interfaces implement in turn; and that no two of a definition's
  /// entries, or of a union's members, are one type, as written or under the type arguments of an
  /// instantiation the program writes.
  ///
  /// A step is one of: an item looked at of a list of another definition's, an interface's
  /// `implements` entries and fields, a field's arguments, a type's entries, a type parameter's
  /// bounds and a bound's interface's entries, a union's members, and a written instantiation's
  /// definition's entries and members, each one another package wrote, which is passed by,
  /// included; a node a type is rebuilt at, its root and each of its arguments and elements, with
  /// an entry's type arguments in place of a definition's type parameters, once for each type and
  /// arguments, a rebuild remembered costing nothing; and a pair of types compared, a pair compared
  /// already included. A definition's own entries and members, read once for it, cost nothing.
  ///
  /// Past it, a definition's check stops: `resource-limit` once, at the `implements` entry it was
  /// checking, and nothing more is said of the definition. An instantiation whose entries or
  /// members could not be compared is `resource-limit` at each occurrence of it. At `0` each check
  /// that would take a step stops and is reported, a definition's once, at the entry it was
  /// checking. What a rebuild derives counts toward
  /// [`max_instantiations`](Self::max_instantiations) and
  /// [`max_generic_depth`](Self::max_generic_depth); nothing here draws on
  /// [`max_substitutions`](Self::max_substitutions) or [`max_bound_steps`](Self::max_bound_steps).
  /// The built-in library writes no `implements` entry and no union, and costs nothing here.
  ///
  /// The default is 2^20. GitLab's schema, the largest of smear's fixtures, with 2,041 types and 188
  /// `implements` entries, takes about 6,500 steps, and every fixture takes fewer than 2^15. A valid
  /// GraphQL document can meet it: checking that `implements` is transitively closed costs up to
  /// the square of a type's entries, as it does in `smear-schema`, so 468 object types that each
  /// implement a complete chain of 64 interfaces of one field each, about 200 KB of SDL, take
  /// 1,049,088 steps, and 467 take 1,046,944.
  #[inline]
  pub const fn max_implementation_steps(&self) -> u32 {
    self.max_implementation_steps
  }

  /// How many bytes of text the supplied files may hold together. Their paths and their packages'
  /// names do not count, nor does the built-in packages' own text, which is smear's.
  ///
  /// It is checked once the package set is numbered and its names and paths are checked, and it
  /// bounds what comes after: the parsing of the supplied files, and everything built from their
  /// texts. Past it, the package set is refused at the root package and no supplied file is
  /// parsed, so nothing is built from a supplied text; the built-in library is still analyzed, and
  /// the packages' sources and directory modules are still numbered. It does not bound the
  /// numbering and ordering before it, which can read the supplied texts themselves, as [`Limits`]
  /// says.
  ///
  /// The default is 2^26, 64 MiB: far more than any package set written by hand. Because the bound
  /// is a `u32`, every byte offset into an accepted package set's texts fits one.
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
  /// A parser that keeps a bounded number of records, as the lossless layer does under its own
  /// limits, lends no more than it kept; the errors it found beyond them are counted, and the list
  /// is truncated.
  ///
  /// The default is 16 384, smear-schema's ceiling on each of its two quadratic lists: more than
  /// an author reads, and a bound on memory against an input built to be refused many times over.
  #[inline]
  pub const fn max_diagnostics(&self) -> u32 {
    self.max_diagnostics
  }

  /// How many names the analysis may look at, over the whole analysis, looking for a "did you mean"
  /// suggestion for names that do not resolve, each module a glob names that a search reads
  /// costing one more.
  ///
  /// Past it, a name that does not resolve is still reported, without a suggestion. A search looks
  /// at the names visible where an unresolved name was written, which is quadratic over the
  /// program; this bound makes it linear in the worst case.
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

  /// The same limits with `max` as their [`max_modules`](Self::max_modules) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_modules(mut self, max: u32) -> Self {
    self.max_modules = max;
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

  /// The same limits with `max` as their [`max_globs`](Self::max_globs) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_globs(mut self, max: u32) -> Self {
    self.max_globs = max;
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

  /// The same limits with `max` as their [`max_substitutions`](Self::max_substitutions) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_substitutions(mut self, max: u32) -> Self {
    self.max_substitutions = max;
    self
  }

  /// The same limits with `max` as their [`max_bound_steps`](Self::max_bound_steps) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_bound_steps(mut self, max: u32) -> Self {
    self.max_bound_steps = max;
    self
  }

  /// The same limits with `max` as their [`max_value_steps`](Self::max_value_steps) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_value_steps(mut self, max: u32) -> Self {
    self.max_value_steps = max;
    self
  }

  /// The same limits with `max` as their
  /// [`max_implementation_steps`](Self::max_implementation_steps) bound.
  #[inline]
  #[must_use]
  pub const fn with_max_implementation_steps(mut self, max: u32) -> Self {
    self.max_implementation_steps = max;
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

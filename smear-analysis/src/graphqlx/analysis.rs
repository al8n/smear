//! The entrances, [`analyze`] and [`analyze_with`], and what they answer, [`Analysis`] and
//! [`Trees`].

use core::{cmp::Ordering, iter::FusedIterator};
use std::{string::String, vec::Vec};

use smear_parser::{
  graphqlx::{Layer, Syntactic},
  layer::SyntaxTree,
};
use tokora::diagnostic::Location;

use super::{
  bind, check, coerce, collect, declare,
  diagnostic::{Diagnostics, Reporter},
  id::{PackageId, SourceId},
  implement, instantiate,
  limits::Limits,
  lower,
  model::{Model, ModelBuilder, PackageKind},
  package::Package,
  parse::{self, Documents},
  resolve,
  rules::Rules,
  set::{self, PackageView, ROOT},
  source::SourceMap,
  text::SourceText,
  tiers,
  types::Types,
  visitor::{Packages, Visitor, drive},
};

/// What one analysis found: the model, the diagnostics, and the source map.
///
/// It has no type parameter and no lifetime: it is the same type whatever text type the packages
/// were supplied in, and it borrows nothing from them. The model and the diagnostics are complete
/// whatever was refused; [`is_ok`](Self::is_ok) is the verdict.
///
/// Two analyses are equal when their models, their diagnostics (the list, the error count and
/// whether the list is truncated), their source maps and the diagnostic ceilings they were made
/// under are equal. So two analyses of one package set made under two ceilings are unequal, even
/// when neither list was truncated: a walk over each keeps to its own ceiling. The models are
/// compared as [`Model`]'s equality says, every row and every id, which is the question to ask of
/// two analyses of the same packages: [`analyze_with`] says when two parser layers answer equal
/// ones. It is `PartialEq` and not `Eq` because a float value is compared as an `f64` is; no
/// literal spells a NaN, so an analysis is equal to itself.
#[derive(Debug, Clone, PartialEq)]
pub struct Analysis {
  model: Model,
  diagnostics: Diagnostics,
  sources: SourceMap,
  /// The diagnostic ceiling it was made with, which a consumer's walk keeps to as well.
  ceiling: usize,
}

impl Analysis {
  /// The analysis `analyze` would conclude from the finished `model`, with `rules` run in place of
  /// smear's own rule set, as the test-support door hands it to its tests: a source map of the
  /// model's packages' sources, and `limits`' diagnostic ceiling.
  #[cfg(feature = "test-support")]
  pub(crate) fn of_model<V: Visitor + ?Sized>(
    model: Model,
    limits: &Limits,
    rules: &mut V,
  ) -> Self {
    let sources = SourceMap::of_model(&model);
    let reporter = Reporter::new(limits.max_diagnostics() as usize);
    conclude(model, sources, reporter, rules)
  }

  /// The diagnostic ceiling the analysis was made with: how many diagnostics a list it answers,
  /// or a walk over it answers, keeps.
  #[inline]
  pub(crate) fn ceiling(&self) -> usize {
    self.ceiling
  }

  /// The model of what the packages declare.
  #[inline]
  pub fn model(&self) -> &Model {
    &self.model
  }

  /// The diagnostics, in their one total order, under the ceiling the analysis was given.
  #[inline]
  pub fn diagnostics(&self) -> &Diagnostics {
    &self.diagnostics
  }

  /// The sources the diagnostics' locations number.
  #[inline]
  pub fn sources(&self) -> &SourceMap {
    &self.sources
  }

  /// Whether nothing was refused: no diagnostic of
  /// [`Severity::Error`](tokora::diagnostic::Severity::Error) was reported, kept or dropped by the
  /// ceiling, and no syntax error was counted that a parser did not lend.
  #[inline]
  pub fn is_ok(&self) -> bool {
    !self.diagnostics.has_errors()
  }
}

/// Analyzes `root` beside `dependencies`, within `limits`.
///
/// # What it does
///
/// - Numbers the packages, `core`, `alloc` and `std` first, then `root`, then `dependencies` by
///   name, and numbers each package's package source and then its files by path. The order the
///   dependencies, or a package's files, are passed in changes no id and no diagnostic.
/// - Checks the package set, reporting each violation at the whole of the offending package's
///   package source: two files with one path; a path that is absolute, uses `\`, is not UTF-8, or
///   has an empty, `.` or `..` segment; a package name that is not a Name, or is `core`, `alloc` or
///   `std`; two supplied packages with one name; a declared dependency that is not supplied.
///   Supplied sources over [`Limits::max_source_bytes`] are reported at the root, and then no
///   supplied file is parsed.
/// - Parses every file with the GraphQLx syntactic parser. A file's syntax errors are reported at
///   that file, with the parser's own code, message, labels and help, and the other files are
///   parsed all the same.
/// - Builds each package's module tree: its root module; in the
///   [`Directory`](super::Layout::Directory) layout a module for every directory that holds a file
///   and for every directory on the way to one, each directory's files sharing its module, and in
///   the [`Flat`](super::Layout::Flat) layout every file in the root module; and a module for every
///   inline `mod`, inside the module it is written in. A directory whose name is not a Name, a
///   directory or a `mod` nested deeper than [`Limits::max_module_depth`], and a `mod` of a module
///   the package already has are reported. Every package's root module is held; the first module a
///   supplied package's directory or `mod` would add past [`Limits::max_modules`] is reported, and
///   no module is added after it.
/// - Collects every module's declarations and numbers them: each type and directive definition a
///   placed file declares is a definition of the model, the built-in packages' included, and so is
///   every intrinsic primitive the packages write where they may have it, which `core` holds first
///   in its root module. A file holding only white space, commas, byte order marks and comments
///   declares nothing, and is no syntax error. In the root package's root module, a scalar named
///   as a GraphQL scalar (`Int`, `Float`, `String`, `Boolean`, `ID`) and a directive named as a
///   GraphQL specified directive replace the built-in of that name, which keeps its id. Two
///   definitions of one name in one namespace of one module, a supplied definition named with
///   `__`, and a type named as a child module of its module are reported. A type of the root
///   package's root module named as one of GraphQL's introspection types, `__Type` and the seven
///   others, is `redefined-built-in-type`, as GraphQL reports it, and no reserved name. The first
///   definition a supplied package would add past [`Limits::max_definitions`] is reported, and
///   nothing after it is collected.
/// - Builds every module's scope: its own declarations and child modules, then its explicit `use`
///   bindings, then its globs, then the prelude, the root modules of `core`, `alloc` and `std`
///   whatever the package's tier, then, where a module is wanted, the packages its package may
///   reach. A type a supplied package declares with the name of a primitive of the prelude, `bool`,
///   `f32`, `f64`, `iN`, `uN`, `string`, `bytes`, `NonZeroIN` or `NonZeroUN`, shadows it in its
///   module, and is a warning.
/// - Resolves every `use` item, each module's to a fixpoint, which `use` cycles do not disturb. A
///   path that names nothing is reported once, as is a package the item's package may not reach, a
///   string source that is a URL, names a file, leaves its package or stands in a `Flat` package,
///   a name two globs give two items where an item meets it, a group nested past
///   [`Limits::max_use_depth`], and the first glob of a module that would name a module past
///   [`Limits::max_globs`]. An item that names one thing while the module's items are being
///   resolved and another once all are, a `use` of a name its module declares, and two `use` items
///   that give one name two items are reported too. A name that does not resolve gets, in its
///   help, the nearest name visible where it is written, of the namespace wanted, while the
///   analysis has [`Limits::max_near_miss_comparisons`] left to spend.
/// - Lowers what each definition holds beyond its identity. Each extension's target is resolved
///   where the extension is written: one that names nothing, one that names a definition of
///   another kind than the extension's keyword (`extension-kind-mismatch`), and one that is an
///   alias such as `NonZeroU32`, which spells an instantiation and is no definition of its own,
///   is reported and contributes nothing; every other is a contribution of its target. An
///   extension's parameter list names its target's type parameters by position, one name each,
///   and a list of another length is `generic-arity`; an extension that writes no list names none
///   of them. A definition holds its contributions,
///   its own, where its name is written, then its extensions' by source and position, each where
///   its target path is written; its type parameters, of which a definition that replaces a
///   GraphQL built-in declares none, a list it writes being `generic-arity`; a directive
///   definition's locations and
///   whether it is `repeatable`; and its members, the direct members of each contribution in turn
///   as written and then each field's arguments, each with its description. A replaced built-in
///   holds what the root package's definition writes, and smear's own definition of it
///   contributes nothing. Every type written is then a type occurrence, and every directive
///   application is one too, appended in the order the model's walk reaches it, each before what
///   it nests: a type parameter's default, a bound, an `implements` entry, a union member, an
///   application, a member's type and applications. Each
///   keeps its path as written and the arguments written for it, and its path is resolved where
///   its contribution is written, with the contribution's generic parameters in scope, an
///   extension's own names naming its target's by position; one that names nothing is reported,
///   with the code of its place, and stays unresolved. A bound is a definition's own type
///   parameter's: any other left side of a `where` predicate, and any `where` clause on an
///   extension, is `invalid-bound`; and so is a right side that names no interface, the bound
///   still recorded as written. An application whose directive resolves keeps its type
///   arguments and the arguments its directive declares, in the directive's order, an argument
///   written twice twice; one it does not declare is `undefined-directive-argument`, with the
///   nearest name it does declare in its help. Each value written is read as it is written, for
///   coercion. Then the schema's
///   contributions: every package's schema definitions, then its schema extensions, each by source
///   and position and each marked as what it is. The root package's first schema definition gives
///   the schema its description, its roots and its applications, and each schema extension the
///   root package writes its roots and its applications; any other schema item is a contribution
///   alone. The first root written for an operation is the schema's, and a second one is
///   `duplicate-root-operation-type`; with no schema definition of the root package's, an object
///   type its root module declares named `Query`, `Mutation` or `Subscription` is presumed that
///   operation's root.
/// - Gives each definition its built-in meaning, read off `core`'s directives by what each
///   application resolved to, never by how it is spelled, and only off the applications the
///   package that wrote the definition writes: an interface that applies `@marker` is a marker,
///   and a scalar that applies `@set` or `@map` stands for that collection, the first of the two
///   when it applies both. Then checks each definition's defaults where they are written:
///   a type parameter with no default that follows one with a default is `generic-default`, once
///   for the definition; and so is each group of its defaults that need each other, a default that
///   names its own parameter included, once, at the default of the group's lowest parameter,
///   which a use can never fill.
/// - Gives every type occurrence its type, each type interned where the model's walk first needs
///   it, its arguments before it: a list is the list of its element's type; a type parameter is
///   that parameter of its definition, in a default's own tree too; an alias such as `NonZeroU32`
///   is the `NonZero` it spells; and an occurrence of a definition is that definition with every
///   argument, the written ones first, a collection's key or element non-null however it is
///   written, then each one left out filled from its default, substituted, a default that names a
///   parameter written after it filled first. Type arguments on a type parameter or an alias, more
///   type arguments than a definition has parameters or fewer than it takes, and an argument left
///   to a default that cannot fill it, are `generic-arity`, where they are written; an occurrence
///   inside a default whose filling leads back to a default being filled is `generic-default`. A
///   type nested deeper than [`Limits::max_generic_depth`], an instantiation past
///   [`Limits::max_instantiations`] and filling past [`Limits::max_substitutions`] are
///   `resource-limit`, each where it is written, and the built-in library is typed whatever the
///   limits. An occurrence that cannot be typed, or that holds one that cannot, has the one
///   unresolved type. Each closed type an occurrence of a generic definition resolves to is one of
///   the definition's instantiations. An application writes no more type arguments than its
///   directive has parameters and no fewer than it takes, or it is `generic-arity`, at its path.
///   Last, once every occurrence's bounds are judged, an application of a directive that has a
///   type parameter, written with a count its directive takes, has its complete type arguments,
///   filled as an occurrence of its directive would be, on the same limits, a level above its type
///   arguments: a default that cannot fill one is `generic-arity`, and a limit `resource-limit`,
///   each at its path.
/// - Judges the bounds of every written occurrence of a definition that has one, by its complete
///   type, a slot a default filled included: for each bound in its definition's order, the
///   argument for the bounded parameter, without its own `!`, must satisfy the bound's interface
///   type with the type's arguments in place of the definition's parameters, each with its `!`, as
///   [`Model::satisfies_ty`](super::Model::satisfies_ty) answers. An occurrence that names a
///   parameter of the definition it is written in is judged through that parameter's bounds, and
///   an instantiation only a fill derives is not judged. The first bound that fails is
///   `unsatisfied-bound`, at each occurrence of the type, which keeps its type. A bound of `core`'s
///   `MapKey` that a `NonZero` fails is passed over: that `NonZero` holds what is no integer
///   primitive, and `nonzero-argument` refuses it where it is written. Last, a directive's bounds
///   are judged so at each application whose type arguments are complete, by the application's
///   type, its directive with those arguments, one inside a generic definition through that
///   definition's bounds, and one that fails is `unsatisfied-bound` at its path. Each type is
///   judged once, whatever its nullness and however many applications have it, within
///   [`Limits::max_bound_steps`]; an occurrence or an application whose bounds that budget, or a
///   type they derive past [`Limits::max_generic_depth`] or [`Limits::max_instantiations`], kept
///   from being judged is `resource-limit`, and an occurrence keeps its type too.
/// - Makes every value, in the model's walk, each by the type expected where it stands, a
///   member's type for its default and an argument's for its value, with its `!`: `null` where it
///   is nullable; a list's elements each against its element, and any other value where a list is
///   expected the one-element list that holds it, a list for each level, each list a step of
///   [`Limits::max_value_steps`]; a boolean; an integer in its width's range, read at any length, a
///   decimal one of more than 38 significant digits converted within the same budget; a float, or
///   an integer, rounded once to `f32` or `f64` and finite there; a string, cooked; for `id` a
///   string or an integer in `i32`'s range; for `bytes` a string's UTF-8 encoding or a list of
///   integers from 0 to 255; for `NonZero` an integer that is not zero; for an enum one of its
///   values, a bare name looked up under that enum alone, and a qualified one whose prefix names
///   that enum where the value is written, through any path or alias, the nearest value in its
///   help when it names none; for an
///   input object an object literal, whose fields are found by name and kept in the input object's
///   order, one written twice twice, each coerced against its field's type: a field it does not
///   declare is `undefined-input-object-field`, with the nearest field in its help, and is not
///   kept, and a required field given `null` or left out is `missing-required-input-object-field`,
///   each left out a step of [`Limits::max_value_steps`]; an input object its own package applies
///   `@oneOf` to takes exactly one field, and not `null`; and a field left out that has a default
///   takes a copy of it, as an argument does, but a OneOf input object's. A set takes a set literal
///   and a map a map literal, each element, key and value coerced against the collection's types,
///   a key or an element never `null`; any other literal there, and a set or a map literal where no
///   collection of its shape is expected, is `collection-literal`, and so is an element or a key
///   written twice, told by its coerced value, at the later occurrence. `TypeRef` takes a path,
///   resolved as a type where it is written, that names a definition of any kind. Any other scalar
///   takes any literal, kept as written, and so does a type that still names a type parameter or
///   can take no value, with nothing said. A prefix, or a type value's path, that names nothing is the
///   resolver's to report. A value
///   that does not fit is unresolved and reported where it is written, with GraphQL's
///   `invalid-default-value` or `invalid-directive-argument-value`, but for a number out of the
///   range of any width but the 32-bit signed one, or of `f32`, which is `width-overflow`, and
///   zero for `NonZero`, which is `nonzero-zero`; a required argument given `null` is
///   `missing-required-directive-argument`, at its name. An application's arguments are in its
///   directive's order: each one written, and each one left out whose definition has a default, a
///   copy of the default filling it, each row of the copy not written and located at the default,
///   and silent, the default's own diagnostic saying once what does not fit; a required argument
///   left out is `missing-required-directive-argument`, at the application's path. An
///   application with its complete type arguments reads each argument's type under them, a type
///   that cannot be built there `resource-limit`, "required" read there, and an argument written
///   as a bare nullable type parameter required where they make it non-null; a copy that does not
///   fit the type they give it is `invalid-default-value`, once for the application, at its path,
///   labelled at the default, its count paying a step for each node and judging the copy as a
///   written value is judged. An object literal of a generic input object's instantiation reads
///   its fields' types under its type arguments likewise, a field written as a bare nullable type
///   parameter required where they make it non-null; a copy it makes that does not fit is
///   `invalid-default-value` at the literal, once, but when a type occurrence a supplied package
///   writes has the literal's type: it is then left to that instantiation's check, which reports it
///   where the type is written. A value of a generic type definition whose
///   type names its type parameters, a default of its members or an argument of an application
///   on it, stays a literal, and is checked at each closed instantiation of the definition a
///   supplied package writes, one verdict for each instantiation, each pair of one and a value a
///   step: the first value that does not fit under its arguments is reported once at each
///   occurrence of it, with the value's site code, labelled at the value. So is an argument such
///   an application leaves out, its type there the definition's bare nullable type parameter:
///   where the instantiation's arguments make it non-null, it is
///   `missing-required-directive-argument` at the occurrence, labelled at the application. The
///   values of the applications on a generic directive and on its arguments are checked likewise
///   at each closed application of the directive, reported at its path; the directive's own
///   arguments' defaults are judged only where an application leaves them out. A copy is
///   counted before it is made, each default's count kept, and a default whose copy would hold a
///   copy of itself is not copied: the argument, or the field, is absent, and under type arguments
///   what its count reached is judged as a copy is. The first time a count meets itself, that
///   cycle of defaults is `input-object-default-value-cycle`, at the name of the input field of
///   least id whose default is on it, once for each field, "under the type arguments a use gives
///   it" when a default on it is counted under a type it is not written against: a program that
///   holds such a cycle among closed types gets at least one, not one for each. Each row of a
///   copy, each
///   read of another default's count while counting, each read of a count made already for a
///   later copy, each default left out that needs itself, each required argument found left out,
///   each field a count reads of an object literal against an input object it had not read it
///   against, and each 64 bytes, or part, of the identities of a set's elements or a map's keys a
///   count under type arguments renders to search them for one held twice, once for each literal
///   and the type they are read against, is a step of [`Limits::max_value_steps`]: past it the
///   application is `resource-limit`
///   once, at its path, and filled no further, an object literal likewise at the literal, and an
///   integer whose conversion it cannot pay for is `resource-limit` where it is written. Last,
///   names and texts
///   past what the model's `u32` offsets address, which only a [`Limits::max_source_bytes`] far
///   above its default admits, are reported at the root package's package source.
/// - Binds each scalar a supplied package defines to its host types: each application of `core`'s
///   `@extern` its own package writes on it, in its definition or in an extension of its own, told
///   by what the application resolved to, is one of its externs, in application order, a `lang`
///   written twice twice; its `lang` and its `type` are the strings coerced, and its tier the one
///   its `tier` names or, absent or `null`, the tier the scalar's package is declared at. An
///   `@extern` whose `lang`, `type` or `tier` did not coerce binds nothing, and so does one another
///   package writes, and one on a built-in, a GraphQL built-in the root package replaces among
///   them. An extern holds the strings coercion kept, and binding writes no text.
/// - Checks what each definition says it is, in id order, reading of it, and of every interface
///   the check reads, what its own package writes, its own extensions included: an `implements`
///   entry of the type an earlier entry of the definition has is
///   `smear::schema::duplicate-implements-interface`, and a union's member of the type an earlier
///   member has `smear::schema::duplicate-union-member`, each at the later, with a label at the
///   first, every later one reported. Two instantiations of one interface are two types, and an
///   entry or a member that did not resolve is compared with nothing. Then each entry that names
///   an interface other than the definition, a marker when the definition is no object and no
///   interface, the first of its type, is checked: every interface that interface implements, its
///   own entries read with the entry's type arguments in place of its type parameters, is one of
///   the definition's entries too, compared as a type, or it is
///   `smear::schema::missing-transitive-interface`, at the definition's entry, with a label at the
///   entry that implements it, once for each interface type missing; and what a missing interface
///   implements is owed too, each definition read once for the definition checked. What an entry
///   names besides is another rule's to refuse. Then, for an object or an interface and an
///   interface that is no marker, each field the interface's own package writes: the first of the
///   definition's own fields of its name, or else `smear::schema::missing-interface-field` at the
///   entry; of a type that is a subtype of the interface's field's under the entry's type
///   arguments, or else `smear::schema::invalid-interface-field-type` at its name, a subtype read
///   as GraphQL's, a list of subtypes, a non-null form, a type that implements the interface or is
///   a member of the union, and covariant in every type argument, a type parameter a subtype of
///   itself and of its bounds alone; deprecated, by `core`'s `@deprecated` or the root package's
///   own, only where the interface's is, or else `smear::schema::interface-field-not-deprecated`
///   at its name; taking each argument of the interface's field, the first of its name, or else
///   `smear::schema::missing-interface-field-argument` at the field's name, of exactly its type
///   under the entry's type arguments, or else
///   `smear::schema::invalid-interface-field-argument-type` at the argument's name; and requiring
///   no argument the interface's field does not declare, written non-null with no default, or
///   else `smear::schema::unexpected-required-argument` at its name. Each entry and field of an
///   interface looked at, whoever wrote it, each argument of a field compared, each pair of types
///   compared and each entry, bound and member looked at to compare them, and each node of a type
///   read under type arguments, once for each type and arguments, is a step of
///   [`Limits::max_implementation_steps`]; such a type is depth-checked against
///   [`Limits::max_generic_depth`] and counted toward [`Limits::max_instantiations`]. A check one
///   of them stops is `resource-limit`, once, at the entry it was checking, naming the limit, and
///   nothing more is said of the definition. Last, every type occurrence, in id order, of a
///   definition whose own entries or members name its type parameters, an occurrence inside a
///   generic definition included: its distinct written entries, and its distinct members, each
///   read under the occurrence's type's arguments, a step each, two that are one type there are
///   `duplicate-implements-interface` or `duplicate-union-member` at the occurrence, labelled at
///   the two. Each type is judged once, both nullnesses sharing the verdict, and one a limit stops
///   is `resource-limit` at each occurrence of it, but at an entry whose definition's check
///   stopped there; an instantiation only a fill derives is not judged.
/// - Gives every definition and every type the tier it requires, last, once every type is
///   interned: a built-in keeps its package's tag; a scalar a supplied package defines requires
///   its externs' greatest tier, `core` with none; an enum and a directive `core`; an object or an
///   interface the greatest of its fields' types' and their arguments', an input object its input
///   fields' types', and a union its members' types', a position that applies `core`'s `@heap`
///   `alloc` at least; a named type its definition's and its arguments', a list `alloc` at least
///   and its element's, and a type parameter `core`. Definitions and types that hold one another
///   share the greatest tier any of them reaches, found by one worklist, each rising at most
///   twice.
/// - Runs smear's own rules through [`visit`](super::visit)'s driver, over every supplied package
///   ([`Packages::Supplied`]), and keeps what they report in the same list, under the same ceiling.
///   Each schema definition the root package writes after its first is
///   `duplicate-schema-definition`, with a label at the first. Each schema definition and each
///   schema extension a dependency writes is `schema-outside-root`, and no duplicate of the root
///   package's. An extension of a definition another supplied package defines is
///   `orphan-extension`, at its target path, with a label at the definition's name; and so is an
///   extension of a definition of `core`, `alloc` or `std`, a GraphQL built-in the root package
///   replaces among them, unless the root package writes it and it adds directives and nothing
///   else; the root package's that adds more to one of GraphQL's introspection types is GraphQL's
///   own `introspection-type-extension`, at the same place and with the same label. In the root
///   package's extension of a built-in it did not write, each application of
///   `core`'s `@marker`, `@set` or `@map` is `orphan-extension` too, at its path: what a built-in
///   is is the language's to say; and so is each of `core`'s `@extern`, a built-in's host type
///   being given by `@repr(for: …)` on the schema alone. An enum that declares a type parameter is
///   `generic-enum`, at its
///   first type parameter's name. A marker that declares a field is `marker-misuse`, once, at the
///   first field its own package wrote; so is a marker named anywhere but as the outermost type of
///   an `implements` entry or of a bound, where it is named, and an `implements` entry of a scalar,
///   an enum, an input object or a union that names what is no marker, a type parameter among them.
///   A scalar its own package makes a set with `@set` and that has not exactly one type parameter,
///   or a map with `@map` and that has not exactly two, is `collection-arity`, at the application,
///   and so is one that applies both, at the later. `TypeRef` anywhere but as the type of a
///   directive definition's argument or of an input field, under lists alone, is `typeref-misuse`,
///   where it is named, a type argument of any generic definition included; and a field's
///   argument a value of whose type can hold an input field of type `TypeRef`, through lists,
///   collections' arguments and input objects, each input object's arguments that its input
///   fields hold, over every package's input objects, is `typeref-misuse` too, at the argument's
///   type, labelled at that field; and so is each instantiation, written or a directive's
///   application, that gives such a type to a type parameter an operation's argument reaches, or
///   whose defaults derive one that does, at the argument, labelled where the parameter is
///   reached and at that field. `NonZero` whose type
///   argument, in the occurrence's type, is no integer primitive at least one bit wide written
///   without `!` is `nonzero-argument`, where it is written, with a label at the argument. A type
///   parameter with the name of an earlier one of its definition, and a name an extension's own
///   list gives after it gave it to an earlier one, is `duplicate-type-parameter`, at that name,
///   with a label at the first of that name. An extern of a scalar whose `lang` an earlier extern
///   of the scalar has, the cooked strings compared, is `extern-misuse`, at its `@extern`'s path,
///   with a label at the earlier's; and so is each `@extern` the root package writes on a GraphQL
///   built-in its own definition replaces, in that definition or its own extension. An `@repr` on
///   the schema whose `for` is absent or `null` is `repr-misuse`, at its path, and so is one on a
///   field, an argument or an input field that writes `for`, at `for`'s name; and so is an `@repr`
///   whose `lang` an earlier one of its position has, or on the schema whose `for` and `lang` an
///   earlier one of the schema's has, at its path, with a label at the earlier. Below `std`, a
///   package uses nothing above the tier it declares, `tier` where it is written: a list in a
///   `core` package; an occurrence of another package's definition that needs more, counting the
///   defaults it leaves out and capped at that package's tier, a label at a default that raised it;
///   `core`'s `@heap` on a field, an argument or an input field of a `core` package; and an
///   extern's `tier` above its package's, at that `tier`. A use of a definition of the package's
///   own is not reported, what raises it being reported where it is written. Compile-time metadata
///   counts nowhere and is not judged: every type occurrence under a directive definition, its
///   arguments' types and its type parameters' defaults and bounds, and every type argument of an
///   application of any directive, the written ones and those its directive's defaults fill. The
///   exemption is shallow: an input object a directive's argument names is a definition like any
///   other, and its fields are judged where they are written. GraphQL's own rules judge what a
///   definition a supplied package wrote holds, a GraphQL built-in the root package replaces among
///   them, with GraphQL's codes: a member whose name begins with `__` is `reserved-field-name`,
///   `reserved-argument-name`, `reserved-enum-value-name`, `reserved-input-field-name` or
///   `reserved-directive-argument-name`, by what it is, at its name. A member whose name an
///   earlier member of its list has, the definition's own and its extensions' members being one
///   list and each field's arguments another, is `duplicate-field-name`,
///   `duplicate-argument-name`, `duplicate-enum-value`, `duplicate-input-field-name` or
///   `duplicate-directive-argument-name`, at its name, with a label at the first. An object, or
///   an interface that is no marker, with no field, a union with no member, an enum with no value
///   and an input object with no field, its extensions' members counted, is
///   `empty-fields-definition`, `empty-union-members`, `empty-enum-values` or
///   `empty-input-fields`, at its name. A field whose type's base, under its lists, is an input
///   object is `field-type-not-output-type`, and an argument, an input field or a directive's
///   argument whose base is an object, an interface or a union is `argument-type-not-input-type`,
///   `input-field-type-not-input-type` or `directive-argument-type-not-input-type`, at the base.
///   A base that is a collection is judged by its complete type's arguments, through the
///   collections and lists they hold, and the same code is at the deepest occurrence that writes
///   the way to the one of the wrong kind, with a label at the default that fills a slot the
///   occurrence leaves out. The same codes judge each instantiation the walk reaches, every written
///   type occurrence and every directive application by its complete type arguments: a type
///   parameter stands where its definition's members, union members and applications put it,
///   through lists and collections and wherever a definition it is passed to puts that
///   definition's parameter, and the first argument that is not of the class each place it stands
///   in takes is refused at the argument, or at the occurrence for one a default fills, with a
///   label where the parameter stands; else what the slots the occurrence leaves to their defaults
///   derive, and no occurrence writes, is judged so at the occurrence.
///   An `implements` entry of an object or an interface that names what is no interface, a type
///   parameter among them, is `implements-non-interface`, at the entry. An interface on a cycle
///   of `implements` entries, of any length, is `self-implementing-interface`, at its name. A
///   union member that names what is no object type is `union-member-not-object`, at the member.
///   An argument or an input field written non-null, with no default, that applies `core`'s
///   `@deprecated` is `deprecated-required-argument` or `deprecated-required-input-field`, at its
///   name, with a label at the `@deprecated`. An input object is OneOf when the package that
///   wrote it applies `core`'s `@oneOf` to it, in its definition or in an extension of its own,
///   and a field of one that is written non-null is `one-of-field-not-nullable`, and one that
///   declares a default `one-of-field-has-default`, at its name. Each application of `@oneOf` in
///   an extension of an input object is `one-of-on-input-object-extension`, at its path. Over
///   every input object of the model, the draft's unbreakable-cycle algorithm: an ordinary input
///   object is finite when what each field that is non-null and no list requires is, and a OneOf
///   when one of its fields can be given a finite value; the input objects that are not are
///   walked in id order, each field in member order, and the field that closes a cycle is
///   `circular-non-null-input-field`, or `circular-one-of-input-field` when a nullable field of a
///   OneOf is on it, at its name, once for its input object. A generic input object requires what
///   its type arguments must hold: a type parameter a required field takes, or passes to another
///   input object's parameter it requires, is required of each instantiation, written `T` a
///   nullable field being required where the argument is non-null.
///   GraphQL's own rules on directives judge every application a supplied package writes, with
///   GraphQL's codes: an application at a location its directive's definition does not list, the
///   schema, a kind of definition, a directive definition, a field, an argument, an input field or
///   an enum value, is `unsupported-directive-location`, at its path, with a label at the
///   directive's definition. An application of a directive that is not `repeatable`, which an
///   earlier application to the same location applies, a definition and its extensions being one
///   location and the schema's definition and extensions one, is `duplicate-directive-use`, at its
///   path, with a label at the first. An argument an application writes again, the directive
///   declaring it, is `duplicate-directive-argument-use`, at its name, with a label at the first.
///   A directive on a cycle of what definitions reach is `self-referential-directive`, at its
///   name: a definition reaches the directive of each application on it, and for a directive
///   definition of each on its arguments, or for any other of each on its input fields and enum
///   values; and every definition a type occurrence names at any depth, in a directive
///   definition's arguments, in its input fields, in those applications' type arguments and in its
///   own type parameters' defaults, a scalar that is no collection hiding its type arguments; a
///   directive of `core` on such a cycle is reported in `core`'s source. Each application of
///   `core`'s `@specifiedBy` on a scalar of the built-in library, a GraphQL scalar the root
///   package replaces among them, is `specified-by-on-built-in-scalar`, at its path. GraphQL's
///   own rules on the schema judge its roots: a written root whose type names a definition that is
///   no object type, an alias such as `NonZeroU32` among them, is
///   `root-operation-type-not-object`, at the type. Of the roots that are object types, presumed
///   or written, one that is one type with an earlier one, in the order query, mutation and
///   subscription, is `shared-root-operation-type`, at a written root's type, with a label at the
///   other. When the root package writes no schema definition and no query root is presumed, each
///   schema extension it writes is `schema-extension-without-schema`, at its `extend`. A schema
///   with no query root, neither written nor presumed, is `missing-query-root-operation-type`,
///   once: at the `schema` of the root package's first schema definition, silent when a phase
///   refused something in it, or with none at the root package's package source, where it marks
///   nothing.
///   So a root package with no query root is not ok, as in GraphQL, and a consumer that analyzes a
///   library as the root package filters that code. A rule says nothing about a contribution a
///   phase already refused something in, and the two rules on the roots' types,
///   `root-operation-type-not-object` and `shared-root-operation-type`, say nothing while any of
///   the schema's contributions holds a phase's error.
///
/// Every supplied package is checked, parsed and walked, whether anything reaches it or not.
///
/// # What it marks
///
/// Each error a phase or one of smear's rules reports marks, whether the ceiling keeps it or not,
/// the contribution whose extent holds where it points, every member whose extent holds it, and
/// through the contribution its definition: their `has_errors()` say so, and
/// [`Contribution::extent`](super::Contribution::extent) and
/// [`Member::extent`](super::Member::extent) say what each extent spans. An error that points at
/// no definition, such as one at a package source, marks nothing. A syntax error marks
/// nothing either. What the parser could not read has no image in its file's document, and so no
/// contribution to mark; and a definition the parser read past a reported mistake stands in the
/// document unmarked: its `has_errors()` does not speak of the syntax error, which is in the
/// diagnostics. While smear's rules walk, a rule sees the phases' marks and no rule's: the rules'
/// marks are applied once the walk has ended, so no rule sees another's, and the order the rules
/// are composed in changes nothing.
///
/// # What it keeps
///
/// Nothing of the packages: it borrows them for the call, and the caller may drop or reuse every
/// text as soon as it returns. [`SourceMap::bind`] pairs the result with the same packages again
/// when a renderer needs their texts.
///
/// The analysis reads no file and no network: the caller supplies every package, the root's
/// dependencies with it.
pub fn analyze<S: SourceText>(
  root: &Package<S>,
  dependencies: &[Package<S>],
  limits: &Limits,
) -> Analysis {
  let mut prepared = prepare(views(root, dependencies), limits);
  let documents = parse::files(
    &Syntactic::new(),
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    None,
  );
  finish(prepared, &documents, limits)
}

/// Analyzes `root` beside `dependencies`, within `limits`, with `parser` reading every supplied
/// file: [`analyze`], with the parser chosen by the caller, and what the parser kept for each file.
///
/// `parser` is a [`Layer`] of GraphQLx. [`Syntactic`] is one: it stops at a file's first syntax
/// error, keeps nothing, and is what [`analyze`] reads with. The other is `smear-parser`'s lossless
/// layer, behind its `rowan_0_17` and `std` features: it recovers, so the well-formed rest of a
/// broken file is analyzed, and it keeps each file's tree. A layer's limits, such as the nesting
/// depth a parse descends to, are the layer value's, where [`Limits`] bounds the analysis.
///
/// Everything [`analyze`] does around the parse is done here, in the same order. Each supplied file
/// that is text, or bytes that are UTF-8, is read by `parser`, and every syntax error it lends is a
/// diagnostic at that file, in the analysis's one list. Three kinds of file never reach `parser`
/// and have no tree: the built-in packages' files, a byte text that is not UTF-8, and a text that
/// holds nothing but white space, commas, byte order marks and comments. When the supplied sources
/// are over [`Limits::max_source_bytes`], no supplied file is parsed at all.
///
/// A parser that keeps a bounded number of records, as the lossless layer does under its own
/// limits, lends no more than it kept; the errors it found beyond them are counted, and the list
/// is truncated.
///
/// The trees borrow nothing of the packages at a concrete call: they are `Trees<()>` for
/// [`Syntactic`], and a tree of the lossless layer holds no text of the caller's. A consumer generic
/// over the layer that keeps the trees past the packages bounds `P::Tree: 'static`.
///
/// For a package set no layer reports a syntax error for, every layer answers an equal
/// [`Analysis`], the rules' diagnostics and marks included: the layers hand the same phases the
/// same documents in the same order. Where a layer reports a syntax error, the package ids and the
/// source ids are still those every other layer answers, and nothing else is promised. The trees
/// are beside the analysis, and no part of its equality.
pub fn analyze_with<'a, P, S>(
  parser: &P,
  root: &'a Package<S>,
  dependencies: &'a [Package<S>],
  limits: &Limits,
) -> (Analysis, Trees<P::Tree>)
where
  P: Layer<'a>,
  S: SourceText,
{
  let mut prepared = prepare(views(root, dependencies), limits);
  let mut trees: Vec<(SourceId, P::Tree)> = Vec::new();
  let documents = parse::files(
    parser,
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    Some(&mut trees),
  );
  (finish(prepared, &documents, limits), Trees { trees })
}

/// Every package of one analysis, read through its getters, in id order: the built-ins, then
/// `root`, then `dependencies` by name.
fn views<'a, S: SourceText>(
  root: &'a Package<S>,
  dependencies: &'a [Package<S>],
) -> Vec<PackageView<'a>> {
  let mut supplied: Vec<PackageView<'a>> = Vec::with_capacity(dependencies.len());
  for package in dependencies {
    supplied.push(PackageView::supplied(package));
  }
  set::order(PackageView::supplied(root), supplied)
}

/// What an analysis has settled before any file is parsed.
struct Prepared<'a> {
  packages: Vec<PackageView<'a>>,
  reporter: Reporter,
  sources: SourceMap,
  /// Each package's resolved dependencies, one list per package.
  dependencies: Vec<Vec<PackageId>>,
  /// Whether the supplied sources are within their size limit, so that their files are parsed.
  within: bool,
}

/// Everything after the entrance and before the parse, compiled once whatever the caller's text
/// type and whatever the parser: the source map, and the checks of the package set and of the
/// supplied sources' size.
fn prepare<'a>(packages: Vec<PackageView<'a>>, limits: &Limits) -> Prepared<'a> {
  let mut reporter = Reporter::new(limits.max_diagnostics() as usize);
  let sources = SourceMap::of(&packages);

  let dependencies = check::package_set(&packages, &sources, &mut reporter);
  let within = check::source_bytes(&packages, &sources, limits, &mut reporter);
  Prepared {
    packages,
    reporter,
    sources,
    dependencies,
    within,
  }
}

/// Everything after the parse, compiled once whatever the caller's text type and whatever the
/// parser: the phases that read the documents, and the rules.
fn finish<'a>(prepared: Prepared<'a>, documents: &Documents<'a>, limits: &Limits) -> Analysis {
  let Prepared {
    packages,
    mut reporter,
    sources,
    dependencies,
    within: _,
  } = prepared;
  debug_assert_eq!(
    documents.len(),
    sources.len() as usize,
    "one document entry per source"
  );

  let mut builder = builder_of(&packages, &sources, &dependencies);
  let collected = collect::items(&packages, &sources, documents, limits, &mut reporter);
  let declarations = declare::declarations(collected, &sources, &mut builder, &mut reporter);
  let scopes = resolve::scopes(
    &packages,
    &sources,
    &dependencies,
    &declarations,
    limits,
    &mut builder,
    &mut reporter,
  );
  let raw = lower::lower(
    &packages,
    &declarations,
    &scopes,
    &sources,
    &mut builder,
    &mut reporter,
  );
  let mut types = Types::new(*limits);
  instantiate::instantiate(
    &declarations,
    &sources,
    &mut types,
    &mut builder,
    &mut reporter,
  );
  coerce::coerce(
    raw,
    &declarations,
    &scopes,
    &sources,
    &mut types,
    &mut builder,
    &mut reporter,
  );
  bind::bind(&declarations, &sources, &mut builder);
  implement::implement(
    &declarations,
    &sources,
    &mut types,
    &mut builder,
    &mut reporter,
  );
  tiers::tiers(&declarations, &mut builder);

  conclude(builder.finish(), sources, reporter, &mut Rules::default())
}

/// The model of `root` beside `dependencies` as name resolution leaves it, before lowering, and
/// what `path` names in the module `module` of those packages, where `params` are the generic
/// parameters in scope: the test-support door to the resolver. `None` when `module` names no module. See
/// `Scopes::module_at` and `Scopes::door` for how `module` and `path` are spelled.
///
/// Its files are read as [`analyze`] reads them: by the syntactic layer under its default limits.
#[cfg(feature = "test-support")]
pub(crate) fn resolve_door<S: SourceText>(
  root: &Package<S>,
  dependencies: &[Package<S>],
  module: &str,
  params: &[&[u8]],
  path: &str,
) -> Option<(Model, resolve::Resolved)> {
  let limits = Limits::new();
  let mut prepared = prepare(views(root, dependencies), &limits);
  let documents = parse::files(
    &Syntactic::new(),
    &prepared.packages,
    &prepared.sources,
    prepared.within,
    &mut prepared.reporter,
    None,
  );
  let Prepared {
    packages,
    mut reporter,
    sources,
    dependencies: resolved,
    within: _,
  } = prepared;
  let mut builder = builder_of(&packages, &sources, &resolved);
  let collected = collect::items(&packages, &sources, &documents, &limits, &mut reporter);
  let declarations = declare::declarations(collected, &sources, &mut builder, &mut reporter);
  let scopes = resolve::scopes(
    &packages,
    &sources,
    &resolved,
    &declarations,
    &limits,
    &mut builder,
    &mut reporter,
  );
  let at = scopes.module_at(&builder, module)?;
  let answer = scopes.door(&builder, at, params, path);
  Some((builder.finish(), answer))
}

/// What a parser layer kept beside an analysis: one entry for each supplied file the layer read, in
/// source id order.
///
/// [`analyze_with`] answers it. A file the layer refused, a file that never reached the layer, and
/// every package source have no entry.
///
/// A file that holds nothing but white space, commas, byte order marks and comments never reaches
/// the layer: the analysis accepts it as a document with no item, so it has no entry here. A tool
/// that wants a tree for such a file parses its text with `smear-parser`'s lossless layer itself,
/// and is answered a tree with one syntax error, because for the parser a text with no item is
/// not a document.
#[derive(Debug, Clone)]
pub struct Trees<T> {
  trees: Vec<(SourceId, T)>,
}

impl<T> Trees<T> {
  /// What the layer kept for `source`, or `None` when it kept nothing for it.
  pub fn get(&self, source: SourceId) -> Option<&T> {
    let at = self
      .trees
      .binary_search_by_key(&source, |entry: &(SourceId, T)| entry.0)
      .ok()?;
    Some(&self.trees[at].1)
  }

  /// Every entry with its source, in source id order.
  #[inline]
  pub fn iter(&self) -> TreesIter<'_, T> {
    TreesIter {
      inner: self.trees.iter(),
    }
  }

  /// How many sources have an entry.
  #[inline]
  pub fn len(&self) -> usize {
    self.trees.len()
  }

  /// Whether no source has an entry.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.trees.is_empty()
  }

  /// The node an extent belongs to: [`SyntaxTree::covering_node`], in the tree of the extent's
  /// source.
  ///
  /// `None` for a location with no span, for a source with no tree, and for a span that does not
  /// lie inside its tree's text. An extent the analysis answers has a span, but for one: the own
  /// contribution of a primitive `core` materialises, when no definition of the root package's
  /// replaces it, answers the whole of `core`'s package source.
  ///
  /// Leading trivia lies outside a node, and trailing trivia can lie inside it, up to the next
  /// token, so a node's range may hold a comment written for what follows. A tool that rewrites an
  /// entity replaces its extent, never its node's range.
  pub fn node(&self, at: Location) -> Option<T::Node>
  where
    T: SyntaxTree,
  {
    let span = at.span()?;
    self.get(SourceId::new(at.source()))?.covering_node(span)
  }
}

impl<T> IntoIterator for Trees<T> {
  type Item = (SourceId, T);
  type IntoIter = TreesIntoIter<T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    TreesIntoIter {
      inner: self.trees.into_iter(),
    }
  }
}

impl<'a, T> IntoIterator for &'a Trees<T> {
  type Item = (SourceId, &'a T);
  type IntoIter = TreesIter<'a, T>;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

/// [`Trees`]' entries by reference, in source id order: what [`Trees::iter`] answers.
#[derive(Debug, Clone)]
pub struct TreesIter<'a, T> {
  inner: core::slice::Iter<'a, (SourceId, T)>,
}

impl<'a, T> Iterator for TreesIter<'a, T> {
  type Item = (SourceId, &'a T);

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self
      .inner
      .next()
      .map(|entry: &'a (SourceId, T)| (entry.0, &entry.1))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.inner.size_hint()
  }
}

impl<T> ExactSizeIterator for TreesIter<'_, T> {}

impl<T> FusedIterator for TreesIter<'_, T> {}

/// [`Trees`]' entries by value, in source id order: what [`Trees`]' `into_iter` answers.
#[derive(Debug, Clone)]
pub struct TreesIntoIter<T> {
  inner: std::vec::IntoIter<(SourceId, T)>,
}

impl<T> Iterator for TreesIntoIter<T> {
  type Item = (SourceId, T);

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    self.inner.next()
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    self.inner.size_hint()
  }
}

impl<T> ExactSizeIterator for TreesIntoIter<T> {}

impl<T> FusedIterator for TreesIntoIter<T> {}

/// What every analysis ends with, once its phases have written `model` and reported into
/// `reporter`: the phases' marks applied, `rules` walked over every supplied package and reporting
/// into the same list, and the marks of the rules' errors applied once the walk has ended.
fn conclude<V: Visitor + ?Sized>(
  mut model: Model,
  sources: SourceMap,
  mut reporter: Reporter,
  rules: &mut V,
) -> Analysis {
  let ceiling = reporter.ceiling();
  // The rules read the phases' marks.
  model.mark(&reporter.take_marks());
  // Theirs are kept aside until every rule has walked, so no rule sees another's.
  let reporter = drive(&model, &sources, Packages::Supplied, reporter, rules);
  let (diagnostics, marks) = reporter.finish();
  model.mark(&marks);
  Analysis {
    model,
    diagnostics,
    sources,
    ceiling,
  }
}

/// A model builder holding every package of `packages`, which are in id order and numbered by
/// `sources`, with its resolved `dependencies`, one list per package, and its integer provider.
fn builder_of(
  packages: &[PackageView<'_>],
  sources: &SourceMap,
  dependencies: &[Vec<PackageId>],
) -> ModelBuilder {
  let mut builder = ModelBuilder::default();
  for ((index, view), resolved) in packages.iter().enumerate().zip(dependencies) {
    let kind = match index.cmp(&ROOT) {
      Ordering::Less => PackageKind::BuiltIn,
      Ordering::Equal => PackageKind::Root,
      Ordering::Greater => PackageKind::Dependency,
    };
    let name: String = String::from_utf8_lossy(view.name).into_owned();
    let package = builder.push_package(
      &name,
      kind,
      view.tier,
      view.layout,
      sources.package_sources(index).len(),
    );
    for &dependency in resolved {
      builder.push_dependency(package, dependency);
    }
    if let Some(integers) = view.integers {
      builder.set_integers(package, integers.clone());
    }
  }
  builder
}

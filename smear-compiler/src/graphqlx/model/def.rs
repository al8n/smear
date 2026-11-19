//! Definitions: the types and directives the packages define, and what each one answers.

use core::iter::FusedIterator;

use derive_more::{IsVariant, TryUnwrap, Unwrap};
use tokora::diagnostic::Location;

use crate::graphqlx::{
  id::{ApplicationId, DefId, IdRange, MemberId, ModuleId, Sym, TypeExprId, dense},
  package::Tier,
};

use super::{
  Model,
  member::MemberKind,
  name::Description,
  table::{ContributionRow, DefRow, Run},
};

/// What kind of definition a definition is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum DefKind {
  /// A scalar: a primitive, a custom scalar, or a scalar a host type stands for.
  Scalar,
  /// An object type.
  Object,
  /// An interface, a marker included.
  Interface,
  /// A union.
  Union,
  /// An enum.
  Enum,
  /// An input object.
  InputObject,
  /// A directive definition.
  Directive,
}

impl DefKind {
  /// The kind of the direct members a definition of this kind has, or `None` for one that has
  /// none.
  pub(super) const fn member_kind(self) -> Option<MemberKind> {
    match self {
      Self::Object | Self::Interface => Some(MemberKind::Field),
      Self::InputObject => Some(MemberKind::InputField),
      Self::Enum => Some(MemberKind::EnumValue),
      Self::Directive => Some(MemberKind::Argument),
      Self::Scalar | Self::Union => None,
    }
  }
}

/// Where a definition comes from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum Origin {
  /// A supplied package's own definition.
  User,
  /// A definition of `core`, `alloc` or `std`: an embedded one, a materialised primitive, or a
  /// GraphQL built-in the root package replaces, which keeps its built-in's identity.
  BuiltIn,
  /// One of GraphQL's introspection meta-types, which a consumer may skip.
  Compat,
}

/// The meaning the language gives an intrinsic primitive of `core`.
///
/// A primitive is a definition like any other, and this is how a consumer knows which one it is
/// without matching on its spelling. `i32` and `Int` are two names of the definition whose
/// primitive is `Primitive::signed(32)`.
///
/// # Examples
///
/// An integer primitive is built with its constructor, and read through its payload's accessors:
///
/// ```
/// use smear_compiler::graphqlx::Primitive;
///
/// let int = Primitive::signed(32);
/// match int {
///   Primitive::Int(int) => assert!(int.is_signed() && int.width() == 32),
///   _ => unreachable!(),
/// }
/// ```
///
/// Outside smear, a struct expression is refused: [`IntPrimitive`]'s fields are private, so
/// [`Primitive::signed`] and [`Primitive::unsigned`] are the only ways to build one.
///
/// ```compile_fail,E0451
/// use smear_compiler::graphqlx::{IntPrimitive, Primitive};
///
/// let int = Primitive::Int(IntPrimitive { signed: true, width: 32 });
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Unwrap, TryUnwrap)]
#[unwrap(ref, ref_mut)]
#[try_unwrap(ref, ref_mut)]
#[non_exhaustive]
pub enum Primitive {
  /// `bool`.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  Bool,
  /// `f32`.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  F32,
  /// `f64`.
  #[unwrap(ignore)]
  #[try_unwrap(ignore)]
  F64,
  /// `iN` or `uN`. Built with [`Primitive::signed`] and [`Primitive::unsigned`].
  Int(IntPrimitive),
}

impl Primitive {
  /// `iN`: the signed integer `width` bits wide.
  #[inline]
  pub const fn signed(width: u32) -> Self {
    Self::Int(IntPrimitive::new(true, width))
  }

  /// `uN`: the unsigned integer `width` bits wide.
  #[inline]
  pub const fn unsigned(width: u32) -> Self {
    Self::Int(IntPrimitive::new(false, width))
  }

  /// Its place among `core`'s materialised definitions: `bool`, `f32`, `f64`, then the signed
  /// widths ascending, then the unsigned widths ascending. Two primitives share it only when they
  /// are one.
  pub(crate) const fn rank(self) -> Rank {
    match self {
      Self::Bool => Rank { kind: 0, width: 0 },
      Self::F32 => Rank { kind: 1, width: 0 },
      Self::F64 => Rank { kind: 2, width: 0 },
      Self::Int(IntPrimitive {
        signed: true,
        width,
      }) => Rank { kind: 3, width },
      Self::Int(IntPrimitive {
        signed: false,
        width,
      }) => Rank { kind: 4, width },
    }
  }
}

/// A primitive's place among `core`'s materialised definitions: [`Primitive::rank`]'s answer. It
/// orders as the definitions' ids do.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Rank {
  /// Which kind of primitive: `bool` is 0, `f32` 1, `f64` 2, a signed integer 3 and an unsigned
  /// integer 4.
  pub(crate) kind: u8,
  /// The width of an integer, and 0 for the others.
  pub(crate) width: u32,
}

/// An integer primitive, `iN` or `uN`: [`Primitive::Int`]'s payload. Built with
/// [`Primitive::signed`] and [`Primitive::unsigned`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntPrimitive {
  pub(crate) signed: bool,
  pub(crate) width: u32,
}

impl IntPrimitive {
  /// The integer `width` bits wide, signed when `signed`.
  ///
  /// Crate-private: [`Primitive::signed`] and [`Primitive::unsigned`] are the public doors.
  #[inline]
  pub(crate) const fn new(signed: bool, width: u32) -> Self {
    Self { signed, width }
  }

  /// Whether it is `iN`, signed, rather than `uN`.
  #[inline]
  pub const fn is_signed(&self) -> bool {
    self.signed
  }

  /// Its width in bits: `N`.
  #[inline]
  pub const fn width(&self) -> u32 {
    self.width
  }
}

/// The collection a definition stands for: what `@set` or `@map` makes of it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant)]
#[non_exhaustive]
pub enum Collection {
  /// A set, through `@set`.
  Set,
  /// A map, through `@map`.
  Map,
}

/// Where a directive may be applied: one of the locations a directive definition lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DirectiveLocation {
  /// `QUERY`.
  Query,
  /// `MUTATION`.
  Mutation,
  /// `SUBSCRIPTION`.
  Subscription,
  /// `FIELD`.
  Field,
  /// `FRAGMENT_DEFINITION`.
  FragmentDefinition,
  /// `FRAGMENT_SPREAD`.
  FragmentSpread,
  /// `INLINE_FRAGMENT`.
  InlineFragment,
  /// `VARIABLE_DEFINITION`.
  VariableDefinition,
  /// `SCHEMA`.
  Schema,
  /// `SCALAR`.
  Scalar,
  /// `OBJECT`.
  Object,
  /// `FIELD_DEFINITION`.
  FieldDefinition,
  /// `ARGUMENT_DEFINITION`.
  ArgumentDefinition,
  /// `INTERFACE`.
  Interface,
  /// `UNION`.
  Union,
  /// `ENUM`.
  Enum,
  /// `ENUM_VALUE`.
  EnumValue,
  /// `INPUT_OBJECT`.
  InputObject,
  /// `INPUT_FIELD_DEFINITION`.
  InputFieldDefinition,
  /// `DIRECTIVE_DEFINITION`.
  DirectiveDefinition,
}

/// A bound on one of a definition's type parameters: the `T: Node` of `where T: Node`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeBound {
  pub(super) parameter: u32,
  pub(super) interface: TypeExprId,
  pub(super) location: Location,
}

impl TypeBound {
  /// The bounded parameter: its index among its definition's type parameters.
  #[inline]
  pub const fn parameter(&self) -> u32 {
    self.parameter
  }

  /// The interface or marker the parameter must satisfy, as written. Its type may name the
  /// definition's type parameters, which stand for an instantiation's arguments: `Comparable<T>`
  /// of `where T: Comparable<T>` is, for `Sorted<User>`, `Comparable<User>`. A right side that
  /// names neither an interface nor a marker, a definition of another kind or a type parameter, is
  /// refused where it is written, and kept here all the same.
  #[inline]
  pub const fn interface(&self) -> TypeExprId {
    self.interface
  }

  /// Where the `where` predicate that writes it is written, which every bound of that predicate
  /// shares.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }
}

impl Model {
  /// Every definition id: by package, then module path, then source id, then position.
  #[inline]
  pub fn defs(&self) -> IdRange<DefId> {
    IdRange::new(0, dense(self.defs.len()))
  }

  /// The definition `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of [`defs`](Self::defs): an id belongs to the analysis that issued it.
  #[inline]
  pub fn def(&self, id: DefId) -> Def<'_> {
    assert!(
      (id.get() as usize) < self.defs.len(),
      "definition id {} is past the {} definitions of this model",
      id.get(),
      self.defs.len()
    );
    Def { model: self, id }
  }
}

/// One type or directive definition of the model.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Def<'a> {
  model: &'a Model,
  id: DefId,
}

impl<'a> Def<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> DefId {
    self.id
  }

  /// What kind of definition it is.
  #[inline]
  pub fn kind(&self) -> DefKind {
    self.row().kind
  }

  /// Its name. A second name a module gives it, as `Int` is of `i32`, is not this.
  #[inline]
  pub fn name(&self) -> &'a str {
    let model: &'a Model = self.model;
    model.name(self.row().name)
  }

  /// Its name's symbol.
  #[inline]
  pub fn sym(&self) -> Sym {
    self.row().name
  }

  /// The module it belongs to.
  #[inline]
  pub fn module(&self) -> ModuleId {
    self.row().module
  }

  /// Where its name is written in its definition. A primitive `core` materialises has no position
  /// of its own: it answers the whole of `core`'s package source. A GraphQL built-in the root
  /// package replaces answers where the root package names it.
  #[inline]
  pub fn location(&self) -> Location {
    self.row().location
  }

  /// What each written item contributed to it: its definition, then each extension by source id
  /// and position.
  #[inline]
  pub fn contributions(&self) -> Contributions<'a> {
    Contributions::of(self.model, self.row().contributions)
  }

  /// Where it comes from.
  #[inline]
  pub fn origin(&self) -> Origin {
    self.row().origin
  }

  /// Whether an error a phase or one of smear's rules reported lies within one of its
  /// contributions.
  #[inline]
  pub fn has_errors(&self) -> bool {
    let model: &'a Model = self.model;
    self
      .row()
      .contributions
      .of(&model.contributions)
      .iter()
      .any(|part: &ContributionRow| part.marked)
  }

  /// Its description, if its definition writes one.
  #[inline]
  pub fn description(&self) -> Option<Description<'a>> {
    Description::of(self.model, self.row().description)
  }

  /// Its type parameters, in order.
  #[inline]
  pub fn type_params(&self) -> TypeParams<'a> {
    TypeParams {
      model: self.model,
      run: self.row().type_params,
      front: 0,
      back: self.row().type_params.len,
    }
  }

  /// The names the contribution at `position` among its contributions gives its type parameters,
  /// when it is an extension that writes a list, which names them by position: each name with
  /// where it is written, in written order. None for its own contribution, whose names are its
  /// [`type_params`](Self::type_params), for an extension that writes no list, and for a position
  /// past its contributions.
  //
  // Crate-private: the public model has no view of an extension's own names, and the rule on
  // duplicate type parameter names reads them here.
  #[inline]
  pub(crate) fn extension_params(&self, position: u32) -> &'a [(Sym, Location)] {
    let model: &'a Model = self.model;
    let run = self.row().contributions;
    if position >= run.len {
      return &[];
    }
    match model
      .contributions
      .get(run.start as usize + position as usize)
    {
      Some(part) => part.params.of(&model.written_params),
      None => &[],
    }
  }

  /// The bounds on its type parameters, in order.
  #[inline]
  pub fn bounds(&self) -> &'a [TypeBound] {
    let model: &'a Model = self.model;
    self.row().bounds.of(&model.bounds)
  }

  /// The interfaces and markers its `implements` lists, as written, every contribution's in
  /// order. Only what is written: what it satisfies intrinsically is
  /// [`Model::satisfies`]'s.
  #[inline]
  pub fn implements(&self) -> &'a [TypeExprId] {
    let model: &'a Model = self.model;
    self.row().implements.of(&model.implements)
  }

  /// An object's or an interface's fields, in id order; empty for every other kind.
  #[inline]
  pub fn fields(&self) -> IdRange<MemberId> {
    self.direct(MemberKind::Field)
  }

  /// An input object's fields, in id order; empty for every other kind.
  #[inline]
  pub fn input_fields(&self) -> IdRange<MemberId> {
    self.direct(MemberKind::InputField)
  }

  /// An enum's values, in id order; empty for every other kind.
  #[inline]
  pub fn enum_values(&self) -> IdRange<MemberId> {
    self.direct(MemberKind::EnumValue)
  }

  /// A directive definition's arguments, in id order; empty for every other kind.
  #[inline]
  pub fn arguments(&self) -> IdRange<MemberId> {
    self.direct(MemberKind::Argument)
  }

  /// A union's members, as written, every contribution's in order; empty for every other kind.
  #[inline]
  pub fn union_members(&self) -> &'a [TypeExprId] {
    let model: &'a Model = self.model;
    self.row().union_members.of(&model.union_members)
  }

  /// The locations a directive definition lists, as written; empty for every other kind.
  #[inline]
  pub fn locations(&self) -> &'a [DirectiveLocation] {
    let model: &'a Model = self.model;
    self.row().locations.of(&model.locations)
  }

  /// Whether a directive definition is `repeatable`.
  #[inline]
  pub fn is_repeatable(&self) -> bool {
    self.row().repeatable
  }

  /// Its directive applications, its definition's and then each extension's.
  #[inline]
  pub fn applications(&self) -> &'a [ApplicationId] {
    let model: &'a Model = self.model;
    self.row().applications.of(&model.listed_applications)
  }

  /// The tier it requires: the least of `core`, `alloc` and `std` that holds everything it uses.
  ///
  /// A definition of `core`, `alloc` or `std` requires its package's tier, whatever it names: the
  /// tier is a tag, a meta-type's and a GraphQL built-in's the root package replaces included. A
  /// scalar a supplied package defines requires the greatest of its externs' tiers, and `core` with
  /// none; an enum or a directive requires `core`, a directive's arguments counting nowhere. An
  /// object or an interface requires the greatest of the tiers of its fields' types and of their
  /// arguments' types, an input object of its input fields' types, and a union of its members'
  /// types, each type's as [`Ty::tier`](crate::graphqlx::Ty::tier) answers; a field, an argument
  /// or an input field that applies `core`'s `@heap` requires `alloc` at least. Definitions that
  /// hold one another share the greatest tier any of them reaches.
  ///
  /// In an analysis that [`is_ok`](crate::graphqlx::Analysis::is_ok), it is at most its package's
  /// [`tier`](crate::graphqlx::PackageInfo::tier), a meta-type and a
  /// [metadata](Self::is_metadata) definition aside: a use above a package's tier is
  /// [`Tier`](crate::graphqlx::DiagnosticKind::Tier).
  #[inline]
  pub fn tier(&self) -> Tier {
    self.row().tier
  }

  /// Whether it is compile-time metadata: a scalar, an enum or an input object a supplied package
  /// defines that compile-time metadata reaches and that nothing outside it reaches.
  ///
  /// Compile-time metadata is every type a type-system directive writes, its arguments' types and
  /// its type parameters' defaults and bounds, a type-system directive being one that lists no
  /// executable location such as `FIELD` or `QUERY`; and every type argument of a directive
  /// application, written or filled from its directive's defaults. It reaches each definition such
  /// a type names, through lists and every type argument, and what each metadata definition's own
  /// types name in turn, every package's together. A type written anywhere else that names a
  /// definition, through any type argument and in whatever package, makes it no metadata:
  /// `f(b: Binding)`, `Ref<Binding>` and `[Binding]` each do, and so does the argument type of a
  /// directive an operation can apply, whose values arrive at run time. Such a directive that holds
  /// one of its own type parameters in an argument's type, as `directive @run<T>(v: T) on FIELD`
  /// does, makes no definition metadata at all: an operation can fill that parameter with any of
  /// them, which no schema can know. An object, an interface, a union and a directive are never
  /// metadata, nor is a definition of `core`, `alloc` or `std`. This is not the GraphQL
  /// meta-types, whose origin is [`Compat`](Origin::Compat).
  ///
  /// The answer is the analyzed program's. A package analyzed alone can answer `true` for a
  /// definition that a dependant's field makes runtime, and `false` for one only a dependant's
  /// directive takes, so a backend that compiles a package apart from its dependants cannot leave a
  /// definition out on this answer. A type that names a metadata definition is written only in
  /// metadata, though [`Model::instantiations`] lists it under its definition, which need not be
  /// metadata: `Pair<Note>` under `Pair`. A backend that leaves metadata out leaves such an
  /// instantiation out too. In an analysis that is not ok, an occurrence a phase left with no
  /// complete type, or with the unresolved type, names nothing, so a definition compile-time
  /// metadata reaches can answer `true` when its only use outside metadata is such an occurrence.
  /// An occurrence a phase refused but kept the complete type of, as one whose bound fails, still
  /// reaches what that type names.
  ///
  /// The tier rule judges nothing a metadata definition holds, so in an analysis that is ok its
  /// [`tier`](Self::tier) may be above its package's. A backend that emits only what runs leaves it
  /// out, and one that emits it for a compiler reads its tier. It is computed whatever the verdict.
  ///
  /// # Examples
  ///
  /// An input object only a directive's argument takes is metadata, and a `core` package may hold
  /// it though it needs `alloc`:
  ///
  /// ```
  /// use smear_compiler::graphqlx::{
  ///   Layout, Limits, Namespace, Options, Package, SourceFile, Tier, analyze,
  /// };
  ///
  /// let text = "type Query { ok: Int }\n\
  ///             input Binding { lang: string! }\n\
  ///             directive @bind(b: Binding) on OBJECT\n";
  /// let root = Package::new("app", Layout::Flat, Tier::Core)
  ///   .with_files([SourceFile::new("schema.graphqlx", text)]);
  /// let analysis = analyze(&root, &[], &Limits::new(), &Options::new());
  /// assert!(analysis.is_ok());
  /// let model = analysis.model();
  /// let binding = model.lookup(Namespace::Type, "^::app::Binding").expect("defined");
  /// assert!(model.def(binding).is_metadata());
  /// assert_eq!(model.def(binding).tier(), Tier::Alloc);
  /// ```
  #[inline]
  pub fn is_metadata(&self) -> bool {
    self.row().metadata
  }

  /// Where a type outside compile-time metadata first reaches it, when compile-time metadata
  /// reaches it too: where the tier rule labels its reports within it. `None` for every other
  /// definition.
  //
  // Crate-private: no public view answers where a reach entered, and the tier rule reads it here.
  #[inline]
  pub(crate) fn runtime_entry(&self) -> Option<Location> {
    let entries = &self.model.runtime_entries;
    let at = entries
      .binary_search_by_key(&self.id, |&(def, _): &(DefId, Location)| def)
      .ok()?;
    entries
      .get(at)
      .map(|&(_, location): &(DefId, Location)| location)
  }

  /// The intrinsic primitive it is, or `None` for every definition but `core`'s primitives.
  #[inline]
  pub fn intrinsic(&self) -> Option<Primitive> {
    self.row().intrinsic
  }

  /// Whether it is a marker: an interface `core`'s `@marker` makes one, which declares no field,
  /// and which only `implements` and bounds may name. Only an application the package that wrote
  /// the definition writes makes it one, in its definition or in an extension of its own: an
  /// extension another package writes says nothing of what the definition is. A marker that
  /// declares a field is a marker all the same: the field is
  /// [`MarkerMisuse`](crate::graphqlx::DiagnosticKind::MarkerMisuse), which the marker's
  /// `has_errors()` says.
  #[inline]
  pub fn is_marker(&self) -> bool {
    self.row().marker
  }

  /// The collection it stands for, through `core`'s `@set` or `@map`, if any: the first of the two
  /// that the package that wrote the definition applies to it, in its definition or in an
  /// extension of its own. An extension another package writes says nothing of what the definition
  /// is. A scalar with another count of type parameters than its collection has, or that applies
  /// both, stands for the first all the same: its application is
  /// [`CollectionArity`](crate::graphqlx::DiagnosticKind::CollectionArity), which the scalar's
  /// `has_errors()` says.
  #[inline]
  pub fn collection(&self) -> Option<Collection> {
    self.row().collection
  }

  /// Its host types: each `core` `@extern` its own package writes on it, in its definition or in an
  /// extension of its own, in its applications' order, as (`lang`, `type`, `tier`), each with its
  /// effective tier. Only a scalar a supplied package defines has any, and only an `@extern` whose
  /// `lang` and `type` are strings and whose `tier` is absent, `null` or a value of `core`'s `Tier`
  /// is one: what another package writes binds nothing, and a value that does not fit is refused
  /// where it is written. A `lang` written twice is here twice, and the later is
  /// [`ExternMisuse`](crate::graphqlx::DiagnosticKind::ExternMisuse).
  ///
  /// A built-in has none, a primitive and a GraphQL built-in the root package replaces among them:
  /// a built-in's host type is given by `@repr(for: …)` on the schema alone.
  #[inline]
  pub fn externs(&self) -> Externs<'a> {
    Externs {
      model: self.model,
      run: self.row().externs,
      front: 0,
      back: self.row().externs.len,
    }
  }

  /// Its direct members when they are of `kind`, and none otherwise.
  fn direct(&self, kind: MemberKind) -> IdRange<MemberId> {
    let row = self.row();
    if row.kind.member_kind() == Some(kind) {
      row.members
    } else {
      IdRange::new(0, 0)
    }
  }

  /// The model's record of this definition.
  fn row(&self) -> &'a DefRow {
    let model: &'a Model = self.model;
    &model.defs[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Def<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Def")
      .field("id", &self.id)
      .field("kind", &self.kind())
      .field("name", &self.name())
      .field("module", &self.module())
      .finish()
  }
}

/// What one written item contributed to a definition, or to the schema: the definition itself, or
/// one extension.
///
/// `Copy`: everything it answers is borrowed from the model.
#[derive(Debug, Clone, Copy)]
pub struct Contribution<'a> {
  location: Location,
  extent: Location,
  members: IdRange<MemberId>,
  implements: &'a [TypeExprId],
  union_members: &'a [TypeExprId],
  applications: &'a [ApplicationId],
  extension: bool,
  has_errors: bool,
}

impl<'a> Contribution<'a> {
  /// Where it names what it contributes to: a definition's name, or an extension's path to its
  /// target. For one of the schema's, where its first keyword is written: `schema` for a schema
  /// definition, `extend` for a schema extension.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }

  /// Where it is written, in its own source, which for an extension is the extension's file: a
  /// definition, or a schema definition, from its description, or its first keyword when it has
  /// none, to its last token; an extension from its `extend` to its last token.
  ///
  /// A primitive `core` materialises, when no definition of the root package's replaces it, has no
  /// text of its own: its own contribution answers the whole of `core`'s package source, which has
  /// no span, as its [`Def::location`] does, and an extension of it answers the extension's extent.
  /// A GraphQL built-in the root package replaces, a primitive among them, answers for its own
  /// contribution the extent of the root package's definition.
  ///
  /// In an analysis made through [`analyze_with`](crate::graphqlx::analyze_with) with a layer that
  /// keeps a tree, such as `smear-parser`'s lossless layer, the smallest node of its file's tree
  /// that covers the extent is the node the contribution was written as, and
  /// [`Trees::node`](crate::graphqlx::Trees::node) answers it. A file of a built-in package has no
  /// tree, and `Trees::node` answers `None` for an extent in one.
  #[inline]
  pub const fn extent(&self) -> Location {
    self.extent
  }

  /// Whether it is an extension: `false` for a definition's own contribution, which is its first,
  /// and for a schema definition; `true` for each `extend` of a definition or of the schema, which
  /// follow them.
  #[inline]
  pub const fn is_extension(&self) -> bool {
    self.extension
  }

  /// The direct members it contributed, in id order.
  #[inline]
  pub const fn members(&self) -> IdRange<MemberId> {
    self.members
  }

  /// The `implements` entries it wrote.
  #[inline]
  pub const fn implements(&self) -> &'a [TypeExprId] {
    self.implements
  }

  /// The union members it wrote.
  #[inline]
  pub const fn union_members(&self) -> &'a [TypeExprId] {
    self.union_members
  }

  /// The directive applications it wrote.
  #[inline]
  pub const fn applications(&self) -> &'a [ApplicationId] {
    self.applications
  }

  /// Whether an error a phase or one of smear's rules reported lies within it, from its
  /// description, or its first keyword, to its last token.
  #[inline]
  pub const fn has_errors(&self) -> bool {
    self.has_errors
  }
}

/// The contributions of a definition or of the schema, in order.
#[derive(Clone)]
pub struct Contributions<'a> {
  model: &'a Model,
  run: Run,
  front: u32,
  back: u32,
}

impl<'a> Contributions<'a> {
  /// The contributions of the run `run` of `model`'s contribution table.
  pub(super) fn of(model: &'a Model, run: Run) -> Self {
    Self {
      model,
      run,
      front: 0,
      back: run.len,
    }
  }

  /// The contribution at `position` of the run.
  fn at(&self, position: u32) -> Contribution<'a> {
    let model: &'a Model = self.model;
    let row = &model.contributions[(self.run.start + position) as usize];
    Contribution {
      location: row.location,
      extent: row.extent,
      members: row.members,
      implements: row.implements.of(&model.implements),
      union_members: row.union_members.of(&model.union_members),
      applications: row.applications.of(&model.listed_applications),
      extension: row.extension,
      has_errors: row.marked,
    }
  }
}

impl<'a> Iterator for Contributions<'a> {
  type Item = Contribution<'a>;

  #[inline]
  fn next(&mut self) -> Option<Contribution<'a>> {
    if self.front >= self.back {
      return None;
    }
    let position = self.front;
    self.front += 1;
    Some(self.at(position))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = (self.back - self.front) as usize;
    (remaining, Some(remaining))
  }
}

impl DoubleEndedIterator for Contributions<'_> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    if self.front >= self.back {
      return None;
    }
    self.back -= 1;
    Some(self.at(self.back))
  }
}

impl ExactSizeIterator for Contributions<'_> {}

impl FusedIterator for Contributions<'_> {}

impl core::fmt::Debug for Contributions<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Contributions")
      .field("remaining", &(self.back - self.front))
      .finish()
  }
}

/// One type parameter of a definition.
///
/// `Copy`: everything it answers is borrowed from the model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeParam<'a> {
  name: &'a str,
  sym: Sym,
  index: u32,
  location: Location,
  default: Option<TypeExprId>,
}

impl<'a> TypeParam<'a> {
  /// Its name, as its definition writes it.
  #[inline]
  pub const fn name(&self) -> &'a str {
    self.name
  }

  /// Its name's symbol.
  #[inline]
  pub const fn sym(&self) -> Sym {
    self.sym
  }

  /// Its index among its definition's type parameters: what a type that names it holds.
  #[inline]
  pub const fn index(&self) -> u32 {
    self.index
  }

  /// Where its name is written.
  #[inline]
  pub const fn location(&self) -> Location {
    self.location
  }

  /// Its default, as written, or `None` when it has none.
  #[inline]
  pub const fn default(&self) -> Option<TypeExprId> {
    self.default
  }
}

/// The type parameters of a definition, in order.
#[derive(Clone)]
pub struct TypeParams<'a> {
  model: &'a Model,
  run: Run,
  front: u32,
  back: u32,
}

impl<'a> TypeParams<'a> {
  /// The type parameter at `position` of the run.
  fn at(&self, position: u32) -> TypeParam<'a> {
    let model: &'a Model = self.model;
    let row = &model.type_params[(self.run.start + position) as usize];
    TypeParam {
      name: model.name(row.name),
      sym: row.name,
      index: position,
      location: row.location,
      default: row.default,
    }
  }
}

impl<'a> Iterator for TypeParams<'a> {
  type Item = TypeParam<'a>;

  #[inline]
  fn next(&mut self) -> Option<TypeParam<'a>> {
    if self.front >= self.back {
      return None;
    }
    let position = self.front;
    self.front += 1;
    Some(self.at(position))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = (self.back - self.front) as usize;
    (remaining, Some(remaining))
  }

  /// The parameter `n` places on, in one step: the rows are in order, so it reads the one it is
  /// asked for and skips none.
  #[inline]
  fn nth(&mut self, n: usize) -> Option<TypeParam<'a>> {
    let remaining = self.back - self.front;
    match u32::try_from(n) {
      Ok(n) if n < remaining => {
        self.front += n;
        self.next()
      }
      _ => {
        self.front = self.back;
        None
      }
    }
  }
}

impl DoubleEndedIterator for TypeParams<'_> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    if self.front >= self.back {
      return None;
    }
    self.back -= 1;
    Some(self.at(self.back))
  }
}

impl ExactSizeIterator for TypeParams<'_> {}

impl FusedIterator for TypeParams<'_> {}

impl core::fmt::Debug for TypeParams<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("TypeParams")
      .field("remaining", &(self.back - self.front))
      .finish()
  }
}

/// One `@extern` of a scalar: the host type that stands for it in one language, which smear never
/// reads. Its `lang` and its `type` are the strings written, cooked, and its tier is the one its
/// `tier` names, or its scalar's package's when it names none.
///
/// Two externs are equal when they are one binding: when they answer alike and are bound from one
/// application.
///
/// `Copy`: everything it answers is borrowed from the model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Extern<'a> {
  lang: &'a str,
  ty: &'a str,
  tier: Tier,
  application: Option<ApplicationId>,
}

impl<'a> Extern<'a> {
  /// The `@extern` it is bound from: `Some` for every extern an analysis binds.
  #[inline]
  pub const fn application(&self) -> Option<ApplicationId> {
    self.application
  }

  /// Its `lang`: the host language, as the string is cooked.
  #[inline]
  pub const fn lang(&self) -> &'a str {
    self.lang
  }

  /// Its `type`: the host type, spelled as its language spells it, as the string is cooked.
  #[inline]
  pub const fn r#type(&self) -> &'a str {
    self.ty
  }

  /// Its effective tier: the `tier` it writes, or its declaring package's when it writes none or
  /// `null`.
  #[inline]
  pub const fn tier(&self) -> Tier {
    self.tier
  }
}

/// The `@extern`s of a definition, in order.
#[derive(Clone)]
pub struct Externs<'a> {
  model: &'a Model,
  run: Run,
  front: u32,
  back: u32,
}

impl<'a> Externs<'a> {
  /// The extern at `position` of the run.
  fn at(&self, position: u32) -> Extern<'a> {
    let model: &'a Model = self.model;
    let row = &model.externs[(self.run.start + position) as usize];
    Extern {
      lang: row.lang.text(&model.text),
      ty: row.ty.text(&model.text),
      tier: row.tier,
      application: row.application,
    }
  }
}

impl<'a> Iterator for Externs<'a> {
  type Item = Extern<'a>;

  #[inline]
  fn next(&mut self) -> Option<Extern<'a>> {
    if self.front >= self.back {
      return None;
    }
    let position = self.front;
    self.front += 1;
    Some(self.at(position))
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    let remaining = (self.back - self.front) as usize;
    (remaining, Some(remaining))
  }
}

impl DoubleEndedIterator for Externs<'_> {
  #[inline]
  fn next_back(&mut self) -> Option<Self::Item> {
    if self.front >= self.back {
      return None;
    }
    self.back -= 1;
    Some(self.at(self.back))
  }
}

impl ExactSizeIterator for Externs<'_> {}

impl FusedIterator for Externs<'_> {}

impl core::fmt::Debug for Externs<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Externs")
      .field("remaining", &(self.back - self.front))
      .finish()
  }
}

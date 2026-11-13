//! The codes the analysis reports with: the `smear::graphqlx` family.

use tokora::diagnostic::{Code, Severity};

/// Every kind of diagnostic the GraphQLx analysis reports under its own family, `smear::graphqlx`.
///
/// A diagnostic's [`code`](Self::code) is what a consumer keys off: a rule is waived by filtering
/// its code out, and the code never changes when the message is reworded. A file that does not
/// parse is reported with the parser's own `smear::syntax` code, which is not listed here.
///
/// The list grows as the analysis does, one variant and one code at a time; the enum is
/// `#[non_exhaustive]` so that a code added later breaks no `match`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum DiagnosticKind {
  // -- The package set, each reported at the whole of the offending package's package source ----
  /// Two files of one package have one path.
  DuplicateFilePath,
  /// A file's path is absolute, uses `\`, is not UTF-8, or has an empty, `.` or `..` segment.
  InvalidFilePath,
  /// A supplied package's name is not a GraphQLx Name.
  InvalidPackageName,
  /// A supplied package is named `core`, `alloc` or `std`, which are the built-in packages'.
  ReservedPackageName,
  /// Two supplied packages have one name. Reported at the one the order numbers second.
  DuplicatePackageName,
  /// A package declares a dependency that is not supplied.
  MissingDependency,

  // -- The module tree ---------------------------------------------------------------------------
  /// A directory of a `Directory` package is not a GraphQLx Name, so it cannot be a module.
  /// Reported once per directory, at the whole of its package's package source; the files under
  /// it are parsed, and what they declare is not collected.
  InvalidDirectoryName,
  /// An inline `mod` declares a module its package already has: through a directory, or through
  /// an earlier `mod`. Reported at the later declaration, with a label at the first; the two are
  /// one module.
  ModuleDefinedTwice,
  /// A module declares a type with the name of one of its child modules, so that one name in its
  /// scope would be two things. Reported at the type, with a label at the child module.
  ModuleTypeCollision,

  // -- Items -------------------------------------------------------------------------------------
  /// A file holds an item, or a part of one, of a kind this analyzer does not know: a parser newer
  /// than the analyzer read the file. What the item declares, or the part names, is not in the
  /// model. The walk that collects items and lowering each report it once per file, at the first
  /// they meet; name resolution reports each path whose root is of such a kind, at the path.
  UnknownItem,

  // -- Name resolution ---------------------------------------------------------------------------
  /// A path names a package, after `^::` or as its first segment where a module is wanted, that
  /// the package it is written in cannot reach: neither one of its declared dependencies nor
  /// `core`, `alloc` or `std`. Reported at that segment.
  UnknownPackage,
  /// A path with a root, or with more than one segment, or a path a `use` item writes, names
  /// nothing, or names something of another kind than its place wants. Reported at the segment
  /// that does not resolve, or at the root that cannot be followed. A single segment with no root,
  /// in a place GraphQL has, keeps GraphQL's own code instead.
  UnresolvedPath,
  /// A name two glob `use` items of one module give to two different items, met where it is used:
  /// at the path that names it. Two globs that give one name to one item, and a name no path
  /// names, are no error.
  AmbiguousGlob,
  /// A `use` item's string source is a URL. The syntax is parsed, and nothing is fetched: a
  /// package reached through a URL is supplied by the caller and reached through `^::name`.
  UrlUnsupported,
  /// A `use` item's string source names a file of its package, where a string source names a
  /// directory, whose files are one module. The help names the file's directory.
  FilePathUse,
  /// A `use` item resolved to one thing while the module's `use` items were being resolved, and
  /// to another once all of them were: a name a glob, the prelude or a package gave it was
  /// bound otherwise by a `use` item resolved after it. Reported at the path's first segment.
  UseResolutionChanged,
  /// A `use` item binds a name its module declares in the same namespace, as a definition or, in
  /// the type namespace, as a child module. Reported at the name the `use` binds, with a label at
  /// the declaration.
  UseDeclarationCollision,
  /// Two `use` items, or two names of one, bind one name in one namespace of one module to two
  /// different items. Reported at the later in source order, with a label at the first; one item
  /// bound twice is no error.
  UseNameCollision,
  /// A supplied package declares a type with the name of a primitive of the prelude: `bool`,
  /// `f32`, `f64`, `iN`, `uN`, `string`, `bytes`, `NonZeroIN` or `NonZeroUN`. Within its module
  /// the name then names the declaration. A warning: GraphQL schemas declare such names freely. A
  /// root package's definition that replaces a GraphQL built-in shadows nothing, and is not
  /// reported.
  ShadowedPrimitive,

  // -- Lowering ----------------------------------------------------------------------------------
  /// A `where` predicate whose left side is not one of its definition's own type parameters,
  /// written with no type argument, or any `where` clause on an extension: the model holds a bound
  /// only on a definition's own parameter, under the definition. Reported at the predicate's left
  /// side, or at the extension's clause, and nothing of it is in the model. Or a bound whose right
  /// side names no interface: a definition of another kind, an alias such as `NonZeroU32`, or a
  /// type parameter. Reported at the right side, and the bound is in the model as written.
  InvalidBound,

  // -- Types -------------------------------------------------------------------------------------
  /// A use with a count of type arguments its definition does not take. A type occurrence writes
  /// more type arguments than its definition has type parameters, or fewer than it takes, every
  /// parameter from the first of a trailing run of defaulted ones being left to its default: it is
  /// reported where it is written, with a label at the definition's name. So is a directive
  /// application, at its path. A type parameter, or an alias such as `NonZeroU32`, written with
  /// type arguments is reported where it is written. An occurrence that leaves a type argument to
  /// a default that cannot fill it is reported where it is written, with a label at that default.
  /// Each such occurrence has the unresolved type.
  ///
  /// Lowering reports two lists of type parameters of the wrong length under it too. An
  /// extension's list names its target's type parameters by position, one name each, and names
  /// another number of them: reported at the list, with a label at the definition's name; the
  /// extension still contributes what it writes. Or a definition that replaces a GraphQL built-in
  /// writes one, where the built-in's identity has none: reported at the list, and nothing of the
  /// list is in the model.
  GenericArity,
  /// A default of a definition's type parameter that can never fill. A type parameter with no
  /// default follows one with a default: type arguments bind by position, so that default could
  /// never fill, and the first such parameter is reported, at its name, with a label at the
  /// default before it. Or a group of the definition's defaults need each other, a default that
  /// names its own parameter included: a use that leaves the group's lowest parameter out leaves
  /// out the whole group, so that parameter's default can never fill, and each such group is
  /// reported once, at that default, with a label, for a group of more than one, where the next of
  /// its members to name that parameter first names it. Or an occurrence written inside a default
  /// leaves out an argument whose filling leads back to a default being filled, through other
  /// definitions or through its own: the default it is written in can never fill, and the
  /// occurrence is reported where it is written.
  GenericDefault,
  /// An instantiation of a definition whose type argument does not satisfy a bound the definition
  /// writes on that type parameter. The argument, taken without its own `!`, satisfies the bound's
  /// interface type, with the instantiation's arguments in place of the definition's type
  /// parameters, when it is that interface type, or declares it, or is a type parameter bounded by
  /// it or by an interface that declares it; `core`'s `MapKey` is satisfied intrinsically too. Each
  /// occurrence that writes such an instantiation is reported where it is written, once, for the
  /// first of the definition's bounds that fails, with a label at the bound and one at the type
  /// argument, written or filled from a default. The occurrence keeps its type.
  ///
  /// A bound of `MapKey` that a `NonZero` fails is passed over: that `NonZero` holds what is no
  /// integer primitive, and is [`NonZeroArgument`](Self::NonZeroArgument) where it is written. A
  /// directive application's type arguments are not judged against its directive's bounds.
  UnsatisfiedBound,

  // -- Values ------------------------------------------------------------------------------------
  /// A number out of the range of the type expected where it is written. An integer out of the
  /// range of an integer primitive of any width but the 32-bit signed one, or of the primitive a
  /// `NonZero` holds when that is not `i32`; a float, or an integer where a float is expected, out
  /// of `f32`'s range; and an element of a `bytes` list out of a byte's range, 0 to 255. `i32`,
  /// `Int`, `id`'s integers, `NonZero<i32>`, `f64` and `Float` keep GraphQL's own code, the one of
  /// the place the value is written in. Reported at the literal, whose value is unresolved, or for
  /// a `bytes` element at the element, the list being the one unresolved value; the help gives
  /// the type's range as a formula.
  WidthOverflow,
  /// A zero where `core`'s `NonZero` is expected, of any width, in any radix and with either sign,
  /// in a default or in a directive's argument. Reported at the literal, whose value is unresolved.
  NonZeroZero,
  /// A collection literal out of its place. A `set!{…}` is a value of a set alone, and a
  /// `map!{…}` of a map alone: one where a type that is no collection of its shape is expected,
  /// but a custom scalar that is no collection, which keeps any literal, is reported at the
  /// literal. And a set or a map, an extern one among them, takes its own literal and `null`
  /// alone: any other literal is reported at the literal, with the collection's form in the help.
  /// Either is unresolved, and nothing it holds is a value of its own. Or a set literal that holds
  /// one element twice, or a map literal that holds one key twice, two being one when their
  /// coerced values are, `1` and `0x1` one integer and `"4"` and `4` one `id`: reported at the
  /// later occurrence, with a label at the first, where the literal is written; the literal keeps
  /// everything it holds.
  CollectionLiteral,

  // -- smear's rules -----------------------------------------------------------------------------
  /// A package other than the root package writes a `schema` definition or a schema extension. A
  /// dependency is a library, and the schema is the root package's, which alone defines and
  /// extends it. Reported at the item's first keyword, `schema` or `extend`; the item gives the
  /// schema nothing.
  SchemaOutsideRoot,
  /// An extension of a definition the package that writes it may not extend. A package extends
  /// its own definitions alone, so that a definition is the same in every program that holds it:
  /// an extension of another supplied package's type or directive is refused. A definition of
  /// `core`, `alloc` or `std`, a GraphQL built-in the root package replaces among them, is
  /// extended by the root package alone, and only with directives. Reported at the extension's
  /// target path, with a label at the definition's name when the definition has a position.
  ///
  /// Nor may those directives say what the built-in is: in the root package's extension of a
  /// built-in it did not write, each application of `core`'s `@marker`, `@set` or `@map` is
  /// reported too, at the application's path, with the same label. A built-in the root package
  /// replaces is the root package's own, and its extension may. Nor may they bind it to a host
  /// type: each application of `core`'s `@extern` there is reported likewise, a built-in's host
  /// type being given by `@repr(for: …)` on the schema alone.
  OrphanExtension,
  /// An enum that declares a type parameter. An enum's values are names that hold no type, so an
  /// enum takes no type parameter. Reported once, at the enum's first type parameter's name; each
  /// use of the enum is still counted and typed as any generic definition's is.
  GenericEnum,
  /// A marker, an interface `core`'s `@marker` makes one, used as a marker may not be. A marker
  /// has no field: one that declares any is reported once, at the first field its own package
  /// wrote, with a label at the marker's name. A marker is named only as an `implements` entry or
  /// as a bound's interface, the outermost type there: named anywhere else, a type argument and a
  /// list's element included, it is reported where it is named. And a scalar, an enum, an input
  /// object or a union implements markers only: an entry of its `implements` that names what is no
  /// marker, a type parameter among them, is reported where it is written.
  MarkerMisuse,
  /// A scalar whose own package makes it a collection with a count of type parameters the
  /// collection does not have: `core`'s `@set` makes a scalar of one type parameter a set, and
  /// `@map` one of two a map. The first application of `@set` on a scalar that has not exactly
  /// one, or of `@map` on one that has not exactly two, is reported at its path, with a label at
  /// the scalar's name; one that applies both is reported once, at the later of the two, whatever
  /// its count. An application another package writes gives the scalar no meaning, and is not
  /// judged here.
  CollectionArity,
  /// `core`'s `TypeRef` where it may not stand. It is the type of a value that names a
  /// definition, which only a directive's arguments hold: it stands as the type of a directive
  /// definition's argument or of an input field, under lists alone, and nowhere else, a field's
  /// type or argument, a type argument of any generic definition, a collection's included, an
  /// application's type argument, a union member or a default among them. Reported where it is
  /// named.
  TypeRefMisuse,
  /// `core`'s `NonZero` of what is no integer primitive at least one bit wide, written without
  /// `!`. `NonZero` makes an integer never zero, so its type argument is the integer primitive
  /// itself: a float, `string`, an object, a list, `u0`, a type parameter and an integer written
  /// with `!` are each refused. Judged by the occurrence's type, an alias such as `NonZeroU32`
  /// being `NonZero` of the integer it spells, and reported where `NonZero` is written, with a
  /// label at its type argument.
  NonZeroArgument,
  /// A type parameter with the name of an earlier type parameter of its definition, or a name an
  /// extension's own list gives one of its target's type parameters after it gave it to an
  /// earlier one. A type argument binds by position, and the definition, or the extension, names
  /// a parameter by its name, which would name the first of the two alone. Reported at the later
  /// name, with a label at the first of that name.
  DuplicateTypeParameter,
  /// `core`'s `@extern` misused. A language binds a scalar once: an `@extern` whose `lang` an
  /// earlier one its package binds the scalar with has, told by the cooked strings, is reported at
  /// its path, with a label at the earlier. And `@extern` binds a scalar its own package defines:
  /// one the root package writes on a GraphQL built-in its own definition replaces, in that
  /// definition or its extension, is reported at its path, a built-in's host type being given by
  /// `@repr(for: …)` on the schema alone.
  ExternMisuse,

  // -- Resources ---------------------------------------------------------------------------------
  /// A bound of the analysis's limits was passed.
  ResourceLimit,
}

impl DiagnosticKind {
  /// Every kind, in declaration order.
  pub const ALL: &'static [Self] = &[
    Self::DuplicateFilePath,
    Self::InvalidFilePath,
    Self::InvalidPackageName,
    Self::ReservedPackageName,
    Self::DuplicatePackageName,
    Self::MissingDependency,
    Self::InvalidDirectoryName,
    Self::ModuleDefinedTwice,
    Self::ModuleTypeCollision,
    Self::UnknownItem,
    Self::UnknownPackage,
    Self::UnresolvedPath,
    Self::AmbiguousGlob,
    Self::UrlUnsupported,
    Self::FilePathUse,
    Self::UseResolutionChanged,
    Self::UseDeclarationCollision,
    Self::UseNameCollision,
    Self::ShadowedPrimitive,
    Self::InvalidBound,
    Self::GenericArity,
    Self::GenericDefault,
    Self::UnsatisfiedBound,
    Self::WidthOverflow,
    Self::NonZeroZero,
    Self::CollectionLiteral,
    Self::SchemaOutsideRoot,
    Self::OrphanExtension,
    Self::GenericEnum,
    Self::MarkerMisuse,
    Self::CollectionArity,
    Self::TypeRefMisuse,
    Self::NonZeroArgument,
    Self::DuplicateTypeParameter,
    Self::ExternMisuse,
    Self::ResourceLimit,
  ];

  /// The kind's code: `smear::graphqlx::<rule>`.
  #[inline]
  pub const fn code(&self) -> Code {
    match self {
      Self::DuplicateFilePath => Code::new("smear::graphqlx::duplicate-file-path"),
      Self::InvalidFilePath => Code::new("smear::graphqlx::invalid-file-path"),
      Self::InvalidPackageName => Code::new("smear::graphqlx::invalid-package-name"),
      Self::ReservedPackageName => Code::new("smear::graphqlx::reserved-package-name"),
      Self::DuplicatePackageName => Code::new("smear::graphqlx::duplicate-package-name"),
      Self::MissingDependency => Code::new("smear::graphqlx::missing-dependency"),
      Self::InvalidDirectoryName => Code::new("smear::graphqlx::invalid-directory-name"),
      Self::ModuleDefinedTwice => Code::new("smear::graphqlx::module-defined-twice"),
      Self::ModuleTypeCollision => Code::new("smear::graphqlx::module-type-collision"),
      Self::UnknownItem => Code::new("smear::graphqlx::unknown-item"),
      Self::UnknownPackage => Code::new("smear::graphqlx::unknown-package"),
      Self::UnresolvedPath => Code::new("smear::graphqlx::unresolved-path"),
      Self::AmbiguousGlob => Code::new("smear::graphqlx::ambiguous-glob"),
      Self::UrlUnsupported => Code::new("smear::graphqlx::url-unsupported"),
      Self::FilePathUse => Code::new("smear::graphqlx::file-path-use"),
      Self::UseResolutionChanged => Code::new("smear::graphqlx::use-resolution-changed"),
      Self::UseDeclarationCollision => Code::new("smear::graphqlx::use-declaration-collision"),
      Self::UseNameCollision => Code::new("smear::graphqlx::use-name-collision"),
      Self::ShadowedPrimitive => Code::new("smear::graphqlx::shadowed-primitive"),
      Self::InvalidBound => Code::new("smear::graphqlx::invalid-bound"),
      Self::GenericArity => Code::new("smear::graphqlx::generic-arity"),
      Self::GenericDefault => Code::new("smear::graphqlx::generic-default"),
      Self::UnsatisfiedBound => Code::new("smear::graphqlx::unsatisfied-bound"),
      Self::WidthOverflow => Code::new("smear::graphqlx::width-overflow"),
      Self::NonZeroZero => Code::new("smear::graphqlx::nonzero-zero"),
      Self::CollectionLiteral => Code::new("smear::graphqlx::collection-literal"),
      Self::SchemaOutsideRoot => Code::new("smear::graphqlx::schema-outside-root"),
      Self::OrphanExtension => Code::new("smear::graphqlx::orphan-extension"),
      Self::GenericEnum => Code::new("smear::graphqlx::generic-enum"),
      Self::MarkerMisuse => Code::new("smear::graphqlx::marker-misuse"),
      Self::CollectionArity => Code::new("smear::graphqlx::collection-arity"),
      Self::TypeRefMisuse => Code::new("smear::graphqlx::typeref-misuse"),
      Self::NonZeroArgument => Code::new("smear::graphqlx::nonzero-argument"),
      Self::DuplicateTypeParameter => Code::new("smear::graphqlx::duplicate-type-parameter"),
      Self::ExternMisuse => Code::new("smear::graphqlx::extern-misuse"),
      Self::ResourceLimit => Code::new("smear::graphqlx::resource-limit"),
    }
  }

  /// The short phrase the analysis puts beside the kind's primary location.
  pub(crate) const fn title(&self) -> &'static str {
    match self {
      Self::DuplicateFilePath => "duplicate file path",
      Self::InvalidFilePath => "invalid file path",
      Self::InvalidPackageName => "invalid package name",
      Self::ReservedPackageName => "reserved package name",
      Self::DuplicatePackageName => "duplicate package name",
      Self::MissingDependency => "dependency not supplied",
      Self::InvalidDirectoryName => "invalid directory name",
      Self::ModuleDefinedTwice => "module defined twice",
      Self::ModuleTypeCollision => "module and type of one name",
      Self::UnknownItem => "item of an unknown kind",
      Self::UnknownPackage => "unknown package",
      Self::UnresolvedPath => "unresolved path",
      Self::AmbiguousGlob => "name two globs give",
      Self::UrlUnsupported => "URL source not supported",
      Self::FilePathUse => "string source names a file",
      Self::UseResolutionChanged => "use resolved otherwise",
      Self::UseDeclarationCollision => "use and declaration of one name",
      Self::UseNameCollision => "two uses of one name",
      Self::ShadowedPrimitive => "declaration shadows a primitive",
      Self::InvalidBound => "invalid bound",
      Self::GenericArity => "wrong number of type arguments",
      Self::GenericDefault => "type parameter default that can never fill",
      Self::UnsatisfiedBound => "unsatisfied bound",
      Self::WidthOverflow => "out of the type's range",
      Self::NonZeroZero => "zero where `NonZero` is expected",
      Self::CollectionLiteral => "collection literal out of place",
      Self::SchemaOutsideRoot => "schema outside the root package",
      Self::OrphanExtension => "orphan extension",
      Self::GenericEnum => "generic enum",
      Self::MarkerMisuse => "marker misused",
      Self::CollectionArity => "wrong number of a collection's type parameters",
      Self::TypeRefMisuse => "`TypeRef` where it may not stand",
      Self::NonZeroArgument => "invalid `NonZero` argument",
      Self::DuplicateTypeParameter => "duplicate type parameter",
      Self::ExternMisuse => "`@extern` misused",
      Self::ResourceLimit => "resource limit exceeded",
    }
  }

  /// The severity the analysis reports the kind at.
  #[inline]
  pub const fn severity(&self) -> Severity {
    match self {
      Self::DuplicateFilePath
      | Self::InvalidFilePath
      | Self::InvalidPackageName
      | Self::ReservedPackageName
      | Self::DuplicatePackageName
      | Self::MissingDependency
      | Self::InvalidDirectoryName
      | Self::ModuleDefinedTwice
      | Self::ModuleTypeCollision
      | Self::UnknownItem
      | Self::UnknownPackage
      | Self::UnresolvedPath
      | Self::AmbiguousGlob
      | Self::UrlUnsupported
      | Self::FilePathUse
      | Self::UseResolutionChanged
      | Self::UseDeclarationCollision
      | Self::UseNameCollision
      | Self::InvalidBound
      | Self::GenericArity
      | Self::GenericDefault
      | Self::UnsatisfiedBound
      | Self::WidthOverflow
      | Self::NonZeroZero
      | Self::CollectionLiteral
      | Self::SchemaOutsideRoot
      | Self::OrphanExtension
      | Self::GenericEnum
      | Self::MarkerMisuse
      | Self::CollectionArity
      | Self::TypeRefMisuse
      | Self::NonZeroArgument
      | Self::DuplicateTypeParameter
      | Self::ExternMisuse
      | Self::ResourceLimit => Severity::Error,
      Self::ShadowedPrimitive => Severity::Warning,
    }
  }
}

/// The codes of GraphQL's own rules that the analysis reports under `smear-schema`'s codes, since
/// the rule is the same rule in both dialects (K17).
pub(crate) mod schema {
  use tokora::diagnostic::Code;

  /// Two definitions in one module give a type one name: `smear-schema`'s `DuplicateTypeName`.
  pub(crate) const DUPLICATE_TYPE_NAME: Code = Code::new("smear::schema::duplicate-type-name");

  /// Two definitions in one module give a directive one name: `smear-schema`'s
  /// `DuplicateDirectiveDefinition`.
  pub(crate) const DUPLICATE_DIRECTIVE_DEFINITION: Code =
    Code::new("smear::schema::duplicate-directive-definition");

  /// A supplied package's type name begins with `__`: `smear-schema`'s `ReservedTypeName`.
  pub(crate) const RESERVED_TYPE_NAME: Code = Code::new("smear::schema::reserved-type-name");

  /// A supplied package's directive name begins with `__`: `smear-schema`'s
  /// `ReservedDirectiveName`.
  pub(crate) const RESERVED_DIRECTIVE_NAME: Code =
    Code::new("smear::schema::reserved-directive-name");

  /// A type position names, with one segment and no root, no type: `smear-schema`'s
  /// `UndefinedType`.
  pub(crate) const UNDEFINED_TYPE: Code = Code::new("smear::schema::undefined-type");

  /// A directive application names, with one segment and no root, no directive: `smear-schema`'s
  /// `UndefinedDirective`.
  pub(crate) const UNDEFINED_DIRECTIVE: Code = Code::new("smear::schema::undefined-directive");

  /// An extension's target names, with one segment and no root, nothing to extend:
  /// `smear-schema`'s `UndefinedExtensionTarget`.
  pub(crate) const UNDEFINED_EXTENSION_TARGET: Code =
    Code::new("smear::schema::undefined-extension-target");

  /// An `implements` entry names, with one segment and no root, no interface: `smear-schema`'s
  /// `UndefinedImplementsInterface`.
  pub(crate) const UNDEFINED_IMPLEMENTS_INTERFACE: Code =
    Code::new("smear::schema::undefined-implements-interface");

  /// A union member names, with one segment and no root, no type: `smear-schema`'s
  /// `UndefinedUnionMember`.
  pub(crate) const UNDEFINED_UNION_MEMBER: Code =
    Code::new("smear::schema::undefined-union-member");

  /// A root operation names, with one segment and no root, no type: `smear-schema`'s
  /// `UndefinedRootOperationType`.
  pub(crate) const UNDEFINED_ROOT_OPERATION_TYPE: Code =
    Code::new("smear::schema::undefined-root-operation-type");

  /// An extension's keyword is not the kind of the definition its target names: `smear-schema`'s
  /// `ExtensionKindMismatch`.
  pub(crate) const EXTENSION_KIND_MISMATCH: Code =
    Code::new("smear::schema::extension-kind-mismatch");

  /// An application names an argument its directive does not declare: `smear-schema`'s
  /// `UndefinedDirectiveArgument`.
  pub(crate) const UNDEFINED_DIRECTIVE_ARGUMENT: Code =
    Code::new("smear::schema::undefined-directive-argument");

  /// The schema is given a second root for one operation: `smear-schema`'s
  /// `DuplicateRootOperationType`.
  pub(crate) const DUPLICATE_ROOT_OPERATION_TYPE: Code =
    Code::new("smear::schema::duplicate-root-operation-type");

  /// The root package writes a second schema definition: `smear-schema`'s
  /// `DuplicateSchemaDefinition`.
  pub(crate) const DUPLICATE_SCHEMA_DEFINITION: Code =
    Code::new("smear::schema::duplicate-schema-definition");

  /// A member's default does not fit the type the member is declared with: `smear-schema`'s
  /// `InvalidDefaultValue`.
  pub(crate) const INVALID_DEFAULT_VALUE: Code = Code::new("smear::schema::invalid-default-value");

  /// An application's argument does not fit the type its directive declares for it:
  /// `smear-schema`'s `InvalidDirectiveArgumentValue`.
  pub(crate) const INVALID_DIRECTIVE_ARGUMENT_VALUE: Code =
    Code::new("smear::schema::invalid-directive-argument-value");

  /// An application leaves out an argument its directive requires, or passes it `null`:
  /// `smear-schema`'s `MissingRequiredDirectiveArgument`.
  pub(crate) const MISSING_REQUIRED_DIRECTIVE_ARGUMENT: Code =
    Code::new("smear::schema::missing-required-directive-argument");

  /// An object literal names a field its input object does not declare: `smear-schema`'s
  /// `UndefinedInputObjectField`.
  pub(crate) const UNDEFINED_INPUT_OBJECT_FIELD: Code =
    Code::new("smear::schema::undefined-input-object-field");

  /// An object literal leaves out a field its input object requires, or passes it `null`:
  /// `smear-schema`'s `MissingRequiredInputObjectField`.
  pub(crate) const MISSING_REQUIRED_INPUT_OBJECT_FIELD: Code =
    Code::new("smear::schema::missing-required-input-object-field");
}

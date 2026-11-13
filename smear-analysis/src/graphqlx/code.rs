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
  /// target path, with a label at the definition's name when the definition has a position. The
  /// root package's extension that adds more than directives to one of GraphQL's introspection
  /// types, `__Type` and the seven others, is refused the same way under GraphQL's own code,
  /// `smear::schema::introspection-type-extension`, as GraphQL refuses it.
  ///
  /// Nor may those directives say what the built-in is: in the root package's extension of a
  /// built-in it did not write, each application of `core`'s `@marker`, `@set` or `@map` is
  /// reported too, at the application's path, with the same label. A built-in the root package
  /// replaces is the root package's own, and its extension may.
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
    Self::SchemaOutsideRoot,
    Self::OrphanExtension,
    Self::GenericEnum,
    Self::MarkerMisuse,
    Self::CollectionArity,
    Self::TypeRefMisuse,
    Self::NonZeroArgument,
    Self::DuplicateTypeParameter,
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
      Self::SchemaOutsideRoot => Code::new("smear::graphqlx::schema-outside-root"),
      Self::OrphanExtension => Code::new("smear::graphqlx::orphan-extension"),
      Self::GenericEnum => Code::new("smear::graphqlx::generic-enum"),
      Self::MarkerMisuse => Code::new("smear::graphqlx::marker-misuse"),
      Self::CollectionArity => Code::new("smear::graphqlx::collection-arity"),
      Self::TypeRefMisuse => Code::new("smear::graphqlx::typeref-misuse"),
      Self::NonZeroArgument => Code::new("smear::graphqlx::nonzero-argument"),
      Self::DuplicateTypeParameter => Code::new("smear::graphqlx::duplicate-type-parameter"),
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
      Self::SchemaOutsideRoot => "schema outside the root package",
      Self::OrphanExtension => "orphan extension",
      Self::GenericEnum => "generic enum",
      Self::MarkerMisuse => "marker misused",
      Self::CollectionArity => "wrong number of a collection's type parameters",
      Self::TypeRefMisuse => "`TypeRef` where it may not stand",
      Self::NonZeroArgument => "invalid `NonZero` argument",
      Self::DuplicateTypeParameter => "duplicate type parameter",
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
      | Self::SchemaOutsideRoot
      | Self::OrphanExtension
      | Self::GenericEnum
      | Self::MarkerMisuse
      | Self::CollectionArity
      | Self::TypeRefMisuse
      | Self::NonZeroArgument
      | Self::DuplicateTypeParameter
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

  /// A type of the root package's root module is named as one of GraphQL's introspection types:
  /// `smear-schema`'s `RedefinedBuiltInType`.
  pub(crate) const REDEFINED_BUILT_IN_TYPE: Code =
    Code::new("smear::schema::redefined-built-in-type");

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

  /// The root package's extension adds more than directives to an introspection type:
  /// `smear-schema`'s `IntrospectionTypeExtension`.
  pub(crate) const INTROSPECTION_TYPE_EXTENSION: Code =
    Code::new("smear::schema::introspection-type-extension");

  /// A field's name begins with `__`: `smear-schema`'s `ReservedFieldName`.
  pub(crate) const RESERVED_FIELD_NAME: Code = Code::new("smear::schema::reserved-field-name");

  /// A field's argument's name begins with `__`: `smear-schema`'s `ReservedArgumentName`.
  pub(crate) const RESERVED_ARGUMENT_NAME: Code =
    Code::new("smear::schema::reserved-argument-name");

  /// An enum value's name begins with `__`: `smear-schema`'s `ReservedEnumValueName`.
  pub(crate) const RESERVED_ENUM_VALUE_NAME: Code =
    Code::new("smear::schema::reserved-enum-value-name");

  /// An input field's name begins with `__`: `smear-schema`'s `ReservedInputFieldName`.
  pub(crate) const RESERVED_INPUT_FIELD_NAME: Code =
    Code::new("smear::schema::reserved-input-field-name");

  /// A directive definition's argument's name begins with `__`: `smear-schema`'s
  /// `ReservedDirectiveArgumentName`.
  pub(crate) const RESERVED_DIRECTIVE_ARGUMENT_NAME: Code =
    Code::new("smear::schema::reserved-directive-argument-name");

  /// An object's or an interface's field has the name of an earlier field: `smear-schema`'s
  /// `DuplicateFieldName`.
  pub(crate) const DUPLICATE_FIELD_NAME: Code = Code::new("smear::schema::duplicate-field-name");

  /// A field's argument has the name of an earlier argument: `smear-schema`'s
  /// `DuplicateArgumentName`.
  pub(crate) const DUPLICATE_ARGUMENT_NAME: Code =
    Code::new("smear::schema::duplicate-argument-name");

  /// An enum value has the name of an earlier value: `smear-schema`'s `DuplicateEnumValue`.
  pub(crate) const DUPLICATE_ENUM_VALUE: Code = Code::new("smear::schema::duplicate-enum-value");

  /// An input object's field has the name of an earlier field: `smear-schema`'s
  /// `DuplicateInputFieldName`.
  pub(crate) const DUPLICATE_INPUT_FIELD_NAME: Code =
    Code::new("smear::schema::duplicate-input-field-name");

  /// A directive definition's argument has the name of an earlier argument: `smear-schema`'s
  /// `DuplicateDirectiveArgumentName`.
  pub(crate) const DUPLICATE_DIRECTIVE_ARGUMENT_NAME: Code =
    Code::new("smear::schema::duplicate-directive-argument-name");

  /// An object, or an interface that is no marker, holds no field: `smear-schema`'s
  /// `EmptyFieldsDefinition`.
  pub(crate) const EMPTY_FIELDS_DEFINITION: Code =
    Code::new("smear::schema::empty-fields-definition");

  /// A union holds no member: `smear-schema`'s `EmptyUnionMembers`.
  pub(crate) const EMPTY_UNION_MEMBERS: Code = Code::new("smear::schema::empty-union-members");

  /// An enum holds no value: `smear-schema`'s `EmptyEnumValues`.
  pub(crate) const EMPTY_ENUM_VALUES: Code = Code::new("smear::schema::empty-enum-values");

  /// An input object holds no field: `smear-schema`'s `EmptyInputFields`.
  pub(crate) const EMPTY_INPUT_FIELDS: Code = Code::new("smear::schema::empty-input-fields");

  /// A field's type, under its lists, is an input object: `smear-schema`'s
  /// `FieldTypeNotOutputType`.
  pub(crate) const FIELD_TYPE_NOT_OUTPUT_TYPE: Code =
    Code::new("smear::schema::field-type-not-output-type");

  /// A field's argument's type, under its lists, is an object, an interface or a union:
  /// `smear-schema`'s `ArgumentTypeNotInputType`.
  pub(crate) const ARGUMENT_TYPE_NOT_INPUT_TYPE: Code =
    Code::new("smear::schema::argument-type-not-input-type");

  /// An input field's type, under its lists, is an object, an interface or a union:
  /// `smear-schema`'s `InputFieldTypeNotInputType`.
  pub(crate) const INPUT_FIELD_TYPE_NOT_INPUT_TYPE: Code =
    Code::new("smear::schema::input-field-type-not-input-type");

  /// A directive definition's argument's type, under its lists, is an object, an interface or a
  /// union: `smear-schema`'s `DirectiveArgumentTypeNotInputType`.
  pub(crate) const DIRECTIVE_ARGUMENT_TYPE_NOT_INPUT_TYPE: Code =
    Code::new("smear::schema::directive-argument-type-not-input-type");

  /// An object's or an interface's `implements` entry names what is no interface:
  /// `smear-schema`'s `ImplementsNonInterface`.
  pub(crate) const IMPLEMENTS_NON_INTERFACE: Code =
    Code::new("smear::schema::implements-non-interface");

  /// An interface lies on a cycle of `implements` entries: `smear-schema`'s
  /// `SelfImplementingInterface`.
  pub(crate) const SELF_IMPLEMENTING_INTERFACE: Code =
    Code::new("smear::schema::self-implementing-interface");

  /// A union's member names what is no object type: `smear-schema`'s `UnionMemberNotObject`.
  pub(crate) const UNION_MEMBER_NOT_OBJECT: Code =
    Code::new("smear::schema::union-member-not-object");

  /// A required argument, a field's or a directive's, applies `@deprecated`: `smear-schema`'s
  /// `DeprecatedRequiredArgument`.
  pub(crate) const DEPRECATED_REQUIRED_ARGUMENT: Code =
    Code::new("smear::schema::deprecated-required-argument");

  /// A required input field applies `@deprecated`: `smear-schema`'s
  /// `DeprecatedRequiredInputField`.
  pub(crate) const DEPRECATED_REQUIRED_INPUT_FIELD: Code =
    Code::new("smear::schema::deprecated-required-input-field");

  /// A OneOf input object's field is written non-null: `smear-schema`'s `OneOfFieldNotNullable`.
  pub(crate) const ONE_OF_FIELD_NOT_NULLABLE: Code =
    Code::new("smear::schema::one-of-field-not-nullable");

  /// A OneOf input object's field declares a default value: `smear-schema`'s
  /// `OneOfFieldHasDefault`.
  pub(crate) const ONE_OF_FIELD_HAS_DEFAULT: Code =
    Code::new("smear::schema::one-of-field-has-default");

  /// An extension of an input object applies `@oneOf`: `smear-schema`'s
  /// `OneOfOnInputObjectExtension`.
  pub(crate) const ONE_OF_ON_INPUT_OBJECT_EXTENSION: Code =
    Code::new("smear::schema::one-of-on-input-object-extension");

  /// A directive is applied at a location its definition does not list: `smear-schema`'s
  /// `UnsupportedDirectiveLocation`.
  pub(crate) const UNSUPPORTED_DIRECTIVE_LOCATION: Code =
    Code::new("smear::schema::unsupported-directive-location");

  /// A directive that is not repeatable is applied again where it is applied: `smear-schema`'s
  /// `DuplicateDirectiveUse`.
  pub(crate) const DUPLICATE_DIRECTIVE_USE: Code =
    Code::new("smear::schema::duplicate-directive-use");
}

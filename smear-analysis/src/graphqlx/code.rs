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
  /// side, or at the extension's clause, and nothing of it is in the model.
  InvalidBound,

  // -- smear's rules -----------------------------------------------------------------------------
  /// A package other than the root package writes a `schema` definition or a schema extension. A
  /// dependency is a library, and the schema is the root package's, which alone defines and
  /// extends it. Reported at the item's first keyword, `schema` or `extend`; the item gives the
  /// schema nothing.
  SchemaOutsideRoot,

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
    Self::SchemaOutsideRoot,
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
      Self::SchemaOutsideRoot => Code::new("smear::graphqlx::schema-outside-root"),
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
      Self::SchemaOutsideRoot => "schema outside the root package",
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
      | Self::SchemaOutsideRoot
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
}

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
      | Self::ResourceLimit => Severity::Error,
    }
  }
}

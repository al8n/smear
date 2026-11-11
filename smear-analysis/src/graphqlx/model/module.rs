//! Modules: each package's root module, its directory modules, and its inline `mod`s.

/// How a module came to be.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ModuleKind {
  /// A package's root module: `~::` inside the package, and `^::name` from a package that reaches
  /// it.
  Root,
  /// A module a file of a `Directory` package is, by its path: `media/vision.graphqlx` is the
  /// module `~::media::vision`.
  Directory,
  /// A module an inline `mod name { … }` declares.
  Inline,
}

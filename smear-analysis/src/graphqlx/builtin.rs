//! The built-in packages, `core`, `alloc` and `std`: smear's own, numbered before every supplied
//! package, and never supplied, shadowed or replaced by a caller.

use super::package::Tier;

/// One built-in package.
pub(crate) struct BuiltIn {
  /// The package's name, which no supplied package may take.
  pub(crate) name: &'static str,
  /// The tier the package's own definitions require.
  pub(crate) tier: Tier,
  /// The package's embedded source files, as `(path, text)` pairs, in any order: the analysis
  /// numbers them by path, as it numbers a supplied package's files.
  pub(crate) files: &'static [(&'static str, &'static str)],
}

/// The built-in packages, in [`PackageId`](super::PackageId) order.
pub(crate) const BUILT_INS: [BuiltIn; 3] = [
  BuiltIn {
    name: "core",
    tier: Tier::Core,
    files: CORE,
  },
  BuiltIn {
    name: "alloc",
    tier: Tier::Alloc,
    files: ALLOC,
  },
  BuiltIn {
    name: "std",
    tier: Tier::Std,
    files: STD,
  },
];

// THE BUILT-IN LIBRARY'S SOURCE GOES HERE. Each list is one package's embedded GraphQLx files,
// `(path, text)`, read exactly as a supplied package's files are: numbered by path after the
// package's own package source, parsed with the same parser, and answered by `SourceMap::bind`.
// They are empty until the model's definitions arrive; filling them is the whole of the change
// the source map and `bind` need.

/// `core`'s embedded files.
const CORE: &[(&str, &str)] = &[];

/// `alloc`'s embedded files.
const ALLOC: &[(&str, &str)] = &[];

/// `std`'s embedded files.
const STD: &[(&str, &str)] = &[];

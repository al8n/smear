//! The GraphQLx analyzer's name resolution, asked through the one door to its resolver: each path
//! root, the order a module's scope is searched in, the package names last, the type and directive
//! namespaces, the prelude at every tier, primitive widths, and the warning a type that shadows a
//! primitive gets.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, DiagnosticKind, IntegerProvider, Layout, Limits, Package, PackageId,
    SourceFile, SourceId, Tier, analyze, test_support,
  },
  diagnostic::{Diagnose, Location, Severity},
  lexer::tokora::SimpleSpan,
};

/// A package named `name` of the `Directory` layout at `tier`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  tier: Tier,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, Layout::Directory, tier).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// What `path` names in the module `module` of `root` beside `dependencies`, with no generic
/// parameter in scope.
fn named(
  root: &Package<&'static str>,
  dependencies: &[Package<&'static str>],
  module: &str,
  path: &str,
) -> Option<String> {
  test_support::resolve(root, dependencies, module, &[], path)
}

/// Asserts that every path of `cases` names what it says in `module`.
fn all(
  root: &Package<&'static str>,
  dependencies: &[Package<&'static str>],
  module: &str,
  cases: &[(&str, Option<&str>)],
) {
  for &(path, expected) in cases {
    assert_eq!(
      named(root, dependencies, module, path).as_deref(),
      expected,
      "`{path}` in `{module}`"
    );
  }
}

/// The id of the file at `path` in the package `package`.
fn file(analysis: &Analysis, package: u32, path: &str) -> SourceId {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .find(|id: &SourceId| sources.source(*id).path() == path)
    .unwrap_or_else(|| panic!("no file `{path}` in package {package}"))
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  let start = text
    .match_indices(needle)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{needle}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + needle.len()))
}

#[test]
fn each_root_starts_where_it_says() {
  let geo = package(
    "geo",
    Tier::Std,
    &[
      ("lib.graphqlx", "scalar Point\n"),
      ("shapes/c.graphqlx", "scalar Circle\n"),
    ],
  );
  let root = package(
    "app",
    Tier::Std,
    &[
      ("schema.graphqlx", "scalar Top\n"),
      ("media/a.graphqlx", "scalar Image\n"),
      ("media/vision/b.graphqlx", "scalar Frame\n"),
    ],
  )
  .with_dependencies(["geo"]);
  let dependencies = [geo];
  assert!(analyze(&root, &dependencies, &Limits::new()).is_ok());

  all(
    &root,
    &dependencies,
    "^::app::media::vision",
    &[
      ("Frame", Some("^::app::media::vision::Frame")),
      ("~::Top", Some("^::app::Top")),
      ("~::media::Image", Some("^::app::media::Image")),
      (
        "~::media::vision::Frame",
        Some("^::app::media::vision::Frame"),
      ),
      ("..::Image", Some("^::app::media::Image")),
      ("..::..::Top", Some("^::app::Top")),
      ("..::vision::Frame", Some("^::app::media::vision::Frame")),
      ("^::geo::Point", Some("^::geo::Point")),
      ("^::geo::shapes::Circle", Some("^::geo::shapes::Circle")),
      ("geo::shapes::Circle", Some("^::geo::shapes::Circle")),
      ("mod ~::media", Some("mod ^::app::media")),
      ("mod ^::geo", Some("mod ^::geo")),
      (
        "mod ..::..::media::vision",
        Some("mod ^::app::media::vision"),
      ),
      // `..` above the root module names nothing, however many are written.
      ("..::..::..::Top", None),
      ("..::..::..::..::..::..::..::..::..::Top", None),
      // A later segment is looked up among its module's own declarations and child modules.
      ("~::Image", None),
      ("~::media::Frame", None),
      ("^::geo::Circle", None),
    ],
  );
  all(
    &root,
    &dependencies,
    "^::app",
    &[("..::Top", None), ("~::Top", Some("^::app::Top"))],
  );
}

#[test]
fn a_package_reaches_its_declared_dependencies_and_the_built_ins_and_nothing_else() {
  let base = package("base", Tier::Std, &[("lib.graphqlx", "scalar Unit\n")]);
  let geo =
    package("geo", Tier::Std, &[("lib.graphqlx", "scalar Point\n")]).with_dependencies(["base"]);
  let other = package("other", Tier::Std, &[("lib.graphqlx", "scalar Thing\n")]);
  let root =
    package("app", Tier::Std, &[("schema.graphqlx", "scalar Top\n")]).with_dependencies(["geo"]);
  let dependencies = [base, geo, other];
  assert!(analyze(&root, &dependencies, &Limits::new()).is_ok());

  all(
    &root,
    &dependencies,
    "^::app",
    &[
      ("^::geo::Point", Some("^::geo::Point")),
      ("geo::Point", Some("^::geo::Point")),
      // A dependency's dependency.
      ("^::base::Unit", None),
      ("base::Unit", None),
      // A package supplied and not declared.
      ("^::other::Thing", None),
      // The package's own name: it reaches its own items through `~::`.
      ("^::app::Top", None),
      ("app::Top", None),
      ("^::nowhere::Top", None),
      // The built-in packages, declared or not.
      ("^::alloc::string", Some("^::alloc::string")),
      ("std::HashMap", Some("^::std::HashMap")),
      ("mod core", Some("mod ^::core")),
    ],
  );
  all(
    &root,
    &dependencies,
    "^::geo",
    &[
      ("^::base::Unit", Some("^::base::Unit")),
      ("base::Unit", Some("^::base::Unit")),
      ("^::other::Thing", None),
    ],
  );
}

#[test]
fn a_scope_answers_in_its_order_with_package_names_last() {
  let string = package("string", Tier::Std, &[("lib.graphqlx", "scalar Inside\n")]);
  let geo = package("geo", Tier::Std, &[("lib.graphqlx", "scalar Point\n")]);
  let root = package(
    "app",
    Tier::Std,
    &[
      ("schema.graphqlx", "scalar T\nscalar HashMap\n"),
      ("geo/a.graphqlx", "scalar Local\n"),
      ("plain/b.graphqlx", "scalar Other\n"),
    ],
  )
  .with_dependencies(["geo", "string"]);
  let dependencies = [geo, string];
  assert!(analyze(&root, &dependencies, &Limits::new()).is_ok());

  // A generic parameter before the module's own declaration of its name, never a directive, and
  // nothing named through one.
  assert_eq!(
    test_support::resolve(&root, &dependencies, "^::app", &["U", "T"], "T").as_deref(),
    Some("param T")
  );
  assert_eq!(
    test_support::resolve(&root, &dependencies, "^::app", &["T"], "@T"),
    None
  );
  assert_eq!(
    test_support::resolve(&root, &dependencies, "^::app", &["geo"], "geo::Local"),
    None
  );

  all(
    &root,
    &dependencies,
    "^::app",
    &[
      ("T", Some("^::app::T")),
      // The module's own declaration before the prelude, which `^::` still reaches.
      ("HashMap", Some("^::app::HashMap")),
      ("^::std::HashMap", Some("^::std::HashMap")),
      // A child module before a package of its name, which `^::` still reaches.
      ("geo::Local", Some("^::app::geo::Local")),
      ("geo::Point", None),
      ("^::geo::Point", Some("^::geo::Point")),
      // The prelude before a package of its name: `string` is `alloc`'s type, so nothing is named
      // through it.
      ("string::Inside", None),
      ("^::string::Inside", Some("^::string::Inside")),
    ],
  );
  all(
    &root,
    &dependencies,
    "^::app::plain",
    &[
      ("HashMap", Some("^::std::HashMap")),
      // A package's name answers only where a module is wanted.
      ("geo::Point", Some("^::geo::Point")),
      ("geo", None),
      ("mod geo", Some("mod ^::geo")),
      ("mod nowhere", None),
    ],
  );
}

#[test]
fn types_and_modules_share_a_namespace_and_directives_have_their_own() {
  let root = package(
    "app",
    Tier::Std,
    &[
      (
        "schema.graphqlx",
        "scalar flag\ndirective @flag on FIELD_DEFINITION\nscalar Top\n",
      ),
      ("media/a.graphqlx", "scalar Image\n"),
    ],
  );
  assert!(analyze(&root, &[], &Limits::new()).is_ok());

  all(
    &root,
    &[],
    "^::app",
    &[
      ("flag", Some("^::app::flag")),
      ("@flag", Some("@^::app::flag")),
      ("@~::flag", Some("@^::app::flag")),
      // A directive is found only where a directive has the name.
      ("@Top", None),
      ("@media", None),
      // A module is not a type, and a type is not a module.
      ("media", None),
      ("mod media", Some("mod ^::app::media")),
      ("mod Top", None),
      ("Top::Image", None),
      ("media::Image", Some("^::app::media::Image")),
      // The prelude's directives.
      ("@deprecated", Some("@^::core::deprecated")),
      ("@^::core::deprecated", Some("@^::core::deprecated")),
      ("deprecated", None),
    ],
  );
}

#[test]
fn the_prelude_answers_at_every_tier() {
  for tier in [Tier::Core, Tier::Alloc, Tier::Std] {
    let root = Package::new("app", Layout::Flat, tier).with_files([SourceFile::new(
      "schema.graphqlx",
      "type Query { a: Int b: NonZeroU32 }\n",
    )]);
    for (path, expected) in [
      ("string", "^::alloc::string"),
      ("String", "^::alloc::string"),
      ("Int", "^::core::i32"),
      ("i32", "^::core::i32"),
      ("HashMap", "^::std::HashMap"),
      ("NonZeroU32", "^::core::NonZero<^::core::u32>"),
      ("@specifiedBy", "@^::core::specifiedBy"),
    ] {
      assert_eq!(
        named(&root, &[], "^::app", path).as_deref(),
        Some(expected),
        "`{path}` at {tier:?}"
      );
    }
  }
}

#[test]
fn a_width_resolves_only_in_a_package_that_may_have_it() {
  let wide = Package::new("wide", Layout::Directory, Tier::Std)
    .with_files([SourceFile::new("lib.graphqlx", "type W { a: i256 }\n")])
    .with_integers(IntegerProvider::numo());
  let root = package(
    "app",
    Tier::Std,
    &[("schema.graphqlx", "type Q { a: i8 }\n")],
  )
  .with_dependencies(["wide"]);
  let dependencies = [wide];
  assert!(analyze(&root, &dependencies, &Limits::new()).is_ok());

  all(
    &root,
    &dependencies,
    "^::wide",
    &[
      ("i256", Some("^::core::i256")),
      ("^::core::i256", Some("^::core::i256")),
    ],
  );
  // `core` holds `i256` because `wide` writes it, and `app` selects no provider that gives it.
  all(
    &root,
    &dependencies,
    "^::app",
    &[
      ("i256", None),
      ("^::core::i256", None),
      ("i8", Some("^::core::i8")),
    ],
  );
}

#[test]
fn a_type_named_as_a_primitive_shadows_it_in_its_module_with_a_warning() {
  let schema = "scalar Int\nscalar String\ntype Query { a: i32 }\n";
  let shadow = "scalar string\nscalar i32\nscalar NonZero\nscalar Boolean\nscalar NonZeroI16\n\
                directive @bool on FIELD_DEFINITION\n";
  let plain = "type Q { b: NonZeroU8 }\n";
  let root = package(
    "app",
    Tier::Std,
    &[
      ("schema.graphqlx", schema),
      ("shadow/a.graphqlx", shadow),
      ("plain/b.graphqlx", plain),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());

  // The warnings: `string`, `i32` and `NonZeroI16`; not `NonZero` or `Boolean`, which are no
  // primitive's names; not the root module's `Int` and `String`, which replace GraphQL's
  // built-ins; not a directive.
  let source = file(&analysis, 3, "shadow/a.graphqlx");
  let found: Vec<(&str, Location)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect();
  let code = DiagnosticKind::ShadowedPrimitive.code().as_str();
  assert_eq!(
    found,
    [
      (code, at(source, shadow, "string", 0)),
      (code, at(source, shadow, "i32", 0)),
      (code, at(source, shadow, "NonZeroI16", 0)),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.severity(), Severity::Warning);
    assert!(
      diagnostic
        .help()
        .is_some_and(|help: &str| help.contains("^::"))
    );
  }
  assert!(analysis.is_ok());

  all(
    &root,
    &[],
    "^::app::shadow",
    &[
      ("string", Some("^::app::shadow::string")),
      ("i32", Some("^::app::shadow::i32")),
      ("^::core::i32", Some("^::core::i32")),
      ("^::alloc::string", Some("^::alloc::string")),
      // `NonZeroU8` names `core`'s `NonZero` even where `NonZero` is shadowed.
      ("NonZero", Some("^::app::shadow::NonZero")),
      ("NonZeroU8", Some("^::core::NonZero<^::core::u8>")),
    ],
  );
  all(
    &root,
    &[],
    "^::app::plain",
    &[
      ("string", Some("^::alloc::string")),
      ("i32", Some("^::core::i32")),
    ],
  );
  // The root module's replacing definitions keep the built-ins' ids.
  all(
    &root,
    &[],
    "^::app",
    &[
      ("Int", Some("^::core::i32")),
      ("String", Some("^::alloc::string")),
    ],
  );
}

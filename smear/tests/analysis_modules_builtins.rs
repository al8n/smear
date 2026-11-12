//! The GraphQLx analyzer's built-in library: `core`, `alloc` and `std`, one embedded file each,
//! parsed on every analysis exactly as a supplied file is; the built-in definitions a consumer keys
//! on, the second names, the origins and tier tags; the primitives `core` materialises for what a
//! program writes; and the GraphQL built-ins a root package replaces.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Builtins, DefId, DefKind, Diagnostic, IntegerProvider, Layout, Limits, Model,
    ModuleId, Namespace, Origin, Package, PackageId, PackageKind, Primitive, SourceFile, SourceId,
    Tier, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// A package named `name` of `layout` at tier `Std`, holding `files`, `(path, text)`.
fn package(
  name: &str,
  layout: Layout,
  files: &[(&'static str, &'static str)],
) -> Package<&'static str> {
  Package::new(name, layout, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// An analysis of a root package that holds no file: the built-in library alone.
fn library() -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  analyze(&root, &[], &Limits::new())
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

/// Where each diagnostic points, every one of them `smear::schema::undefined-type`.
fn undefined(analysis: &Analysis) -> Vec<Location> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      assert_eq!(
        diagnostic.code().as_str(),
        "smear::schema::undefined-type",
        "{diagnostic:?}"
      );
      diagnostic.primary()
    })
    .collect()
}

/// The definition the canonical name `name` names in `namespace`.
fn def(model: &Model, namespace: Namespace, name: &str) -> DefId {
  model
    .lookup(namespace, name)
    .unwrap_or_else(|| panic!("no definition `{name}`"))
}

/// The names of the definitions `module` holds, in id order.
fn names(model: &Model, module: ModuleId) -> Vec<&str> {
  model
    .module(module)
    .defs()
    .into_iter()
    .map(|id: DefId| model.def(id).name())
    .collect()
}

/// An analysis of a root package that holds no file is an analysis of the built-in library alone,
/// and it reports nothing: every embedded file parses, and each is answered by `bind`.
#[test]
fn the_built_in_library_analyzes_with_no_diagnostic() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  let model = analysis.model();
  let texts = analysis.sources().bind(&root, &[]);
  for (index, name) in ["core", "alloc", "std"].into_iter().enumerate() {
    let package = model.package(PackageId::new(
      u32::try_from(index).expect("three built-in packages"),
    ));
    assert_eq!(package.name(), name);
    assert_eq!(package.kind(), PackageKind::BuiltIn);
    let sources: Vec<SourceId> = package.sources().into_iter().collect();
    assert_eq!(sources.len(), 2, "{name}: its package source and one file");
    let file = analysis.sources().source(sources[1]);
    assert_eq!(file.path(), "lib.graphqlx");
    let text = texts.text(sources[1]).expect("smear's own text");
    assert_eq!(text.len(), file.len());
    assert!(!text.is_empty());
  }
}

/// The three shapes the built-in library writes that no parser test holds exactly, each alone in a
/// file of a root package, so that a failure names its shape: a bare generic scalar before a
/// described definition; enum values spelled as GraphQL's directive locations and type kinds; and
/// an interface with a directive and no fields before a described definition.
#[test]
fn each_shape_the_library_relies_on_parses_alone() {
  let shapes: [(&str, &str, &[&str]); 3] = [
    (
      "generic.graphqlx",
      "\"\"\"\nNever zero.\n\"\"\"\nscalar NonZero<T>\n\n\"Names a definition.\"\n\
       scalar TypeRef\n",
      &["NonZero", "TypeRef"],
    ),
    (
      "keywords.graphqlx",
      "\"Kinds.\"\nenum Kind {\n  \"A scalar.\"\n  SCALAR\n  OBJECT\n  INTERFACE\n  UNION\n  \
       ENUM\n  INPUT_OBJECT\n  LIST\n  NON_NULL\n  QUERY\n  MUTATION\n  SUBSCRIPTION\n  FIELD\n  \
       FRAGMENT_DEFINITION\n  FRAGMENT_SPREAD\n  INLINE_FRAGMENT\n  VARIABLE_DEFINITION\n  \
       SCHEMA\n  FIELD_DEFINITION\n  ARGUMENT_DEFINITION\n  ENUM_VALUE\n  \
       INPUT_FIELD_DEFINITION\n  DIRECTIVE_DEFINITION\n}\n",
      &["Kind"],
    ),
    (
      "bodiless.graphqlx",
      "\"\"\"\nA marker.\n\"\"\"\ninterface MapKey @marker\n\n\"Tiers.\"\nenum Tier {\n  \
       \"No allocator.\"\n  CORE\n}\n\n\"Marks.\"\ndirective @marker on INTERFACE\n",
      &["MapKey", "Tier", "marker"],
    ),
  ];
  for (path, text, declared) in shapes {
    let root = package("app", Layout::Flat, &[(path, text)]);
    let analysis = analyze(&root, &[], &Limits::new());
    let source = file(&analysis, 3, path);
    let here: Vec<String> = analysis
      .diagnostics()
      .iter()
      .filter(|diagnostic: &&Diagnostic| diagnostic.primary().source() == source.get())
      .map(|diagnostic: &Diagnostic| diagnostic.to_string())
      .collect();
    assert!(here.is_empty(), "{path}: {here:#?}");
    let model = analysis.model();
    let root_module = model.package(model.root_package()).root_module();
    assert_eq!(names(model, root_module), declared, "{path}");
  }
}

#[test]
fn every_builtin_is_a_definition_of_its_kind_in_its_package() {
  let analysis = library();
  let model = analysis.model();
  let builtins: Builtins<'_> = model.builtins();
  let expected: [(DefId, &str, DefKind, &str); 17] = [
    (builtins.marker(), "marker", DefKind::Directive, "core"),
    (builtins.set(), "set", DefKind::Directive, "core"),
    (builtins.map(), "map", DefKind::Directive, "core"),
    (builtins.r#extern(), "extern", DefKind::Directive, "core"),
    (builtins.repr(), "repr", DefKind::Directive, "core"),
    (builtins.heap(), "heap", DefKind::Directive, "core"),
    (
      builtins.deprecated(),
      "deprecated",
      DefKind::Directive,
      "core",
    ),
    (
      builtins.specified_by(),
      "specifiedBy",
      DefKind::Directive,
      "core",
    ),
    (builtins.one_of(), "oneOf", DefKind::Directive, "core"),
    (builtins.skip(), "skip", DefKind::Directive, "core"),
    (builtins.include(), "include", DefKind::Directive, "core"),
    (builtins.type_ref(), "TypeRef", DefKind::Scalar, "core"),
    (builtins.map_key(), "MapKey", DefKind::Interface, "core"),
    (builtins.non_zero(), "NonZero", DefKind::Scalar, "core"),
    (builtins.string(), "string", DefKind::Scalar, "alloc"),
    (builtins.bytes(), "bytes", DefKind::Scalar, "alloc"),
    (builtins.id(), "id", DefKind::Scalar, "alloc"),
  ];
  for (id, name, kind, package) in expected {
    let definition = model.def(id);
    assert_eq!(definition.name(), name);
    assert_eq!(definition.kind(), kind, "{name}");
    assert_eq!(definition.origin(), Origin::BuiltIn, "{name}");
    let module = model.module(definition.module());
    assert_eq!(model.package(module.package()).name(), package, "{name}");
    assert!(module.path().is_empty(), "{name}");

    // Its canonical name finds it again, in its own namespace.
    let canonical = model.canonical_name(id).to_string();
    let namespace = if kind == DefKind::Directive {
      Namespace::Directive
    } else {
      Namespace::Type
    };
    assert_eq!(model.lookup(namespace, &canonical), Some(id), "{canonical}");
  }

  // `std` holds the two hashed collections.
  let std_root = model.package(PackageId::new(2)).root_module();
  assert_eq!(names(model, std_root), ["HashMap", "HashSet"]);
}

#[test]
fn each_second_name_finds_its_definition() {
  // `Int` and `Float` name primitives a program has to write; `Boolean` names `bool`, which
  // `core`'s own definitions write.
  let root = package(
    "app",
    Layout::Flat,
    &[("schema.graphqlx", "type Query { a: Int, b: Float }\n")],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  for (alias, target) in [
    ("^::core::Int", "^::core::i32"),
    ("^::core::Float", "^::core::f64"),
    ("^::core::Boolean", "^::core::bool"),
    ("^::alloc::String", "^::alloc::string"),
    ("^::alloc::ID", "^::alloc::id"),
  ] {
    let found = def(model, Namespace::Type, alias);
    assert_eq!(found, def(model, Namespace::Type, target), "{alias}");
    assert_eq!(model.canonical_name(found).to_string(), target);
    // A second name is not the definition's name.
    assert_ne!(Some(model.def(found).name()), alias.rsplit("::").next());
  }

  // With no program, there is no `i32`, and so no second name for it.
  let analysis = library();
  let model = analysis.model();
  assert_eq!(model.lookup(Namespace::Type, "^::core::Int"), None);
  assert_eq!(model.lookup(Namespace::Type, "^::core::i32"), None);
  assert!(model.lookup(Namespace::Type, "^::core::Boolean").is_some());
}

#[test]
fn the_meta_types_are_compat_and_the_tiers_are_tags() {
  let analysis = library();
  let model = analysis.model();
  let meta = [
    "__Schema",
    "__Type",
    "__Field",
    "__InputValue",
    "__EnumValue",
    "__TypeKind",
    "__Directive",
    "__DirectiveLocation",
  ];
  for name in meta {
    let id = def(model, Namespace::Type, &format!("^::core::{name}"));
    assert_eq!(model.def(id).origin(), Origin::Compat, "{name}");
  }

  // Every other definition of the built-in library is `BuiltIn`, and each requires its package's
  // tier, whatever it names: `@extern` takes `string!` arguments and is still `core`'s.
  let mut compat = 0;
  for id in model.defs() {
    let definition = model.def(id);
    let package = model.package(model.module(definition.module()).package());
    assert_eq!(package.kind(), PackageKind::BuiltIn);
    assert_eq!(definition.tier(), package.tier(), "{}", definition.name());
    if definition.origin() == Origin::Compat {
      compat += 1;
    } else {
      assert_eq!(
        definition.origin(),
        Origin::BuiltIn,
        "{}",
        definition.name()
      );
    }
  }
  assert_eq!(compat, meta.len());
  let builtins = model.builtins();
  assert_eq!(model.def(builtins.r#extern()).tier(), Tier::Core);
  assert_eq!(model.def(builtins.string()).tier(), Tier::Alloc);
  let hash_map = def(model, Namespace::Type, "^::std::HashMap");
  assert_eq!(model.def(hash_map).tier(), Tier::Std);
}

/// A program that writes six primitives, one through a second name and one through a `NonZero`
/// name.
const WRITES: &str =
  "type Query {\n  a: u64\n  b: i8\n  c: f32\n  d: Int\n  e: [NonZeroU32!]\n  f: i64\n}\n";

#[test]
fn core_materialises_the_written_primitives_first_in_their_order() {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", WRITES)]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();

  // `bool` too, which the library writes; then `f32`, the signed widths ascending, and the unsigned
  // widths ascending. They come first in `core`'s root module, ahead of `core`'s own definitions.
  let core_root = model.package(PackageId::new(0)).root_module();
  let primitives = [
    ("bool", Primitive::Bool),
    ("f32", Primitive::F32),
    ("i8", Primitive::signed(8)),
    ("i32", Primitive::signed(32)),
    ("i64", Primitive::signed(64)),
    ("u32", Primitive::unsigned(32)),
    ("u64", Primitive::unsigned(64)),
  ];
  let held = names(model, core_root);
  assert_eq!(held[..primitives.len()], primitives.map(|(name, _)| name));
  assert_eq!(held[primitives.len()], "NonZero");

  let whole_core = Location::entire(0);
  let builtins = model.builtins();
  for (index, (name, primitive)) in primitives.into_iter().enumerate() {
    let id = DefId::new(u32::try_from(index).expect("seven primitives"));
    let definition = model.def(id);
    assert_eq!(definition.name(), name);
    assert_eq!(definition.kind(), DefKind::Scalar);
    assert_eq!(definition.intrinsic(), Some(primitive));
    assert_eq!(definition.origin(), Origin::BuiltIn);
    assert_eq!(definition.tier(), Tier::Core);
    assert_eq!(definition.location(), whole_core);
    assert_eq!(definition.description(), None);
    let contributions: Vec<Location> = definition
      .contributions()
      .map(|part| part.location())
      .collect();
    assert_eq!(contributions, [whole_core]);
    assert_eq!(builtins.primitive(primitive), Some(id));
  }

  // A width the program never writes has no definition.
  for unwritten in [
    Primitive::F64,
    Primitive::signed(16),
    Primitive::unsigned(8),
    Primitive::unsigned(16),
  ] {
    assert_eq!(builtins.primitive(unwritten), None, "{unwritten:?}");
  }
  // Only the built-in library's other definitions follow: nothing else is `intrinsic`.
  let intrinsic = model
    .defs()
    .into_iter()
    .filter(|id: &DefId| model.def(*id).intrinsic().is_some())
    .count();
  assert_eq!(intrinsic, primitives.len());
}

#[test]
fn a_width_beyond_cores_own_is_materialised_only_for_a_package_with_a_provider() {
  let wide = "type Wide { a: i256, b: u24, c: NonZeroU7 }\n";
  // Without a provider, the program writes widths it may not have, and none is materialised: each
  // spelling is reported where it is used, as a type the field's place cannot name.
  let root = package("app", Layout::Flat, &[("schema.graphqlx", wide)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    undefined(&analysis),
    ["i256", "u24", "NonZeroU7"].map(|name: &str| at(schema, wide, name, 0))
  );
  let builtins = analysis.model().builtins();
  for width in [
    Primitive::signed(256),
    Primitive::unsigned(24),
    Primitive::unsigned(7),
  ] {
    assert_eq!(builtins.primitive(width), None, "{width:?}");
  }

  // With `numo`, each is.
  let root = package("app", Layout::Flat, &[("schema.graphqlx", wide)])
    .with_integers(IntegerProvider::numo());
  let analysis = analyze(&root, &[], &Limits::new());
  let builtins = analysis.model().builtins();
  for width in [
    Primitive::signed(256),
    Primitive::unsigned(24),
    Primitive::unsigned(7),
  ] {
    assert!(builtins.primitive(width).is_some(), "{width:?}");
  }

  // A width is materialised when one package that may have it writes it, whoever else writes it
  // too; `geo` has `numo`, and the root, which has none, writes another width alone. The root may
  // name neither: only `geo`'s field resolves.
  let narrow = "type Q { a: i256, b: u40 }\n";
  let root =
    package("app", Layout::Flat, &[("schema.graphqlx", narrow)]).with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("geo.graphqlx", "scalar G\ntype P { a: i256 }\n")],
  )
  .with_integers(IntegerProvider::numo());
  let analysis = analyze(&root, &[geo], &Limits::new());
  let schema = file(&analysis, 3, "schema.graphqlx");
  assert_eq!(
    undefined(&analysis),
    [at(schema, narrow, "i256", 0), at(schema, narrow, "u40", 0)]
  );
  let builtins = analysis.model().builtins();
  assert!(builtins.primitive(Primitive::signed(256)).is_some());
  assert_eq!(builtins.primitive(Primitive::unsigned(40)), None);
}

/// A root module replacing two GraphQL scalars and a specified directive, one of them described.
const REPLACES: &str = "\"My Int.\"\nscalar Int @specifiedBy(url: \"https://example.com/int\")\n\
                        \"My deprecation.\"\ndirective @deprecated(reason: String) on FIELD_DEFINITION\n\
                        scalar String\n";

/// The same names in a submodule: ordinary declarations there.
const ELSEWHERE: &str = "scalar Int\ndirective @deprecated on FIELD_DEFINITION\n";

#[test]
fn a_root_module_replaces_graphql_built_ins_and_they_keep_their_ids() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("schema.graphqlx", REPLACES),
      ("sub/other.graphqlx", ELSEWHERE),
    ],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("geo.graphqlx", ELSEWHERE)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let builtins = model.builtins();
  let schema = file(&analysis, 3, "schema.graphqlx");

  // `Int` is `i32`'s, which is materialised although no type position writes it.
  let int = builtins
    .primitive(Primitive::signed(32))
    .expect("a replaced primitive is materialised");
  let definition = model.def(int);
  assert_eq!(definition.name(), "i32");
  assert_eq!(
    definition.module(),
    model.package(PackageId::new(0)).root_module()
  );
  assert_eq!(definition.origin(), Origin::BuiltIn);
  assert_eq!(model.canonical_name(int).to_string(), "^::core::i32");
  assert_eq!(model.lookup(Namespace::Type, "^::core::Int"), Some(int));
  // The second `Int` of the text: the first is in the description.
  let named = at(schema, REPLACES, "Int", 1);
  assert_eq!(definition.location(), named);
  let description = definition.description().expect("the root's description");
  assert_eq!(description.text(), "My Int.");
  let contributions: Vec<Location> = definition
    .contributions()
    .map(|part| part.location())
    .collect();
  assert_eq!(contributions, [named]);

  // `@deprecated` keeps `core`'s id, with the root's location and description.
  let deprecated = model.def(builtins.deprecated());
  assert_eq!(deprecated.name(), "deprecated");
  assert_eq!(deprecated.origin(), Origin::BuiltIn);
  assert_eq!(deprecated.location(), at(schema, REPLACES, "deprecated", 0));
  assert_eq!(
    deprecated.description().map(|text| text.text()),
    Some("My deprecation.")
  );

  // `String` is `string`'s, and the root writes no description, so it has none: the embedded
  // definition contributes nothing.
  let string = model.def(builtins.string());
  assert_eq!(string.name(), "string");
  assert_eq!(string.location(), at(schema, REPLACES, "String", 1));
  assert_eq!(string.description(), None);
  assert_eq!(string.tier(), Tier::Alloc);

  // The root module holds none of them.
  let app_root = model.package(model.root_package()).root_module();
  assert!(model.module(app_root).defs().is_empty());

  // In a submodule, and in a dependency, the same names are ordinary declarations.
  for path in ["^::app::sub", "^::geo"] {
    let module = model.module_by_path(path).expect("the module");
    assert_eq!(names(model, module), ["Int", "deprecated"]);
    for id in model.module(module).defs() {
      assert_eq!(model.def(id).origin(), Origin::User, "{path}");
      assert_ne!(id, int);
      assert_ne!(id, builtins.deprecated());
    }
  }
}

#[test]
fn only_the_first_scalar_of_a_graphql_name_replaces_and_only_a_scalar() {
  let text = "scalar Int\nscalar Int\ntype Float { x: i8 }\n";
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
  let analysis = analyze(&root, &[], &Limits::new());
  let model = analysis.model();
  let schema = file(&analysis, 3, "schema.graphqlx");

  // The second `Int` is a second declaration of the name, which binds the first, the replacement.
  let diagnostics: Vec<(&str, Location, Option<Location>)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect();
  assert_eq!(
    diagnostics,
    [(
      "smear::schema::duplicate-type-name",
      at(schema, text, "Int", 1),
      Some(at(schema, text, "Int", 0))
    )]
  );

  // It keeps an id of its own, in the root module, beside the object named `Float`, which is no
  // scalar and so replaces nothing: `f64` is not materialised.
  let app_root = model.package(model.root_package()).root_module();
  assert_eq!(names(model, app_root), ["Int", "Float"]);
  let int = model
    .builtins()
    .primitive(Primitive::signed(32))
    .expect("the first `Int` replaces `i32`");
  assert_eq!(model.def(int).location(), at(schema, text, "Int", 0));
  assert_eq!(model.builtins().primitive(Primitive::F64), None);
}

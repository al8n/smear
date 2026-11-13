//! Where each entity of a model is written: every contribution, member and inline module answers
//! its extent; only what has no text of its own answers an extent with no span; an extent is in
//! its own file; and replacing an extent's text changes that entity alone. Through the lossless
//! layer, every extent names one node of its file's tree, of its entity's kind, from the extent's
//! start, and no definition or member keeps a recovery hole; and a syntax error marks nothing, a
//! definition the parser read past a mistake included.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use core::ops::Range;

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, DefId, IdRange, Layout, Limits, MemberId, Model, ModuleKind, Namespace,
    Package, PackageId, Primitive, SourceFile, SourceId, Tier, analyze,
  },
  diagnostic::Location,
  lexer::tokora::SimpleSpan,
};

#[allow(dead_code)]
#[path = "support/analysis_view.rs"]
mod view;

use view::{APP, ASTRO, GEO, NESTED, package, render};

/// `analysis_modules_builtins.rs`'s program that writes six primitives, one through a second name
/// and one through a `NonZero` name.
const WRITES: &str =
  "type Query {\n  a: u64\n  b: i8\n  c: f32\n  d: Int\n  e: [NonZeroU32!]\n  f: i64\n}\n";

/// `analysis_modules_builtins.rs`'s root module replacing two GraphQL scalars and a specified
/// directive, one of them described.
const REPLACES: &str = "\"My Int.\"\nscalar Int @specifiedBy(url: \"https://example.com/int\")\n\
                        \"My deprecation.\"\ndirective @deprecated(reason: String) on FIELD_DEFINITION\n\
                        scalar String\n";

/// `analysis_modules_builtins.rs`'s same names in a submodule: ordinary declarations there.
const ELSEWHERE: &str = "scalar Int\ndirective @deprecated on FIELD_DEFINITION\n";

/// One file of a program: its package's name, its path and its text.
#[derive(Clone)]
struct File {
  package: &'static str,
  path: &'static str,
  text: String,
}

/// The program every kind of entity is written in: `analysis_lower.rs`'s root package of three
/// files beside its two dependencies, with `analysis_modules_tree.rs`'s described `mod` holding
/// another as a fourth file of the root.
fn program() -> Vec<File> {
  let mut files: Vec<File> = Vec::new();
  for (package, written) in [("app", &APP[..]), ("geo", &GEO[..]), ("astro", &ASTRO[..])] {
    for &(path, text) in written {
      files.push(File {
        package,
        path,
        text: String::from(text),
      });
    }
  }
  files.push(File {
    package: "app",
    path: "n.graphqlx",
    text: String::from(NESTED),
  });
  files
}

/// The package `name` of `layout` at tier `Std`, holding those of `files` that are its own.
fn package_of<'f>(files: &'f [File], name: &str, layout: Layout) -> Package<&'f str> {
  let mut own: Vec<SourceFile<&'f str>> = Vec::new();
  for file in files {
    if file.package == name {
      own.push(SourceFile::new(file.path, file.text.as_str()));
    }
  }
  Package::new(name, layout, Tier::Std).with_files(own)
}

/// An analysis of `files`: the root package `app`, `Directory`, beside its dependencies `geo` and
/// `astro`, each `Flat`.
fn analyzed(files: &[File]) -> Analysis {
  let root = package_of(files, "app", Layout::Directory).with_dependencies(["geo", "astro"]);
  let dependencies = [
    package_of(files, "geo", Layout::Flat),
    package_of(files, "astro", Layout::Flat),
  ];
  analyze(&root, &dependencies, &Limits::new())
}

/// The text of the file `path` of the package `package` in `files`.
fn text_of<'f>(files: &'f [File], package: &str, path: &str) -> &'f str {
  for file in files {
    if file.package == package && file.path == path {
      return file.text.as_str();
    }
  }
  panic!("no file `{path}` in `{package}`")
}

/// The id of the file `path` of the package named `package`.
fn file(analysis: &Analysis, package: &str, path: &str) -> SourceId {
  let model = analysis.model();
  let sources = analysis.sources();
  for id in model.packages() {
    if model.package(id).name() != package {
      continue;
    }
    for source in model.package(id).sources() {
      if sources.source(source).path() == path {
        return source;
      }
    }
  }
  panic!("no file `{path}` in `{package}`")
}

/// Where an entity is written in `text`, the file `source`: from the first place `first` is
/// written, to the end of the first `last` written from there on.
fn spanning(source: SourceId, text: &str, first: &str, last: &str) -> Location {
  let start = text
    .find(first)
    .unwrap_or_else(|| panic!("`{first}` is not written"));
  let end = text[start..]
    .find(last)
    .map(|at: usize| start + at + last.len())
    .unwrap_or_else(|| panic!("`{last}` is not written after `{first}`"));
  Location::new(source.get(), SimpleSpan::new(start, end))
}

/// The definition the canonical name `name` names in `namespace`.
fn def(model: &Model, namespace: Namespace, name: &str) -> DefId {
  model
    .lookup(namespace, name)
    .unwrap_or_else(|| panic!("no definition `{name}`"))
}

/// The member of `range` named `name`.
fn member_named(model: &Model, range: IdRange<MemberId>, name: &str) -> MemberId {
  for id in range {
    if model.member(id).name() == name {
      return id;
    }
  }
  panic!("no member `{name}`")
}

/// The first contribution of `def` that is an extension, or that is none: its own.
fn contribution(model: &Model, def: DefId, extension: bool) -> Contribution<'_> {
  model
    .def(def)
    .contributions()
    .find(|part: &Contribution<'_>| part.is_extension() == extension)
    .unwrap_or_else(|| panic!("`{}` has no such contribution", model.def(def).name()))
}

/// Every kind of entity answers the range `str::find` gives for its first and last token: a
/// definition from its description, and one with none from its keyword; an extension from its
/// `extend`; a schema definition from its description, and a schema extension from its
/// `extend`; a field from its description; a field's argument, an input field, an enum value and
/// a directive's argument; and an inline module from its description, and one nested in it from
/// its `mod`, each to its `}`.
#[test]
fn every_kind_of_entity_answers_its_extent() {
  let files = program();
  let analysis = analyzed(&files);
  assert!(
    analysis.is_ok() && analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let b = file(&analysis, "app", "b.graphqlx");
  let inner = file(&analysis, "app", "a/inner.graphqlx");
  let z = file(&analysis, "app", "z.graphqlx");
  let n = file(&analysis, "app", "n.graphqlx");
  let b_text = text_of(&files, "app", "b.graphqlx");
  let inner_text = text_of(&files, "app", "a/inner.graphqlx");
  let z_text = text_of(&files, "app", "z.graphqlx");
  let n_text = text_of(&files, "app", "n.graphqlx");

  let query = def(model, Namespace::Type, "^::app::Query");
  assert_eq!(
    contribution(model, query, false).extent(),
    spanning(b, b_text, "\"The root.\"", "[Node!]!\n}"),
    "a definition, from its description"
  );
  let node = def(model, Namespace::Type, "^::app::Node");
  assert_eq!(
    contribution(model, node, false).extent(),
    spanning(b, b_text, "interface Node", "id: ID\n}"),
    "a definition with no description, from its keyword"
  );
  assert_eq!(
    contribution(model, query, true).extent(),
    spanning(z, z_text, "extend type Query", "(level: 4)\n}"),
    "an extension, from its `extend`"
  );
  let late = def(model, Namespace::Type, "^::app::Late");
  assert_eq!(
    contribution(model, late, false).extent(),
    spanning(z, z_text, "scalar Late", "Late"),
    "a scalar, to its name"
  );
  let color = def(model, Namespace::Type, "^::app::a::Color");
  assert_eq!(
    contribution(model, color, true).extent(),
    spanning(inner, inner_text, "extend enum Color", "BLUE\n}"),
    "an enum's extension"
  );

  let schema: Vec<Contribution<'_>> = model
    .schema()
    .contributions()
    .collect::<Vec<Contribution<'_>>>();
  let definition = schema
    .iter()
    .find(|part: &&Contribution<'_>| !part.is_extension())
    .expect("the schema definition");
  assert_eq!(
    definition.extent(),
    spanning(b, b_text, "\"The schema.\"", "query: Query\n}"),
    "a schema definition, from its description"
  );
  let extension = schema
    .iter()
    .find(|part: &&Contribution<'_>| part.is_extension() && part.extent().source() == inner.get())
    .expect("the schema extension of `a/inner.graphqlx`");
  assert_eq!(
    extension.extent(),
    spanning(inner, inner_text, "extend schema", "mutation: Change\n}"),
    "a schema extension, from its `extend`"
  );

  let fields = model.def(query).fields();
  let nodes = member_named(model, fields, "nodes");
  assert_eq!(
    model.member(nodes).extent(),
    spanning(b, b_text, "\"Listed.\"", "[Node!]!"),
    "a field, from its description"
  );
  let node_field = member_named(model, fields, "node");
  let arguments = model.member(node_field).arguments();
  assert_eq!(
    model.member(member_named(model, arguments, "id")).extent(),
    spanning(b, b_text, "id: ID", "ID"),
    "a field's argument"
  );
  assert_eq!(
    model
      .member(member_named(model, arguments, "depth"))
      .extent(),
    spanning(b, b_text, "depth", "= 1"),
    "a field's argument, to its default"
  );
  let filter = def(model, Namespace::Type, "^::app::a::Filter");
  assert_eq!(
    model
      .member(member_named(
        model,
        model.def(filter).input_fields(),
        "color"
      ))
      .extent(),
    spanning(inner, inner_text, "color: Color", "Color"),
    "an input field"
  );
  assert_eq!(
    model
      .member(member_named(model, model.def(color).enum_values(), "RED"))
      .extent(),
    spanning(inner, inner_text, "RED", "RED"),
    "an enum value"
  );
  let mark = def(model, Namespace::Directive, "@^::app::a::mark");
  assert_eq!(
    model
      .member(member_named(model, model.def(mark).arguments(), "level"))
      .extent(),
    spanning(inner, inner_text, "level: Int", "Int"),
    "a directive's argument"
  );

  let outer = model
    .module_by_path("^::app::outer")
    .expect("the described `mod`");
  assert_eq!(
    model.module(outer).extent(),
    Some(spanning(n, n_text, "\"The outer module.\"", "  }\n}")),
    "an inline module, from its description"
  );
  let nested = model
    .module_by_path("^::app::outer::inner")
    .expect("the nested `mod`");
  assert_eq!(
    model.module(nested).extent(),
    Some(spanning(n, n_text, "mod inner", "}")),
    "an inline module nested in another, from its `mod`"
  );
}

/// What has no text of its own answers no span, and nothing else does. A primitive `core`
/// materialises for a type position and no definition of the root package's replaces has one
/// contribution, and its extent is the whole of `core`'s package source, as its location is. A
/// root module and a directory module have no extent. A module two `mod`s declare has the extent
/// of the first in source order.
#[test]
fn what_has_no_text_of_its_own_answers_no_span() {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", WRITES)]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let core = model
    .package(PackageId::new(0))
    .sources()
    .into_iter()
    .next()
    .expect("`core`'s package source");
  let whole = Location::entire(core.get());
  let materialised = model
    .builtins()
    .primitive(Primitive::unsigned(64))
    .expect("`u64` is written, so it is materialised");
  let extents: Vec<Location> = model
    .def(materialised)
    .contributions()
    .map(|part: Contribution<'_>| part.extent())
    .collect::<Vec<Location>>();
  assert_eq!(extents, [whole], "a materialised primitive's extent");
  assert_eq!(model.def(materialised).location(), whole);

  let files = program();
  let analysis = analyzed(&files);
  let model = analysis.model();
  let app = model.package(model.root_package()).root_module();
  assert_eq!(model.module(app).extent(), None, "a root module");
  let directory = model
    .module_by_path("^::app::a")
    .expect("the directory `a`");
  assert_eq!(model.module(directory).kind(), ModuleKind::Directory);
  assert_eq!(model.module(directory).extent(), None, "a directory module");

  // `analysis_modules_tree.rs`'s `mod` in two files of one directory, passed in reverse: the
  // first in source order is the first by path.
  let first = "mod shared {\n  scalar A\n}\n";
  let second = "mod shared {\n  scalar B\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[("x/b.graphqlx", second), ("x/a.graphqlx", first)],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, "app", "x/a.graphqlx");
  let shared = analysis
    .model()
    .module_by_path("^::app::x::shared")
    .expect("one module");
  assert_eq!(
    analysis.model().module(shared).extent(),
    Some(spanning(a, first, "mod shared", "}")),
    "a module two `mod`s declare"
  );
}

/// A contribution's extent is in its own file: an extension in another file than its definition
/// answers that file, and so do the members it contributes. A primitive the root package's
/// `scalar Int` replaces answers the extent of that definition, in the root's file, from its
/// description to its last token.
#[test]
fn a_contribution_answers_its_own_files_extent() {
  let files = program();
  let analysis = analyzed(&files);
  let model = analysis.model();
  let b = file(&analysis, "app", "b.graphqlx");
  let z = file(&analysis, "app", "z.graphqlx");
  let query = def(model, Namespace::Type, "^::app::Query");
  let sources: Vec<(bool, u32)> = model
    .def(query)
    .contributions()
    .map(|part: Contribution<'_>| (part.is_extension(), part.extent().source()))
    .collect::<Vec<(bool, u32)>>();
  assert_eq!(sources, [(false, b.get()), (true, z.get())]);
  let more = member_named(model, model.def(query).fields(), "more");
  assert_eq!(
    model.member(more).extent().source(),
    z.get(),
    "a member an extension contributed"
  );

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
  let schema = file(&analysis, "app", "schema.graphqlx");
  let int = model
    .builtins()
    .primitive(Primitive::signed(32))
    .expect("a replaced primitive is materialised");
  let extents: Vec<Location> = model
    .def(int)
    .contributions()
    .map(|part: Contribution<'_>| part.extent())
    .collect::<Vec<Location>>();
  assert_eq!(
    extents,
    [spanning(schema, REPLACES, "\"My Int.\"", "/int\")")],
    "`i32`, which the root package's `scalar Int` replaces"
  );
}

/// Whether `at` lies outside `range` of `source`: in another source, with no span, or with no byte
/// in the range. `None`, the extent of a module no `mod` declares, lies outside every range.
fn outside(at: Option<Location>, source: SourceId, range: &Range<usize>) -> bool {
  let Some(at) = at else {
    return true;
  };
  match at.span() {
    Some(span) if at.source() == source.get() => {
      span.end() <= range.start || range.end <= span.start()
    }
    _ => true,
  }
}

/// Whether `at` lies inside `range` of `source`.
fn inside(at: Option<Location>, source: SourceId, range: &Range<usize>) -> bool {
  match at {
    Some(at) if at.source() == source.get() => at
      .span()
      .is_some_and(|span: SimpleSpan| range.start <= span.start() && span.end() <= range.end),
    _ => false,
  }
}

/// Writes `now` over the bytes of `extent` in `files`, which `before` was made from, and holds what
/// that makes to what an extent promises: those bytes spell `was`; the text with `now`, a spelling
/// of the same length, in their place analyzes with no diagnostic; and of the two renderings, every
/// line of an entity outside the replaced range is unchanged, and at least one inside it changed.
fn replace(files: &[File], before: &Analysis, extent: Location, was: &str, now: &str) {
  assert_eq!(
    was.len(),
    now.len(),
    "{was:?} and {now:?} are no spellings of one length"
  );
  let source = SourceId::new(extent.source());
  let written = before.sources().source(source);
  let package = before.model().package(written.package()).name();
  let span = extent
    .span()
    .unwrap_or_else(|| panic!("{was:?}: the extent has no span"));
  let range: Range<usize> = span.start()..span.end();
  let mut edited: Vec<File> = files.to_vec();
  let mut found = false;
  for file in &mut edited {
    if file.package == package && file.path == written.path() {
      assert_eq!(
        &file.text[range.clone()],
        was,
        "the extent spells another text"
      );
      file.text.replace_range(range.clone(), now);
      found = true;
    }
  }
  assert!(found, "{was:?}: no file holds {extent:?}");
  let after = analyzed(&edited);
  assert!(
    after.is_ok() && after.diagnostics().is_empty(),
    "{was:?} written {now:?}: {:#?}",
    after.diagnostics()
  );
  let (old, new) = (render(before.model()), render(after.model()));
  assert_eq!(
    old.len(),
    new.len(),
    "{was:?} written {now:?} renumbered the model"
  );
  let mut changed = 0usize;
  for (one, two) in old.iter().zip(new.iter()) {
    if outside(one.at, source, &range) {
      assert_eq!(
        one.text, two.text,
        "{was:?} written {now:?} changed a line outside it"
      );
    } else if inside(one.at, source, &range) && one.text != two.text {
      changed += 1;
    }
  }
  assert!(
    changed >= 1,
    "{was:?} written {now:?} changed no line inside it"
  );
}

/// T13: replacing the text of an extent with another spelling of the same length changes only that
/// entity, for a field, an enum value, a definition nothing names and an inline module.
///
/// The rendering is keyed by id, and ids are numbered by module path and then by position, so no
/// replacement here may renumber: the inline module has no sibling for its new name to sort
/// around, and a definition's or a member's new name moves no id.
#[test]
fn replacing_an_extent_changes_only_its_entity() {
  let files = program();
  let before = analyzed(&files);
  assert!(
    before.is_ok() && before.diagnostics().is_empty(),
    "{:#?}",
    before.diagnostics()
  );
  let model = before.model();
  let query = def(model, Namespace::Type, "^::app::Query");
  let nodes = member_named(model, model.def(query).fields(), "nodes");
  let color = def(model, Namespace::Type, "^::app::a::Color");
  let red = member_named(model, model.def(color).enum_values(), "RED");
  let late = def(model, Namespace::Type, "^::app::Late");
  let inner = model
    .module_by_path("^::app::outer::inner")
    .expect("the nested `mod`");
  let replacements = [
    (
      model.member(nodes).extent(),
      "\"Listed.\"\n  nodes: [Node!]!",
      "\"Picked.\"\n  notes: [Node!]!",
    ),
    (model.member(red).extent(), "RED", "TAN"),
    (
      contribution(model, late, false).extent(),
      "scalar Late",
      "scalar Tale",
    ),
    (
      model
        .module(inner)
        .extent()
        .expect("an inline module has an extent"),
      "mod inner {\n    scalar X\n  }",
      "mod inane {\n    scalar X\n  }",
    ),
  ];
  for (extent, was, now) in replacements {
    replace(&files, &before, extent, was, now);
  }
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use std::collections::BTreeMap;

  use smear::{
    analysis::graphqlx::{
      Analysis, DefId, DefKind, Diagnostic, Layout, Limits, MemberId, MemberKind, Model, Namespace,
      Package, PackageKind, SourceFile, SourceId, Tier, Trees, analyze_with,
    },
    diagnostic::{Diagnose, Location, Severity},
    lexer::tokora::SimpleSpan,
    parser::graphqlx::{
      Lossless,
      kinds::SyntaxKind as K,
      lossless::{Parse, SyntaxElement, SyntaxNode},
    },
  };

  use super::{
    contribution, def,
    view::{blank, corpus, sets},
  };

  /// The root package `app`, `Flat`, of the one file `schema.graphqlx`, holding `text`.
  fn one_file(text: &str) -> Package<&str> {
    Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text)])
  }

  /// Every member of `def`: its direct members, then each field's arguments.
  fn members_of(model: &Model, def: DefId) -> Vec<MemberId> {
    let held = model.def(def);
    let direct: Vec<MemberId> = held
      .fields()
      .into_iter()
      .chain(held.input_fields())
      .chain(held.enum_values())
      .chain(held.arguments())
      .collect::<Vec<MemberId>>();
    let mut all: Vec<MemberId> = direct.clone();
    for member in &direct {
      all.extend(model.member(*member).arguments());
    }
    all
  }

  /// The node a contribution of a definition of `kind` is written as.
  fn written_as(kind: DefKind, extension: bool) -> K {
    match (kind, extension) {
      (DefKind::Scalar, false) => K::ScalarTypeDefinition,
      (DefKind::Scalar, true) => K::ScalarTypeExtension,
      (DefKind::Object, false) => K::ObjectTypeDefinition,
      (DefKind::Object, true) => K::ObjectTypeExtension,
      (DefKind::Interface, false) => K::InterfaceTypeDefinition,
      (DefKind::Interface, true) => K::InterfaceTypeExtension,
      (DefKind::Union, false) => K::UnionTypeDefinition,
      (DefKind::Union, true) => K::UnionTypeExtension,
      (DefKind::Enum, false) => K::EnumTypeDefinition,
      (DefKind::Enum, true) => K::EnumTypeExtension,
      (DefKind::InputObject, false) => K::InputObjectTypeDefinition,
      (DefKind::InputObject, true) => K::InputObjectTypeExtension,
      (DefKind::Directive, false) => K::DirectiveDefinition,
      (DefKind::Directive, true) => K::DirectiveExtension,
      (other, _) => panic!("a definition of a kind this sweep does not know: {other:?}"),
    }
  }

  /// The node a member of `kind` is written as.
  fn member_written_as(kind: MemberKind) -> K {
    match kind {
      MemberKind::Field => K::FieldDefinition,
      MemberKind::Argument | MemberKind::InputField => K::InputValueDefinition,
      MemberKind::EnumValue => K::EnumValueDefinition,
      other => panic!("a member of a kind this sweep does not know: {other:?}"),
    }
  }

  /// One extent the sweep held to its node: what the entity is, for a failure; the kind of node it
  /// is written as; the extent's span; and the node `Trees::node` answers for the extent.
  struct Reached {
    what: String,
    kind: K,
    span: SimpleSpan,
    node: SyntaxNode,
  }

  /// The extents of `analysis`, named `name`, each held to its node in `trees` and appended to
  /// `all`: each contribution of each definition and of the schema, each member, and each module a
  /// `mod` declares. An extent whose source has no tree is a built-in package's, and
  /// `Trees::node` answers it nothing; every other has a span, and a node.
  fn reach(name: &str, analysis: &Analysis, trees: &Trees<Parse>, all: &mut Vec<Reached>) {
    let model = analysis.model();
    let mut extents: Vec<(String, K, Location)> = Vec::new();
    for id in model.defs() {
      let held = model.def(id);
      for (index, part) in held.contributions().enumerate() {
        extents.push((
          format!("{name}: `{}`'s contribution {index}", held.name()),
          written_as(held.kind(), part.is_extension()),
          part.extent(),
        ));
      }
      for member in members_of(model, id) {
        let written = model.member(member);
        extents.push((
          format!("{name}: `{}`'s member `{}`", held.name(), written.name()),
          member_written_as(written.kind()),
          written.extent(),
        ));
      }
    }
    for (index, part) in model.schema().contributions().enumerate() {
      let kind = if part.is_extension() {
        K::SchemaExtension
      } else {
        K::SchemaDefinition
      };
      extents.push((
        format!("{name}: the schema's contribution {index}"),
        kind,
        part.extent(),
      ));
    }
    for id in model.modules() {
      if let Some(extent) = model.module(id).extent() {
        extents.push((
          format!("{name}: module {}", id.get()),
          K::ModDefinition,
          extent,
        ));
      }
    }
    for (what, kind, extent) in extents {
      let source = SourceId::new(extent.source());
      match (extent.span(), trees.get(source)) {
        (Some(span), Some(_)) => {
          let node = trees
            .node(extent)
            .unwrap_or_else(|| panic!("{what}: no node covers {extent:?}"));
          all.push(Reached {
            what,
            kind,
            span,
            node,
          });
        }
        (_, None) => {
          let package = analysis.sources().source(source).package();
          assert_eq!(
            model.package(package).kind(),
            PackageKind::BuiltIn,
            "{what}: {extent:?} is in a supplied file the layer kept no tree of"
          );
          assert!(
            trees.node(extent).is_none(),
            "{what}: a node for {extent:?}, in a file with no tree"
          );
        }
        (None, Some(_)) => {
          panic!("{what}: {extent:?} has no span, in a file the layer kept a tree of")
        }
      }
    }
  }

  /// Every extent the analyses through the lossless layer answer, of the package sets written by
  /// hand and of every corpus entry, valid and invalid, as the one file of a package, each held
  /// to its node.
  fn reached() -> Vec<Reached> {
    let mut all: Vec<Reached> = Vec::new();
    for set in sets() {
      let (analysis, trees) = analyze_with(
        &Lossless::new(),
        &set.root,
        &set.dependencies,
        &Limits::new(),
      );
      reach(set.name, &analysis, &trees, &mut all);
    }
    for (name, text) in corpus("") {
      let root = one_file(text.as_str());
      let (analysis, trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
      reach(&name, &analysis, &trees, &mut all);
    }
    all
  }

  /// For each kind of node the sweep reaches, the fewest extents of it the sweep must hold to a
  /// node: how many the package sets written by hand hold, every one in a supplied file that is
  /// not blank, and so in a file the lossless layer keeps a tree of. The corpus adds to each.
  /// `sets` documents the sets; counted there:
  ///
  /// - object types: `Query`, `Change`, `Point`, `Line` and `Star` in the first set, `T` in the
  ///   fourth, `Query` in the fifth, `Query` and `Point` in the sixth, `Point` in the seventh;
  /// - scalars: `Late`; `X` and `Y`; `Real`; `Kept`, `X` twice and `same`;
  /// - directives: `mark`; `d` twice and `same`;
  /// - object type extensions: `Query`'s in the first set, `Point`'s in the seventh;
  /// - schema definitions: one in the first set, two in the fifth, one in the sixth;
  /// - schema extensions: the first set's two;
  /// - fields: ten in the first set (`node`, `nodes`, `id`, `done`, `more`, `x`, `y`, `head`,
  ///   `tail`, `name`), two in the fourth, one in the fifth, two in the sixth, two in the seventh;
  /// - input values: the first set's `id`, `depth`, `color`, `value`, `level` and `first`;
  /// - enum values: `RED`, `GREEN` and `BLUE`;
  /// - modules: `outer`, `inner` and `other`.
  ///
  /// No set written by hand holds a union or an extension of another kind, so those kinds have no
  /// floor; the sweep prints what it held of each.
  const FLOORS: [(K, usize); 14] = [
    (K::ObjectTypeDefinition, 10),
    (K::InterfaceTypeDefinition, 1),
    (K::EnumTypeDefinition, 1),
    (K::InputObjectTypeDefinition, 1),
    (K::DirectiveDefinition, 4),
    (K::ScalarTypeDefinition, 8),
    (K::ObjectTypeExtension, 2),
    (K::EnumTypeExtension, 1),
    (K::SchemaDefinition, 4),
    (K::SchemaExtension, 2),
    (K::FieldDefinition, 17),
    (K::InputValueDefinition, 6),
    (K::EnumValueDefinition, 3),
    (K::ModDefinition, 3),
  ];

  /// Prints how many extents of each kind `all` holds, and holds each count to its floor.
  fn held_to_the_floors(all: &[Reached], what: &str) {
    let mut counts: BTreeMap<K, usize> = BTreeMap::new();
    for entity in all {
      *counts.entry(entity.kind).or_default() += 1;
    }
    println!("{what}: {counts:?}");
    for (kind, floor) in FLOORS {
      let count = counts.get(&kind).copied().unwrap_or(0);
      assert!(
        count >= floor,
        "{what}: {count} extents of {kind:?}, under the floor of {floor}"
      );
    }
  }

  /// T13: every extent names one node of its file's tree.
  ///
  /// Through the lossless layer, over the package sets written by hand and every corpus entry,
  /// valid and invalid: for every contribution, member and module whose extent has a span in a
  /// file with a tree, `Trees::node` answers a node; the node is of the kind the entity is written
  /// as; it starts where the extent starts, leading trivia lying outside it; and between the
  /// extent's end and the node's end there is nothing but what the lexer skips. An extent in a
  /// file with no tree is a built-in package's, and has no node.
  #[test]
  fn every_extent_names_one_node_of_its_files_tree() {
    let all = reached();
    for entity in &all {
      let range = entity.node.text_range();
      let (start, end) = (usize::from(range.start()), usize::from(range.end()));
      assert_eq!(
        entity.node.kind(),
        entity.kind,
        "{}: the node is no {:?}",
        entity.what,
        entity.kind
      );
      assert_eq!(
        start,
        entity.span.start(),
        "{}: the node starts at {start}, and the extent at {}",
        entity.what,
        entity.span.start()
      );
      assert!(
        entity.span.end() <= end,
        "{}: the node ends at {end}, before the extent's end at {}",
        entity.what,
        entity.span.end()
      );
      let text = entity.node.text().to_string();
      let tail = &text[entity.span.end() - start..];
      assert!(
        blank(tail.as_bytes()),
        "{}: the node runs on past the extent over {tail:?}",
        entity.what
      );
    }
    held_to_the_floors(&all, "every extent names one node");
  }

  /// T12: no definition, extension, schema item or member the model holds keeps a recovery hole:
  /// under the node of its extent there is no `Error` node and no `Gap` token. A module may hold
  /// one: it keeps its node around an item that broke inside it. Over the sweep of
  /// `every_extent_names_one_node_of_its_files_tree`, and under its floors.
  #[test]
  fn no_definition_or_member_keeps_a_recovery_hole() {
    let all = reached();
    for entity in &all {
      if entity.kind == K::ModDefinition {
        continue;
      }
      assert!(
        !entity
          .node
          .descendants_with_tokens()
          .any(|element: SyntaxElement| matches!(element.kind(), K::Error | K::Gap)),
        "{}: the node holds a recovery hole: {:?}",
        entity.what,
        entity.node.text()
      );
    }
    held_to_the_floors(&all, "no definition or member keeps a recovery hole");
  }

  /// Whether every error `analysis` holds is a syntax error.
  fn only_syntax_errors(analysis: &Analysis) -> bool {
    analysis
      .diagnostics()
      .iter()
      .all(|diagnostic: &Diagnostic| {
        diagnostic.severity() != Severity::Error
          || diagnostic.code().as_str().starts_with("smear::syntax::")
      })
  }

  /// That nothing `analysis`, named `name`, holds is marked: no definition, no contribution of a
  /// definition or of the schema, and no member. Answers whether it holds a definition of a
  /// supplied package.
  fn nothing_marked(analysis: &Analysis, name: &str) -> bool {
    let model = analysis.model();
    let mut supplied = false;
    for id in model.defs() {
      let held = model.def(id);
      assert!(!held.has_errors(), "{name}: `{}` is marked", held.name());
      for part in held.contributions() {
        assert!(
          !part.has_errors(),
          "{name}: a contribution of `{}` is marked",
          held.name()
        );
      }
      for member in members_of(model, id) {
        assert!(
          !model.member(member).has_errors(),
          "{name}: `{}`'s member `{}` is marked",
          held.name(),
          model.member(member).name()
        );
      }
      let package = model.module(held.module()).package();
      if model.package(package).kind() != PackageKind::BuiltIn {
        supplied = true;
      }
    }
    for part in model.schema().contributions() {
      assert!(!part.has_errors(), "{name}: a schema item is marked");
    }
    supplied
  }

  /// The definition the six broken texts are written after.
  const QUERY: &str = "type Query { ok: Int }";

  /// `recovery_graphqlx.rs`'s `a_missing_field_type_keeps_the_definition_after_it`: each of the
  /// six costs its broken definition and keeps the scalar after it.
  const SIX: [&str; 6] = [
    "type T { f: } scalar S",
    "type T { f(a: ): Int } scalar S",
    "type T @d(a: ) { f: Int } scalar S",
    "input I { a: Int = } scalar S",
    "type T { f: [] } scalar S",
    "type T { f: Box<> } scalar S",
  ];

  /// T12: a syntax error marks nothing.
  ///
  /// Through the lossless layer, over the invalid corpus and the six texts, each written after
  /// `type Query { ok: Int }`: in every analysis whose errors are all syntax errors, no definition,
  /// contribution or member has errors.
  #[test]
  fn a_syntax_error_marks_nothing() {
    let mut texts: Vec<(String, String)> = corpus("invalid_");
    for broken in SIX {
      texts.push((
        format!("`{broken}` after `{QUERY}`"),
        format!("{QUERY}\n{broken}"),
      ));
    }
    let mut held = 0usize;
    for (name, text) in &texts {
      let root = one_file(text.as_str());
      let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
      if !only_syntax_errors(&analysis) {
        continue;
      }
      if nothing_marked(&analysis, name) {
        held += 1;
      }
    }
    println!("T12: {held} analyses whose errors are all syntax errors hold a supplied definition");
    // The six texts: each keeps `Query` and the scalar after its broken definition, and none
    // names anything that does not resolve.
    assert!(
      held >= 6,
      "only {held} analyses with syntax errors alone hold a supplied definition"
    );
  }

  /// Whether `at` lies within `extent`: in its source, and between its first byte and its last.
  fn holds(extent: Location, at: Location) -> bool {
    match (extent.span(), at.span()) {
      (Some(outer), Some(inner)) => {
        extent.source() == at.source()
          && outer.start() <= inner.start()
          && inner.end() <= outer.end()
      }
      _ => false,
    }
  }

  /// T12: two definitions the projection reads whole past a reported mistake, each in the model
  /// and unmarked, the syntax error in the diagnostics alone. They are instances of the
  /// projection's lenient class, a token with no AST image that the parser leaves out of a
  /// hole-free node, and not the only places a syntax error lies inside an extent.
  ///
  /// First, `recovery_graphqlx.rs`'s `an_unclosed_list_keeps_the_definition_after_it`, its
  /// second text with its type named `Query`: the list's error lies inside `Query`'s extent.
  /// Then the projection header's own example, a type with no closing brace, whose error is not
  /// asserted to lie anywhere.
  #[test]
  fn a_definition_read_past_a_mistake_holds_it_unmarked() {
    let text = "type Query { f: [Int } scalar S";
    let root = one_file(text);
    let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
    assert!(
      only_syntax_errors(&analysis),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    let model = analysis.model();
    let query = def(model, Namespace::Type, "^::app::Query");
    let own = contribution(model, query, false);
    let extent = own.extent();
    let syntax: Vec<Location> = analysis
      .diagnostics()
      .iter()
      .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str().starts_with("smear::syntax::"))
      .map(|diagnostic: &Diagnostic| diagnostic.primary())
      .collect::<Vec<Location>>();
    assert!(
      syntax.iter().any(|at: &Location| holds(extent, *at)),
      "{text}: no syntax diagnostic lies inside `Query`'s extent {extent:?}: {syntax:?}"
    );
    assert!(
      !model.def(query).has_errors() && !own.has_errors(),
      "{text}: `Query` is marked"
    );
    for field in model.def(query).fields() {
      assert!(
        !model.member(field).has_errors(),
        "{text}: `Query`'s field `{}` is marked",
        model.member(field).name()
      );
    }

    let text = "type Query { ok: Int";
    let root = one_file(text);
    let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
    assert!(
      only_syntax_errors(&analysis) && !analysis.is_ok(),
      "{text}: {:#?}",
      analysis.diagnostics()
    );
    assert!(
      analysis
        .model()
        .lookup(Namespace::Type, "^::app::Query")
        .is_some(),
      "{text}: `Query` is not in the model"
    );
    assert!(
      nothing_marked(&analysis, text),
      "{text}: no supplied definition"
    );
  }
}

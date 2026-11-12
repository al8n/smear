//! The GraphQLx analyzer's lowering: what each definition holds beyond its identity. Its own
//! contribution, its type parameters, a directive definition's locations, and its members, its
//! direct members before every argument of its fields, each with its description; what the
//! built-in library holds; and none of it moved by the order files and dependencies are passed in.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, DefId, Description, Diagnostic, DirectiveLocation, IdRange, Layout,
    Limits, MemberId, MemberKind, Model, Namespace, Origin, Package, PackageId, Primitive,
    SourceFile, SourceId, Tier, analyze,
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

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`.
fn analyze_one(text: &'static str) -> Analysis {
  let root = package("app", Layout::Flat, &[("schema.graphqlx", text)]);
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

/// The root package's one file, `schema.graphqlx`.
fn schema_file(analysis: &Analysis) -> SourceId {
  file(analysis, 3, "schema.graphqlx")
}

/// The whole of the package `package`'s package source.
fn package_source(analysis: &Analysis, package: u32) -> Location {
  let id = analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .next()
    .expect("every package has a package source");
  Location::entire(id.get())
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

/// Where the `nth`, counting from 0, of the names `name` written right after `before` is in `text`,
/// the file `source`: a name, so one followed by no letter, digit or `_`.
fn named(source: SourceId, text: &str, before: &str, name: &str, nth: usize) -> Location {
  let written = format!("{before}{name}");
  let start = text
    .match_indices(written.as_str())
    .map(|(start, _): (usize, &str)| start + before.len())
    .filter(|start: &usize| {
      text[start + name.len()..]
        .chars()
        .next()
        .is_none_or(|next: char| !(next.is_ascii_alphanumeric() || next == '_'))
    })
    .nth(nth)
    .unwrap_or_else(|| panic!("`{written}` does not occur {} times", nth + 1));
  Location::new(source.get(), SimpleSpan::new(start, start + name.len()))
}

/// The definition the canonical name `name` names in `namespace`.
fn def(model: &Model, namespace: Namespace, name: &str) -> DefId {
  model
    .lookup(namespace, name)
    .unwrap_or_else(|| panic!("no definition `{name}`"))
}

/// The ids `range` holds, in order.
fn ids(range: IdRange<MemberId>) -> Vec<MemberId> {
  range.into_iter().collect()
}

/// The names of the members `range` holds, in order.
fn member_names(model: &Model, range: IdRange<MemberId>) -> Vec<&str> {
  range
    .into_iter()
    .map(|id: MemberId| model.member(id).name())
    .collect()
}

/// A description's text and where it is written.
fn described(description: Option<Description<'_>>) -> Option<(&str, Location)> {
  description.map(|written| (written.text(), written.location()))
}

/// What lowering writes of a member: its kind, name, owner, parent and location, and its
/// description's text and location.
type Lowered<'a> = (
  MemberKind,
  &'a str,
  DefId,
  Option<MemberId>,
  Location,
  Option<(&'a str, Location)>,
);

/// What lowering wrote of the member `id`.
fn lowered(model: &Model, id: MemberId) -> Lowered<'_> {
  let member = model.member(id);
  (
    member.kind(),
    member.name(),
    member.owner(),
    member.parent(),
    member.location(),
    described(member.description()),
  )
}

/// Each contribution of `def`: where it is, whether it is an extension, its direct members, and
/// whether it has errors.
fn contributions(model: &Model, def: DefId) -> Vec<(Location, bool, Vec<MemberId>, bool)> {
  model
    .def(def)
    .contributions()
    .map(|part: Contribution<'_>| {
      (
        part.location(),
        part.is_extension(),
        ids(part.members()),
        part.has_errors(),
      )
    })
    .collect()
}

/// One member of every kind: a field with two arguments, one of them described; a field with an
/// empty description; an input field described by a block string; two enum values, one described;
/// a directive definition's argument; and an interface's field.
const MEMBERS: &str = r#"type Query {
  "Fetches one."
  node("Its key." id: ID, flag: Boolean): Node
  ""
  version: Int
}
input Filter {
  """
  Two
  lines
  """
  tags: [String]
}
enum Color {
  RED
  "Leafy." GREEN
}
directive @cache("Seconds." ttl: Int) on FIELD_DEFINITION
interface Node {
  id: ID
}
"#;

#[test]
fn every_kind_of_member_is_lowered_with_its_name_owner_parent_and_description() {
  let analysis = analyze_one(MEMBERS);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let query = def(model, Namespace::Type, "^::app::Query");
  let filter = def(model, Namespace::Type, "^::app::Filter");
  let color = def(model, Namespace::Type, "^::app::Color");
  let node_type = def(model, Namespace::Type, "^::app::Node");
  let cache = def(model, Namespace::Directive, "@^::app::cache");

  // A field described, and a field described empty, which is no absent description.
  let fields = ids(model.def(query).fields());
  assert_eq!(
    member_names(model, model.def(query).fields()),
    ["node", "version"]
  );
  let (node, version) = (fields[0], fields[1]);
  assert_eq!(
    lowered(model, node),
    (
      MemberKind::Field,
      "node",
      query,
      None,
      named(schema, MEMBERS, "  ", "node", 0),
      Some(("Fetches one.", at(schema, MEMBERS, "\"Fetches one.\"", 0))),
    )
  );
  assert_eq!(
    lowered(model, version),
    (
      MemberKind::Field,
      "version",
      query,
      None,
      named(schema, MEMBERS, "  ", "version", 0),
      Some(("", at(schema, MEMBERS, "\"\"", 0))),
    )
  );

  // A field's arguments, each with its field as its parent and the field's owner as its own: one
  // described, one not.
  let arguments = ids(model.member(node).arguments());
  assert_eq!(
    member_names(model, model.member(node).arguments()),
    ["id", "flag"]
  );
  assert_eq!(
    lowered(model, arguments[0]),
    (
      MemberKind::Argument,
      "id",
      query,
      Some(node),
      named(schema, MEMBERS, "\" ", "id", 0),
      Some(("Its key.", at(schema, MEMBERS, "\"Its key.\"", 0))),
    )
  );
  assert_eq!(
    lowered(model, arguments[1]),
    (
      MemberKind::Argument,
      "flag",
      query,
      Some(node),
      named(schema, MEMBERS, ", ", "flag", 0),
      None,
    )
  );
  assert!(model.member(version).arguments().is_empty());

  // An input field, its block string cooked: the indentation and the blank first and last lines
  // gone.
  let tags = ids(model.def(filter).input_fields());
  assert_eq!(tags.len(), 1);
  assert_eq!(
    lowered(model, tags[0]),
    (
      MemberKind::InputField,
      "tags",
      filter,
      None,
      named(schema, MEMBERS, "  ", "tags", 0),
      Some((
        "Two\nlines",
        at(schema, MEMBERS, "\"\"\"\n  Two\n  lines\n  \"\"\"", 0)
      )),
    )
  );

  // Enum values, one described.
  let values = ids(model.def(color).enum_values());
  assert_eq!(
    member_names(model, model.def(color).enum_values()),
    ["RED", "GREEN"]
  );
  assert_eq!(
    lowered(model, values[0]),
    (
      MemberKind::EnumValue,
      "RED",
      color,
      None,
      named(schema, MEMBERS, "  ", "RED", 0),
      None,
    )
  );
  assert_eq!(
    lowered(model, values[1]),
    (
      MemberKind::EnumValue,
      "GREEN",
      color,
      None,
      named(schema, MEMBERS, "\" ", "GREEN", 0),
      Some(("Leafy.", at(schema, MEMBERS, "\"Leafy.\"", 0))),
    )
  );

  // A directive definition's argument: a direct member, with no parent.
  let ttl = ids(model.def(cache).arguments());
  assert_eq!(ttl.len(), 1);
  assert_eq!(
    lowered(model, ttl[0]),
    (
      MemberKind::Argument,
      "ttl",
      cache,
      None,
      named(schema, MEMBERS, "\" ", "ttl", 0),
      Some(("Seconds.", at(schema, MEMBERS, "\"Seconds.\"", 0))),
    )
  );

  // An interface's field.
  let id = ids(model.def(node_type).fields());
  assert_eq!(id.len(), 1);
  assert_eq!(
    lowered(model, id[0]),
    (
      MemberKind::Field,
      "id",
      node_type,
      None,
      named(schema, MEMBERS, "  ", "id", 0),
      None,
    )
  );

  // Each definition's direct members are its one contribution's, and nothing holds an error.
  for owner in [query, filter, color, node_type, cache] {
    let definition = model.def(owner);
    let direct: Vec<MemberId> = definition
      .fields()
      .into_iter()
      .chain(definition.input_fields())
      .chain(definition.enum_values())
      .chain(definition.arguments())
      .collect();
    let parts = contributions(model, owner);
    assert_eq!(parts.len(), 1);
    assert_eq!(parts[0].2, direct);
    assert!(!definition.has_errors());
  }
  for member in fields.iter().chain(&arguments).chain(&tags).chain(&values) {
    assert!(!model.member(*member).has_errors());
  }
}

#[test]
fn direct_members_come_before_every_argument() {
  let text = "type T {\n  first(a: Int, b: Int): Int\n  second: Int\n}\n\
              type U {\n  only(c: Int): Int\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let t = def(model, Namespace::Type, "^::app::T");
  let u = def(model, Namespace::Type, "^::app::U");

  let fields = ids(model.def(t).fields());
  assert_eq!(
    member_names(model, model.def(t).fields()),
    ["first", "second"]
  );
  let (first, second) = (fields[0], fields[1]);
  let arguments = ids(model.member(first).arguments());
  assert_eq!(
    member_names(model, model.member(first).arguments()),
    ["a", "b"]
  );
  // `second` is a direct member, so its id is below every argument's, `first`'s included.
  assert_eq!(second.get(), first.get() + 1);
  assert_eq!(arguments[0].get(), second.get() + 1);
  assert_eq!(arguments[1].get(), arguments[0].get() + 1);
  for argument in &arguments {
    assert_eq!(model.member(*argument).parent(), Some(first));
    assert_eq!(model.member(*argument).owner(), t);
  }

  // The next definition's members come after every one of `T`'s, its arguments included.
  let only = ids(model.def(u).fields());
  assert_eq!(only[0].get(), arguments[1].get() + 1);
  let c = ids(model.member(only[0]).arguments());
  assert_eq!(c[0].get(), only[0].get() + 1);
  assert_eq!(model.member(c[0]).parent(), Some(only[0]));
}

/// Generic definitions of every kind that has members or none, and one that is not generic.
const GENERIC: &str = "type Page<T, U = Int> {\n  items: [T]\n}\nscalar Wrapper<X>\n\
                       input Box<W> {\n  value: W\n}\ninterface Container<C> {\n  value: C\n}\n\
                       directive @tagged<V>(value: V) on FIELD_DEFINITION\nscalar Plain\n";

#[test]
fn type_parameters_are_rows_of_their_definition_in_written_order() {
  let analysis = analyze_one(GENERIC);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let expected = [
    (
      def(model, Namespace::Type, "^::app::Page"),
      vec![
        ("T", 0, named(schema, GENERIC, "<", "T", 0)),
        ("U", 1, named(schema, GENERIC, ", ", "U", 0)),
      ],
    ),
    (
      def(model, Namespace::Type, "^::app::Wrapper"),
      vec![("X", 0, named(schema, GENERIC, "<", "X", 0))],
    ),
    (
      def(model, Namespace::Type, "^::app::Box"),
      vec![("W", 0, named(schema, GENERIC, "<", "W", 0))],
    ),
    (
      def(model, Namespace::Type, "^::app::Container"),
      vec![("C", 0, named(schema, GENERIC, "<", "C", 0))],
    ),
    (
      def(model, Namespace::Directive, "@^::app::tagged"),
      vec![("V", 0, named(schema, GENERIC, "<", "V", 0))],
    ),
    (def(model, Namespace::Type, "^::app::Plain"), vec![]),
  ];
  for (owner, params) in expected {
    let found: Vec<(&str, u32, Location)> = model
      .def(owner)
      .type_params()
      .map(|param| (param.name(), param.index(), param.location()))
      .collect();
    assert_eq!(found, params, "{}", model.def(owner).name());
  }
}

/// Every location a directive may be written at, in the parser's order, on a repeatable directive;
/// and two, out of that order, on one that is not.
const LOCATIONS: &str = "directive @everywhere repeatable on QUERY | MUTATION | SUBSCRIPTION | FIELD\n  \
                         | FRAGMENT_DEFINITION | FRAGMENT_SPREAD | INLINE_FRAGMENT\n  \
                         | VARIABLE_DEFINITION | SCHEMA | SCALAR | OBJECT | FIELD_DEFINITION\n  \
                         | ARGUMENT_DEFINITION | INTERFACE | UNION | ENUM | ENUM_VALUE | INPUT_OBJECT\n  \
                         | INPUT_FIELD_DEFINITION | DIRECTIVE_DEFINITION\n\
                         directive @once on ENUM_VALUE | OBJECT\n";

#[test]
fn a_directive_definition_keeps_its_locations_as_written_and_whether_it_is_repeatable() {
  let analysis = analyze_one(LOCATIONS);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();

  let everywhere = model.def(def(model, Namespace::Directive, "@^::app::everywhere"));
  assert_eq!(
    everywhere.locations(),
    [
      DirectiveLocation::Query,
      DirectiveLocation::Mutation,
      DirectiveLocation::Subscription,
      DirectiveLocation::Field,
      DirectiveLocation::FragmentDefinition,
      DirectiveLocation::FragmentSpread,
      DirectiveLocation::InlineFragment,
      DirectiveLocation::VariableDefinition,
      DirectiveLocation::Schema,
      DirectiveLocation::Scalar,
      DirectiveLocation::Object,
      DirectiveLocation::FieldDefinition,
      DirectiveLocation::ArgumentDefinition,
      DirectiveLocation::Interface,
      DirectiveLocation::Union,
      DirectiveLocation::Enum,
      DirectiveLocation::EnumValue,
      DirectiveLocation::InputObject,
      DirectiveLocation::InputFieldDefinition,
      DirectiveLocation::DirectiveDefinition,
    ]
  );
  assert!(everywhere.is_repeatable());
  assert!(everywhere.arguments().is_empty());

  // Written order, not the order GraphQL lists them in.
  let once = model.def(def(model, Namespace::Directive, "@^::app::once"));
  assert_eq!(
    once.locations(),
    [DirectiveLocation::EnumValue, DirectiveLocation::Object]
  );
  assert!(!once.is_repeatable());
}

#[test]
fn a_definitions_own_contribution_is_named_where_its_name_is_and_holds_its_errors() {
  let text = "\"Described.\"\ntype A {\n  f: Int\n}\nscalar B\ntype A {\n  g: Int\n}\n";
  let analysis = analyze_one(text);
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let found: Vec<(&str, Location)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect();
  assert_eq!(
    found,
    [(
      "smear::schema::duplicate-type-name",
      named(schema, text, "type ", "A", 1)
    )]
  );

  let root = model.package(model.root_package()).root_module();
  let held: Vec<DefId> = model.module(root).defs().into_iter().collect();
  assert_eq!(held.len(), 3);
  let (first, b, second) = (held[0], held[1], held[2]);
  let f = ids(model.def(first).fields());
  let g = ids(model.def(second).fields());

  // Each definition's one contribution is its own, named where the definition is; the error of
  // the second `A` lies within the second's alone, and within none of its members.
  assert_eq!(
    contributions(model, first),
    [(named(schema, text, "type ", "A", 0), false, f, false)]
  );
  assert_eq!(
    contributions(model, b),
    [(named(schema, text, "scalar ", "B", 0), false, vec![], false)]
  );
  assert_eq!(
    contributions(model, second),
    [(named(schema, text, "type ", "A", 1), false, g.clone(), true)]
  );
  assert!(!model.def(first).has_errors());
  assert!(model.def(second).has_errors());
  assert!(!model.member(g[0]).has_errors());

  // A primitive `core` materialises has one contribution, the whole of `core`'s package source.
  let i32_def = model
    .builtins()
    .primitive(Primitive::signed(32))
    .expect("`Int` writes `i32`");
  assert_eq!(
    contributions(model, i32_def),
    [(package_source(&analysis, 0), false, vec![], false)]
  );
}

#[test]
fn a_replaced_directive_has_the_roots_arguments() {
  let text = "\"Ours.\"\ndirective @deprecated(\"Why.\" reason: String, since: String) \
              on FIELD_DEFINITION | ENUM_VALUE\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let deprecated = model.builtins().deprecated();
  let definition = model.def(deprecated);

  // It keeps the built-in's identity.
  assert_eq!(
    model.lookup(Namespace::Directive, "@^::core::deprecated"),
    Some(deprecated)
  );
  assert_eq!(
    definition.module(),
    model.package(PackageId::new(0)).root_module()
  );
  assert_eq!(definition.origin(), Origin::BuiltIn);

  // Its one contribution, its locations and its arguments are the root package's: smear's own
  // definition, with its one argument `reason: String! = "No longer supported"` and its five
  // locations, contributes nothing.
  let name = named(schema, text, "@", "deprecated", 0);
  assert_eq!(definition.location(), name);
  let arguments = ids(definition.arguments());
  assert_eq!(
    contributions(model, deprecated),
    [(name, false, arguments.clone(), false)]
  );
  assert_eq!(
    member_names(model, definition.arguments()),
    ["reason", "since"]
  );
  assert_eq!(
    lowered(model, arguments[0]),
    (
      MemberKind::Argument,
      "reason",
      deprecated,
      None,
      named(schema, text, "\" ", "reason", 0),
      Some(("Why.", at(schema, text, "\"Why.\"", 0))),
    )
  );
  assert_eq!(
    lowered(model, arguments[1]),
    (
      MemberKind::Argument,
      "since",
      deprecated,
      None,
      named(schema, text, ", ", "since", 0),
      None,
    )
  );
  assert_eq!(
    definition.locations(),
    [
      DirectiveLocation::FieldDefinition,
      DirectiveLocation::EnumValue
    ]
  );
  assert!(!definition.is_repeatable());
  assert_eq!(
    definition
      .description()
      .map(|written: Description<'_>| written.location()),
    Some(at(schema, text, "\"Ours.\"", 0))
  );
}

/// The text of `@deprecated`'s `reason` argument's description in the built-in library, cooked.
const REASON: &str = "Explains why this element was deprecated, usually also including a \
                      suggestion for how to access supported similar data. Formatted using the \
                      Markdown syntax, as specified by [CommonMark](https://commonmark.org/).";

#[test]
fn the_built_in_library_lowers_with_no_diagnostic() {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let builtins = model.builtins();

  // `@extern(lang: string!, type: string!, tier: Tier) repeatable on SCALAR`.
  let ext = model.def(builtins.r#extern());
  assert_eq!(
    member_names(model, ext.arguments()),
    ["lang", "type", "tier"]
  );
  for argument in ext.arguments() {
    let member = model.member(argument);
    assert_eq!(member.kind(), MemberKind::Argument);
    assert_eq!(member.owner(), builtins.r#extern());
    assert_eq!(member.parent(), None);
  }
  assert!(ext.is_repeatable());
  assert_eq!(ext.locations(), [DirectiveLocation::Scalar]);

  // `enum Tier`'s three values, each described.
  let tier = model.def(def(model, Namespace::Type, "^::core::Tier"));
  assert_eq!(
    member_names(model, tier.enum_values()),
    ["CORE", "ALLOC", "STD"]
  );
  let descriptions: Vec<Option<&str>> = tier
    .enum_values()
    .into_iter()
    .map(|id: MemberId| described(model.member(id).description()).map(|(text, _)| text))
    .collect();
  assert_eq!(
    descriptions,
    [
      Some("Needs no allocator."),
      Some("Needs an allocator."),
      Some("Needs the standard library.")
    ]
  );

  // `BTreeMap<K, V>`'s parameters.
  let map = model.def(def(model, Namespace::Type, "^::alloc::BTreeMap"));
  let params: Vec<(&str, u32)> = map
    .type_params()
    .map(|param| (param.name(), param.index()))
    .collect();
  assert_eq!(params, [("K", 0), ("V", 1)]);

  // `@deprecated`'s argument, its block string cooked; `@skip`'s, described inline.
  let deprecated = model.def(builtins.deprecated());
  let reason = ids(deprecated.arguments());
  assert_eq!(member_names(model, deprecated.arguments()), ["reason"]);
  assert_eq!(
    described(model.member(reason[0]).description()).map(|(text, _)| text),
    Some(REASON)
  );
  let skip = model.def(builtins.skip());
  let condition = ids(skip.arguments());
  assert_eq!(member_names(model, skip.arguments()), ["if"]);
  assert_eq!(
    described(model.member(condition[0]).description()).map(|(text, _)| text),
    Some("Skipped when true.")
  );

  // A meta-type's field and that field's argument.
  let meta = model.def(def(model, Namespace::Type, "^::core::__Schema"));
  assert_eq!(
    member_names(model, meta.fields()),
    [
      "description",
      "types",
      "queryType",
      "mutationType",
      "subscriptionType",
      "directives"
    ]
  );
  let directives = ids(meta.fields())[5];
  assert_eq!(
    member_names(model, model.member(directives).arguments()),
    ["includeDeprecated"]
  );

  // Every definition's one contribution is its own; `bool`, which `core` writes, has the whole
  // of `core`'s package source for it.
  for id in model.defs() {
    let parts: Vec<bool> = model
      .def(id)
      .contributions()
      .map(|part: Contribution<'_>| part.is_extension())
      .collect();
    assert_eq!(parts, [false], "{}", model.def(id).name());
  }
  let boolean = builtins
    .primitive(Primitive::Bool)
    .expect("`core` writes `Boolean!`");
  assert_eq!(
    contributions(model, boolean),
    [(package_source(&analysis, 0), false, vec![], false)]
  );
}

/// Everything lowering writes, read through the public model, and every diagnostic: what no order
/// of the files or of the dependencies may change.
fn render(analysis: &Analysis) -> Vec<String> {
  let model = analysis.model();
  let mut lines: Vec<String> = Vec::new();
  for id in model.defs() {
    let definition = model.def(id);
    lines.push(format!(
      "def {} {:?} {} {:?} {:?} {:?}",
      id.get(),
      definition.kind(),
      definition.name(),
      definition.module(),
      definition.location(),
      described(definition.description()),
    ));
    for part in contributions(model, id) {
      lines.push(format!("  contribution {part:?}"));
    }
    for param in definition.type_params() {
      lines.push(format!(
        "  parameter {} {} {:?}",
        param.index(),
        param.name(),
        param.location()
      ));
    }
    lines.push(format!(
      "  locations {:?} repeatable {}",
      definition.locations(),
      definition.is_repeatable()
    ));
    let direct = definition
      .fields()
      .into_iter()
      .chain(definition.input_fields())
      .chain(definition.enum_values())
      .chain(definition.arguments());
    for member in direct {
      lines.push(format!(
        "  member {} {:?}",
        member.get(),
        lowered(model, member)
      ));
      for argument in model.member(member).arguments() {
        lines.push(format!(
          "    argument {} {:?}",
          argument.get(),
          lowered(model, argument)
        ));
      }
    }
  }
  for diagnostic in analysis.diagnostics() {
    lines.push(format!("{diagnostic:?}"));
  }
  lines
}

/// A root package of three files in two modules, with extensions in two of them, and two
/// dependencies of two files and one.
const APP: [(&str, &str); 3] = [
  (
    "b.graphqlx",
    "\"The root.\"\ntype Query {\n  node(id: ID, depth: Int = 1): Node\n  \"Listed.\"\n  \
     nodes: [Node!]!\n}\ninterface Node {\n  id: ID\n}\n",
  ),
  (
    "a/inner.graphqlx",
    "enum Color {\n  RED\n  GREEN\n}\nextend enum Color {\n  BLUE\n}\n\
     input Filter<T> {\n  color: Color\n  value: T\n}\n\
     directive @mark(level: Int) repeatable on FIELD_DEFINITION | OBJECT\n",
  ),
  (
    "z.graphqlx",
    "extend type Query {\n  more(first: Int): Int\n}\nscalar Late\n",
  ),
];

/// The dependency `geo`'s files.
const GEO: [(&str, &str); 2] = [
  ("p.graphqlx", "type Point {\n  x: Float\n  y: Float\n}\n"),
  (
    "l.graphqlx",
    "type Line {\n  head: Point\n  tail: Point\n}\n",
  ),
];

/// The dependency `astro`'s file.
const ASTRO: [(&str, &str); 1] = [(
  "s.graphqlx",
  "\"A star.\"\ntype Star {\n  name: String\n}\n",
)];

/// The root package and its dependencies, each package's files, and the dependencies, in the
/// order given or reversed.
fn program(reversed: bool) -> (Package<&'static str>, Vec<Package<&'static str>>) {
  let order = |files: &[(&'static str, &'static str)]| -> Vec<(&'static str, &'static str)> {
    let mut files = files.to_vec();
    if reversed {
      files.reverse();
    }
    files
  };
  let root = package("app", Layout::Directory, &order(&APP)).with_dependencies(["geo", "astro"]);
  let mut dependencies = vec![
    package("geo", Layout::Flat, &order(&GEO)),
    package("astro", Layout::Flat, &order(&ASTRO)),
  ];
  if reversed {
    dependencies.reverse();
  }
  (root, dependencies)
}

#[test]
fn reversing_files_and_dependencies_changes_no_id_and_no_diagnostic() {
  let (root, dependencies) = program(false);
  let forward = analyze(&root, &dependencies, &Limits::new());
  assert!(forward.is_ok(), "{:#?}", forward.diagnostics());
  let (root, dependencies) = program(true);
  let backward = analyze(&root, &dependencies, &Limits::new());
  assert_eq!(render(&forward), render(&backward));

  // What the render compares is what lowering wrote: `Query`'s members and arguments, among the
  // rest.
  let model = forward.model();
  let query = def(model, Namespace::Type, "^::app::Query");
  assert_eq!(
    member_names(model, model.def(query).fields()),
    ["node", "nodes"]
  );
  let node = ids(model.def(query).fields())[0];
  assert_eq!(
    member_names(model, model.member(node).arguments()),
    ["id", "depth"]
  );
}

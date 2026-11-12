//! The GraphQLx analyzer's lowering: what each definition holds beyond its identity. Its
//! contributions, its own and its extensions', its type parameters, a directive definition's
//! locations, and its members, its direct members before every argument of its fields, each with
//! its description; extensions that name nothing or a definition of another kind; every written
//! type an occurrence, numbered outer first, with its path and arguments as written and what it
//! resolves to where it is written; bounds; what the built-in library holds; the schema's
//! contributions and its description; and none of it moved by the order files and dependencies
//! are passed in.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Contribution, Cx, DefId, Description, Diagnostic, DirectiveLocation, IdRange, Layout,
    Limits, MemberId, MemberKind, Model, Namespace, Origin, Package, PackageId, Packages, Path,
    PathRoot, Primitive, SourceFile, SourceId, Sym, Tier, TypeExprId, TypeExprKind, Visitor,
    analyze, visit,
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

/// Each diagnostic's code, primary location, and first label's location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location, Option<Location>)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| {
      (
        diagnostic.code().as_str(),
        diagnostic.primary(),
        diagnostic.label(0).map(|label| label.location()),
      )
    })
    .collect()
}

/// The names of `members`, in order.
fn names_of<'a>(model: &'a Model, members: &[MemberId]) -> Vec<&'a str> {
  members
    .iter()
    .map(|member: &MemberId| model.member(*member).name())
    .collect()
}

/// The type occurrence `id`, written out: its path as written, then `=` and the name of the
/// definition it names, `#` and the index of the parameter it names, or `?` when it names nothing;
/// a definition's arguments in `<>`; a list in `[]`; `!` when it is non-null.
fn written(model: &Model, id: TypeExprId) -> String {
  let occurrence = model.type_expr(id);
  let mut text = match occurrence.kind() {
    TypeExprKind::List { element, .. } => format!("[{}]", written(model, element)),
    TypeExprKind::Named { def, args, .. } => {
      let path = spelled(model, occurrence.path());
      if args.is_empty() {
        format!("{path}={}", model.def(def).name())
      } else {
        let args: Vec<String> = args
          .iter()
          .map(|arg: &TypeExprId| written(model, *arg))
          .collect();
        format!("{path}={}<{}>", model.def(def).name(), args.join(", "))
      }
    }
    TypeExprKind::Param { index, .. } => format!("{}=#{index}", spelled(model, occurrence.path())),
    _ => format!("{}=?", spelled(model, occurrence.path())),
  };
  if occurrence.non_null() {
    text.push('!');
  }
  text
}

/// `path` as written, its root and its segments; nothing for no path.
fn spelled(model: &Model, path: Option<Path<'_>>) -> String {
  let Some(path) = path else {
    return String::new();
  };
  let root = match path.root() {
    PathRoot::Relative => String::new(),
    PathRoot::Package => String::from("~::"),
    PathRoot::Dependency => String::from("^::"),
    PathRoot::Parent(levels) => "..::".repeat(levels.get() as usize),
    _ => String::from("?::"),
  };
  let segments: Vec<&str> = path
    .segments()
    .iter()
    .map(|segment: &Sym| model.name(*segment))
    .collect();
  format!("{root}{}", segments.join("::"))
}

/// The type of the member `id`, written out.
fn member_type(model: &Model, id: MemberId) -> String {
  written(model, model.member(id).ty().expect("the member has a type"))
}

/// The types of the members `range` holds, written out, in order.
fn member_types(model: &Model, range: IdRange<MemberId>) -> Vec<String> {
  range
    .into_iter()
    .map(|id: MemberId| member_type(model, id))
    .collect()
}

/// What a walk of the root package reaches, in its order.
#[derive(Default)]
struct Reached {
  types: Vec<TypeExprId>,
}

impl Visitor for Reached {
  fn type_expr(&mut self, _cx: &mut Cx<'_>, ty: TypeExprId) {
    self.types.push(ty);
  }
}

/// What a walk of `analysis`'s root package reaches.
fn reached(analysis: &Analysis) -> Reached {
  let mut reached = Reached::default();
  let _ = visit(analysis, Packages::Root, &mut reached);
  reached
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

/// A definition of every kind that has extensions, each extended once below it.
const EXTENDED: &str = "type Query {\n  a: Int\n}\nextend type Query {\n  b(x: Int): Int\n}\n\
                        interface Node {\n  id: ID\n}\nextend interface Node {\n  name: String\n}\n\
                        input Filter {\n  f: Int\n}\nextend input Filter {\n  g: Int\n}\n\
                        enum Color {\n  RED\n}\nextend enum Color {\n  GREEN\n}\n\
                        scalar Date\nextend scalar Date @specifiedBy(url: \"https://example.com\")\n\
                        union Either = Query\nextend union Either = Query\n\
                        directive @d on FIELD\nextend directive @d @deprecated\n";

#[test]
fn an_extension_is_a_contribution_of_its_target() {
  let analysis = analyze_one(EXTENDED);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let expected = [
    (
      Namespace::Type,
      "Query",
      "type ",
      "extend type ",
      vec!["a"],
      vec!["b"],
    ),
    (
      Namespace::Type,
      "Node",
      "interface ",
      "extend interface ",
      vec!["id"],
      vec!["name"],
    ),
    (
      Namespace::Type,
      "Filter",
      "input ",
      "extend input ",
      vec!["f"],
      vec!["g"],
    ),
    (
      Namespace::Type,
      "Color",
      "enum ",
      "extend enum ",
      vec!["RED"],
      vec!["GREEN"],
    ),
    (
      Namespace::Type,
      "Date",
      "scalar ",
      "extend scalar ",
      vec![],
      vec![],
    ),
    (
      Namespace::Type,
      "Either",
      "union ",
      "extend union ",
      vec![],
      vec![],
    ),
    (
      Namespace::Directive,
      "d",
      "directive @",
      "extend directive @",
      vec![],
      vec![],
    ),
  ];
  for (namespace, name, keyword, extend, own, added) in expected {
    let canonical = match namespace {
      Namespace::Directive => format!("@^::app::{name}"),
      _ => format!("^::app::{name}"),
    };
    let id = def(model, namespace, &canonical);
    let parts = contributions(model, id);
    assert_eq!(parts.len(), 2, "{name}");
    // Its own contribution first, then the extension, named where its target path is.
    assert_eq!(
      (parts[0].0, parts[0].1, parts[1].0, parts[1].1),
      (
        named(schema, EXTENDED, keyword, name, 0),
        false,
        named(schema, EXTENDED, extend, name, 0),
        true
      ),
      "{name}"
    );
    // Each holds the members it writes, and the definition holds both runs, its own first.
    assert_eq!(names_of(model, &parts[0].2), own, "{name}");
    assert_eq!(names_of(model, &parts[1].2), added, "{name}");
    let definition = model.def(id);
    let direct: Vec<MemberId> = definition
      .fields()
      .into_iter()
      .chain(definition.input_fields())
      .chain(definition.enum_values())
      .chain(definition.arguments())
      .collect();
    let both: Vec<MemberId> = parts[0].2.iter().chain(&parts[1].2).copied().collect();
    assert_eq!(direct, both, "{name}");
    assert!(!definition.has_errors(), "{name}");
  }

  // The argument of the field the extension adds follows every direct member of `Query`.
  let query = def(model, Namespace::Type, "^::app::Query");
  let fields = ids(model.def(query).fields());
  let x = ids(model.member(fields[1]).arguments());
  assert_eq!(
    member_names(model, model.member(fields[1]).arguments()),
    ["x"]
  );
  assert_eq!(x[0].get(), fields[1].get() + 1);
  assert_eq!(model.member(x[0]).parent(), Some(fields[1]));
}

#[test]
fn contributions_order_members_by_source_then_position() {
  // An extension above its definition and one below it in one file; two more in files passed in
  // the reverse of their paths' order; and one in the module `m`, which names its target from the
  // package's root.
  let a = "extend type Shape {\n  fromA: Int\n}\n";
  let b = "extend type Shape {\n  above: Int\n}\ntype Shape {\n  own: Int\n}\n\
           extend type Shape {\n  below: Int\n}\n";
  let c = "extend type Shape {\n  fromC: Int\n}\n";
  let m = "extend type ~::Shape {\n  fromM: Int\n}\n";
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("m/x.graphqlx", m),
      ("c.graphqlx", c),
      ("b.graphqlx", b),
      ("a.graphqlx", a),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let (a_file, b_file, c_file, m_file) = (
    file(&analysis, 3, "a.graphqlx"),
    file(&analysis, 3, "b.graphqlx"),
    file(&analysis, 3, "c.graphqlx"),
    file(&analysis, 3, "m/x.graphqlx"),
  );
  assert!(a_file < b_file && b_file < c_file && c_file < m_file);

  let shape = def(model, Namespace::Type, "^::app::Shape");
  assert_eq!(
    member_names(model, model.def(shape).fields()),
    ["own", "fromA", "above", "below", "fromC", "fromM"]
  );
  let locations: Vec<(Location, bool, usize)> = contributions(model, shape)
    .into_iter()
    .map(|(location, extension, members, _)| (location, extension, members.len()))
    .collect();
  assert_eq!(
    locations,
    [
      (named(b_file, b, "type ", "Shape", 0), false, 1),
      (named(a_file, a, "extend type ", "Shape", 0), true, 1),
      (named(b_file, b, "extend type ", "Shape", 0), true, 1),
      (named(b_file, b, "extend type ", "Shape", 1), true, 1),
      (named(c_file, c, "extend type ", "Shape", 0), true, 1),
      (at(m_file, m, "~::Shape", 0), true, 1),
    ]
  );
}

#[test]
fn an_extension_of_another_kind_contributes_nothing() {
  let text = "enum E {\n  A\n}\nextend type E {\n  f: Undefined\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  // One diagnostic, at the target, labelled at the definition: nothing inside the extension is
  // lowered, so the type it names is not looked for.
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::extension-kind-mismatch",
      named(schema, text, "extend type ", "E", 0),
      Some(named(schema, text, "enum ", "E", 0)),
    )]
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    diagnostic.to_string(),
    "`extend type` names `E`, which is an enum"
  );
  assert_eq!(
    diagnostic.label(0).map(|label| label.text()),
    Some("the definition it extends")
  );
  assert!(diagnostic.help().is_some());

  // The enum keeps its own contribution alone, and the error is in none of it.
  let model = analysis.model();
  let e = def(model, Namespace::Type, "^::app::E");
  assert_eq!(
    contributions(model, e),
    [(
      named(schema, text, "enum ", "E", 0),
      false,
      ids(model.def(e).enum_values()),
      false
    )]
  );
  assert_eq!(member_names(model, model.def(e).enum_values()), ["A"]);
  assert!(!model.def(e).has_errors());
}

#[test]
fn an_extension_of_nothing_reports_once() {
  let text = "extend type Missing {\n  f: Undefined\n}\nscalar Kept\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  // The target alone is reported: nothing inside the extension is lowered.
  let codes: Vec<(&str, Location)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect();
  assert_eq!(
    codes,
    [(
      "smear::schema::undefined-extension-target",
      named(schema, text, "extend type ", "Missing", 0)
    )]
  );
  let model = analysis.model();
  let kept = def(model, Namespace::Type, "^::app::Kept");
  assert_eq!(contributions(model, kept).len(), 1);
}

#[test]
fn a_built_in_is_extended_by_the_root() {
  // `Int` is a second name of `i32`, which `core` materialises; the root package's `Float`
  // replaces `f64`, which keeps its id.
  let text = "directive @x on SCALAR\nextend scalar Int @x\nscalar Float\nextend scalar Float @x\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let schema = schema_file(&analysis);
  let builtins = model.builtins();
  let i32_def = builtins
    .primitive(Primitive::signed(32))
    .expect("`Int` writes `i32`");
  assert_eq!(
    contributions(model, i32_def),
    [
      (package_source(&analysis, 0), false, vec![], false),
      (
        named(schema, text, "extend scalar ", "Int", 0),
        true,
        vec![],
        false
      ),
    ]
  );
  let f64_def = builtins
    .primitive(Primitive::F64)
    .expect("the root package replaces `f64`");
  assert_eq!(
    contributions(model, f64_def),
    [
      (
        named(schema, text, "scalar ", "Float", 0),
        false,
        vec![],
        false
      ),
      (
        named(schema, text, "extend scalar ", "Float", 0),
        true,
        vec![],
        false
      ),
    ]
  );
}

#[test]
fn a_mismatched_extension_of_a_long_name_quotes_an_excerpt() {
  let name = "E".repeat(4096);
  let text = format!("enum {name} {{\n  A\n}}\nextend type {name} {{\n  f: Int\n}}\n");
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text.as_str())]);
  let analysis = analyze(&root, &[], &Limits::new());
  let messages: Vec<String> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect();
  assert_eq!(messages.len(), 1);
  let expected = format!(
    "`extend type` names `{}…{}`, which is an enum",
    "E".repeat(64),
    "E".repeat(64)
  );
  assert_eq!(messages[0], expected);
}

/// Types of every shape, in the root module: a list of a non-null of a list, a dependency's type,
/// a type from the package's root, a generic type with as many arguments as it has parameters, and
/// with too few and too many.
const TYPES: &str = "type Shapes {\n  grid: [[Int!]]!\n  point: ^::geo::Point\n  own: ~::Local\n  \
                     map: BTreeMap<string, Int>\n  few: BTreeMap<string>\n  many: Local<Int, Int>\n}\n\
                     scalar Local\n";

/// A type from the parent module.
const UP: &str = "type Up {\n  back: ..::Local\n}\n";

#[test]
fn a_type_keeps_its_written_path_arguments_and_nullness() {
  let root = package(
    "app",
    Layout::Directory,
    &[("schema.graphqlx", TYPES), ("inner/up.graphqlx", UP)],
  )
  .with_dependencies(["geo"]);
  let geo = package(
    "geo",
    Layout::Flat,
    &[("p.graphqlx", "type Point {\n  x: Int\n}\n")],
  );
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = file(&analysis, 3, "schema.graphqlx");

  // `Int` stays `Int` and names `i32`; arguments are recorded however many the definition has
  // parameters.
  let shapes = def(model, Namespace::Type, "^::app::Shapes");
  assert_eq!(
    member_types(model, model.def(shapes).fields()),
    [
      "[[Int=i32!]]!",
      "^::geo::Point=Point",
      "~::Local=Local",
      "BTreeMap=BTreeMap<string=string, Int=i32>",
      "BTreeMap=BTreeMap<string=string>",
      "Local=Local<Int=i32, Int=i32>",
    ]
  );
  let up = def(model, Namespace::Type, "^::app::inner::Up");
  assert_eq!(
    member_types(model, model.def(up).fields()),
    ["..::Local=Local"]
  );

  // Each occurrence is where it is written, its `!` included, and its path where the path is.
  let grid = model
    .member(ids(model.def(shapes).fields())[0])
    .ty()
    .expect("typed");
  let TypeExprKind::List { element: inner, .. } = model.type_expr(grid).kind() else {
    panic!("a list");
  };
  let TypeExprKind::List { element: int, .. } = model.type_expr(inner).kind() else {
    panic!("a list");
  };
  assert_eq!(
    model.type_expr(grid).location(),
    at(schema, TYPES, "[[Int!]]!", 0)
  );
  assert_eq!(
    model.type_expr(inner).location(),
    at(schema, TYPES, "[Int!]", 0)
  );
  assert_eq!(
    model.type_expr(int).location(),
    at(schema, TYPES, "Int!", 0)
  );
  assert_eq!(
    model.type_expr(int).path_location(),
    Some(at(schema, TYPES, "Int", 0))
  );
  assert_eq!(model.type_expr(grid).path_location(), None);
}

#[test]
fn type_occurrences_are_numbered_outer_first() {
  let text = "scalar A<X, Y>\nscalar B<Z>\nscalar C\nscalar D\n\
              type T {\n  f: A<B<C>, D>\n  g: Int\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let t = def(model, Namespace::Type, "^::app::T");
  let fields = ids(model.def(t).fields());
  let f = model.member(fields[0]).ty().expect("typed");
  assert_eq!(written(model, f), "A=A<B=B<C=C>, D=D>");

  // `A`, then `B` and what it nests, then `D`; then the next field's type.
  let first = f.get();
  let TypeExprKind::Named { args, .. } = model.type_expr(f).kind() else {
    panic!("`A` resolves");
  };
  assert_eq!(
    args,
    [TypeExprId::new(first + 1), TypeExprId::new(first + 3)]
  );
  assert_eq!(
    model.member(fields[1]).ty(),
    Some(TypeExprId::new(first + 4))
  );
  let walked: Vec<u32> = reached(&analysis)
    .types
    .iter()
    .map(|id: &TypeExprId| id.get())
    .collect();
  assert_eq!(walked, [first, first + 1, first + 2, first + 3, first + 4]);
}

#[test]
fn a_parameter_shadows_a_type_of_its_module() {
  let text = "scalar T\ntype Box<T> {\n  value: T\n  wrapped: Box<T>\n}\n\
              type Plain {\n  value: T\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let boxed = def(model, Namespace::Type, "^::app::Box");
  assert_eq!(
    member_types(model, model.def(boxed).fields()),
    ["T=#0", "Box=Box<T=#0>"]
  );
  let value = model.member(ids(model.def(boxed).fields())[0]).ty();
  let Some(TypeExprKind::Param { def: owner, .. }) = value.map(|id| model.type_expr(id).kind())
  else {
    panic!("a parameter");
  };
  assert_eq!(owner, boxed);
  let plain = def(model, Namespace::Type, "^::app::Plain");
  assert_eq!(member_types(model, model.def(plain).fields()), ["T=T"]);
}

#[test]
fn an_extension_names_parameters_by_position() {
  // Each extension names the parameters its own way; one names one more than `Pair` has, which
  // is recorded and not counted.
  let text = "type Pair<A, B> {\n  first: A\n}\n\
              extend type Pair<X, Y> {\n  second: Y\n  both: Pair<Y, X>\n}\n\
              extend type Pair<P, Q, R> {\n  third: R\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let pair = def(model, Namespace::Type, "^::app::Pair");
  assert_eq!(
    member_types(model, model.def(pair).fields()),
    ["A=#0", "Y=#1", "Pair=Pair<Y=#1, X=#0>", "R=#2"]
  );
  for field in model.def(pair).fields() {
    if let Some(TypeExprKind::Param { def: owner, .. }) = model
      .member(field)
      .ty()
      .map(|id| model.type_expr(id).kind())
    {
      assert_eq!(owner, pair);
    }
  }
}

#[test]
fn an_unresolved_type_stays_with_its_arguments_and_marks_what_holds_it() {
  let text = "type T {\n  bad: Missing<Int>\n  good: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::schema::undefined-type",
      named(schema, text, "  bad: ", "Missing", 0),
      None
    )]
  );
  let model = analysis.model();
  let t = def(model, Namespace::Type, "^::app::T");
  let fields = ids(model.def(t).fields());
  let bad = model.member(fields[0]).ty().expect("typed");
  assert_eq!(
    member_types(model, model.def(t).fields()),
    ["Missing=?", "Int=i32"]
  );

  // Its argument is an occurrence of its own, which the walk reaches.
  let argument = TypeExprId::new(bad.get() + 1);
  assert_eq!(written(model, argument), "Int=i32");
  assert_eq!(
    reached(&analysis).types,
    [bad, argument, TypeExprId::new(bad.get() + 2)]
  );

  // The field, its contribution and its definition hold the error; the other field does not.
  assert!(model.member(fields[0]).has_errors());
  assert!(!model.member(fields[1]).has_errors());
  assert!(contributions(model, t)[0].3);
  assert!(model.def(t).has_errors());
}

#[test]
fn each_place_keeps_graphqls_code_for_a_name_that_names_nothing() {
  let text = "type T implements Nope {\n  f: Gone\n  g: BTreeMap<Lost, Int>\n  h: ~::Nowhere\n  \
              i: ^::geo::Thing\n}\nunion U = Absent\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let codes: Vec<(&str, Location)> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect();
  assert_eq!(
    codes,
    [
      (
        "smear::schema::undefined-implements-interface",
        at(schema, text, "Nope", 0)
      ),
      ("smear::schema::undefined-type", at(schema, text, "Gone", 0)),
      ("smear::schema::undefined-type", at(schema, text, "Lost", 0)),
      (
        "smear::graphqlx::unresolved-path",
        at(schema, text, "Nowhere", 0)
      ),
      (
        "smear::graphqlx::unknown-package",
        at(schema, text, "geo", 0)
      ),
      (
        "smear::schema::undefined-union-member",
        at(schema, text, "Absent", 0)
      ),
    ]
  );
  // Each stays an occurrence, as written.
  let model = analysis.model();
  let t = def(model, Namespace::Type, "^::app::T");
  assert_eq!(
    model
      .def(t)
      .implements()
      .iter()
      .map(|id: &TypeExprId| written(model, *id))
      .collect::<Vec<String>>(),
    ["Nope=?"]
  );
  assert_eq!(
    member_types(model, model.def(t).fields()),
    [
      "Gone=?",
      "BTreeMap=BTreeMap<Lost=?, Int=i32>",
      "~::Nowhere=?",
      "^::geo::Thing=?"
    ]
  );
  let u = def(model, Namespace::Type, "^::app::U");
  assert_eq!(
    model
      .def(u)
      .union_members()
      .iter()
      .map(|id: &TypeExprId| written(model, *id))
      .collect::<Vec<String>>(),
    ["Absent=?"]
  );
}

/// Each bound of `def`: its parameter, its interface written out, and where it is.
fn bounds(model: &Model, def: DefId) -> Vec<(u32, String, Location)> {
  model
    .def(def)
    .bounds()
    .iter()
    .map(|bound| {
      (
        bound.parameter(),
        written(model, bound.interface()),
        bound.location(),
      )
    })
    .collect()
}

#[test]
fn a_where_clause_is_recorded_parameter_by_parameter() {
  let text = "interface Ord @marker\ninterface Hash @marker\n\
              scalar Table<K, V> where K: Ord & Hash V: Hash\n\
              type Pair<A, B> where B: Ord {\n  a: A\n}\n";
  let analysis = analyze_one(text);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  let model = analysis.model();
  let schema = schema_file(&analysis);
  // Predicate by predicate and bound by bound, each at its predicate.
  let table = def(model, Namespace::Type, "^::app::Table");
  let k = at(schema, text, "K: Ord & Hash", 0);
  assert_eq!(
    bounds(model, table),
    [
      (0, String::from("Ord=Ord"), k),
      (0, String::from("Hash=Hash"), k),
      (1, String::from("Hash=Hash"), at(schema, text, "V: Hash", 0)),
    ]
  );
  let pair = def(model, Namespace::Type, "^::app::Pair");
  assert_eq!(
    bounds(model, pair),
    [(1, String::from("Ord=Ord"), at(schema, text, "B: Ord", 0))]
  );
}

#[test]
fn a_bound_on_something_that_is_no_parameter_is_refused() {
  let text = "interface Ord @marker\nscalar Plain\n\
              scalar Table<K> where Plain: Ord K<Int>: Ord K: Ord\n\
              scalar Other<K> where Missing: Ord\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  // A type and a parameter with an argument are refused; a name that names nothing is name
  // resolution's to report.
  assert_eq!(
    found(&analysis),
    [
      (
        "smear::graphqlx::invalid-bound",
        at(schema, text, "Plain", 1),
        None
      ),
      (
        "smear::graphqlx::invalid-bound",
        at(schema, text, "K<Int>", 0),
        None
      ),
      (
        "smear::schema::undefined-type",
        at(schema, text, "Missing", 0),
        None
      ),
    ]
  );
  let model = analysis.model();
  let table = def(model, Namespace::Type, "^::app::Table");
  assert_eq!(
    bounds(model, table),
    [(0, String::from("Ord=Ord"), at(schema, text, "K: Ord", 0))]
  );
  assert!(model.def(table).has_errors());
  let other = def(model, Namespace::Type, "^::app::Other");
  assert!(bounds(model, other).is_empty());
}

#[test]
fn a_where_clause_on_an_extension_is_refused() {
  let text = "interface Ord @marker\ntype Pair<A> {\n  a: A\n}\n\
              extend type Pair<X> where X: Ord {\n  b: Int\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      "smear::graphqlx::invalid-bound",
      at(schema, text, "where X: Ord", 0),
      None
    )]
  );
  // The extension still contributes its field; the definition has no bound.
  let model = analysis.model();
  let pair = def(model, Namespace::Type, "^::app::Pair");
  assert!(model.def(pair).bounds().is_empty());
  assert_eq!(member_names(model, model.def(pair).fields()), ["a", "b"]);
  assert_eq!(
    member_types(model, model.def(pair).fields()),
    ["A=#0", "Int=i32"]
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
  assert_eq!(
    member_types(model, ext.arguments()),
    ["string=string!", "string=string!", "Tier=Tier"]
  );
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
  let bound: Vec<(u32, String)> = map
    .bounds()
    .iter()
    .map(|bound| (bound.parameter(), written(model, bound.interface())))
    .collect();
  assert_eq!(bound, [(0, String::from("MapKey=MapKey"))]);

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

/// Each contribution of the schema: where it is, and whether it is an extension.
fn schema_contributions(model: &Model) -> Vec<(Location, bool)> {
  model
    .schema()
    .contributions()
    .map(|part: Contribution<'_>| (part.location(), part.is_extension()))
    .collect()
}

/// A schema extension in the directory module `a`, whose file numbers before the root module's
/// files although the module comes after the root module.
const SCHEMA_A: &str = "extend schema {\n  subscription: Tick\n}\ntype Tick {\n  at: Int\n}\n";

/// A schema extension written before the schema definition, which is described.
const SCHEMA_B: &str = "extend schema @x\n\"The schema.\"\nschema {\n  query: Query\n}\n\
                        type Query {\n  ok: Int\n}\ndirective @x repeatable on SCHEMA\n";

/// A schema extension in the file that numbers last.
const SCHEMA_C: &str = "extend schema {\n  mutation: Mutation\n}\ntype Mutation {\n  run: Int\n}\n";

#[test]
fn the_schemas_contributions_are_its_definitions_then_its_extensions_each_marked() {
  let root = package(
    "app",
    Layout::Directory,
    &[
      ("c.graphqlx", SCHEMA_C),
      ("b.graphqlx", SCHEMA_B),
      ("a/x.graphqlx", SCHEMA_A),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  let a = file(&analysis, 3, "a/x.graphqlx");
  let b = file(&analysis, 3, "b.graphqlx");
  let c = file(&analysis, 3, "c.graphqlx");
  assert!(a < b && b < c, "the files number by path");

  // The definition first, wherever it is written; then each extension by source and position,
  // although the module of `a/x.graphqlx` comes after the root module that holds the others. Each
  // is named at its first keyword, six bytes.
  assert_eq!(
    schema_contributions(model),
    [
      (named(b, SCHEMA_B, "\n", "schema", 0), false),
      (named(a, SCHEMA_A, "", "extend", 0), true),
      (named(b, SCHEMA_B, "", "extend", 0), true),
      (named(c, SCHEMA_C, "", "extend", 0), true),
    ]
  );
  for part in model.schema().contributions() {
    assert!(part.members().is_empty());
    assert!(!part.has_errors());
  }

  // The schema definition's description is the schema's.
  assert_eq!(
    described(model.schema().description()),
    Some(("The schema.", at(b, SCHEMA_B, "\"The schema.\"", 0)))
  );
}

#[test]
fn a_second_schema_definition_and_a_dependencys_are_contributions_and_nothing_else() {
  let one = "\"First.\"\nschema {\n  query: Query\n}\ntype Query {\n  ok: Int\n}\n";
  let two = "\"Second.\"\nschema {\n  query: Query\n}\n";
  let theirs = "\"Theirs.\"\nschema {\n  query: Point\n}\nextend schema {\n  mutation: Point\n}\n\
                type Point {\n  x: Int\n}\n";
  let root = package(
    "app",
    Layout::Flat,
    &[("two.graphqlx", two), ("one.graphqlx", one)],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("g.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let model = analysis.model();
  let one_file = file(&analysis, 3, "one.graphqlx");
  let two_file = file(&analysis, 3, "two.graphqlx");
  let geo_file = file(&analysis, 4, "g.graphqlx");

  // Every schema item is a contribution: the definitions by source, the root package's and then
  // the dependency's, then the dependency's extension.
  assert_eq!(
    schema_contributions(model),
    [
      (named(one_file, one, "\n", "schema", 0), false),
      (named(two_file, two, "\n", "schema", 0), false),
      (named(geo_file, theirs, "\n", "schema", 0), false),
      (named(geo_file, theirs, "\n", "extend", 0), true),
    ]
  );
  // And nothing else: only the root package's first schema definition describes the schema.
  assert_eq!(
    described(model.schema().description()),
    Some(("First.", at(one_file, one, "\"First.\"", 0)))
  );

  // With no schema definition of the root package's, a dependency's describes nothing either.
  let root = package(
    "app",
    Layout::Flat,
    &[("one.graphqlx", "type Query {\n  ok: Int\n}\n")],
  )
  .with_dependencies(["geo"]);
  let geo = package("geo", Layout::Flat, &[("g.graphqlx", theirs)]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  let model = analysis.model();
  let geo_file = file(&analysis, 4, "g.graphqlx");
  assert_eq!(
    schema_contributions(model),
    [
      (named(geo_file, theirs, "\n", "schema", 0), false),
      (named(geo_file, theirs, "\n", "extend", 0), true),
    ]
  );
  assert_eq!(model.schema().description(), None);

  // And a program that writes no schema item has no contribution to it.
  let analysis = analyze_one("type Query {\n  ok: Int\n}\n");
  assert!(schema_contributions(analysis.model()).is_empty());
  assert_eq!(analysis.model().schema().description(), None);
}

/// The type of the member `id`, if it has one: its id, written out, and where it is.
fn typed(model: &Model, id: MemberId) -> Option<(u32, String, Location)> {
  let ty = model.member(id).ty()?;
  Some((ty.get(), written(model, ty), model.type_expr(ty).location()))
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
        "  parameter {} {} {:?} {:?}",
        param.index(),
        param.name(),
        param.location(),
        param
          .default()
          .map(|id: TypeExprId| (id.get(), written(model, id)))
      ));
    }
    for bound in definition.bounds() {
      lines.push(format!(
        "  bound {} {} {} {:?}",
        bound.parameter(),
        bound.interface().get(),
        written(model, bound.interface()),
        bound.location()
      ));
    }
    for (what, list) in [
      ("implements", definition.implements()),
      ("union member", definition.union_members()),
    ] {
      for id in list {
        lines.push(format!(
          "  {what} {} {} {:?}",
          id.get(),
          written(model, *id),
          model.type_expr(*id).location()
        ));
      }
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
        "  member {} {:?} {:?}",
        member.get(),
        lowered(model, member),
        typed(model, member)
      ));
      for argument in model.member(member).arguments() {
        lines.push(format!(
          "    argument {} {:?} {:?}",
          argument.get(),
          lowered(model, argument),
          typed(model, argument)
        ));
      }
    }
  }
  for (location, extension) in schema_contributions(model) {
    lines.push(format!(
      "schema contribution {location:?} extension {extension}"
    ));
  }
  lines.push(format!(
    "schema description {:?}",
    described(model.schema().description())
  ));
  for diagnostic in analysis.diagnostics() {
    lines.push(format!("{diagnostic:?}"));
  }
  lines
}

/// A root package of three files in two modules, with extensions of definitions and of the schema
/// in each, and two dependencies of two files and one.
const APP: [(&str, &str); 3] = [
  (
    "b.graphqlx",
    "\"The root.\"\ntype Query {\n  node(id: ID, depth: Int = 1): Node\n  \"Listed.\"\n  \
     nodes: [Node!]!\n}\ninterface Node {\n  id: ID\n}\n\"The schema.\"\nschema {\n  \
     query: Query\n}\n",
  ),
  (
    "a/inner.graphqlx",
    "enum Color {\n  RED\n  GREEN\n}\nextend enum Color {\n  BLUE\n}\n\
     input Filter<T> {\n  color: Color\n  value: T\n}\n\
     directive @mark(level: Int) repeatable on FIELD_DEFINITION | OBJECT\n\
     type Change {\n  done: Int\n}\nextend schema {\n  mutation: Change\n}\n",
  ),
  (
    "z.graphqlx",
    "extend type Query {\n  more(first: Int): Int\n}\nscalar Late\n\
     extend schema {\n  subscription: Query\n}\n",
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
  let root =
    package("app", Layout::Directory, &order(&APP[..])).with_dependencies(["geo", "astro"]);
  let mut dependencies = vec![
    package("geo", Layout::Flat, &order(&GEO[..])),
    package("astro", Layout::Flat, &order(&ASTRO[..])),
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

  // What the render compares is what lowering wrote: `Query`'s members, the extension's among
  // them, and arguments, and the schema's three contributions, among the rest.
  let model = forward.model();
  let query = def(model, Namespace::Type, "^::app::Query");
  assert_eq!(
    member_names(model, model.def(query).fields()),
    ["node", "nodes", "more"]
  );
  assert_eq!(contributions(model, query).len(), 2);
  let node = ids(model.def(query).fields())[0];
  assert_eq!(
    member_names(model, model.member(node).arguments()),
    ["id", "depth"]
  );
  let extensions: Vec<bool> = schema_contributions(model)
    .into_iter()
    .map(|(_, extension): (Location, bool)| extension)
    .collect();
  assert_eq!(extensions, [false, true, true]);
}

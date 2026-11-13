//! The GraphQLx analyzer's bounds: a bound's right side is an interface, which lowering checks
//! where the bound is written; what a type satisfies, by definition and exactly, which the model
//! reads off what the type declares; and the bounds of every instantiation the program writes,
//! judged exactly, with the `!` of each argument kept inside the bound's interface, once a type
//! whatever its nullness and once a pair, within a budget of their own, each refusal reported
//! where the instantiation is written; and that the phase and the accessor agree.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Cx, DefId, Diagnostic, IdRange, Layout, Limits, MemberId, Model, Namespace, Package,
    PackageId, Packages, SourceFile, SourceId, Tier, TyId, TyKind, TypeExprId, TypeExprKind,
    Visitor, analyze, visit,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a bound the model cannot hold as written.
const INVALID_BOUND: &str = "smear::graphqlx::invalid-bound";

/// The code of a type name that names nothing.
const UNDEFINED: &str = "smear::schema::undefined-type";

/// The code of an instantiation whose type argument does not satisfy a bound.
const UNSATISFIED: &str = "smear::graphqlx::unsatisfied-bound";

/// The code of a bound of the analysis's limits that is passed.
const LIMIT: &str = "smear::graphqlx::resource-limit";

/// The code of a count of type arguments that does not match.
const ARITY: &str = "smear::graphqlx::generic-arity";

/// The code of `NonZero` of what is no integer primitive.
const NONZERO: &str = "smear::graphqlx::nonzero-argument";

/// What a bound's report tells the author of a named type argument.
const NAMED_HELP: &str = "declare the interface on the type argument with exactly these type \
                          arguments, or pass a type that declares it";

/// What a bound's report tells the author of a type argument that is a type parameter.
const PARAM_HELP: &str = "bound this type parameter by the interface in this definition's `where` \
                          clause";

/// What a bound's report tells the author of a type argument that is a list.
const LIST_HELP: &str = "a list satisfies no bound: pass a type that declares the interface";

/// The definitions most tests write their uses after: `Node`; `Comparable`, and `Ord`, which
/// declares it; `User`, which declares `Node` and `Comparable<User>`; `Box`, which declares
/// `Comparable<Box<T>>`; and `Page` and `Sorted`, each with a bound, the second's naming its
/// parameter.
const DEFINITIONS: &str = "interface Node {\n  id: ID\n}\n\
                           interface Comparable<X> {\n  compare(other: X): Int\n}\n\
                           interface Ord<X> implements Comparable<X> {\n  \
                           compare(other: X): Int\n}\n\
                           type User implements Node & Comparable<User> {\n  id: ID\n  \
                           compare(other: User): Int\n}\n\
                           type Box<T> implements Comparable<Box<T>> {\n  item: T\n  \
                           compare(other: Box<T>): Int\n}\n\
                           type Page<T> where T: Node {\n  items: [T]\n}\n\
                           type Sorted<T> where T: Comparable<T> {\n  items: [T]\n}\n";

/// The text of a program of `DEFINITIONS` followed by `uses`.
fn program(uses: &str) -> String {
  format!("{DEFINITIONS}{uses}")
}

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

/// The type the canonical name `name` names.
fn def(model: &Model, name: &str) -> DefId {
  model
    .lookup(Namespace::Type, name)
    .unwrap_or_else(|| panic!("no type `{name}`"))
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

/// Each diagnostic's message, in the list's order.
fn messages(analysis: &Analysis) -> Vec<String> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .collect()
}

/// The type `id`, written out: a definition's name, with its arguments in `<>` when it has any; a
/// type parameter as its definition's name, `#` and its index; a list in `[]`; the unresolved type
/// as `?`; and `!` after a type that is non-null.
fn shown(model: &Model, id: TyId) -> String {
  let ty = model.ty(id);
  let mut text = match ty.kind() {
    TyKind::Named { def, args, .. } => {
      let name = model.def(def).name();
      if args.is_empty() {
        String::from(name)
      } else {
        let args: Vec<String> = args.iter().map(|arg: &TyId| shown(model, *arg)).collect();
        format!("{name}<{}>", args.join(", "))
      }
    }
    TyKind::Param { def, index, .. } => format!("{}#{index}", model.def(def).name()),
    TyKind::List { element, .. } => format!("[{}]", shown(model, element)),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  text
}

/// The occurrence that is the type of the field `field` of the definition the canonical name
/// `owner` names.
fn field_occurrence(model: &Model, owner: &str, field: &str) -> TypeExprId {
  let member = model
    .def(def(model, owner))
    .fields()
    .into_iter()
    .find(|id: &MemberId| model.member(*id).name() == field)
    .unwrap_or_else(|| panic!("no field `{field}` in `{owner}`"));
  model.member(member).ty().expect("a field has a type")
}

/// The type of the field `field` of the definition the canonical name `owner` names.
fn field_type(model: &Model, owner: &str, field: &str) -> TyId {
  model.type_expr(field_occurrence(model, owner, field)).ty()
}

/// The type of that field, written out.
fn field_shown(model: &Model, owner: &str, field: &str) -> String {
  shown(model, field_type(model, owner, field))
}

/// An analysis of the root package `app`, `Flat`, holding the one file `schema.graphqlx`, whose
/// text is `text`, within `limits`.
fn analyze_text(text: &str, limits: &Limits) -> Analysis {
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)]);
  analyze(&root, &[], limits)
}

/// The ids `range` holds, in order.
fn ids(range: IdRange<MemberId>) -> Vec<MemberId> {
  range.into_iter().collect()
}

/// Each diagnostic's code, in the list's order.
fn codes(analysis: &Analysis) -> Vec<&'static str> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.code().as_str())
    .collect()
}

/// Each diagnostic's help, in the list's order.
fn helps(analysis: &Analysis) -> Vec<Option<&str>> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.help())
    .collect()
}

/// Each label of `diagnostic`, where it is and what it says, in order.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, &str)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), label.text()))
    .collect()
}

/// The diagnostic at `index` of the list.
fn nth(analysis: &Analysis, index: usize) -> &Diagnostic {
  analysis
    .diagnostics()
    .iter()
    .nth(index)
    .unwrap_or_else(|| panic!("no diagnostic {index}: {:#?}", analysis.diagnostics()))
}

/// Whether an `unsatisfied-bound` points at the occurrence `occurrence`.
fn refused(analysis: &Analysis, occurrence: TypeExprId) -> bool {
  let at = analysis.model().type_expr(occurrence).location();
  analysis
    .diagnostics()
    .iter()
    .any(|diagnostic: &Diagnostic| {
      diagnostic.code().as_str() == UNSATISFIED && diagnostic.primary() == at
    })
}

/// Every type occurrence a walk reaches, in its order.
#[derive(Default)]
struct Occurrences {
  types: Vec<TypeExprId>,
}

impl Visitor for Occurrences {
  fn type_expr(&mut self, _cx: &mut Cx<'_>, ty: TypeExprId) {
    self.types.push(ty);
  }
}

/// Every type occurrence of `analysis` the packages `packages` wrote, in the walk's order.
fn every_occurrence(analysis: &Analysis, packages: Packages) -> Vec<TypeExprId> {
  let mut walked = Occurrences::default();
  let _ = visit(analysis, packages, &mut walked);
  walked.types
}

/// `Node`, `Comparable` and `Ord`, which declares `Comparable`; `User`, which declares `Node` and
/// `Comparable<User>`, and `Other`, which declares `Comparable<User>` too; a definition whose
/// parameter is bounded by `Ord`; and a type whose fields have every type the accessors are asked
/// about.
const ASKED: &str = "interface Node {\n  id: ID\n}\n\
                     interface Comparable<X> {\n  compare(other: X): Int\n}\n\
                     interface Ord<X> implements Comparable<X> {\n  compare(other: X): Int\n}\n\
                     type User implements Node & Comparable<User> {\n  id: ID\n  \
                     compare(other: User): Int\n}\n\
                     type Other implements Comparable<User> {\n  compare(other: User): Int\n}\n\
                     type Outer<U> where U: Ord<U> {\n  u: U\n  ord: Ord<U>\n  \
                     cmp: Comparable<U>\n}\n\
                     type Probe {\n  user: User\n  sure: User!\n  other: Other\n  node: Node\n  \
                     int: Int\n  name: String!\n  float: Float\n  alias: NonZeroU32\n  \
                     generic: NonZero<u32>\n  float_key: NonZero<f64>\n  users: [User]\n  \
                     by_user: Comparable<User>\n  by_other: Comparable<Other>\n  \
                     by_sure: Comparable<User!>\n}\n";

#[test]
fn satisfies_answers_by_definition_and_satisfies_ty_exactly() {
  let analysis = analyze_one(ASKED);
  let model = analysis.model();
  let probe = |field: &str| -> TyId { field_type(model, "^::app::Probe", field) };
  // Each type is the one meant.
  for (field, written) in [
    ("sure", "User!"),
    ("name", "string!"),
    ("float", "f64"),
    ("alias", "NonZero<u32>"),
    ("float_key", "NonZero<f64>"),
    ("by_sure", "Comparable<User!>"),
  ] {
    assert_eq!(shown(model, probe(field)), written, "{field}");
  }
  let node = def(model, "^::app::Node");
  let comparable = def(model, "^::app::Comparable");
  let ord = def(model, "^::app::Ord");
  let map_key = model.builtins().map_key();

  // By definition, some instantiation of `Comparable`: each declares one.
  assert!(model.satisfies(probe("user"), comparable));
  assert!(model.satisfies(probe("other"), comparable));
  // Exactly: `User` declares `Comparable<User>`, and `Other` declares it too, and not
  // `Comparable<Other>`.
  assert!(model.satisfies_ty(probe("user"), probe("by_user")));
  assert!(model.satisfies_ty(probe("other"), probe("by_user")));
  assert!(!model.satisfies_ty(probe("other"), probe("by_other")));
  // The `!` of an argument is part of the interface type: `User` does not declare
  // `Comparable<User!>`.
  assert!(!model.satisfies_ty(probe("user"), probe("by_sure")));
  // Non-null is transparent on the type asked about.
  assert!(model.satisfies_ty(probe("sure"), probe("by_user")));
  assert!(model.satisfies(probe("sure"), node));
  // An interface satisfies itself.
  assert!(model.satisfies(probe("node"), node));
  assert!(model.satisfies_ty(probe("by_user"), probe("by_user")));

  // `MapKey`, intrinsically: an integer and `string`, whatever their nullness, and no float.
  assert!(model.satisfies(probe("int"), map_key));
  assert!(model.satisfies(probe("name"), map_key));
  assert!(!model.satisfies(probe("float"), map_key));
  // `NonZero` of an integer, read off both its spellings, which are one type; and not of a float.
  assert_eq!(probe("alias"), probe("generic"));
  assert!(model.satisfies(probe("alias"), map_key));
  assert!(!model.satisfies(probe("float_key"), map_key));
  // A list satisfies nothing.
  assert!(!model.satisfies(probe("users"), node));

  // `Outer`'s parameter satisfies its bound, `Ord`, exactly `Ord<U>`; and what `Ord` declares,
  // `Comparable`, exactly `Comparable<U>`; and not `Node`.
  let u = field_type(model, "^::app::Outer", "u");
  assert_eq!(shown(model, u), "Outer#0");
  assert!(model.satisfies(u, ord));
  assert!(model.satisfies_ty(u, field_type(model, "^::app::Outer", "ord")));
  assert!(model.satisfies(u, comparable));
  assert!(model.satisfies_ty(u, field_type(model, "^::app::Outer", "cmp")));
  assert!(!model.satisfies(u, node));

  // What is no interface on the right is satisfied by nothing, the type itself included.
  let user = def(model, "^::app::User");
  assert!(!model.satisfies(probe("user"), user));
  assert!(!model.satisfies_ty(probe("user"), probe("user")));
}

#[test]
fn a_bounds_right_side_is_an_interface() {
  // An object, an alias, which spells `NonZero<u32>`, and another parameter of the definition are
  // each refused at the right side; a path that names nothing is name resolution's alone.
  let text = "type User {\n  id: ID\n}\nscalar A<T> where T: User\n\
              scalar B<T> where T: NonZeroU32\nscalar C<T, U> where T: U\n\
              scalar D<T> where T: Missing\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let user = at(schema, text, "User", 1);
  let alias = at(schema, text, "NonZeroU32", 0);
  let other = named(schema, text, "T: ", "U", 0);
  let missing = at(schema, text, "Missing", 0);
  assert_eq!(
    found(&analysis),
    [
      (INVALID_BOUND, user, None),
      (INVALID_BOUND, alias, None),
      (INVALID_BOUND, other, None),
      (UNDEFINED, missing, None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let refused = "a bound's right side is an interface or a marker, which the type parameter must \
                 implement";
  assert_eq!(messages(&analysis)[..3], [refused, refused, refused]);
  let help = "name an interface, as `where T: Node` does, or an interface with type arguments, as \
              `where T: Comparable<T>` does";
  let helps: Vec<Option<&str>> = analysis
    .diagnostics()
    .iter()
    .take(3)
    .map(|diagnostic: &Diagnostic| diagnostic.help())
    .collect();
  assert_eq!(helps, [Some(help), Some(help), Some(help)]);

  // Each bound is still the model's, as written.
  let model = analysis.model();
  for (name, right) in [
    ("^::app::A", user),
    ("^::app::B", alias),
    ("^::app::C", other),
    ("^::app::D", missing),
  ] {
    let bounds = model.def(def(model, name)).bounds();
    assert_eq!(bounds.len(), 1, "{name}");
    assert_eq!(bounds[0].parameter(), 0, "{name}");
    assert_eq!(
      model.type_expr(bounds[0].interface()).location(),
      right,
      "{name}"
    );
  }
}

#[test]
fn a_declared_interface_satisfies_its_bound() {
  // `User` declares `Node`, and `i32` does not.
  let text = program("type Query {\n  good: Page<User>\n  bad: Page<Int>\n  fine: Int\n}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let bound = named(schema, &text, "T: ", "Node", 0);
  let used = at(schema, &text, "Page<Int>", 0);
  assert_eq!(
    found(&analysis),
    [(UNSATISFIED, used, Some(bound))],
    "{:#?}",
    analysis.diagnostics()
  );
  // It names the definition, the parameter and the bound's interface, and no type; it is labelled
  // at the bound and at the written type argument.
  assert_eq!(
    messages(&analysis),
    [
      "`Page` bounds its type parameter `T` by `Node`, and the type argument for `T` here does not \
       satisfy that bound"
    ]
  );
  assert_eq!(
    labels(nth(&analysis, 0)),
    [
      (bound, "the bound"),
      (named(schema, &text, "Page<", "Int", 0), "the type argument"),
    ]
  );
  assert_eq!(helps(&analysis), [Some(NAMED_HELP)]);

  // The occurrence keeps its type, which is one of `Page`'s instantiations; it marks its member
  // and its contribution, and its sibling marks nothing.
  let model = analysis.model();
  let bad = field_type(model, "^::app::Query", "bad");
  assert_eq!(shown(model, bad), "Page<i32>");
  assert!(
    model
      .instantiations(def(model, "^::app::Page"))
      .contains(&bad)
  );
  let query = def(model, "^::app::Query");
  let fields = ids(model.def(query).fields());
  assert!(!model.member(fields[0]).has_errors());
  assert!(model.member(fields[1]).has_errors());
  assert!(!model.member(fields[2]).has_errors());
  assert!(
    model
      .def(query)
      .contributions()
      .next()
      .is_some_and(|own| own.has_errors())
  );
}

#[test]
fn the_interface_itself_satisfies_its_bound() {
  let text = program("type Query {\n  node: Page<Node>\n}\n");
  let analysis = analyze_text(&text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn non_null_is_transparent_on_the_argument() {
  // The argument is judged without its `!`: its own type is another `TyId`.
  let text = program("type Query {\n  user: Page<User!>\n  node: Page<Node!>\n}\n");
  let analysis = analyze_text(&text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_list_satisfies_no_bound() {
  // Not even `MapKey`.
  let text = program("type Query {\n  page: Page<[User]>\n  map: HashMap<[Int], Int>\n}\n");
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let hash_map = def(model, "^::std::HashMap");
  let key_bound = model
    .type_expr(model.def(hash_map).bounds()[0].interface())
    .location();
  assert_eq!(
    found(&analysis),
    [
      (
        UNSATISFIED,
        at(schema, &text, "Page<[User]>", 0),
        Some(named(schema, &text, "T: ", "Node", 0))
      ),
      (
        UNSATISFIED,
        at(schema, &text, "HashMap<[Int], Int>", 0),
        Some(key_bound)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    labels(nth(&analysis, 1))[1],
    (at(schema, &text, "[Int]", 0), "the type argument")
  );
  assert_eq!(helps(&analysis), [Some(LIST_HELP), Some(LIST_HELP)]);
}

#[test]
fn map_keys_are_intrinsic() {
  // An integer, `bool`, `string`, `id`, an enum, other widths, and a scalar that declares `MapKey`
  // are keys; a float and an object are not. Each is told by its definition, never by a spelling.
  let text = "enum Color {\n  RED\n}\nscalar Keyish implements MapKey\ntype Thing {\n  id: ID\n}\n\
              type Query {\n  a: HashMap<Int, Int>\n  b: HashMap<Boolean, Int>\n  \
              c: HashMap<String, Int>\n  d: HashMap<ID, Int>\n  e: HashMap<Color, Int>\n  \
              f: HashSet<u8>\n  g: BTreeMap<i64, Int>\n  h: HashMap<Keyish, Int>\n  \
              x: HashMap<Float, Int>\n  y: HashMap<Thing, Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let key_bound = model
    .type_expr(model.def(def(model, "^::std::HashMap")).bounds()[0].interface())
    .location();
  assert_eq!(
    found(&analysis),
    [
      (
        UNSATISFIED,
        at(schema, text, "HashMap<Float, Int>", 0),
        Some(key_bound)
      ),
      (
        UNSATISFIED,
        at(schema, text, "HashMap<Thing, Int>", 0),
        Some(key_bound)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  // The root package's own `String` replaces `string`, and keeps its identity: still a key.
  let analysis = analyze_one("scalar String\ntype Query {\n  m: HashMap<String, Int>\n}\n");
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_non_zero_integer_is_a_key() {
  // `NonZero` of an integer is a key, told by `NonZero`'s definition and its argument's: the alias
  // and the generic form are one key type.
  let text = "type Query {\n  a: HashMap<NonZeroU32, Int>\n  b: HashMap<NonZero<u32>, Int>\n  \
              c: HashSet<NonZeroU64>\n}\n";
  let analysis = analyze_one(text);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  assert_eq!(
    field_type(model, "^::app::Query", "a"),
    field_type(model, "^::app::Query", "b")
  );

  // A `NonZero` of a float is no key, and its map is not refused for it: the rule on `NonZero`'s
  // argument refuses it, once, where it is written.
  let text = "type Query {\n  d: HashMap<NonZero<Float>, Int>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert!(
    !codes(&analysis).contains(&UNSATISFIED),
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    found(&analysis),
    [(
      NONZERO,
      at(schema, text, "NonZero<Float>", 0),
      Some(at(schema, text, "Float", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // That is for `MapKey` alone: a `NonZero` of an integer, which no rule refuses, fails any other
  // bound it does not satisfy, as every type does.
  let text = "interface Node {\n  id: ID\n}\nscalar P<T> where T: Node\n\
              type Query {\n  p: P<NonZeroU32>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, text, "P<NonZeroU32>", 0),
      Some(named(schema, text, "T: ", "Node", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_generic_bound_is_checked_exactly() {
  // `Other` declares `Comparable<User>`, and `Sorted<Other>` asks `Comparable<Other>`.
  let text = program(
    "type Other implements Comparable<User> {\n  compare(other: User): Int\n}\n\
     type Query {\n  user: Sorted<User>\n  other: Sorted<Other>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, &text, "Sorted<Other>", 0),
      Some(at(schema, &text, "Comparable<T>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_argument_is_substituted_as_it_is_written() {
  // The `!` of an argument stays inside the bound's interface: `Sorted<User!>` asks
  // `Comparable<User!>`, which `User` does not declare, and `Strict`, which declares it, serves
  // `Sorted<Strict!>` and not `Sorted<Strict>`. A parameter passed on with its own `!`; a slot a
  // default filled from a non-null argument; and a set's element, non-null however it is written.
  let text = program(
    "type Strict implements Comparable<Strict!> {\n  compare(other: Strict!): Int\n}\n\
     type Out3<U> where U: Comparable<U> {\n  s: Sorted<U!>\n}\n\
     type Out4<U> where U: Comparable<U!> {\n  s: Sorted<U!>\n}\n\
     type P2<T, U = T> where U: Comparable<U> {\n  t: T\n}\n\
     scalar Bag<T> @set where T: Comparable<T>\n\
     type Query {\n  a: Sorted<User!>\n  b: Sorted<Strict!>\n  c: Sorted<Strict>\n  \
     d: P2<User!>\n  e: Bag<Strict>\n  f: Bag<User>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let sorted = Some(at(schema, &text, "Comparable<T>", 0));
  let p2 = Some(at(schema, &text, "Comparable<U>", 1));
  let bag = Some(at(schema, &text, "Comparable<T>", 1));
  assert_eq!(
    found(&analysis),
    [
      (UNSATISFIED, at(schema, &text, "Sorted<U!>", 0), sorted),
      (UNSATISFIED, at(schema, &text, "Sorted<User!>", 0), sorted),
      (UNSATISFIED, at(schema, &text, "Sorted<Strict>", 0), sorted),
      (UNSATISFIED, at(schema, &text, "P2<User!>", 0), p2),
      (UNSATISFIED, at(schema, &text, "Bag<User>", 0), bag),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // The filled slot is labelled at the default that filled it.
  assert_eq!(
    labels(nth(&analysis, 3))[1],
    (
      named(schema, &text, "U = ", "T", 0),
      "filled from this default"
    )
  );
}

#[test]
fn a_generic_argument_satisfies_through_its_own_arguments() {
  // `Box<User>` declares `Comparable<Box<User>>`, its entry with `User` for `T`. `Wrap<User>`
  // declares `Comparable<User>`, and `Sorted<Wrap<User>>` asks `Comparable<Wrap<User>>`.
  let text = program(
    "type Wrap<T> implements Comparable<T> {\n  compare(other: T): Int\n}\n\
     type Query {\n  boxed: Sorted<Box<User>>\n  wrapped: Sorted<Wrap<User>>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, &text, "Sorted<Wrap<User>>", 0),
      Some(at(schema, &text, "Comparable<T>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn an_open_entry_is_matched_and_never_compared_by_id() {
  // Inside `Box`, both bounds ask `Comparable<Box<T>>`, the very type `Box`'s entry has. `Box<T>`
  // declares it; `Box<i32>` declares `Comparable<Box<i32>>`, another type.
  let text = "interface Comparable<X> {\n  compare(other: X): Int\n}\n\
              scalar Pin<X, Y> where X: Comparable<Y>\n\
              type Box<T> implements Comparable<Box<T>> {\n  item: T\n  \
              compare(other: Box<T>): Int\n  same: Pin<Box<T>, Box<T>>\n  \
              odd: Pin<Box<Int>, Box<T>>\n}\n";
  let analysis = analyze_one(text);
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, text, "Pin<Box<Int>, Box<T>>", 0),
      Some(at(schema, text, "Comparable<Y>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_parameter_satisfies_through_its_bounds() {
  // `Ord<U>` declares `Comparable<U>`, the bound; `Bare`'s parameter has no bound; `Same`'s bound
  // is the bound; `Node` declares nothing.
  let definitions = "type Outer<U> where U: Ord<U> {\n  s: Sorted<U>\n}\n\
                     type Bare<U> {\n  s: Sorted<U>\n}\n\
                     type Same<U> where U: Comparable<U> {\n  s: Sorted<U>\n}\n\
                     type Other2<U> where U: Node {\n  s: Sorted<U>\n}\n";
  // Once, where it is written, whatever uses `Bare` has.
  for uses in ["", "type Query {\n  a: Bare<User>\n  b: Bare<Int>\n}\n"] {
    let text = program(&format!("{definitions}{uses}"));
    let analysis = analyze_text(&text, &Limits::new());
    let schema = schema_file(&analysis);
    let sorted = Some(at(schema, &text, "Comparable<T>", 0));
    assert_eq!(
      found(&analysis),
      [
        (UNSATISFIED, at(schema, &text, "Sorted<U>", 1), sorted),
        (UNSATISFIED, at(schema, &text, "Sorted<U>", 3), sorted),
      ],
      "{uses}: {:#?}",
      analysis.diagnostics()
    );
    assert_eq!(helps(&analysis), [Some(PARAM_HELP), Some(PARAM_HELP)]);
  }
}

#[test]
fn satisfaction_is_by_declaration() {
  // `Lazy` declares `Ord<Lazy>` and not `Comparable<Lazy>`, which `Ord` declares: what an interface
  // declares is not followed. GraphQL's rule on transitive interfaces, L4's, refuses `Lazy` itself.
  let text = program(
    "type Lazy implements Ord<Lazy> {\n  compare(other: Lazy): Int\n}\n\
     type Query {\n  s: Sorted<Lazy>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, &text, "Sorted<Lazy>", 0),
      Some(at(schema, &text, "Comparable<T>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // Written too, it holds.
  let text = program(
    "type Lazy implements Ord<Lazy> & Comparable<Lazy> {\n  compare(other: Lazy): Int\n}\n\
     type Query {\n  s: Sorted<Lazy>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_filled_argument_is_judged_like_a_written_one() {
  // `P`'s `U` is filled from `T`: `P<Int>` is refused at the use, labelled at that default.
  let text = program(
    "type P<T, U = T> where U: Node {\n  t: T\n}\n\
     type Query {\n  good: P<User>\n  bad: P<Int>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let bound = named(schema, &text, "U: ", "Node", 0);
  assert_eq!(
    found(&analysis),
    [(UNSATISFIED, at(schema, &text, "P<Int>", 0), Some(bound))],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    labels(nth(&analysis, 0)),
    [
      (bound, "the bound"),
      (
        named(schema, &text, "U = ", "T", 0),
        "filled from this default"
      ),
    ]
  );

  // A default no use fills says nothing; each use it fills is refused.
  let definition = "type Q<T, C = Int> where C: Node {\n  t: T\n}\n";
  let text = program(definition);
  let analysis = analyze_text(&text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
  let text = program(&format!(
    "{definition}type Query {{\n  a: Q<User>\n  b: Q<User>\n}}\n"
  ));
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let bound = Some(named(schema, &text, "C: ", "Node", 0));
  assert_eq!(
    found(&analysis),
    [
      (UNSATISFIED, at(schema, &text, "Q<User>", 0), bound),
      (UNSATISFIED, at(schema, &text, "Q<User>", 1), bound),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  for index in 0..2 {
    assert_eq!(
      labels(nth(&analysis, index))[1],
      (
        named(schema, &text, "C = ", "Int", 0),
        "filled from this default"
      )
    );
  }
}

#[test]
fn a_defaults_instantiation_is_judged_where_it_is_written_and_no_derived_one() {
  // `Cursor<T>` is judged where it is written, through `Page2`'s `T`, which no bound has; the
  // `Cursor<User>` and `Cursor<i32>` filling `Page2<User>` and `Page2<Int>` derives are not.
  let text = program(
    "scalar Cursor<X> where X: Node\nscalar Page2<T, C = Cursor<T>>\n\
     type Query {\n  good: Page2<User>\n  bad: Page2<Int>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, &text, "Cursor<T>", 0),
      Some(named(schema, &text, "X: ", "Node", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(helps(&analysis), [Some(PARAM_HELP)]);
}

#[test]
fn each_occurrence_reports_once_at_its_first_failing_bound() {
  // Three occurrences of one type that fails both its bounds: three diagnostics, each for the
  // first. And bounds go in the order they are written, not the parameters'.
  let text = program(
    "scalar D<T> where T: Node & Comparable<T>\nscalar D2<A, B> where B: Node A: Node\n\
     type Query {\n  a: D<Int>\n  b: D<Int>\n  c: D<Int>\n  d: D2<Int, Int>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let first = Some(named(schema, &text, "T: ", "Node", 1));
  assert_eq!(
    found(&analysis),
    [
      (UNSATISFIED, at(schema, &text, "D<Int>", 0), first),
      (UNSATISFIED, at(schema, &text, "D<Int>", 1), first),
      (UNSATISFIED, at(schema, &text, "D<Int>", 2), first),
      (
        UNSATISFIED,
        at(schema, &text, "D2<Int, Int>", 0),
        Some(named(schema, &text, "B: ", "Node", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let message = |name: &str, parameter: &str| -> String {
    format!(
      "`{name}` bounds its type parameter `{parameter}` by `Node`, and the type argument for \
       `{parameter}` here does not satisfy that bound"
    )
  };
  assert_eq!(
    messages(&analysis),
    [
      message("D", "T"),
      message("D", "T"),
      message("D", "T"),
      message("D2", "B"),
    ]
  );
  assert_eq!(
    labels(nth(&analysis, 3))[1],
    (
      named(schema, &text, "D2<Int, ", "Int", 0),
      "the type argument"
    )
  );
}

#[test]
fn nested_occurrences_are_judged_each() {
  // An outer and an inner occurrence each for its own argument; an outer whose inner one holds
  // and is no `Node`; and an inner that fails inside an outer that holds, and keeps its type.
  let text = program(
    "scalar Key<T> implements MapKey where T: Node\n\
     type Query {\n  a: HashMap<[Int], HashSet<Float>>\n  b: Page<Page<User>>\n  \
     c: HashMap<Key<Int>, Int>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  let model = analysis.model();
  let bound_of = |name: &str| -> Option<Location> {
    let def = def(model, name);
    Some(
      model
        .type_expr(model.def(def).bounds()[0].interface())
        .location(),
    )
  };
  assert_eq!(
    found(&analysis),
    [
      (
        UNSATISFIED,
        at(schema, &text, "HashMap<[Int], HashSet<Float>>", 0),
        bound_of("^::std::HashMap")
      ),
      (
        UNSATISFIED,
        at(schema, &text, "HashSet<Float>", 0),
        bound_of("^::std::HashSet")
      ),
      (
        UNSATISFIED,
        at(schema, &text, "Page<Page<User>>", 0),
        bound_of("^::app::Page")
      ),
      (
        UNSATISFIED,
        at(schema, &text, "Key<Int>", 0),
        bound_of("^::app::Key")
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(model, "^::app::Query", "a"),
    "HashMap<[i32]!, HashSet<f64!>>"
  );
  assert_eq!(
    field_shown(model, "^::app::Query", "c"),
    "HashMap<Key<i32>!, i32>"
  );
}

#[test]
fn an_occurrence_in_a_bound_is_judged() {
  // `Sorted<T>`, an argument of `D`'s bound, asks `Comparable<T>`, and `T`'s one bound is
  // `Comparable<Sorted<T>>`, which `Comparable` does not declare. `Shape<S>` holds by its
  // parameter's own bound, and `Shape<Circle>` by `Circle`'s declaration.
  let text = program(
    "scalar D<T> where T: Comparable<Sorted<T>>\n\
     interface Shape<S> where S: Shape<S> {\n  id: ID\n}\n\
     type Circle implements Shape<Circle> {\n  id: ID\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, &text, "Sorted<T>", 1),
      Some(at(schema, &text, "Comparable<T>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn what_a_phase_refused_is_not_judged() {
  // An argument that names nothing; a bound that names nothing; a bound lowering refused; and a
  // count: each is said once, by what refused it, and the bounds of none of them are judged.
  let text = program(
    "scalar K<T> where T: Missing\nscalar N<T> where T: User\n\
     type Query {\n  a: Page<Missing>\n  b: K<Int>\n  c: N<Int>\n  d: Page<User, User>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (UNDEFINED, at(schema, &text, "Missing", 0), None),
      (INVALID_BOUND, named(schema, &text, "T: ", "User", 0), None),
      (UNDEFINED, at(schema, &text, "Missing", 1), None),
      (
        ARITY,
        at(schema, &text, "Page<User, User>", 0),
        Some(named(schema, &text, "type ", "Page", 0))
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A type of `count` fields, each typed `ty` and named `f` and its index.
fn fields(count: usize, ty: &str) -> String {
  let mut text = String::from("type Query {\n");
  for index in 0..count {
    text.push_str(&format!("  f{index}: {ty}\n"));
  }
  text.push_str("}\n");
  text
}

#[test]
fn a_hundred_occurrences_of_one_instantiation_are_judged_once() {
  // One judgement of `Page<User>` takes two steps: one for its bound, and one for `User`'s first
  // `implements` entry, `Node`. The ninety-nine other occurrences look its verdict up.
  let text = program(&fields(100, "Page<User>"));
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(2));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // One step short, each occurrence is reported, and each keeps its type.
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(1));
  let schema = schema_file(&analysis);
  let expected: Vec<(&str, Location, Option<Location>)> = (0..100)
    .map(|nth: usize| (LIMIT, at(schema, &text, "Page<User>", nth), None))
    .collect();
  assert_eq!(found(&analysis), expected);
  assert!(
    messages(&analysis)
      .iter()
      .all(|message: &String| message.contains("`Limits::max_bound_steps`, 1"))
  );
  assert_eq!(
    helps(&analysis)[0],
    Some(
      "raise the bound with `Limits::with_max_bound_steps`, or write fewer instantiations of \
       definitions with bounds"
    )
  );
  let model = analysis.model();
  for index in 0..100 {
    assert_eq!(
      field_shown(model, "^::app::Query", &format!("f{index}")),
      "Page<User>"
    );
  }
}

#[test]
fn a_type_is_judged_once_whatever_its_nullness() {
  // The two nullnesses of `Page<User>` share one verdict of two steps.
  let mut uses = fields(50, "Page<User>");
  uses.push_str(&fields(50, "Page<User>!").replace("Query", "Sure"));
  let text = program(&uses);
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(2));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // `Page<Int>` takes one step, and fails: each of its occurrences, of both nullnesses, reports.
  let mut uses = fields(50, "Page<Int>");
  uses.push_str(&fields(50, "Page<Int>!").replace("Query", "Sure"));
  let text = program(&uses);
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(2));
  assert_eq!(codes(&analysis), [UNSATISFIED; 100]);
}

#[test]
fn a_pair_is_judged_once() {
  // Fifty definitions bounded by `Node`, each instantiated with `Wide`: fifty steps for the bounds,
  // and ten for the one pair of `Wide` and `Node`, whose nine first entries are passed over. Asked
  // for each, the pair would take five hundred and fifty.
  let mut text = String::from("interface Node {\n  id: ID\n}\n");
  let mut interfaces: Vec<String> = Vec::new();
  for index in 1..=9 {
    text.push_str(&format!("interface A{index} {{\n  id: ID\n}}\n"));
    interfaces.push(format!("A{index}"));
  }
  interfaces.push(String::from("Node"));
  text.push_str(&format!(
    "type Wide implements {} {{\n  id: ID\n}}\n",
    interfaces.join(" & ")
  ));
  let mut uses = String::from("type Query {\n");
  for index in 1..=50 {
    text.push_str(&format!("scalar D{index}<T> where T: Node\n"));
    uses.push_str(&format!("  f{index}: D{index}<Wide>\n"));
  }
  uses.push_str("}\n");
  text.push_str(&uses);
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(64));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn judging_stops_at_its_budget() {
  // `Sorted<User>` takes five steps: one for its bound, two to put `User` into `Comparable<T>`,
  // its root and its one argument, and two for `User`'s entries, `Node` and `Comparable<User>`.
  let text = program("type Query {\n  s: Sorted<User>\n}\n");
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(4));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, &text, "Sorted<User>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "s"),
    "Sorted<User>"
  );
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(5));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_mismatch_takes_a_step_for_each_pair_compared_up_to_it() {
  // `Need<G<Int, Float>>` takes five steps: one for its bound, one for `G`'s entry, one for the
  // entry's root pair, and one for each argument compared, the first, which holds, and the second,
  // which differs. A matcher that paid for a node's arguments as it met the node would take six.
  // That no step does more than one pair's work holds by construction, and is checked by reading:
  // a test sees the steps, not the work.
  let text = "interface I<A, B, C> {\n  id: ID\n}\n\
              type G<T, U> implements I<T, U, T> {\n  id: ID\n}\n\
              scalar Need<X> where X: I<Int, Int, Int>\n\
              type Query {\n  need: Need<G<Int, Float>>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_bound_steps(5));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, text, "Need<G<Int, Float>>", 0),
      Some(at(schema, text, "I<Int, Int, Int>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let analysis = analyze_text(text, &Limits::new().with_max_bound_steps(4));
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "Need<G<Int, Float>>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn judging_has_a_budget_of_its_own() {
  // A substitution budget of 0 refuses no map: judging draws on its own.
  let text = program("type Query {\n  map: HashMap<string, Int>\n  page: Page<User>\n}\n");
  let analysis = analyze_text(&text, &Limits::new().with_max_substitutions(0));
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // A bound-step budget of 0 refuses every instantiation of a definition with a bound, and fills
  // what is left out all the same.
  let text = program(
    "type Filled<T, C = [T]> {\n  c: C\n}\n\
     type Query {\n  map: HashMap<string, Int>\n  page: Page<User>\n  filled: Filled<User>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new().with_max_bound_steps(0));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [
      (LIMIT, at(schema, &text, "HashMap<string, Int>", 0), None),
      (LIMIT, at(schema, &text, "Page<User>", 0), None),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "filled"),
    "Filled<User, [User]>"
  );

  // Fills that spend their budget leave a later map judged.
  let text =
    "scalar Fill<T, U = [T]>\ntype Query {\n  f: Fill<Int>\n  m: HashMap<string, Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_substitutions(1));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "Fill<Int>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(messages(&analysis)[0].contains("`Limits::max_substitutions`, 1"));
}

#[test]
fn a_bound_under_its_arguments_counts_as_an_instantiation() {
  // The program writes two instantiations, `C<S#0>` in the bound and `S<i32>`; judging `S<i32>`
  // derives a third, `C<i32>`.
  let text =
    "interface C<X> {\n  v: X\n}\nscalar S<T> where T: C<T>\ntype Query {\n  a: S<Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_instantiations(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "S<Int>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(messages(&analysis)[0].contains("`Limits::max_instantiations`, 2"));
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "a"),
    "S<i32>"
  );

  // With room for it, the bound is judged, and fails.
  let analysis = analyze_text(text, &Limits::new().with_max_instantiations(3));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, text, "S<Int>", 0),
      Some(at(schema, text, "C<T>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_bound_under_its_arguments_nests_as_deep_as_it_is_built() {
  // No type written here nests more than two levels deep, and `I<[Box<i32>]>`, which judging
  // `S<Box<Int>>` derives, nests three: under a depth of two the occurrence is reported and keeps
  // its type, and under three its bound is judged, and fails.
  let text = "interface I<A> {\n  id: ID\n}\nscalar Box<X>\nscalar S<T> where T: I<[T]>\n\
              type Query {\n  a: S<Box<Int>>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(2));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "S<Box<Int>>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  assert!(messages(&analysis)[0].contains("`Limits::max_generic_depth`, 2"));
  assert_eq!(
    field_shown(analysis.model(), "^::app::Query", "a"),
    "S<Box<i32>>"
  );
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  assert_eq!(
    found(&analysis),
    [(
      UNSATISFIED,
      at(schema, text, "S<Box<Int>>", 0),
      Some(at(schema, text, "I<[T]>", 0))
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  // A list the bound derives is checked as it is built: `[[Box<Box<i32>>]]` nests four levels
  // deep, where everything written nests three.
  let text = "interface I<A> {\n  id: ID\n}\nscalar Box<X>\nscalar S2<T> where T: I<[[T]]>\n\
              type Query {\n  a: S2<Box<Box<Int>>>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_generic_depth(3));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "S2<Box<Box<Int>>>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn a_type_that_doubles_is_judged_in_its_distinct_nodes() {
  // Each `Dk` fills its `Y` with a `D(k-1)` of a pair of its `X`: `D20<X>`'s tree holds more than
  // 2^20 nodes, over a few dozen distinct ones. `S<E<Int>>`'s bound, put under its argument, and
  // `E`'s entry, compared with it under `E<i32>`'s argument, are each that type, so a rebuild or a
  // comparison that walked either as a tree would pass the budget.
  let mut text = String::from("scalar Pair<A, B>\nscalar D1<X, Y = Pair<X, X>>\n");
  for k in 2..=20 {
    text.push_str(&format!("scalar D{k}<X, Y = D{}<Pair<X, X>>>\n", k - 1));
  }
  text.push_str(
    "interface I<Z> {\n  id: ID\n}\ntype E<T> implements I<D20<E<T>>> {\n  id: ID\n}\n\
     scalar S<X> where X: I<D20<X>>\ntype Query {\n  a: S<E<Int>>\n}\n",
  );
  let analysis = analyze_text(&text, &Limits::new());
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn the_built_in_library_is_judged_whatever_the_limits() {
  // At 0, every limit leaves the library alone: it writes no instantiation of a definition with a
  // bound, and its values cost nothing to complete. The root package holds its query root alone.
  let limits = Limits::new()
    .with_max_generic_depth(0)
    .with_max_instantiations(0)
    .with_max_substitutions(0)
    .with_max_bound_steps(0)
    .with_max_value_steps(0);
  let root: Package<&str> =
    Package::new("app", Layout::Flat, Tier::Std).with_files([SourceFile::new(
      "schema.graphqlx",
      "type Query {\n  x: Int\n}\n",
    )]);
  let analysis = analyze(&root, &[], &limits);
  assert!(
    analysis.diagnostics().is_empty(),
    "{:#?}",
    analysis.diagnostics()
  );

  // The root package's map is refused beside it, in the root package's file alone.
  let text = "type Query {\n  m: HashMap<string, Int>\n}\n";
  let analysis = analyze_text(text, &Limits::new().with_max_bound_steps(0));
  let schema = schema_file(&analysis);
  assert_eq!(
    found(&analysis),
    [(LIMIT, at(schema, text, "HashMap<string, Int>", 0), None)],
    "{:#?}",
    analysis.diagnostics()
  );
  let model = analysis.model();
  for diagnostic in analysis.diagnostics() {
    let source = SourceId::new(diagnostic.primary().source());
    for package in 0..3 {
      assert!(
        !model
          .package(PackageId::new(package))
          .sources()
          .contains(source)
      );
    }
  }
}

/// Instantiations of definitions whose bounds name no parameter, of every shape: a declared
/// interface, the interface itself, a non-null argument, a list, and map keys and set elements
/// that are keys and that are not.
const CLOSED: &str = "enum Color {\n  RED\n}\n\
                      type Closed {\n  a: Page<User>\n  b: Page<Int>\n  c: Page<User!>\n  \
                      d: Page<Node>\n  e: Page<[User]>\n  f: HashMap<string, Int>\n  \
                      g: HashMap<Float, Int>\n  h: HashSet<NonZeroU8>\n  i: BTreeSet<Color>\n  \
                      j: HashMap<[Int], Int>\n}\n";

/// Bounds that name their parameter, each with a type meant to satisfy it and one meant not to,
/// and each interface type a bound asks written as a field beside its use: under a list; under a
/// set's element, which is non-null however it is written; twice; inside a default filled; under a
/// default that names a later parameter; a type parameter of the definition the use is written in;
/// and a non-null argument.
const NAMING: &str = "interface I<A> {\n  id: ID\n}\ninterface I2<A, B> {\n  id: ID\n}\n\
                      scalar Keys<E> @set\nscalar Box2<X, Y = [X]>\n\
                      scalar Box3<X, Y = Z, Z = [X]>\n\
                      scalar L<T> where T: I<[T]>\nscalar K2<T> where T: I<Keys<T>>\n\
                      scalar Tw<T> where T: I2<T, T>\nscalar Fd<T> where T: I<Box2<T>>\n\
                      scalar Fw<T> where T: I<Box3<T>>\n\
                      type Lh implements I<[Lh]> {\n  id: ID\n}\n\
                      type Lf implements I<Lf> {\n  id: ID\n}\n\
                      type Kh implements I<Keys<Kh>> {\n  id: ID\n}\n\
                      type Kf implements I<Keys<Int>> {\n  id: ID\n}\n\
                      type Th implements I2<Th, Th> {\n  id: ID\n}\n\
                      type Tf implements I2<Tf, Int> {\n  id: ID\n}\n\
                      type Fh implements I<Box2<Fh>> {\n  id: ID\n}\n\
                      type Ff implements I<Box2<Ff, Ff>> {\n  id: ID\n}\n\
                      type Wh implements I<Box3<Wh>> {\n  id: ID\n}\n\
                      type Wf implements I<Box3<Wf, Wf>> {\n  id: ID\n}\n\
                      type Nn implements Comparable<Nn!> {\n  compare(other: Nn!): Int\n}\n\
                      type Gen<V> where V: I<[V]> {\n  l: L<V>\n  j: I<[V]>\n}\n\
                      type Gen2<V> where V: I<V> {\n  l: L<V>\n  j: I<[V]>\n}\n\
                      type Uses {\n  lh: L<Lh>\n  j_lh: I<[Lh]>\n  lf: L<Lf>\n  j_lf: I<[Lf]>\n  \
                      kh: K2<Kh>\n  j_kh: I<Keys<Kh>>\n  kf: K2<Kf>\n  j_kf: I<Keys<Kf>>\n  \
                      th: Tw<Th>\n  j_th: I2<Th, Th>\n  tf: Tw<Tf>\n  j_tf: I2<Tf, Tf>\n  \
                      fh: Fd<Fh>\n  j_fh: I<Box2<Fh>>\n  ff: Fd<Ff>\n  j_ff: I<Box2<Ff>>\n  \
                      wh: Fw<Wh>\n  j_wh: I<Box3<Wh>>\n  wf: Fw<Wf>\n  j_wf: I<Box3<Wf>>\n  \
                      nh: Sorted<Nn!>\n  j_nh: Comparable<Nn!>\n  nf: Sorted<User!>\n  \
                      j_nf: Comparable<User!>\n}\n";

/// Each use of `NAMING` whose one bound names its parameter: the type that writes it, the field it
/// types, the field typed with the interface type its bound asks, and whether it is meant to hold.
const ASKS: [(&str, &str, &str, bool); 14] = [
  ("^::app::Uses", "lh", "j_lh", true),
  ("^::app::Uses", "lf", "j_lf", false),
  ("^::app::Uses", "kh", "j_kh", true),
  ("^::app::Uses", "kf", "j_kf", false),
  ("^::app::Uses", "th", "j_th", true),
  ("^::app::Uses", "tf", "j_tf", false),
  ("^::app::Uses", "fh", "j_fh", true),
  ("^::app::Uses", "ff", "j_ff", false),
  ("^::app::Uses", "wh", "j_wh", true),
  ("^::app::Uses", "wf", "j_wf", false),
  ("^::app::Uses", "nh", "j_nh", true),
  ("^::app::Uses", "nf", "j_nf", false),
  ("^::app::Gen", "l", "j", true),
  ("^::app::Gen2", "l", "j", false),
];

#[test]
fn the_phase_and_the_accessor_agree() {
  // Bounds that name no parameter: for every occurrence of a definition with a bound, every
  // argument satisfies its bound's interface type exactly when no `unsatisfied-bound` points at
  // the occurrence.
  let text = program(CLOSED);
  let analysis = analyze_text(&text, &Limits::new());
  let model = analysis.model();
  let mut judged = 0;
  let mut failed = 0;
  for occurrence in every_occurrence(&analysis, Packages::Root) {
    let TypeExprKind::Named { def, .. } = model.type_expr(occurrence).kind() else {
      continue;
    };
    let bounds = model.def(def).bounds();
    let TyKind::Named { args, .. } = model.ty(model.type_expr(occurrence).ty()).kind() else {
      continue;
    };
    if bounds.is_empty() {
      continue;
    }
    let mut holds = true;
    for bound in bounds {
      let goal = model.type_expr(bound.interface()).ty();
      assert!(model.ty(goal).is_closed());
      holds &= model.satisfies_ty(args[bound.parameter() as usize], goal);
    }
    assert_eq!(
      refused(&analysis, occurrence),
      !holds,
      "{:?}",
      model.type_expr(occurrence)
    );
    judged += 1;
    if !holds {
      failed += 1;
    }
  }
  assert_eq!((judged, failed), (10, 4));

  // Bounds that name their parameter: the interface type each asks is the one written beside it,
  // which the pass's rebuild must reach by identity, as typing it directly does.
  let text = program(NAMING);
  let analysis = analyze_text(&text, &Limits::new());
  let model = analysis.model();
  for (owner, used, written, meant) in ASKS {
    let occurrence = field_occurrence(model, owner, used);
    let TyKind::Named { def, args, .. } = model.ty(model.type_expr(occurrence).ty()).kind() else {
      panic!("{owner}.{used} is a named type");
    };
    let bound = model.def(def).bounds()[0];
    let argument = args[bound.parameter() as usize];
    let goal = field_type(model, owner, written);
    let holds = model.satisfies_ty(argument, goal);
    assert_eq!(holds, meant, "{owner}.{used}");
    assert_eq!(refused(&analysis, occurrence), !holds, "{owner}.{used}");
  }
  // And nothing else is refused.
  assert_eq!(
    codes(&analysis),
    [UNSATISFIED; 7],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The root package's files: §2's definitions, a dependency's bounded definition, and three
/// refusals, one of a bound that names its parameter.
const ORDERED_APP: [(&str, &str); 2] = [
  (
    "a.graphqlx",
    "type Query {\n  good: Sorted<User>\n  bad: Sorted<Int>\n  far: ^::geo::Keyed<Int>\n  \
     map: HashMap<Float, Int>\n  near: ^::geo::Keyed<^::geo::Spot>\n}\n",
  ),
  (
    "b.graphqlx",
    "interface Node {\n  id: ID\n}\ninterface Comparable<X> {\n  compare(other: X): Int\n}\n\
     type User implements Node & Comparable<User> {\n  id: ID\n  compare(other: User): Int\n}\n\
     type Sorted<T> where T: Comparable<T> {\n  items: [T]\n}\n",
  ),
];

/// The dependency `geo`'s file.
const ORDERED_GEO: (&str, &str) = (
  "g.graphqlx",
  "interface Mark {\n  id: ID\n}\nscalar Keyed<T> where T: Mark\n\
   type Spot implements Mark {\n  id: ID\n}\n",
);

/// The type `id`, written out as `shown` writes it, each type in it, `id` itself and each argument
/// and element at any depth, after its own id and a `:`.
fn structure(model: &Model, id: TyId) -> String {
  let ty = model.ty(id);
  let mut text = match ty.kind() {
    TyKind::Named { def, args, .. } => {
      let name = model.def(def).name();
      if args.is_empty() {
        String::from(name)
      } else {
        let args: Vec<String> = args
          .iter()
          .map(|arg: &TyId| structure(model, *arg))
          .collect();
        format!("{name}<{}>", args.join(", "))
      }
    }
    TyKind::Param { def, index, .. } => format!("{}#{index}", model.def(def).name()),
    TyKind::List { element, .. } => format!("[{}]", structure(model, element)),
    _ => String::from("?"),
  };
  if ty.non_null() {
    text.push('!');
  }
  format!("{}:{text}", id.get())
}

/// Every occurrence's type, written out with its id and its arguments' ids, and every diagnostic.
fn rendered(analysis: &Analysis) -> Vec<String> {
  let model = analysis.model();
  let mut lines: Vec<String> = Vec::new();
  for id in every_occurrence(analysis, Packages::All) {
    let ty = model.type_expr(id).ty();
    lines.push(format!("{} {}", id.get(), structure(model, ty)));
  }
  for diagnostic in analysis.diagnostics() {
    lines.push(format!("{diagnostic:?}"));
  }
  lines
}

#[test]
fn reversing_files_and_dependencies_changes_no_verdict() {
  let mut runs: Vec<Vec<String>> = Vec::new();
  for reversed in [false, true] {
    let mut files = ORDERED_APP.to_vec();
    if reversed {
      files.reverse();
    }
    let root = package("app", Layout::Flat, &files).with_dependencies(["geo", "astro"]);
    let mut dependencies = vec![
      package("geo", Layout::Flat, &[ORDERED_GEO]),
      package(
        "astro",
        Layout::Flat,
        &[("s.graphqlx", "type Star {\n  near: Int\n}\n")],
      ),
    ];
    if reversed {
      dependencies.reverse();
    }
    let analysis = analyze(&root, &dependencies, &Limits::new());
    assert_eq!(
      codes(&analysis),
      [UNSATISFIED, UNSATISFIED, UNSATISFIED],
      "{:#?}",
      analysis.diagnostics()
    );
    runs.push(rendered(&analysis));
  }
  assert_eq!(runs[0], runs[1]);
}

#[test]
fn long_names_are_quoted_as_excerpts() {
  // A definition, a parameter and an interface each named with four kilobytes, in one report.
  let name = "D".repeat(4096);
  let parameter = "P".repeat(4096);
  let interface = "I".repeat(4096);
  let text = format!(
    "interface {interface} {{\n  id: ID\n}}\nscalar {name}<{parameter}> where {parameter}: \
     {interface}\ntype Query {{\n  f: {name}<Int>\n}}\n"
  );
  let analysis = analyze_text(&text, &Limits::new());
  let quoted =
    |letter: &str| -> String { format!("{}…{}", letter.repeat(64), letter.repeat(64)) };
  assert_eq!(
    messages(&analysis),
    [format!(
      "`{}` bounds its type parameter `{}` by `{}`, and the type argument for `{}` here does not \
       satisfy that bound",
      quoted("D"),
      quoted("P"),
      quoted("I"),
      quoted("P"),
    )]
  );
}

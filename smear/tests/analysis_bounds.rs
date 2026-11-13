//! The GraphQLx analyzer's bounds: a bound's right side is an interface, which lowering checks
//! where the bound is written; and what a type satisfies, by definition and exactly, which the
//! model reads off what the type declares.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, Layout, Limits, MemberId, Model, Namespace, Package, PackageId,
    SourceFile, SourceId, Tier, TyId, TyKind, TypeExprId, analyze,
  },
  diagnostic::{Diagnose, Location},
  lexer::tokora::SimpleSpan,
};

/// The code of a bound the model cannot hold as written.
const INVALID_BOUND: &str = "smear::graphqlx::invalid-bound";

/// The code of a type name that names nothing.
const UNDEFINED: &str = "smear::schema::undefined-type";

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

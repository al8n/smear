//! The GraphQLx analyzer's bounds: a bound's right side is an interface, which lowering checks
//! where the bound is written.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, DefId, Diagnostic, Layout, Limits, Model, Namespace, Package, PackageId, SourceFile,
    SourceId, Tier, analyze,
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

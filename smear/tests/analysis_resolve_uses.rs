//! The GraphQLx analyzer's `use` items: every form and what it binds, where a binding is seen,
//! globs, the fixpoints and their limits, the checks once every item is resolved, string sources,
//! namespaces, and determinism.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, DiagnosticKind, Layout, Limits, Package, PackageId, SourceFile, SourceId,
    Tier, analyze, test_support,
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

/// A package of the `Directory` layout, as `package` makes one.
fn directory(name: &str, files: &[(&'static str, &'static str)]) -> Package<&'static str> {
  package(name, Layout::Directory, files)
}

/// Asserts that every path of `cases` names what it says in `module`, with no generic parameter
/// in scope.
fn all(
  root: &Package<&'static str>,
  dependencies: &[Package<&'static str>],
  module: &str,
  cases: &[(&str, Option<&str>)],
) {
  for &(path, expected) in cases {
    assert_eq!(
      test_support::resolve(root, dependencies, module, &[], path).as_deref(),
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

/// Where `inner` is, at its first occurrence inside the `nth` occurrence, counting from 0, of
/// `outer` in `text`, the file `source`.
fn inside(source: SourceId, text: &str, outer: &str, nth: usize, inner: &str) -> Location {
  let base = text
    .match_indices(outer)
    .nth(nth)
    .map(|(start, _): (usize, &str)| start)
    .unwrap_or_else(|| panic!("`{outer}` does not occur {} times", nth + 1));
  let offset = outer
    .find(inner)
    .unwrap_or_else(|| panic!("`{inner}` is not in `{outer}`"));
  let start = base + offset;
  Location::new(source.get(), SimpleSpan::new(start, start + inner.len()))
}

/// Where the `nth` occurrence, counting from 0, of `needle` is in `text`, the file `source`.
fn at(source: SourceId, text: &str, needle: &str, nth: usize) -> Location {
  inside(source, text, needle, nth, needle)
}

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The code of `kind`.
fn code(kind: DiagnosticKind) -> &'static str {
  kind.code().as_str()
}

/// Each label of `diagnostic`: where it is and what it says.
fn labels(diagnostic: &Diagnostic) -> Vec<(Location, String)> {
  (0..diagnostic.labels())
    .filter_map(|index: usize| diagnostic.label(index))
    .map(|label| (label.location(), String::from(label.text())))
    .collect()
}

#[test]
fn every_form_binds_what_it_names() {
  let geo = directory(
    "geo",
    &[
      (
        "lib.graphqlx",
        "scalar Point\nscalar Circle\ndirective @shape on FIELD_DEFINITION\n",
      ),
      ("extra/x.graphqlx", "scalar Extra\n"),
    ],
  );
  let client = "use ~::media::Image\nuse ~::media::Video as Clip\n\
                use ^::geo::{Point, Circle as Round}\nuse ^::geo::shape\nuse ~::media\nuse ^::geo\n\
                use ..::media::*\n";
  let root = directory(
    "app",
    &[
      ("schema.graphqlx", "scalar Top\n"),
      ("media/a.graphqlx", "scalar Image\nscalar Video\n"),
      ("client/b.graphqlx", client),
    ],
  )
  .with_dependencies(["geo"]);
  let dependencies = [geo];
  let analysis = analyze(&root, &dependencies, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &dependencies,
    "^::app::client",
    &[
      ("Image", Some("^::app::media::Image")),
      // An alias renames; the glob gives the name itself.
      ("Clip", Some("^::app::media::Video")),
      ("Video", Some("^::app::media::Video")),
      ("Point", Some("^::geo::Point")),
      ("Round", Some("^::geo::Circle")),
      ("Circle", None),
      // A `use` binds a name in each namespace its module has it in.
      ("@shape", Some("@^::geo::shape")),
      ("shape", None),
      // A `use` of a module binds the module, and of a package its root module.
      ("mod media", Some("mod ^::app::media")),
      ("media::Image", Some("^::app::media::Image")),
      ("mod geo", Some("mod ^::geo")),
      ("geo::extra::Extra", Some("^::geo::extra::Extra")),
      // What a module's `use` items bind is its own: nothing reaches it through a path.
      ("~::client::Image", None),
    ],
  );
}

#[test]
fn a_use_binds_for_every_file_of_its_module_and_for_its_inline_mod_alone() {
  let a = "use ^::geo::Point\nscalar A\nmod inner {\n  use ^::geo::Circle\n  scalar B\n}\n";
  let b = "use Point as Copy\nscalar C\n";
  let c = "use Point as Far\nscalar D\n";
  let geo = directory("geo", &[("lib.graphqlx", "scalar Point\nscalar Circle\n")]);
  let root = directory(
    "app",
    &[
      ("lib/a.graphqlx", a),
      ("lib/b.graphqlx", b),
      ("lib/child/c.graphqlx", c),
    ],
  )
  .with_dependencies(["geo"]);
  let dependencies = [geo];
  let analysis = analyze(&root, &dependencies, &Limits::new());

  // `b`, a file of the same module, sees the `use` of `a`; `child`, a module of its own, does not.
  let source = file(&analysis, 3, "lib/child/c.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::UnresolvedPath),
      at(source, c, "Point", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  all(
    &root,
    &dependencies,
    "^::app::lib",
    &[
      ("Point", Some("^::geo::Point")),
      ("Copy", Some("^::geo::Point")),
      ("Circle", None),
    ],
  );
  all(
    &root,
    &dependencies,
    "^::app::lib::inner",
    &[("Circle", Some("^::geo::Circle")), ("Point", None)],
  );
  all(
    &root,
    &dependencies,
    "^::app::lib::child",
    &[("Point", None), ("Far", None)],
  );
}

#[test]
fn groups_nest_to_the_limit_and_one_past_it_is_a_resource_limit() {
  let within = "use ~::deep::{a::{b::Leaf}}\n";
  let past = "use ~::{deep::{a::{b::Leaf}}}\n";
  let root = directory(
    "app",
    &[
      ("deep/a/b/c.graphqlx", "scalar Leaf\n"),
      ("top/x.graphqlx", within),
      ("top2/y.graphqlx", past),
    ],
  );
  let limits = Limits::new().with_max_use_depth(2);
  let analysis = analyze(&root, &[], &limits);

  let source = file(&analysis, 3, "top2/y.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      at(source, past, "a::{b::Leaf}", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  // The door analyzes with the default limits, under which both groups nest within reach.
  all(
    &root,
    &[],
    "^::app::top",
    &[("Leaf", Some("^::app::deep::a::b::Leaf"))],
  );
}

#[test]
fn a_glob_gives_the_declarations_and_the_child_modules_of_its_module() {
  let root = directory(
    "app",
    &[
      (
        "lib/a.graphqlx",
        "scalar Thing\ndirective @mark on FIELD_DEFINITION\n",
      ),
      ("lib/sub/b.graphqlx", "scalar Deep\n"),
      ("user/c.graphqlx", "use ~::lib::*\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &[],
    "^::app::user",
    &[
      ("Thing", Some("^::app::lib::Thing")),
      ("@mark", Some("@^::app::lib::mark")),
      ("mod sub", Some("mod ^::app::lib::sub")),
      ("sub::Deep", Some("^::app::lib::sub::Deep")),
    ],
  );
}

#[test]
fn a_name_two_globs_give_two_items_is_an_error_only_where_it_is_used() {
  let quiet = "use ~::one::*\nuse ~::two::*\n";
  let loud = "use ~::one::*\nuse ~::two::*\nuse Same as Picked\n";
  let root = directory(
    "app",
    &[
      ("one/a.graphqlx", "scalar Same\nscalar OnlyOne\n"),
      ("two/b.graphqlx", "scalar Same\nscalar OnlyTwo\n"),
      ("quiet/c.graphqlx", quiet),
      ("loud/d.graphqlx", loud),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());

  let source = file(&analysis, 3, "loud/d.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::AmbiguousGlob),
      at(source, loud, "Same", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  assert_eq!(
    labels(diagnostic)
      .into_iter()
      .map(|(location, _): (Location, String)| location)
      .collect::<Vec<Location>>(),
    [
      at(source, loud, "~::one::*", 0),
      at(source, loud, "~::two::*", 0)
    ]
  );

  all(
    &root,
    &[],
    "^::app::quiet",
    &[
      ("OnlyOne", Some("^::app::one::OnlyOne")),
      ("OnlyTwo", Some("^::app::two::OnlyTwo")),
      ("Same", None),
    ],
  );
  all(&root, &[], "^::app::loud", &[("Picked", None)]);
}

#[test]
fn a_declaration_and_an_explicit_use_each_go_before_a_glob() {
  let root = directory(
    "app",
    &[
      (
        "lib/a.graphqlx",
        "scalar Name\nscalar Other\nscalar HashMap\n",
      ),
      ("alt/b.graphqlx", "scalar Name\n"),
      ("decl/c.graphqlx", "use ~::lib::*\nscalar Name\n"),
      ("expl/d.graphqlx", "use ~::lib::*\nuse ~::alt::Name\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &[],
    "^::app::decl",
    &[
      ("Name", Some("^::app::decl::Name")),
      ("Other", Some("^::app::lib::Other")),
      // A glob goes before the prelude.
      ("HashMap", Some("^::app::lib::HashMap")),
    ],
  );
  all(
    &root,
    &[],
    "^::app::expl",
    &[
      ("Name", Some("^::app::alt::Name")),
      ("Other", Some("^::app::lib::Other")),
    ],
  );
}

/// The `geo` package of the glob chain: `a` is a child of its root module, `b` of `a`, and `Leaf`
/// is declared in `b`.
fn chained() -> (Package<&'static str>, Package<&'static str>, &'static str) {
  let user = "use geo::*\nuse a::*\nuse b::*\nuse Leaf as L\n";
  let geo = directory(
    "geo",
    &[
      ("lib.graphqlx", "scalar Top\n"),
      ("a/x.graphqlx", "scalar InA\n"),
      ("a/b/y.graphqlx", "scalar Leaf\n"),
    ],
  );
  let root = directory("app", &[("user/u.graphqlx", user)]).with_dependencies(["geo"]);
  (root, geo, user)
}

#[test]
fn a_chain_of_globs_resolves_over_several_rounds() {
  let (root, geo, _) = chained();
  let dependencies = [geo];
  // Four rounds: the package's glob, then `a`'s, then `b`'s, then `Leaf`.
  let limits = Limits::new().with_max_glob_iterations(4);
  let analysis = analyze(&root, &dependencies, &limits);
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &dependencies,
    "^::app::user",
    &[
      ("Top", Some("^::geo::Top")),
      ("InA", Some("^::geo::a::InA")),
      ("L", Some("^::geo::a::b::Leaf")),
      ("Leaf", Some("^::geo::a::b::Leaf")),
    ],
  );
}

#[test]
fn a_glob_fixpoint_past_its_limit_is_a_resource_limit() {
  let (root, geo, user) = chained();
  let dependencies = [geo];
  let limits = Limits::new().with_max_glob_iterations(3);
  let analysis = analyze(&root, &dependencies, &limits);

  // Reported once, at the head the fourth round would resolve; that item is reported no further.
  let source = file(&analysis, 3, "user/u.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      at(source, user, "Leaf", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// The first glob that would name a module past `Limits::max_globs` is reported once, where it is
/// written, with the limit, and gives nothing; a later glob of yet another module gives nothing
/// either, unreported; and another glob of a module already globbed names no module more.
#[test]
fn a_glob_past_the_glob_limit_is_refused_and_reported_once() {
  let text = "use a::*\nuse b::*\nuse a::*\nuse c::*\nuse d::*\nuse B as GotB\nuse C as GotC\n\
              use D as GotD\n";
  let root = directory(
    "app",
    &[
      ("lib.graphqlx", text),
      ("a/x.graphqlx", "scalar A\n"),
      ("b/x.graphqlx", "scalar B\n"),
      ("c/x.graphqlx", "scalar C\n"),
      ("d/x.graphqlx", "scalar D\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new().with_max_globs(2));
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        code(DiagnosticKind::ResourceLimit),
        at(lib, text, "c::*", 0)
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        inside(lib, text, "use C as GotC", 0, "C")
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        inside(lib, text, "use D as GotD", 0, "D")
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostic = analysis.diagnostics().iter().next().expect("a diagnostic");
  let message = diagnostic.to_string();
  assert!(message.contains("limit of 2 modules"), "{message}");
  assert!(
    diagnostic
      .help()
      .is_some_and(|help: &str| help.contains("`Limits::max_globs`")),
    "{:?}",
    diagnostic.help()
  );
}

/// Globs that name as many modules as `Limits::max_globs` allows are no error, however many globs
/// name them; at a limit of 0 the first glob is refused.
#[test]
fn the_glob_limit_met_exactly_is_no_error() {
  let text = "use a::*\nuse b::*\nuse a::*\nuse B as GotB\n";
  let root = directory(
    "app",
    &[
      ("lib.graphqlx", text),
      ("a/x.graphqlx", "scalar A\n"),
      ("b/x.graphqlx", "scalar B\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new().with_max_globs(2));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let analysis = analyze(&root, &[], &Limits::new().with_max_globs(0));
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        code(DiagnosticKind::ResourceLimit),
        at(lib, text, "a::*", 0)
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        inside(lib, text, "use B as GotB", 0, "B")
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// Which glob the glob limit refuses depends on the program alone: a module's `use` items resolve
/// in the order of its files' paths and of their places in them, so the files and the dependencies
/// given in reverse change no diagnostic.
#[test]
fn the_glob_limit_refuses_the_same_glob_whatever_the_order_given() {
  let second = "use ~::b::*\nuse ^::other::*\nuse ~::a::*\n";
  let files: [(&'static str, &'static str); 4] = [
    ("m/1.graphqlx", "use ~::a::*\nuse ^::geo::*\n"),
    ("m/2.graphqlx", second),
    ("a/x.graphqlx", "scalar A\n"),
    ("b/x.graphqlx", "scalar B\n"),
  ];
  let dependencies = [
    directory("geo", &[("lib.graphqlx", "scalar Point\n")]),
    directory("other", &[("lib.graphqlx", "scalar Far\n")]),
  ];
  let limits = Limits::new().with_max_globs(2);
  let forward = analyze(
    &directory("app", &files).with_dependencies(["geo", "other"]),
    &dependencies,
    &limits,
  );
  let mut reversed_files = files;
  reversed_files.reverse();
  let reversed_dependencies = [dependencies[1].clone(), dependencies[0].clone()];
  let backward = analyze(
    &directory("app", &reversed_files).with_dependencies(["other", "geo"]),
    &reversed_dependencies,
    &limits,
  );

  let source = file(&forward, 3, "m/2.graphqlx");
  assert_eq!(
    found(&forward),
    [(
      code(DiagnosticKind::ResourceLimit),
      at(source, second, "~::b::*", 0)
    )],
    "{:#?}",
    forward.diagnostics()
  );
  assert_eq!(forward.diagnostics(), backward.diagnostics());
}

/// Many globs of one module are one row, and a search for a near miss pays for each module a glob
/// names.
///
/// A module's globs were kept one row per glob, and a lookup read every row, which skipped a
/// repeated module but read it all the same; the search for a near miss read them again, and spent
/// nothing on a module that declares nothing. So G globs of one empty module beside N names that do
/// not resolve cost G × N rows read by the names' lookups, and G more for each search while the
/// budget lasted. Now a module's globs make one row per module they name, at most
/// `Limits::max_globs`, and a search spends a comparison on each module a glob names.
///
/// Nothing here can count that work, so the sizes tell the two costs apart: 2^20 globs of one
/// empty module beside 2^20 items whose head names nothing, about 24 MiB of text. Read row by row,
/// the lookups alone cost about 10^12 row reads, which is minutes; with one row, they cost about
/// what parsing the items costs.
#[test]
fn many_globs_of_one_module_are_one_row_and_a_near_miss_search_pays_for_each() {
  const GLOBS: usize = 1 << 20;
  const MISSING: usize = 1 << 20;
  let mut text = String::with_capacity(9 * GLOBS + 16 * MISSING + 16);
  text.push_str("mod e {}\n");
  for _ in 0..GLOBS {
    text.push_str("use e::*\n");
  }
  for index in 0..MISSING {
    text.push_str(&format!("use m{index}::T\n"));
  }
  let root: Package<&str> = Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("lib.graphqlx", text.as_str())]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert_eq!(analysis.diagnostics().error_count(), MISSING);
  let unresolved = code(DiagnosticKind::UnresolvedPath);
  assert!(
    analysis
      .diagnostics()
      .iter()
      .all(|diagnostic: &Diagnostic| diagnostic.code().as_str() == unresolved)
  );
}

/// A chain of renames needs a round a link and no glob: `use A1 as A2` waits in the explicit
/// fixpoint until both namespaces have `A1`. So a chain longer than the round limit is refused as
/// a chain of globs is, at the head the next round would resolve, and the message says rounds.
#[test]
fn a_chain_of_renames_past_the_round_limit_is_a_resource_limit() {
  let text = "scalar Seed\nuse Seed as A1\nuse A1 as A2\nuse A2 as A3\nuse A3 as A4\n";
  let root = directory("app", &[("lib.graphqlx", text)]);
  // Four rounds, one a link.
  let analysis = analyze(&root, &[], &Limits::new().with_max_glob_iterations(4));
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let analysis = analyze(&root, &[], &Limits::new().with_max_glob_iterations(3));
  let source = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::ResourceLimit),
      inside(source, text, "use A3 as A4", 0, "A3")
    )],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostic = analysis
    .diagnostics()
    .iter()
    .next()
    .expect("one diagnostic");
  let message = diagnostic.to_string();
  assert!(message.contains("more than 3 rounds"), "{message}");
  assert!(!message.contains("glob"), "{message}");
  assert!(
    diagnostic
      .help()
      .is_some_and(|help: &str| help.contains("`Limits::max_glob_iterations`")),
    "{:?}",
    diagnostic.help()
  );
}

/// A round of `use` resolution looks up only the items that what the round before bound or globbed
/// can answer.
///
/// Every round used to read every item of its module, and to ask of each waiting one whether the
/// round before could have answered it: a module of U waiting items that needs R rounds cost U × R
/// questions, a name lookup each, and the default limit allows 1 024 rounds. Now the waiting items
/// are indexed by their heads' names, and a round looks up only the items that the names it bound
/// first, and the modules it globbed first, can answer.
///
/// Nothing here can count that work, so the sizes tell the two costs apart. A chain of 1 000
/// renames needs 1 000 rounds, and beside it 2^20 items wait for a name nothing binds, about 16
/// MiB of text. Asked every round, the waiting items cost about 10^9 questions, which is minutes;
/// looked up once, they cost about what parsing them costs.
#[test]
fn a_round_of_use_resolution_reads_only_the_items_it_can_answer() {
  const CHAIN: usize = 1_000;
  const WAITING: usize = 1 << 20;
  let mut text = String::with_capacity(16 * WAITING + 24 * CHAIN);
  text.push_str("scalar Seed\nuse Seed as A1\n");
  for link in 1..CHAIN {
    text.push_str(&format!("use A{link} as A{}\n", link + 1));
  }
  for index in 0..WAITING {
    text.push_str(&format!("use w{index}::T\n"));
  }
  let root: Package<&str> = Package::new("app", Layout::Directory, Tier::Std)
    .with_files([SourceFile::new("lib.graphqlx", text.as_str())]);
  let last = format!("A{CHAIN}");
  assert_eq!(
    test_support::resolve(&root, &[], "^::app", &[], &last).as_deref(),
    Some("^::app::Seed")
  );
}

#[test]
fn binding_fives_errors_are_reported_and_one_item_bound_twice_is_not() {
  let changed = "use ~::lib::*\nuse a::X\nuse n::a\n";
  let declared = "scalar Clash\nuse ~::lib::Lib as Clash\n";
  let twice = "use ~::lib::Lib as Dup\nuse ~::lib::a::X as Dup\nuse ~::lib::Lib as Same\n\
               use ~::lib::Lib as Same\n";
  let root = directory(
    "app",
    &[
      ("lib/x.graphqlx", "scalar Lib\n"),
      ("lib/a/y.graphqlx", "scalar X\n"),
      ("lib/n/z.graphqlx", "scalar Nz\n"),
      ("lib/n/a/w.graphqlx", "scalar X\n"),
      ("m/u.graphqlx", changed),
      ("m2/u.graphqlx", declared),
      ("m3/u.graphqlx", twice),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());

  let m = file(&analysis, 3, "m/u.graphqlx");
  let m2 = file(&analysis, 3, "m2/u.graphqlx");
  let m3 = file(&analysis, 3, "m3/u.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      // `a` was a glob's `lib::a` when `use a::X` was resolved, and is `use n::a`'s `lib::n::a`
      // once both are.
      (
        code(DiagnosticKind::UseResolutionChanged),
        inside(m, changed, "use a::X", 0, "a")
      ),
      (
        code(DiagnosticKind::UseDeclarationCollision),
        at(m2, declared, "Clash", 1)
      ),
      (
        code(DiagnosticKind::UseNameCollision),
        at(m3, twice, "Dup", 1)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let diagnostics: Vec<&Diagnostic> = analysis.diagnostics().iter().collect();
  assert_eq!(
    labels(diagnostics[0]),
    [(
      inside(m, changed, "use n::a", 0, "a"),
      String::from("this `use` binds it once every `use` is resolved")
    )]
  );
  assert_eq!(
    labels(diagnostics[1]),
    [(at(m2, declared, "Clash", 0), String::from("declared here"))]
  );
  assert_eq!(
    labels(diagnostics[2]),
    [(at(m3, twice, "Dup", 0), String::from("first bound here"))]
  );

  // A declaration goes before a `use` of its name, and the first binding of a name answers.
  all(
    &root,
    &[],
    "^::app::m2",
    &[("Clash", Some("^::app::m2::Clash"))],
  );
  all(
    &root,
    &[],
    "^::app::m3",
    &[
      ("Dup", Some("^::app::lib::Lib")),
      ("Same", Some("^::app::lib::Lib")),
    ],
  );
  all(
    &root,
    &[],
    "^::app::m",
    &[
      ("X", Some("^::app::lib::a::X")),
      ("a::X", Some("^::app::lib::n::a::X")),
    ],
  );
}

#[test]
fn use_cycles_are_legal() {
  let root = directory(
    "app",
    &[
      ("a/a.graphqlx", "use ~::b::B\nuse ..::b::*\nscalar A\n"),
      ("b/b.graphqlx", "use ~::a::A\nuse ..::a::*\nscalar B\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  all(&root, &[], "^::app::a", &[("B", Some("^::app::b::B"))]);
  all(&root, &[], "^::app::b", &[("A", Some("^::app::a::A"))]);
}

#[test]
fn a_use_that_never_resolves_is_reported_once() {
  let (_, geo, _) = chained();
  let user = "use geo::*\nuse a::*\nuse ghost::Z\nuse b::*\n";
  let root = directory("app", &[("user/u.graphqlx", user)]).with_dependencies(["geo"]);
  let analysis = analyze(&root, &[geo], &Limits::new());

  let source = file(&analysis, 3, "user/u.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::UnresolvedPath),
      at(source, user, "ghost", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

#[test]
fn each_root_of_a_use_starts_where_it_says() {
  let base = directory("base", &[("lib.graphqlx", "scalar Unit\n")]);
  let geo = directory("geo", &[("lib.graphqlx", "scalar Point\n")]).with_dependencies(["base"]);
  let text = "use ..::..::..::Top\nuse ^::nope::X\nuse ^::base::Unit\nuse ^::app::Top\n\
              use ..::..::Top as Fine\nuse ~::Top as Same\nuse ^::geo::Point\n";
  let root = directory(
    "app",
    &[
      ("schema.graphqlx", "scalar Top\n"),
      ("x/y/z.graphqlx", text),
    ],
  )
  .with_dependencies(["geo"]);
  let dependencies = [base, geo];
  let analysis = analyze(&root, &dependencies, &Limits::new());

  let source = file(&analysis, 3, "x/y/z.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      // `..` above the root module.
      (
        code(DiagnosticKind::UnresolvedPath),
        at(source, text, "..::..::..::", 0)
      ),
      // No such package; a dependency's dependency; the package's own name.
      (
        code(DiagnosticKind::UnknownPackage),
        at(source, text, "nope", 0)
      ),
      (
        code(DiagnosticKind::UnknownPackage),
        at(source, text, "base", 0)
      ),
      (
        code(DiagnosticKind::UnknownPackage),
        at(source, text, "app", 0)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );

  all(
    &root,
    &dependencies,
    "^::app::x::y",
    &[
      ("Fine", Some("^::app::Top")),
      ("Same", Some("^::app::Top")),
      ("Point", Some("^::geo::Point")),
    ],
  );
}

#[test]
fn a_package_name_answers_a_use_of_it_alone_and_last() {
  let geo = directory("geo", &[("lib.graphqlx", "scalar Point\n")]);
  let root = directory(
    "app",
    &[
      ("user/a.graphqlx", "use geo\nuse geo::Point as P\n"),
      ("shadow/b.graphqlx", "use geo::Point as P\nscalar Before\n"),
      ("shadow/geo/c.graphqlx", "scalar Point\n"),
    ],
  )
  .with_dependencies(["geo"]);
  let dependencies = [geo];
  let analysis = analyze(&root, &dependencies, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &dependencies,
    "^::app::user",
    &[
      ("mod geo", Some("mod ^::geo")),
      ("geo", None),
      ("P", Some("^::geo::Point")),
    ],
  );
  // A child module of the package's name goes before it.
  all(
    &root,
    &dependencies,
    "^::app::shadow",
    &[("P", Some("^::app::shadow::geo::Point"))],
  );
}

#[test]
fn a_string_source_names_the_module_of_a_directory() {
  let vision = "scalar Frame\nuse \"../shared\"\nuse \"../shared\" as common\n\
                use \"../shared\"::Id\nuse \"..\"::*\nuse \"../..\" as top\nuse \"./\" as here\n";
  let root = directory(
    "app",
    &[
      ("schema.graphqlx", "scalar Root\n"),
      ("media/m.graphqlx", "scalar InMedia\n"),
      ("media/shared/s.graphqlx", "scalar Id\n"),
      ("media/vision/v.graphqlx", vision),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  all(
    &root,
    &[],
    "^::app::media::vision",
    &[
      ("mod shared", Some("mod ^::app::media::shared")),
      ("shared::Id", Some("^::app::media::shared::Id")),
      ("common::Id", Some("^::app::media::shared::Id")),
      ("Id", Some("^::app::media::shared::Id")),
      ("InMedia", Some("^::app::media::InMedia")),
      ("top::Root", Some("^::app::Root")),
      ("here::Frame", Some("^::app::media::vision::Frame")),
    ],
  );
}

#[test]
fn a_string_source_is_refused_where_it_names_no_directory_module() {
  let outside = "use \"../../outside\"\n";
  let named_file = "use \"./c.graphqlx\" as c\n";
  let url = "use \"https://example.com/geo/v1\" as geo\n";
  let bare = "use \"vision\" as v\n";
  let missing = "use \"./nowhere\" as n\n";
  let unnamed = "use \"..\"\n";
  let root = directory(
    "app",
    &[
      ("bad/a.graphqlx", outside),
      ("bad/b.graphqlx", named_file),
      ("bad/c.graphqlx", "scalar InC\n"),
      ("bad/d.graphqlx", url),
      ("bad/e.graphqlx", bare),
      ("bad/f.graphqlx", missing),
      ("bad/g.graphqlx", unnamed),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());

  let source = |path: &str| -> SourceId { file(&analysis, 3, path) };
  assert_eq!(
    found(&analysis),
    [
      (
        code(DiagnosticKind::UnresolvedPath),
        at(source("bad/a.graphqlx"), outside, "\"../../outside\"", 0)
      ),
      (
        code(DiagnosticKind::FilePathUse),
        at(source("bad/b.graphqlx"), named_file, "\"./c.graphqlx\"", 0)
      ),
      (
        code(DiagnosticKind::UrlUnsupported),
        at(
          source("bad/d.graphqlx"),
          url,
          "\"https://example.com/geo/v1\"",
          0
        )
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        at(source("bad/e.graphqlx"), bare, "\"vision\"", 0)
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        at(source("bad/f.graphqlx"), missing, "\"./nowhere\"", 0)
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        at(source("bad/g.graphqlx"), unnamed, "\"..\"", 0)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  // The help of a source that names a file names its directory.
  let named = analysis
    .diagnostics()
    .iter()
    .find(|diagnostic: &&Diagnostic| {
      diagnostic.code().as_str() == code(DiagnosticKind::FilePathUse)
    })
    .and_then(|diagnostic: &Diagnostic| diagnostic.help());
  assert!(
    named.is_some_and(|help: &str| help.contains("`bad`")),
    "{named:?}"
  );

  // In a `Flat` package every string source is refused.
  let flat_text = "use \"./x\" as x\nscalar Q\n";
  let flat = package("flat", Layout::Flat, &[("x/a.graphqlx", flat_text)]);
  let analysis = analyze(&flat, &[], &Limits::new());
  let source = file(&analysis, 3, "x/a.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::UnresolvedPath),
      at(source, flat_text, "\"./x\"", 0)
    )],
    "{:#?}",
    analysis.diagnostics()
  );
}

/// A string source starts at the directory of the file it is written in, whatever `mod` it is
/// written in: a `mod` of a directory module is refused as a module declared twice, but its items
/// are collected into that module, whose directory is not the file's. Each name a source walks
/// through must be a module, so `..` does not undo a name that names nothing.
#[test]
fn a_string_source_starts_at_its_files_directory_whatever_mod_it_is_in() {
  let text = "mod a {\n  use \"./b\" as near\n  use \"./a/b\" as far\n  \
              use \"./nowhere/../b\" as gone\n}\n";
  let root = directory(
    "app",
    &[
      ("lib.graphqlx", text),
      ("a/x.graphqlx", "scalar InA\n"),
      ("a/b/y.graphqlx", "scalar InAB\n"),
      ("b/z.graphqlx", "scalar InB\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let lib = file(&analysis, 3, "lib.graphqlx");
  assert_eq!(
    found(&analysis),
    [
      (
        code(DiagnosticKind::ModuleDefinedTwice),
        inside(lib, text, "mod a", 0, "a")
      ),
      (
        code(DiagnosticKind::UnresolvedPath),
        at(lib, text, "\"./nowhere/../b\"", 0)
      ),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  all(
    &root,
    &[],
    "^::app::a",
    &[
      ("near::InB", Some("^::app::b::InB")),
      ("far::InAB", Some("^::app::a::b::InAB")),
      ("mod gone", None),
    ],
  );
}

/// A long file path is not read again for each string source written in the file.
///
/// A string source names a directory relative to the file it is written in, and the file's
/// directory was found by splitting the file's path, then the directory reached was joined into a
/// path and searched for among the package's files, for every string source: a file whose path is
/// L bytes and that holds U string sources cost L × U, and a path is not source text, which
/// `Limits::max_source_bytes` counts. Now each file's directory module, and the files of each
/// directory by name, are tabled once, and a string source walks from its file's directory module
/// by its own segments.
///
/// Nothing here can count that work, so the sizes tell the two costs apart. A file named with 32
/// MiB of `N` holds 60 000 items `use "./" as here`, about 1 MiB of text. Read for each item, the
/// path costs about 2 × 10^12 bytes in all, which is minutes at memory speed; read once, it costs
/// about what reading the file's text costs.
#[test]
fn a_long_file_path_is_not_read_again_for_each_string_source() {
  const LONG: usize = 1 << 25;
  const SOURCES: usize = 60_000;
  let path = format!("{}.graphqlx", "N".repeat(LONG));
  let text = "use \"./\" as here\n".repeat(SOURCES);
  let root: Package<&str> = Package::new("app", Layout::Directory, Tier::Std)
    .with_files([SourceFile::new(path.as_str(), text.as_str())]);
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert_eq!(
    test_support::resolve(&root, &[], "^::app", &[], "mod here").as_deref(),
    Some("mod ^::app")
  );
}

#[test]
fn a_use_binds_in_each_namespace_and_types_and_modules_share_one() {
  let root = directory(
    "app",
    &[
      (
        "lib/a.graphqlx",
        "scalar both\ndirective @both on FIELD_DEFINITION\nscalar justType\n",
      ),
      (
        "user/b.graphqlx",
        "use ~::lib::both\nuse ~::lib::justType as onlyType\n",
      ),
      (
        "mixed/c.graphqlx",
        "use ~::lib as Mixed\nuse ~::lib::justType as Mixed\n",
      ),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());

  let mixed = "use ~::lib as Mixed\nuse ~::lib::justType as Mixed\n";
  let source = file(&analysis, 3, "mixed/c.graphqlx");
  assert_eq!(
    found(&analysis),
    [(
      code(DiagnosticKind::UseNameCollision),
      at(source, mixed, "Mixed", 1)
    )],
    "{:#?}",
    analysis.diagnostics()
  );

  all(
    &root,
    &[],
    "^::app::user",
    &[
      ("both", Some("^::app::lib::both")),
      ("@both", Some("@^::app::lib::both")),
      ("onlyType", Some("^::app::lib::justType")),
      ("@onlyType", None),
    ],
  );
}

#[test]
fn reversing_files_and_dependencies_changes_no_diagnostic() {
  let files: [(&'static str, &'static str); 5] = [
    ("lib/x.graphqlx", "scalar Lib\n"),
    ("lib/a/y.graphqlx", "scalar X\n"),
    ("lib/n/a/w.graphqlx", "scalar X\n"),
    (
      "m/u.graphqlx",
      "use ~::lib::*\nuse a::X\nuse n::a\nuse ^::nope::Q\n",
    ),
    (
      "bad/b.graphqlx",
      "use \"./x.graphqlx\"\nuse ghost::G\nuse ^::geo::Point\n",
    ),
  ];
  let dependencies = [
    directory("geo", &[("lib.graphqlx", "scalar Point\n")]),
    directory("other", &[("lib.graphqlx", "use ^::core::*\nuse Gone\n")]),
  ];
  let forward = analyze(
    &directory("app", &files).with_dependencies(["geo", "other"]),
    &dependencies,
    &Limits::new(),
  );
  let mut reversed_files = files;
  reversed_files.reverse();
  let reversed_dependencies = [dependencies[1].clone(), dependencies[0].clone()];
  let backward = analyze(
    &directory("app", &reversed_files).with_dependencies(["other", "geo"]),
    &reversed_dependencies,
    &Limits::new(),
  );
  assert!(!forward.diagnostics().is_empty());
  assert_eq!(forward.diagnostics(), backward.diagnostics());
}

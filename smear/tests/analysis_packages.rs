//! The GraphQLx analyzer's package set: how packages and sources are numbered, what the model
//! answers about each package, and each package-level diagnostic, which points at the whole of the
//! offending package's package source.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Bound, Diagnostic, DiagnosticBuilder, DiagnosticKind, Diagnostics, IdIter, IdRange,
    IntegerProvider, Layout, Limits, Model, Package, PackageId, PackageInfo, PackageKind, Source,
    SourceFile, SourceId, SourceMap, Tier, analyze,
  },
  diagnostic::{Diagnose, Location, Severity},
};

/// A file that parses.
const OK: &str = "type Query { ok: Int }\n";

/// A package of `Directory` layout at tier `Std`, holding `files`, `(path, text)`.
fn package(name: &str, files: &[(&'static str, &'static str)]) -> Package<&'static str> {
  Package::new(name, Layout::Directory, Tier::Std).with_files(
    files
      .iter()
      .map(|&(path, text): &(&'static str, &'static str)| SourceFile::new(path, text)),
  )
}

/// Each diagnostic's code and primary location, in the list's order.
fn found(analysis: &Analysis) -> Vec<(&'static str, Location)> {
  analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| (diagnostic.code().as_str(), diagnostic.primary()))
    .collect()
}

/// The whole of package `package`'s package source.
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

/// The code of `kind`.
fn code(kind: DiagnosticKind) -> &'static str {
  kind.code().as_str()
}

/// Asserts that `analysis` reported exactly `kind` at `at`, `count` times, and nothing else, each
/// with the kind's severity, a primary label and a help line.
fn only(analysis: &Analysis, kind: DiagnosticKind, at: Location, count: usize) {
  assert_eq!(
    found(analysis),
    vec![(code(kind), at); count],
    "{:#?}",
    analysis.diagnostics()
  );
  for diagnostic in analysis.diagnostics() {
    assert_eq!(diagnostic.severity(), kind.severity());
    assert_eq!(diagnostic.severity(), Severity::Error);
    assert!(diagnostic.primary_label().is_some());
    assert!(diagnostic.help().is_some());
    assert_eq!(diagnostic.labels(), 0);
    assert!(!diagnostic.to_string().is_empty());
  }
  assert!(!analysis.is_ok());
  assert_eq!(analysis.diagnostics().error_count(), count);
}

#[test]
fn a_clean_package_set_reports_nothing() {
  let root = package("app", &[("schema.graphqlx", OK)])
    .with_dependencies(["geo"])
    .with_integers(IntegerProvider::numo());
  let geo = Package::new("geo", Layout::Flat, Tier::Alloc)
    .with_files([SourceFile::new("point.graphqlx", "type Point { x: Int }\n")]);
  let analysis = analyze(&root, &[geo], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());
  assert!(analysis.diagnostics().is_empty());
  assert!(!analysis.diagnostics().truncated());
  assert_eq!(analysis.diagnostics().error_count(), 0);

  let model = analysis.model();
  let app = model.package(model.root_package());
  assert_eq!(app.name(), "app");
  assert_eq!(app.layout(), Layout::Directory);
  assert_eq!(app.tier(), Tier::Std);
  assert_eq!(app.integers(), Some(&IntegerProvider::numo()));
  let geo = model.package(PackageId::new(4));
  assert_eq!(geo.name(), "geo");
  assert_eq!(geo.layout(), Layout::Flat);
  assert_eq!(geo.tier(), Tier::Alloc);
  assert_eq!(geo.integers(), None);
  assert_eq!(app.dependencies(), [geo.id()]);
  assert!(geo.dependencies().is_empty());
}

#[test]
fn numo_provides_every_width_to_65536() {
  let numo = IntegerProvider::numo();
  assert_eq!(numo.name(), "numo");
  assert_eq!(numo.signed(), 0..=65536);
  assert_eq!(numo.unsigned(), 0..=65536);
}

#[test]
fn the_built_ins_come_first_then_the_root_then_the_dependencies_by_name() {
  let root =
    package("app", &[("schema.graphqlx", OK)]).with_dependencies(["zeta", "alpha", "core"]);
  let dependencies = [
    package("zeta", &[("z.graphqlx", OK)]),
    package("alpha", &[("a.graphqlx", OK)]),
  ];
  let analysis = analyze(&root, &dependencies, &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let model = analysis.model();
  assert_eq!(model.packages().len(), 6);
  assert_eq!(model.root_package(), PackageId::new(3));
  let packages: Vec<(&str, PackageKind, Tier)> = model
    .packages()
    .into_iter()
    .map(|id: PackageId| {
      let package = model.package(id);
      assert_eq!(package.id(), id);
      (package.name(), package.kind(), package.tier())
    })
    .collect();
  assert_eq!(
    packages,
    [
      ("core", PackageKind::BuiltIn, Tier::Core),
      ("alloc", PackageKind::BuiltIn, Tier::Alloc),
      ("std", PackageKind::BuiltIn, Tier::Std),
      ("app", PackageKind::Root, Tier::Std),
      ("alpha", PackageKind::Dependency, Tier::Std),
      ("zeta", PackageKind::Dependency, Tier::Std),
    ]
  );

  // The declared dependencies, resolved and in id order: `core` is the built-in, which no
  // supplied package can be.
  assert_eq!(
    model.package(model.root_package()).dependencies(),
    [PackageId::new(0), PackageId::new(4), PackageId::new(5)]
  );
  for built_in in 0..3 {
    assert!(
      model
        .package(PackageId::new(built_in))
        .dependencies()
        .is_empty()
    );
  }
}

#[test]
fn each_package_has_a_package_source_then_its_files_by_path_segment_by_segment() {
  // `media/vision.graphqlx` comes before `media.graphqlx`: segment by segment, `media` is a prefix
  // of `media.graphqlx`, where comparing the whole paths bytewise would put `.` before `/`.
  let root = package(
    "app",
    &[
      ("schema.graphqlx", OK),
      ("media.graphqlx", "scalar Media\n"),
      ("media/vision.graphqlx", "scalar Image\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let model = analysis.model();
  let sources = analysis.sources();
  let mut described: Vec<(u32, &str, usize, bool)> = Vec::new();
  for id in sources.sources() {
    let source = sources.source(id);
    assert_eq!(source.id(), id);
    let len = source.len();
    assert_eq!(source.is_empty(), len == 0);
    described.push((
      source.package().get(),
      source.path(),
      source.len(),
      source.is_package_source(),
    ));
  }
  assert_eq!(
    described,
    [
      (0, "", 0, true),
      (1, "", 0, true),
      (2, "", 0, true),
      (3, "", 0, true),
      (3, "media/vision.graphqlx", "scalar Image\n".len(), false),
      (3, "media.graphqlx", "scalar Media\n".len(), false),
      (3, "schema.graphqlx", OK.len(), false),
    ]
  );
  assert_eq!(sources.len(), 7);
  assert!(!sources.is_empty());

  let range = model.package(model.root_package()).sources();
  assert_eq!(range.len(), 4);
  assert!(!range.is_empty());
  assert!(range.contains(SourceId::new(3)));
  assert!(range.contains(SourceId::new(6)));
  assert!(!range.contains(SourceId::new(2)));
  assert!(!range.contains(SourceId::new(7)));
  assert_eq!(
    range.into_iter().collect::<Vec<SourceId>>(),
    [3, 4, 5, 6].map(SourceId::new)
  );
  assert_eq!(
    range.into_iter().rev().collect::<Vec<SourceId>>(),
    [6, 5, 4, 3].map(SourceId::new)
  );
  assert_eq!(range.into_iter().len(), 4);
}

#[test]
fn two_files_with_one_path_are_refused() {
  let root = package("app", &[("a.graphqlx", OK), ("a.graphqlx", "scalar A\n")]);
  let analysis = analyze(&root, &[], &Limits::new());
  only(
    &analysis,
    DiagnosticKind::DuplicateFilePath,
    package_source(&analysis, 3),
    1,
  );
}

#[test]
fn a_file_path_is_relative_slash_separated_utf8_and_plain() {
  let root = Package::new("app", Layout::Directory, Tier::Std).with_files([
    SourceFile::new("fine/path.graphqlx", OK),
    SourceFile::new("/absolute.graphqlx", OK),
    SourceFile::new("C:/drive.graphqlx", OK),
    SourceFile::new("back\\slash.graphqlx", OK),
    SourceFile::new("double//slash.graphqlx", OK),
    SourceFile::new("trailing/", OK),
    SourceFile::new("", OK),
    SourceFile::new("./dot.graphqlx", OK),
    SourceFile::new("up/../dot.graphqlx", OK),
    SourceFile::new(&b"not-\xffutf8.graphqlx"[..], OK),
  ]);
  let analysis = analyze(&root, &[], &Limits::new());
  only(
    &analysis,
    DiagnosticKind::InvalidFilePath,
    package_source(&analysis, 3),
    9,
  );

  // A path that is not UTF-8 is kept as text with the replacement character.
  let sources = analysis.sources();
  assert!(
    sources
      .sources()
      .into_iter()
      .any(|id: SourceId| sources.source(id).path() == "not-\u{FFFD}utf8.graphqlx")
  );
}

#[test]
fn a_package_name_is_a_name() {
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["my-geo"]);
  let analysis = analyze(
    &root,
    &[package("my-geo", &[("g.graphqlx", OK)])],
    &Limits::new(),
  );
  only(
    &analysis,
    DiagnosticKind::InvalidPackageName,
    package_source(&analysis, 4),
    1,
  );

  let root = package("9lives", &[("a.graphqlx", OK)]);
  let analysis = analyze(&root, &[], &Limits::new());
  only(
    &analysis,
    DiagnosticKind::InvalidPackageName,
    package_source(&analysis, 3),
    1,
  );
}

#[test]
fn a_supplied_package_cannot_take_a_built_in_name() {
  // The root's `core` is the built-in, not the supplied package that claims the name.
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["core"]);
  let analysis = analyze(
    &root,
    &[package("core", &[("c.graphqlx", OK)])],
    &Limits::new(),
  );
  only(
    &analysis,
    DiagnosticKind::ReservedPackageName,
    package_source(&analysis, 4),
    1,
  );
  assert_eq!(
    analysis
      .model()
      .package(analysis.model().root_package())
      .dependencies(),
    [PackageId::new(0)]
  );

  let analysis = analyze(&package("std", &[("a.graphqlx", OK)]), &[], &Limits::new());
  only(
    &analysis,
    DiagnosticKind::ReservedPackageName,
    package_source(&analysis, 3),
    1,
  );
}

#[test]
fn two_supplied_packages_cannot_share_a_name() {
  // Two dependencies named `geo`: the one numbered second is reported, and which one that is does
  // not depend on the order they were passed in.
  let first = package("geo", &[("a.graphqlx", OK)]);
  let second = package("geo", &[("b.graphqlx", OK)]);
  let root = package("app", &[("a.graphqlx", OK)]);
  let forward = analyze(&root, &[first.clone(), second.clone()], &Limits::new());
  let backward = analyze(&root, &[second, first], &Limits::new());
  only(
    &forward,
    DiagnosticKind::DuplicatePackageName,
    package_source(&forward, 5),
    1,
  );
  assert_eq!(forward.diagnostics(), backward.diagnostics());

  // A dependency named as the root is reported; the root keeps its name.
  let analysis = analyze(
    &root,
    &[package("app", &[("b.graphqlx", OK)])],
    &Limits::new(),
  );
  only(
    &analysis,
    DiagnosticKind::DuplicatePackageName,
    package_source(&analysis, 4),
    1,
  );
}

#[test]
fn a_declared_dependency_must_be_supplied() {
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["geo", "geo"]);
  let analysis = analyze(&root, &[], &Limits::new());
  // A name declared twice is declared once.
  only(
    &analysis,
    DiagnosticKind::MissingDependency,
    package_source(&analysis, 3),
    1,
  );

  // A package does not reach itself as a dependency.
  let geo = package("geo", &[("g.graphqlx", OK)]).with_dependencies(["geo"]);
  let analysis = analyze(
    &package("app", &[("a.graphqlx", OK)]),
    &[geo],
    &Limits::new(),
  );
  only(
    &analysis,
    DiagnosticKind::MissingDependency,
    package_source(&analysis, 4),
    1,
  );
}

/// The most bytes of one spelling a message quotes: `2 * 64 + 3` of the spelling's own bytes, each
/// written as at most the three bytes of a U+FFFD, and the three of the `…` between its two ends.
const EXCERPT_BOUND: usize = 396;

/// The spellings `message` quotes, each between a pair of backticks, in order.
fn quoted(message: &str) -> Vec<&str> {
  message.split('`').skip(1).step_by(2).collect()
}

/// The message of `analysis`'s first diagnostic.
fn first_message(analysis: &Analysis) -> String {
  analysis
    .diagnostics()
    .iter()
    .next()
    .map(|diagnostic: &Diagnostic| diagnostic.to_string())
    .unwrap_or_default()
}

/// A root package named `name` that holds three files at one path and one at an absolute path, and
/// declares a dependency nobody supplies: four diagnostics, each quoting the name.
fn quoting(name: &str, limits: &Limits) -> Analysis {
  let root = package(
    name,
    &[
      ("a.graphqlx", OK),
      ("a.graphqlx", OK),
      ("a.graphqlx", OK),
      ("/absolute.graphqlx", OK),
    ],
  )
  .with_dependencies(["geo"]);
  analyze(&root, &[], limits)
}

/// A message quotes a long name by its first and its last 64 bytes, so it holds a fixed number of
/// bytes of the name however long the name is and however many diagnostics quote it. Under a
/// ceiling of 0 every error is still counted.
#[test]
fn a_long_name_is_quoted_by_its_two_ends_and_counted_under_any_ceiling() {
  let head = "H".repeat(64);
  let tail = "T".repeat(64);
  let name = format!("{head}{}{tail}", "M".repeat(4096 - 128));
  let analysis = quoting(&name, &Limits::new());

  let at = package_source(&analysis, 3);
  assert_eq!(
    found(&analysis),
    [
      (code(DiagnosticKind::DuplicateFilePath), at),
      (code(DiagnosticKind::DuplicateFilePath), at),
      (code(DiagnosticKind::InvalidFilePath), at),
      (code(DiagnosticKind::MissingDependency), at),
    ],
    "{:#?}",
    analysis.diagnostics()
  );
  let excerpt = format!("{head}…{tail}");
  for diagnostic in analysis.diagnostics() {
    let message = diagnostic.to_string();
    let spellings = quoted(&message);
    assert_eq!(
      spellings.first().copied(),
      Some(excerpt.as_str()),
      "{message}"
    );
    for spelling in spellings {
      assert!(spelling.len() <= EXCERPT_BOUND, "{message}");
    }
  }

  let none = quoting(&name, &Limits::new().with_max_diagnostics(0));
  assert!(none.diagnostics().is_empty());
  assert!(none.diagnostics().truncated());
  assert_eq!(none.diagnostics().error_count(), 4);
  assert_eq!(
    none.diagnostics().error_count(),
    analysis.diagnostics().error_count()
  );
}

/// A long name of two-byte characters is cut between characters at both ends, so it is quoted with
/// no U+FFFD; one that is not UTF-8 is quoted with a U+FFFD for each byte it keeps, within the
/// bound.
#[test]
fn a_long_name_is_cut_between_characters_and_bounded_when_it_is_not_utf8() {
  // Each cut falls inside an `é` and moves back to its first byte.
  let accented = format!("a{}b", "é".repeat(100));
  let analysis = analyze(
    &package(&accented, &[("a.graphqlx", OK)]),
    &[],
    &Limits::new(),
  );
  only(
    &analysis,
    DiagnosticKind::InvalidPackageName,
    package_source(&analysis, 3),
    1,
  );
  let message = first_message(&analysis);
  assert!(!message.contains('\u{FFFD}'), "{message}");
  let expected = format!("a{}…{}b", "é".repeat(31), "é".repeat(32));
  assert_eq!(
    quoted(&message).first().copied(),
    Some(expected.as_str()),
    "{message}"
  );

  let invalid = [0xFF_u8; 300];
  let root = Package::new(&invalid[..], Layout::Directory, Tier::Std)
    .with_files([SourceFile::new("a.graphqlx", OK)]);
  let analysis = analyze(&root, &[], &Limits::new());
  only(
    &analysis,
    DiagnosticKind::InvalidPackageName,
    package_source(&analysis, 3),
    1,
  );
  let message = first_message(&analysis);
  let spelling = quoted(&message).first().copied().unwrap_or_default();
  assert!(
    spelling.len() <= EXCERPT_BOUND,
    "{} bytes: {message}",
    spelling.len()
  );
  assert_eq!(
    spelling,
    format!("{}…{}", "\u{FFFD}".repeat(64), "\u{FFFD}".repeat(64))
  );
}

/// A name of 128 bytes is quoted whole, and one of 129 by its first and its last 64.
#[test]
fn a_name_of_128_bytes_is_quoted_whole_and_one_of_129_by_its_two_ends() {
  let head = "H".repeat(64);
  let tail = "T".repeat(64);
  for (name, expected) in [
    (format!("{head}{tail}"), format!("{head}{tail}")),
    (format!("{head}M{tail}"), format!("{head}…{tail}")),
  ] {
    let root = package(&name, &[("a.graphqlx", OK)]).with_dependencies(["geo"]);
    let analysis = analyze(&root, &[], &Limits::new());
    only(
      &analysis,
      DiagnosticKind::MissingDependency,
      package_source(&analysis, 3),
      1,
    );
    let message = first_message(&analysis);
    assert_eq!(
      quoted(&message).first().copied(),
      Some(expected.as_str()),
      "{} bytes: {message}",
      name.len()
    );
  }
}

#[test]
fn the_supplied_sources_have_a_size_limit() {
  let files = [("a.graphqlx", OK), ("b.graphqlx", "type B {\n")];
  let total = OK.len() + "type B {\n".len();
  let root = package("app", &files);

  // Over the limit: refused at the root, and no file is parsed, so the syntax error goes unsaid.
  let over = Limits::new().with_max_source_bytes(u32::try_from(total - 1).expect("small"));
  let analysis = analyze(&root, &[], &over);
  only(
    &analysis,
    DiagnosticKind::ResourceLimit,
    package_source(&analysis, 3),
    1,
  );

  // At the limit: parsed, and the syntax error is the only diagnostic.
  let at = Limits::new().with_max_source_bytes(u32::try_from(total).expect("small"));
  let analysis = analyze(&root, &[], &at);
  let codes: Vec<&str> = found(&analysis)
    .into_iter()
    .map(|(code, _): (&'static str, Location)| code)
    .collect();
  assert_eq!(codes.len(), 1, "{:#?}", analysis.diagnostics());
  assert!(codes[0].starts_with("smear::syntax::"), "{codes:?}");
}

/// Every package's name, kind, sources and dependencies, and every source's package, path, length
/// and kind: everything the ids number.
#[allow(clippy::type_complexity)]
fn numbered(
  analysis: &Analysis,
) -> (
  Vec<(String, PackageKind, Vec<SourceId>, Vec<PackageId>)>,
  Vec<(PackageId, String, usize, bool)>,
) {
  let model = analysis.model();
  let packages = model
    .packages()
    .into_iter()
    .map(|id: PackageId| {
      let package = model.package(id);
      (
        String::from(package.name()),
        package.kind(),
        package.sources().into_iter().collect::<Vec<SourceId>>(),
        package.dependencies().to_vec(),
      )
    })
    .collect::<Vec<(String, PackageKind, Vec<SourceId>, Vec<PackageId>)>>();
  let sources = analysis.sources();
  let files = sources
    .sources()
    .into_iter()
    .map(|id: SourceId| {
      let source = sources.source(id);
      (
        source.package(),
        String::from(source.path()),
        source.len(),
        source.is_package_source(),
      )
    })
    .collect::<Vec<(PackageId, String, usize, bool)>>();
  (packages, files)
}

/// W's test 7: permuting the dependencies, and each package's files, changes no id and no
/// diagnostic, refused package sets included.
#[test]
fn permuting_the_dependencies_and_the_files_changes_no_id_and_no_diagnostic() {
  let root_files = [
    ("schema.graphqlx", OK),
    ("b.graphqlx", "scalar B\n"),
    ("a/c.graphqlx", "scalar C\n"),
    ("a.graphqlx", "scalar A\n"),
    ("twice.graphqlx", "scalar One\n"),
    ("twice.graphqlx", "scalar Two\n"),
    ("broken.graphqlx", "type Broken {\n"),
  ];
  let geo_one = [
    ("p.graphqlx", "type Point { x: Int }\n"),
    ("q.graphqlx", OK),
  ];
  let geo_two = [("p.graphqlx", "type Point { y: Int }\n")];
  let zeta = [("z.graphqlx", "scalar Z\n"), ("y/x.graphqlx", "scalar X\n")];
  let alpha = [("a.graphqlx", "scalar Alpha\n")];

  let build = |reversed: bool| -> (Package<&'static str>, Vec<Package<&'static str>>) {
    let order = |files: &[(&'static str, &'static str)]| -> Vec<(&'static str, &'static str)> {
      let mut files = files.to_vec();
      if reversed {
        files.reverse();
      }
      files
    };
    let root = package("app", &order(&root_files[..])).with_dependencies(if reversed {
      ["missing", "alpha", "geo", "zeta"]
    } else {
      ["zeta", "geo", "alpha", "missing"]
    });
    let mut dependencies = vec![
      package("zeta", &order(&zeta[..])),
      package("geo", &order(&geo_one[..])),
      package("alpha", &order(&alpha[..])).with_dependencies(["alpha"]),
      package("geo", &order(&geo_two[..])),
    ];
    if reversed {
      dependencies.reverse();
      dependencies.rotate_left(1);
    }
    (root, dependencies)
  };

  let (root, dependencies) = build(false);
  let forward = analyze(&root, &dependencies, &Limits::new());
  let (root, dependencies) = build(true);
  let backward = analyze(&root, &dependencies, &Limits::new());

  // The case is not vacuous: every kind it is built to provoke is there.
  let codes: Vec<&str> = found(&forward)
    .into_iter()
    .map(|(code, _): (&'static str, Location)| code)
    .collect();
  for kind in [
    DiagnosticKind::DuplicateFilePath,
    DiagnosticKind::DuplicatePackageName,
    DiagnosticKind::MissingDependency,
  ] {
    assert!(codes.contains(&code(kind)), "{codes:?}");
  }
  assert!(
    codes
      .iter()
      .any(|code: &&str| code.starts_with("smear::syntax::")),
    "{codes:?}"
  );

  assert_eq!(numbered(&forward), numbered(&backward));
  assert_eq!(forward.diagnostics(), backward.diagnostics());
}

// W's test 8: every result type and every view is `Send + Sync`, checked by the compiler.
const _: () = {
  const fn assert_send_sync<T: Send + Sync>() {}
  let _ = assert_send_sync::<Analysis>;
  let _ = assert_send_sync::<Model>;
  let _ = assert_send_sync::<SourceMap>;
  let _ = assert_send_sync::<Diagnostics>;
  let _ = assert_send_sync::<Diagnostic>;
  let _ = assert_send_sync::<DiagnosticBuilder>;
  let _ = assert_send_sync::<DiagnosticKind>;
  let _ = assert_send_sync::<PackageInfo<'static>>;
  let _ = assert_send_sync::<PackageKind>;
  let _ = assert_send_sync::<Source<'static>>;
  let _ = assert_send_sync::<Bound<'static>>;
  let _ = assert_send_sync::<IdRange<PackageId>>;
  let _ = assert_send_sync::<IdRange<SourceId>>;
  let _ = assert_send_sync::<IdIter<SourceId>>;
  let _ = assert_send_sync::<PackageId>;
  let _ = assert_send_sync::<SourceId>;
  let _ = assert_send_sync::<Limits>;
  let _ = assert_send_sync::<IntegerProvider>;
  let _ = assert_send_sync::<Layout>;
  let _ = assert_send_sync::<Tier>;
  let _ = assert_send_sync::<Package<&'static str>>;
  let _ = assert_send_sync::<SourceFile<&'static str>>;
};

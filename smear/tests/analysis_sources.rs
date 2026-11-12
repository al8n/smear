//! The GraphQLx analyzer's sources: every file parsed with the GraphQLx syntactic parser, its
//! syntax errors reported at that file, the same analysis whatever text type the files were
//! supplied in, and `SourceMap::bind` pairing the result with the texts again.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Diagnostic, Diagnostics, Layout, Limits, Package, PackageId, SourceFile, SourceId,
    SourceText, Tier, analyze,
  },
  diagnostic::{Code, Diagnose, Location},
  lexer::tokora::{Parse as _, Parser, SimpleSpan},
  parser::graphqlx::{
    GraphQLx,
    ast::TypeSystemDocument,
    error::GraphqlxErrors,
    syntactic::{GraphqlxLexer, type_system_document},
  },
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

/// The id of the file at `path` in package `package`.
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

/// W's test 9: `bind` over byte texts, one of them not UTF-8, and over package sources.
#[test]
fn bind_answers_each_text_a_renderer_can_read() {
  let good: &'static [u8] = b"type A { ok: Int }\n";
  let not_utf8: &'static [u8] = b"type B { ok: Int }\n\xff\n";
  let root = Package::new("app", Layout::Directory, Tier::Std).with_files([
    SourceFile::new("a.graphqlx", good),
    SourceFile::new("b.graphqlx", not_utf8),
  ]);
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "a.graphqlx");
  let b = file(&analysis, 3, "b.graphqlx");

  // The text that is not UTF-8 carries a syntax diagnostic of its own, at its own source.
  assert!(
    analysis
      .diagnostics()
      .iter()
      .any(|diagnostic: &Diagnostic| {
        diagnostic.primary().source() == b.get()
          && diagnostic.code().as_str().starts_with("smear::syntax::")
      }),
    "{:#?}",
    analysis.diagnostics()
  );

  let texts = analysis.sources().bind(&root, &[]);
  assert_eq!(texts.text(a), Some("type A { ok: Int }\n"));
  assert_eq!(texts.path(a), Some("a.graphqlx"));
  assert_eq!(texts.text(b), None);
  assert_eq!(texts.path(b), Some("b.graphqlx"));

  // Every package source, the built-ins' included, is the empty text with the empty path.
  let model = analysis.model();
  for package in model.packages() {
    let first = model
      .package(package)
      .sources()
      .into_iter()
      .next()
      .expect("a package source");
    assert!(analysis.sources().source(first).is_package_source());
    assert_eq!(texts.text(first), Some(""));
    assert_eq!(texts.path(first), Some(""));
  }

  // An id at or past the map's length is the consumer's.
  let past = SourceId::new(analysis.sources().len());
  assert_eq!(texts.text(past), None);
  assert_eq!(texts.path(past), None);
  assert_eq!(texts.text(SourceId::new(u32::MAX)), None);
}

#[test]
fn bind_answers_none_for_a_file_that_is_missing_or_changed() {
  let root = package(
    "app",
    &[
      ("a.graphqlx", OK),
      ("b.graphqlx", "scalar B\n"),
      ("c.graphqlx", "scalar C\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  let (a, b, c) = (
    file(&analysis, 3, "a.graphqlx"),
    file(&analysis, 3, "b.graphqlx"),
    file(&analysis, 3, "c.graphqlx"),
  );

  // `b` is gone and `c` grew: neither is the text the analysis read.
  let edited = package("app", &[("a.graphqlx", OK), ("c.graphqlx", "scalar Cc\n")]);
  let texts = analysis.sources().bind(&edited, &[]);
  assert_eq!(texts.text(a), Some(OK));
  assert_eq!(texts.text(b), None);
  assert_eq!(texts.text(c), None);
  // The paths are the analysis's own, whatever was bound.
  assert_eq!(texts.path(b), Some("b.graphqlx"));
}

/// The ids of the files at `path` in package `package`, in id order.
fn files_at(analysis: &Analysis, package: u32, path: &str) -> Vec<SourceId> {
  let sources = analysis.sources();
  analysis
    .model()
    .package(PackageId::new(package))
    .sources()
    .into_iter()
    .filter(|id: &SourceId| sources.source(*id).path() == path)
    .collect()
}

/// Two files at one path are refused, and nothing but their texts tells them apart, so `bind`
/// answers neither a text: not after one is edited, which reorders the two, and not unchanged. A
/// file with a path of its own is answered all the same.
#[test]
fn files_that_share_a_path_are_answered_no_text() {
  let root = package(
    "app",
    &[
      ("a.graphqlx", "scalar A"),
      ("a.graphqlx", "scalar B"),
      ("c.graphqlx", "scalar C"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  // Files are numbered by path, then by text.
  let shared = files_at(&analysis, 3, "a.graphqlx");
  assert_eq!(shared.len(), 2);
  let c = file(&analysis, 3, "c.graphqlx");
  let read = [
    (shared[0], "scalar A"),
    (shared[1], "scalar B"),
    (c, "scalar C"),
  ];

  // `scalar A` becomes `scalar ZZ`, which sorts after `scalar B`.
  let edited = package(
    "app",
    &[
      ("a.graphqlx", "scalar ZZ"),
      ("a.graphqlx", "scalar B"),
      ("c.graphqlx", "scalar C"),
    ],
  );
  let texts = analysis.sources().bind(&edited, &[]);
  for (id, text) in read {
    assert!(
      texts.text(id).is_none_or(|bound: &str| bound == text),
      "{id:?} read {text:?} and is answered {:?}",
      texts.text(id)
    );
  }
  assert_eq!(texts.text(shared[0]), None);
  assert_eq!(texts.text(shared[1]), None);

  let texts = analysis.sources().bind(&root, &[]);
  assert_eq!(texts.text(shared[0]), None);
  assert_eq!(texts.text(shared[1]), None);
  assert_eq!(texts.text(c), Some("scalar C"));
}

/// A file alone at its path when analyzed is answered no text once the package given holds a
/// second file at that path, though the second is as long as the text the analysis read.
#[test]
fn a_file_is_answered_no_text_once_another_shares_its_path() {
  let root = package("app", &[("a.graphqlx", "scalar A")]);
  let analysis = analyze(&root, &[], &Limits::new());
  let a = file(&analysis, 3, "a.graphqlx");
  assert_eq!(
    analysis.sources().bind(&root, &[]).text(a),
    Some("scalar A")
  );

  // `scalar 0` sorts before `scalar A`.
  let crowded = package(
    "app",
    &[("a.graphqlx", "scalar A"), ("a.graphqlx", "scalar 0")],
  );
  assert_eq!(analysis.sources().bind(&crowded, &[]).text(a), None);
}

/// The root is found by its place and its name, so a root given under another name is answered no
/// text for its files, though they are the same; the built-ins' files, every package source and a
/// dependency's files are answered all the same.
#[test]
fn the_root_given_under_another_name_is_answered_no_text() {
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["geo"]);
  let geo = package("geo", &[("p.graphqlx", "scalar P\n")]);
  let analysis = analyze(&root, &[geo.clone()], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let other = package("other", &[("a.graphqlx", OK)]).with_dependencies(["geo"]);
  let dependencies = [geo];
  let texts = analysis.sources().bind(&other, &dependencies);
  let sources = analysis.sources();
  for id in sources.sources() {
    let source = sources.source(id);
    if source.is_package_source() {
      assert_eq!(texts.text(id), Some(""), "{id:?}");
    } else if source.package() == analysis.model().root_package() {
      assert_eq!(texts.text(id), None, "{id:?}");
    } else {
      assert!(texts.text(id).is_some(), "{id:?}");
    }
  }
  assert_eq!(texts.text(file(&analysis, 3, "a.graphqlx")), None);
  assert_eq!(
    texts.text(file(&analysis, 4, "p.graphqlx")),
    Some("scalar P\n")
  );
}

/// Two dependencies with one name are refused, and nothing but what they hold tells them apart, so
/// none of their files is answered a text: unchanged, or after an edit that would exchange the two.
/// A third dependency's files are answered, and so are those of a dependency named as the root,
/// which is the only dependency with its name.
#[test]
fn dependencies_that_share_a_name_are_answered_no_text() {
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["geo", "zeta"]);
  let first = package("geo", &[("p.graphqlx", "scalar A\n")]);
  let second = package("geo", &[("p.graphqlx", "scalar B\n")]);
  let zeta = package("zeta", &[("z.graphqlx", "scalar Z\n")]);
  let named_as_root = package("app", &[("r.graphqlx", "scalar R\n")]);
  let unchanged = [first, second.clone(), zeta.clone(), named_as_root.clone()];
  let analysis = analyze(&root, &unchanged, &Limits::new());

  // Dependencies are numbered by name, then by what they hold: the `app` is 4, the `geo` holding
  // `scalar A` is 5 and the one holding `scalar B` is 6, and `zeta` is 7.
  let geo = [
    file(&analysis, 5, "p.graphqlx"),
    file(&analysis, 6, "p.graphqlx"),
  ];
  let z = file(&analysis, 7, "z.graphqlx");
  let r = file(&analysis, 4, "r.graphqlx");

  // `scalar A` becomes `scalar C`, which sorts after `scalar B`: the two `geo`s trade places.
  let edited = [
    package("geo", &[("p.graphqlx", "scalar C\n")]),
    second,
    zeta,
    named_as_root,
  ];
  for given in [&unchanged, &edited] {
    let texts = analysis.sources().bind(&root, given);
    for id in geo {
      assert_eq!(texts.text(id), None, "{id:?}");
    }
    assert_eq!(texts.text(z), Some("scalar Z\n"));
    assert_eq!(texts.text(r), Some("scalar R\n"));
  }
}

#[test]
fn bind_finds_each_dependency_by_name_whatever_order_it_is_given_in() {
  let root = package("app", &[("a.graphqlx", OK)]).with_dependencies(["geo", "zeta"]);
  let geo = package("geo", &[("p.graphqlx", "type Point { x: Int }\n")]);
  let zeta = package("zeta", &[("z.graphqlx", "scalar Z\n")]);
  let analysis = analyze(&root, &[geo.clone(), zeta.clone()], &Limits::new());
  assert!(analysis.is_ok(), "{:#?}", analysis.diagnostics());

  let given_order = [geo.clone(), zeta.clone()];
  let given = analysis.sources().bind(&root, &given_order);
  let swapped_order = [zeta, geo];
  let swapped = analysis.sources().bind(&root, &swapped_order);
  for id in analysis.sources().sources() {
    assert!(given.text(id).is_some(), "{id:?}");
    assert_eq!(given.text(id), swapped.text(id), "{id:?}");
  }
  assert_eq!(
    given.text(file(&analysis, 4, "p.graphqlx")),
    Some("type Point { x: Int }\n")
  );
  assert_eq!(
    given.text(file(&analysis, 5, "z.graphqlx")),
    Some("scalar Z\n")
  );
}

/// One syntax error, as everything but its source: code, message, primary label, help and span.
type Reported = (
  Code,
  String,
  Option<String>,
  Option<String>,
  Option<SimpleSpan>,
);

/// What a diagnostic says, apart from its source.
fn reported(diagnostic: &dyn Diagnose) -> Reported {
  (
    diagnostic.code(),
    diagnostic.to_string(),
    diagnostic.primary_label().map(String::from),
    diagnostic.help().map(String::from),
    diagnostic.primary().span(),
  )
}

/// Every syntax error the GraphQLx parser itself reports for `text`, sorted.
fn parser_errors(text: &str) -> Vec<Reported> {
  let errors = Parser::with_parser::<
    '_,
    GraphqlxLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlxErrors<&str>,
    _,
    GraphQLx,
  >(type_system_document)
  .parse_str(text)
  .expect_err("the fixture does not parse");
  let mut errors: Vec<Reported> = errors
    .iter()
    .map(|error| reported(error))
    .collect::<Vec<Reported>>();
  errors.sort();
  errors
}

#[test]
fn a_syntax_error_is_reported_at_its_file_and_the_other_files_are_still_parsed() {
  let broken = "type B {\n";
  let misshapen = "type C { x: }\n";
  let root = package(
    "app",
    &[
      ("a.graphqlx", OK),
      ("b.graphqlx", broken),
      ("c.graphqlx", misshapen),
      ("d.graphqlx", "scalar D\n"),
    ],
  );
  let analysis = analyze(&root, &[], &Limits::new());
  assert!(!analysis.is_ok());
  let b = file(&analysis, 3, "b.graphqlx");
  let c = file(&analysis, 3, "c.graphqlx");

  let mut sources: Vec<u32> = analysis
    .diagnostics()
    .iter()
    .map(|diagnostic: &Diagnostic| diagnostic.primary().source())
    .collect();
  sources.dedup();
  assert_eq!(sources, [b.get(), c.get()], "{:#?}", analysis.diagnostics());

  // At each file, exactly the parser's own diagnostics, moved to that file.
  for (id, text) in [(b, broken), (c, misshapen)] {
    let mut here: Vec<Reported> = analysis
      .diagnostics()
      .iter()
      .filter(|diagnostic: &&Diagnostic| diagnostic.primary().source() == id.get())
      .map(|diagnostic: &Diagnostic| reported(diagnostic))
      .collect::<Vec<Reported>>();
    here.sort();
    let expected = parser_errors(text);
    assert!(!expected.is_empty());
    assert_eq!(here, expected, "{text:?}");
    for (code, _, _, _, span) in &here {
      assert!(code.as_str().starts_with("smear::syntax::"), "{code}");
      assert!(
        span.is_some_and(|span: SimpleSpan| span.end() <= text.len()),
        "{span:?}"
      );
    }
  }
}

/// Everything an analysis answers that does not depend on the text type: the diagnostics, every
/// source's package, path and length, and the text each source binds to.
#[allow(clippy::type_complexity)]
fn summary<S: SourceText>(
  package: &Package<S>,
) -> (
  Diagnostics,
  Vec<(PackageId, String, usize, bool, Option<String>)>,
) {
  let analysis = analyze(package, &[], &Limits::new());
  let texts = analysis.sources().bind(package, &[]);
  let sources = analysis.sources();
  let described = sources
    .sources()
    .into_iter()
    .map(|id: SourceId| {
      let source = sources.source(id);
      (
        source.package(),
        String::from(source.path()),
        source.len(),
        source.is_package_source(),
        texts.text(id).map(String::from),
      )
    })
    .collect::<Vec<(PackageId, String, usize, bool, Option<String>)>>();
  (analysis.diagnostics().clone(), described)
}

/// The files every text type is asked to agree on: text outside ASCII where a document may hold
/// it, and a syntax error.
const FILES: &[(&str, &str)] = &[
  ("a.graphqlx", OK),
  ("b/c.graphqlx", "\"caf\u{e9}, d\u{e9}crit\" scalar C\n"),
  ("b.graphqlx", "type B {\n"),
];

/// The package of `FILES`, each text in the representation `text` makes.
fn files<S>(text: impl Fn(&'static str) -> S) -> Package<S> {
  Package::new("app", Layout::Directory, Tier::Std).with_files(
    FILES
      .iter()
      .map(|&(path, source): &(&'static str, &'static str)| SourceFile::new(path, text(source))),
  )
}

#[test]
fn every_text_type_gives_the_same_ids_diagnostics_and_texts() {
  let baseline = summary(&files(|text: &'static str| text));
  assert!(baseline.0.has_errors(), "the fixture holds a syntax error");
  assert!(
    baseline.1.iter().all(|row| row.3 || row.4.is_some()),
    "every file is UTF-8, so every file binds"
  );

  assert_eq!(
    summary(&files(|text: &'static str| text.as_bytes())),
    baseline,
    "&[u8]"
  );

  #[cfg(feature = "bstr_1")]
  assert_eq!(
    summary(&files(|text: &'static str| bstr_1::BStr::new(text))),
    baseline,
    "&BStr"
  );

  #[cfg(feature = "bytes_1")]
  assert_eq!(
    summary(&files(|text: &'static str| {
      bytes_1::Bytes::from_static(text.as_bytes())
    })),
    baseline,
    "Bytes"
  );

  #[cfg(feature = "hipstr_0_8")]
  {
    assert_eq!(
      summary(&files(|text: &'static str| hipstr_0_8::HipStr::from(text))),
      baseline,
      "HipStr"
    );
    assert_eq!(
      summary(&files(|text: &'static str| {
        hipstr_0_8::HipByt::from(text.as_bytes())
      })),
      baseline,
      "HipByt"
    );
  }

  #[cfg(feature = "smol-bytes_0_2")]
  {
    assert_eq!(
      summary(&files(|text: &'static str| {
        smol_bytes_0_2::shared::Bytes::from(text)
      })),
      baseline,
      "shared::Bytes"
    );
    assert_eq!(
      summary(&files(|text: &'static str| {
        smol_bytes_0_2::compact::Bytes::from(text)
      })),
      baseline,
      "compact::Bytes"
    );
    assert_eq!(
      summary(&files(
        |text: &'static str| smol_bytes_0_2::Utf8Bytes::from(text)
      )),
      baseline,
      "Utf8Bytes"
    );
    assert_eq!(
      summary(&files(|text: &'static str| {
        smol_bytes_0_2::compact::Utf8Bytes::from(text)
      })),
      baseline,
      "compact::Utf8Bytes"
    );
  }
}

/// A consumer reports at a source of its own, at or past the analysis's sources, and its
/// diagnostic joins smear's in one list that renders past every source the analysis has.
#[test]
fn a_consumer_reports_at_its_own_sources_past_the_analysis() {
  let root = package("app", &[("a.graphqlx", "type A {\n")]);
  let analysis = analyze(&root, &[], &Limits::new());
  let own = analysis.sources().len();

  let mine = Diagnostic::builder(
    Code::new("ig::config::unknown-key"),
    Location::new(own, SimpleSpan::new(0, 3)),
  )
  .message("`foo` is not a key this tool reads")
  .build();
  assert_eq!(mine.primary().source(), own);
  assert_eq!(
    analysis.sources().bind(&root, &[]).text(SourceId::new(own)),
    None
  );

  let mut all: Diagnostics = [mine.clone()].into_iter().collect();
  all.merge(analysis.diagnostics().clone());
  assert_eq!(all.len(), analysis.diagnostics().len() + 1);
  assert_eq!(all.iter().next_back(), Some(&mine));
  assert_eq!(all.error_count(), analysis.diagnostics().error_count() + 1);
}

//! One analysis, whichever GraphQLx parser layer reads the files: `analyze` is `analyze_with` over
//! the syntactic layer; the equality two analyses are compared by can answer no; the two layers
//! answer equal analyses wherever neither reports a syntax error; every error the lossless layer
//! lends is one diagnostic at its file; two broken definitions cost themselves and nothing else;
//! and which files never reach a layer and have no tree.
//!
//! Analyses are compared with `==` and a failure names where they part, through `differ`: the
//! `Debug` of an analysis is every table of its model, the built-in library's included.
#![cfg(all(any(feature = "std", feature = "alloc"), feature = "analysis"))]

use smear::{
  analysis::graphqlx::{
    Analysis, Layout, Limits, Package, SourceFile, SourceText, Tier, analyze, analyze_with,
  },
  parser::graphqlx::{Layer, Syntactic},
};

#[allow(dead_code)]
#[path = "support/analysis_view.rs"]
mod view;

use view::{differ, sets};

/// T8's analysis, written once over the layer: `root` beside `dependencies`, read by `parser`,
/// under the default limits, and what the layer kept dropped.
fn analysis<'a, P: Layer<'a>, S: SourceText>(
  parser: &P,
  root: &'a Package<S>,
  dependencies: &'a [Package<S>],
) -> Analysis {
  analyze_with(parser, root, dependencies, &Limits::new()).0
}

/// The root package `app`, `Flat`, of the one file `schema.graphqlx`, holding `text`.
fn one_file<S>(text: S) -> Package<S> {
  Package::new("app", Layout::Flat, Tier::Std)
    .with_files([SourceFile::new("schema.graphqlx", text)])
}

/// `analyze` is `analyze_with` over the syntactic layer at its default limits, with what the layer
/// kept dropped: over every package set written by hand, the three rules' fixtures among them, a
/// set with a syntax error, and a set of byte texts.
#[test]
fn analyze_is_analyze_with_the_syntactic_layer() {
  for set in sets() {
    let alone = analyze(&set.root, &set.dependencies, &Limits::new());
    let with = analysis(&Syntactic::new(), &set.root, &set.dependencies);
    assert!(alone == with, "{}: {}", set.name, differ(&alone, &with));
  }

  // `layers_graphqlx.rs`'s broken file.
  let broken = one_file("type Query { hero: String }\ntype Half { f: }");
  let alone = analyze(&broken, &[], &Limits::new());
  assert!(!alone.is_ok(), "the broken file is refused");
  let with = analysis(&Syntactic::new(), &broken, &[]);
  assert!(alone == with, "a syntax error: {}", differ(&alone, &with));

  // `analysis_modules_declarations.rs`'s byte texts: a comment, and a comment and a stray `}`.
  let bytes: Package<&[u8]> = Package::new("app", Layout::Flat, Tier::Std).with_files([
    SourceFile::new("notes.graphqlx", "# Only a comment.\n".as_bytes()),
    SourceFile::new("junk.graphqlx", "# a comment\n}\n".as_bytes()),
  ]);
  let alone = analyze(&bytes, &[], &Limits::new());
  assert!(!alone.is_ok(), "the stray `}}` is refused");
  let with = analysis(&Syntactic::new(), &bytes, &[]);
  assert!(alone == with, "byte texts: {}", differ(&alone, &with));
}

/// What the equality of two analyses reads, shown by what it tells apart. An analysis equals a
/// second run of itself; it differs from the same text with one field renamed to a name of the
/// same length, from the same tokens one space later in a text of the same length, and from
/// itself made under another diagnostic ceiling. In each pair of the three, every part the guard
/// reads is equal, so the difference is the one part left: the spellings, the locations, the
/// ceiling.
#[test]
fn the_equality_can_answer_no() {
  let limits = Limits::new();
  // `layers_graphqlx.rs`'s two items, which analyze in silence.
  let text = "type Query { hero: String }\nmod inner { type Inner { f: Int } }\n";
  let first = analyze(&one_file(text), &[], &limits);
  assert!(
    first.is_ok() && first.diagnostics().is_empty(),
    "{:#?}",
    first.diagnostics()
  );
  let again = analyze(&one_file(text), &[], &limits);
  assert!(first == again, "a second run: {}", differ(&first, &again));

  let renamed = analyze(
    &one_file("type Query { zero: String }\nmod inner { type Inner { f: Int } }\n"),
    &[],
    &limits,
  );
  assert!(
    first.sources() == renamed.sources() && first.diagnostics() == renamed.diagnostics(),
    "a field renamed moves no source and no diagnostic: {}",
    differ(&first, &renamed)
  );
  assert!(
    first != renamed,
    "a field renamed `zero` answers the same analysis"
  );

  // The trailing newline moved to the front: every token one byte later, and the text as long.
  let later = analyze(
    &one_file(" type Query { hero: String }\nmod inner { type Inner { f: Int } }"),
    &[],
    &limits,
  );
  assert!(
    first.sources() == later.sources() && first.diagnostics() == later.diagnostics(),
    "the same tokens one byte later move no source and no diagnostic: {}",
    differ(&first, &later)
  );
  assert!(
    first != later,
    "the same tokens one byte later answer the same analysis"
  );

  let ceiling = analyze(&one_file(text), &[], &limits.with_max_diagnostics(1));
  assert!(
    first.model() == ceiling.model()
      && first.diagnostics() == ceiling.diagnostics()
      && first.sources() == ceiling.sources(),
    "another ceiling changes nothing it lets through: {}",
    differ(&first, &ceiling)
  );
  assert!(
    first != ceiling,
    "an analysis made under another diagnostic ceiling answers the same analysis"
  );
}

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use smear::{
    analysis::graphqlx::{
      Analysis, DefId, Diagnostic, Layout, Limits, Model, ModuleId, Package, PackageKind,
      SourceFile, SourceId, SourceText, Tier, Trees, analyze, analyze_with,
    },
    diagnostic::{Diagnose, DiagnoseExt, Label, Location, Severity},
    lexer::{
      SyntaxErrorKind,
      limits::{LosslessLimits, SyntacticLimits},
    },
    parser::graphqlx::{
      Lossless, Syntactic,
      error::GraphqlxError,
      lossless::{GraphqlxLosslessErrorValue, Parse},
    },
  };

  use super::{
    analysis, one_file,
    view::{blank, corpus, differ, sets},
  };

  /// One error the lossless layer lends.
  type Lent<'s> = GraphqlxLosslessErrorValue<&'s str>;

  /// How many diagnostics of `analysis` have a code of the parser's, `smear::syntax::…`.
  fn syntax_diagnostics(analysis: &Analysis) -> usize {
    analysis
      .diagnostics()
      .iter()
      .filter(|diagnostic: &&Diagnostic| diagnostic.code().as_str().starts_with("smear::syntax::"))
      .count()
  }

  /// That `trees` holds a tree for exactly the supplied files of `root` and `dependencies` that are
  /// text, or bytes that are UTF-8, and are not blank: none for a file of a built-in package, a
  /// package source, a blank file or bytes that are not UTF-8. Answers how many it holds.
  fn trees_hold_the_texts<S: SourceText>(
    analysis: &Analysis,
    trees: &Trees<Parse>,
    root: &Package<S>,
    dependencies: &[Package<S>],
    name: &str,
  ) -> usize {
    let model = analysis.model();
    let map = analysis.sources();
    let texts = map.bind(root, dependencies);
    let mut held = 0usize;
    for package in model.packages() {
      let built_in = model.package(package).kind() == PackageKind::BuiltIn;
      for id in model.package(package).sources() {
        let read = !built_in
          && !map.source(id).is_package_source()
          && texts
            .text(id)
            .is_some_and(|text: &str| !blank(text.as_bytes()));
        assert_eq!(
          trees.get(id).is_some(),
          read,
          "{name}: {:?} has a tree exactly when the layer read it",
          map.source(id)
        );
        if read {
          held += 1;
        }
      }
    }
    assert_eq!(
      trees.len(),
      held,
      "{name}: the trees are the supplied texts' and no other's"
    );
    held
  }

  /// One corpus entry, the root package `root`, through both layers: `true` when the two analyses
  /// were compared whole, and `false` when the syntactic one holds a syntax diagnostic and the two
  /// were held to less: the lossless one holds one too, and their source maps are equal.
  fn one_entry<S: SourceText>(name: &str, root: &Package<S>) -> bool {
    let syntactic = analysis(&Syntactic::new(), root, &[]);
    let (lossless, trees) = analyze_with(&Lossless::new(), root, &[], &Limits::new());
    trees_hold_the_texts(&lossless, &trees, root, &[], name);
    if syntax_diagnostics(&syntactic) == 0 {
      assert!(
        lossless == syntactic,
        "{name}: {}",
        differ(&syntactic, &lossless)
      );
      true
    } else {
      assert!(
        syntax_diagnostics(&lossless) > 0,
        "{name}: the syntactic layer reports a syntax error and the lossless layer none"
      );
      assert!(
        lossless.sources() == syntactic.sources(),
        "{name}: {}",
        differ(&syntactic, &lossless)
      );
      false
    }
  }

  /// T8: the two layers answer one analysis.
  ///
  /// Over every package set written by hand, each a set no layer reports a syntax error for: the
  /// analysis the syntactic layer answers equals the one the lossless layer answers, and the
  /// lossless side's trees are a tree for each supplied text that is not blank, so that the second
  /// analysis is seen to be the lossless layer's. For each of the three rules' fixtures, the
  /// syntactic analysis first holds a diagnostic of that rule's code, so equal analyses are equal
  /// rule diagnostics and marks.
  ///
  /// Then every `valid_` corpus entry as the one file of a package, and the first ten again as
  /// byte texts: compared whole where the syntactic analysis holds no syntax diagnostic, and held
  /// to less where it holds one, as an entry that is an operation does under the type-system root.
  #[test]
  fn the_two_layers_answer_one_analysis() {
    let mut sets_compared = 0usize;
    for set in sets() {
      let syntactic = analysis(&Syntactic::new(), &set.root, &set.dependencies);
      if let Some(code) = set.rule {
        assert!(
          syntactic
            .diagnostics()
            .iter()
            .any(|diagnostic: &Diagnostic| diagnostic.code().as_str() == code),
          "{}: the fixture does not fire `{code}`: {:#?}",
          set.name,
          syntactic.diagnostics()
        );
      }
      assert_eq!(
        syntax_diagnostics(&syntactic),
        0,
        "{}: a set written by hand holds a syntax error",
        set.name
      );
      let (lossless, trees) = analyze_with(
        &Lossless::new(),
        &set.root,
        &set.dependencies,
        &Limits::new(),
      );
      assert!(
        lossless == syntactic,
        "{}: {}",
        set.name,
        differ(&syntactic, &lossless)
      );
      trees_hold_the_texts(&lossless, &trees, &set.root, &set.dependencies, set.name);
      sets_compared += 1;
    }

    let entries = corpus("valid_");
    let (mut compared, mut held) = (0usize, 0usize);
    for (name, text) in &entries {
      if one_entry(name, &one_file(text.as_str())) {
        compared += 1;
      } else {
        held += 1;
      }
    }
    let (mut bytes_compared, mut bytes_held) = (0usize, 0usize);
    for (name, text) in entries.iter().take(10) {
      if one_entry(name, &one_file(text.as_bytes())) {
        bytes_compared += 1;
      } else {
        bytes_held += 1;
      }
    }
    println!(
      "T8: {sets_compared} sets written by hand compared; of the valid corpus, {compared} entries \
       compared and {held} held to less; as byte texts, {bytes_compared} compared and \
       {bytes_held} held to less"
    );
    // Every one of the seven sets `sets` writes.
    assert!(
      sets_compared >= 7,
      "only {sets_compared} sets written by hand were compared"
    );
    // `layers_graphqlx.rs`'s `the_two_layers_answer_one_reading_over_the_valid_corpus` holds that
    // at least 20 of these entries are type-system documents to the syntactic layer, by the same
    // call the analyzer makes; such an entry's analysis holds no syntax diagnostic.
    assert!(
      compared >= 20,
      "only {compared} valid entries were compared whole"
    );
    // Of the first ten entries in name order, `valid_document_type_system.graphqlx` is the one
    // that holds no operation and no fragment.
    assert!(bytes_compared >= 1, "no byte text was compared whole");
  }

  /// The parts of a diagnostic a syntax error is read by: its code, where it points, its message,
  /// its primary label, its help, and each label's place and text. Places are spans alone: the
  /// layer's errors point into the one text it was handed, and the analysis's into the file.
  #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
  struct Outline {
    code: &'static str,
    primary: Option<(usize, usize)>,
    message: String,
    primary_label: Option<String>,
    help: Option<String>,
    labels: Vec<(Option<(usize, usize)>, String)>,
  }

  /// The span of `location`, as a pair, or `None` when it has none.
  fn span_of(location: Location) -> Option<(usize, usize)> {
    location.span().map(|span| (span.start(), span.end()))
  }

  /// `diagnostic` read through tokora's contract, which a lent error and an analysis's diagnostic
  /// both answer.
  fn outline(diagnostic: &dyn Diagnose) -> Outline {
    Outline {
      code: diagnostic.code().as_str(),
      primary: span_of(diagnostic.primary()),
      message: diagnostic.to_string(),
      primary_label: diagnostic.primary_label().map(String::from),
      help: diagnostic.help().map(String::from),
      labels: diagnostic
        .labels_iter()
        .map(|label: Label<'_>| (span_of(label.location()), String::from(label.text())))
        .collect::<Vec<(Option<(usize, usize)>, String)>>(),
    }
  }

  /// The one file of the root package of `analysis`.
  fn the_file(analysis: &Analysis) -> SourceId {
    let model = analysis.model();
    model
      .package(model.root_package())
      .sources()
      .into_iter()
      .nth(1)
      .expect("the root package holds a file")
  }

  /// T11: every error the lossless layer lends is one diagnostic, at its file, in the one list.
  ///
  /// For every corpus entry, valid and invalid, as the one file of a package read by the lossless
  /// layer: the list is not truncated; every diagnostic with a code of the parser's points at that
  /// file; and those diagnostics are, as a sorted list of outlines, exactly the errors the layer
  /// lends a `Vec` for the same text. A blank text never reaches the layer: it has no tree and no
  /// syntax diagnostic.
  #[test]
  fn every_lent_error_is_one_diagnostic_at_its_file() {
    let (mut lending, mut blanks) = (0usize, 0usize);
    for (name, text) in corpus("") {
      let root = one_file(text.as_str());
      let (analysis, trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
      assert!(
        !analysis.diagnostics().truncated(),
        "{name}: the list is truncated"
      );
      let file = the_file(&analysis);
      let syntax: Vec<&Diagnostic> = analysis
        .diagnostics()
        .iter()
        .filter(|diagnostic: &&Diagnostic| {
          diagnostic.code().as_str().starts_with("smear::syntax::")
        })
        .collect::<Vec<&Diagnostic>>();
      for diagnostic in &syntax {
        assert_eq!(
          diagnostic.primary().source(),
          file.get(),
          "{name}: a syntax diagnostic points at another source: {diagnostic:#?}"
        );
        for label in diagnostic.labels_iter() {
          assert_eq!(
            label.location().source(),
            file.get(),
            "{name}: a syntax diagnostic's label points at another source: {diagnostic:#?}"
          );
        }
      }
      if blank(text.as_bytes()) {
        assert!(trees.get(file).is_none(), "{name}: a blank text has a tree");
        assert!(
          syntax.is_empty(),
          "{name}: a blank text has a syntax diagnostic: {syntax:#?}"
        );
        blanks += 1;
        continue;
      }
      assert!(trees.get(file).is_some(), "{name}: the layer kept no tree");
      let mut lent: Vec<Lent<'_>> = Vec::new();
      let _ = Lossless::new()
        .type_system_document(text.as_str(), &mut lent)
        .expect("a corpus text is short enough for a tree");
      let mut expected: Vec<Outline> = lent
        .iter()
        .map(|error: &Lent<'_>| outline(error))
        .collect::<Vec<Outline>>();
      let mut found: Vec<Outline> = syntax
        .iter()
        .map(|diagnostic: &&Diagnostic| outline(*diagnostic))
        .collect::<Vec<Outline>>();
      expected.sort();
      found.sort();
      assert_eq!(
        found, expected,
        "{name}: the syntax diagnostics are not the errors the layer lends"
      );
      if !lent.is_empty() {
        lending += 1;
      }
    }
    println!("T11: {lending} entries lent an error; {blanks} blank entries reached no layer");
    // `invalid_comment_only_document.graphqlx` holds a comment and nothing else.
    assert!(
      blanks >= 1,
      "no blank entry was read: the shortcut is not seen"
    );
    // The corpus holds 68 `invalid_` entries, two of them blank (`invalid_empty_document` and
    // `invalid_comment_only_document`). `layers_handover_graphqlx.rs` holds every one to a
    // rejection by the syntactic layer, and the lossless layer's verdict agrees, so each of the
    // other 66 lends an error here.
    assert!(lending >= 20, "only {lending} entries lent an error");
  }

  /// The names of the definitions `module` holds, in id order.
  fn names(model: &Model, module: ModuleId) -> Vec<&str> {
    model
      .module(module)
      .defs()
      .into_iter()
      .map(|id: DefId| model.def(id).name())
      .collect::<Vec<&str>>()
  }

  /// Whether `at` lies within `range` of the file `file`.
  fn within(at: Location, file: SourceId, range: core::ops::Range<usize>) -> bool {
    at.source() == file.get()
      && at
        .span()
        .is_some_and(|span| range.start <= span.start() && span.end() <= range.end)
  }

  /// T11: two broken definitions and three whole ones in one file.
  ///
  /// Each broken one is a shape `recovery_graphqlx.rs`'s
  /// `a_missing_field_type_keeps_the_definition_after_it` holds to cost its own definition and
  /// keep the next: a missing field type and a missing default value. The root is named `Query`,
  /// and it names the other two whole ones as field types. Through the lossless layer the file's
  /// module declares the three whole ones and no other, each broken one holds a syntax diagnostic,
  /// and every error is a syntax error: no use of the three is reported unknown. Through `analyze`
  /// the file declares none of the five.
  #[test]
  fn two_broken_definitions_cost_those_two_and_nothing_else() {
    let whole = "type Query { a: A, c: C }\ntype A { x: Int }\nscalar C\n";
    let broken_type = "type B { b: }";
    let broken_input = "input D { d: Int = }";
    let text = format!(
      "{broken_type}\ntype Query {{ a: A, c: C }}\n{broken_input}\ntype A {{ x: Int }}\nscalar C\n"
    );

    let alone = analyze(&one_file(whole), &[], &Limits::new());
    assert!(
      alone.is_ok() && alone.diagnostics().is_empty(),
      "the three whole definitions alone: {:#?}",
      alone.diagnostics()
    );

    let root = one_file(text.as_str());
    let (analysis, _trees) = analyze_with(&Lossless::new(), &root, &[], &Limits::new());
    let model = analysis.model();
    let app = model.package(model.root_package()).root_module();
    assert_eq!(names(model, app), ["Query", "A", "C"], "{text}");
    for diagnostic in analysis.diagnostics().iter() {
      if diagnostic.severity() == Severity::Error {
        assert!(
          diagnostic.code().as_str().starts_with("smear::syntax::"),
          "an error that is no syntax error: {diagnostic:#?}"
        );
      }
    }
    let file = the_file(&analysis);
    let at_input = text
      .find(broken_input)
      .expect("the text holds the broken input");
    for (what, range) in [
      ("the type", 0..broken_type.len()),
      ("the input", at_input..at_input + broken_input.len()),
    ] {
      assert!(
        analysis
          .diagnostics()
          .iter()
          .any(|diagnostic: &Diagnostic| {
            diagnostic.code().as_str().starts_with("smear::syntax::")
              && within(diagnostic.primary(), file, range.clone())
          }),
        "no syntax diagnostic points into {what}: {:#?}",
        analysis.diagnostics()
      );
    }

    let alone = analyze(&root, &[], &Limits::new());
    let model = alone.model();
    let app = model.package(model.root_package()).root_module();
    assert!(
      names(model, app).is_empty(),
      "the syntactic layer reads no document of a broken file"
    );
  }

  /// The depth of `layers_bounds.rs`'s nested text, past the default nesting ceiling of 24.
  const DEPTH: usize = 30;

  /// A ceiling the nested text is within.
  const RAISED: usize = 48;

  /// T14: a byte text that is not UTF-8 is read by the syntactic layer at its default limits,
  /// whatever layer the analysis is given, and has no tree.
  ///
  /// The text is `layers_bounds.rs`'s `nested(30)`, which nests past the default ceiling, and
  /// after it a byte that is not UTF-8. At the parser, first: over these bytes the syntactic layer
  /// at its default limits lends a resource-limit error, and under a ceiling of 48 none of that
  /// kind, so the two readings of the bytes report different errors. The analysis reports the
  /// first through every layer, and equals what `analyze` answers.
  #[test]
  fn a_byte_text_that_is_not_utf8_is_read_under_the_default_limits() {
    let mut bytes: Vec<u8> = format!(
      "scalar A\ntype T {{ f: {}Int{} }}\nscalar Z\n",
      "[".repeat(DEPTH),
      "]".repeat(DEPTH)
    )
    .into_bytes();
    bytes.push(0xFF);

    let mut lent: Vec<GraphqlxError<&[u8]>> = Vec::new();
    let read = Syntactic::new().type_system_document(bytes.as_slice(), &mut lent);
    assert!(read.is_err(), "the bytes are a document");
    let depth = lent
      .iter()
      .find(|error: &&GraphqlxError<&[u8]>| error.kind() == SyntaxErrorKind::ResourceLimit)
      .map(|error: &GraphqlxError<&[u8]>| error.code())
      .expect("at its default limits the syntactic layer lends no resource-limit error");
    let mut lent: Vec<GraphqlxError<&[u8]>> = Vec::new();
    let read = Syntactic::with_limits(SyntacticLimits::with_max_nesting_depth(RAISED))
      .type_system_document(bytes.as_slice(), &mut lent);
    assert!(
      read.is_err(),
      "a byte that is not UTF-8 is a syntax error under any ceiling"
    );
    assert!(
      !lent
        .iter()
        .any(|error: &GraphqlxError<&[u8]>| error.kind() == SyntaxErrorKind::ResourceLimit),
      "under a ceiling of {RAISED} the syntactic layer still lends a resource-limit error"
    );

    let root: Package<&[u8]> = Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", bytes.as_slice())]);
    let alone = analyze(&root, &[], &Limits::new());
    assert!(
      alone
        .diagnostics()
        .iter()
        .any(|diagnostic: &Diagnostic| diagnostic.code() == depth),
      "the analysis does not report the depth: {:#?}",
      alone.diagnostics()
    );
    let file = the_file(&alone);

    let syntactic = Syntactic::with_limits(SyntacticLimits::with_max_nesting_depth(RAISED));
    let (with, trees) = analyze_with(&syntactic, &root, &[], &Limits::new());
    assert!(
      alone == with,
      "the syntactic layer under a raised ceiling: {}",
      differ(&alone, &with)
    );
    assert!(trees.get(file).is_none(), "the bytes have a tree");

    let lossless = Lossless::with_limits(LosslessLimits::with_max_nesting_depth(RAISED));
    let (with, trees) = analyze_with(&lossless, &root, &[], &Limits::new());
    assert!(
      alone == with,
      "the lossless layer under a raised ceiling: {}",
      differ(&alone, &with)
    );
    assert!(trees.get(file).is_none(), "the bytes have a tree");
  }

  /// T14: through the lossless layer, no file of a built-in package has a tree, and every supplied
  /// file that is text and not blank has one. Over every package set written by hand.
  #[test]
  fn a_built_in_file_has_no_tree() {
    let mut held = 0usize;
    for set in sets() {
      let (analysis, trees) = analyze_with(
        &Lossless::new(),
        &set.root,
        &set.dependencies,
        &Limits::new(),
      );
      held += trees_hold_the_texts(&analysis, &trees, &set.root, &set.dependencies, set.name);
    }
    // The sets hold 17 supplied files that are not blank: six, two, one, three, then one, two
    // and two.
    assert!(held >= 17, "only {held} trees were held to their files");
  }
}

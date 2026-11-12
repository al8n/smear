#![cfg(all(
  any(feature = "std", feature = "alloc"),
  feature = "graphqlx",
  feature = "parser"
))]

//! GraphQLx's parser layers at each call site design Y shows: every source the syntactic layer
//! reads, the lossless layer over text, every sink, and the consumers written once over the layer,
//! for every lifetime, and over the analyzer.
//!
//! Each `#[test]` is one call site. What it pins first is that the call compiles as written, in
//! every feature row that compiles this file; what it asserts is what the call site promises.

use core::ops::ControlFlow;

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
use smear::parser::graphqlx::Lossless;
use smear::{
  diagnostic::Diagnose,
  parser::{
    graphqlx::{Layer, Recovery, Report, Syntactic, error::GraphqlxError},
    layer::from_fn,
  },
};

/// A valid document: two items at the top, one of them an inline module holding a third.
const DOC: &str = "type Query { hero: String }\nmod inner { type Inner { f: Int } }";

/// A document the syntactic layer refuses: the field has no type.
const BROKEN: &str = "type Query { hero: }";

// ── The sources the syntactic layer reads ────────────────────────────────────────────────────

/// What the syntactic layer answers for one source, through its inherent method and through the
/// trait, which must agree: the document's top-level item count and its recovery.
macro_rules! syntactic_reads {
  ($source:expr) => {{
    let syntactic = Syntactic::new();
    let inherent = syntactic
      .type_system_document($source, &mut ())
      .expect("the syntactic layer reads the document");
    let through = Layer::type_system_document(&syntactic, $source, &mut ())
      .expect("the syntactic layer reads the document");
    assert!(inherent.is_valid() && through.is_valid());
    assert_eq!((inherent.error_count(), through.error_count()), (0, 0));
    assert_eq!(inherent.recovery(), through.recovery());
    assert_eq!(
      inherent.document().definitions().len(),
      through.document().definitions().len()
    );
    (inherent.document().definitions().len(), inherent.recovery())
  }};
}

/// What `DOC` answers as `&str`, which every other source is held to.
fn over_str() -> (usize, Recovery) {
  syntactic_reads!(DOC)
}

#[test]
fn the_syntactic_layer_reads_str() {
  let (items, recovery) = over_str();
  assert_eq!(items, 2);
  // Every item at every module depth: the type, the module, and the type inside the module.
  assert_eq!(recovery, Recovery::new(3, 0));
}

#[test]
fn the_syntactic_layer_reads_a_byte_slice() {
  assert_eq!(syntactic_reads!(DOC.as_bytes()), over_str());
}

#[cfg(feature = "bstr_1")]
#[test]
fn the_syntactic_layer_reads_bstr() {
  assert_eq!(syntactic_reads!(bstr_1::BStr::new(DOC)), over_str());
}

#[cfg(feature = "bytes_1")]
#[test]
fn the_syntactic_layer_reads_bytes() {
  let body = bytes_1::Bytes::from_static(DOC.as_bytes());
  assert_eq!(syntactic_reads!(&body), over_str());
}

#[cfg(feature = "hipstr_0_8")]
#[test]
fn the_syntactic_layer_reads_hipstr() {
  let text = hipstr_0_8::HipStr::from(DOC);
  assert_eq!(syntactic_reads!(&text), over_str());
}

#[cfg(feature = "hipstr_0_8")]
#[test]
fn the_syntactic_layer_reads_hipbyt() {
  let bytes = hipstr_0_8::HipByt::from(DOC.as_bytes());
  assert_eq!(syntactic_reads!(&bytes), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_shared_bytes() {
  let source = smol_bytes_0_2::shared::Bytes::from(DOC);
  assert_eq!(syntactic_reads!(&source), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_compact_bytes() {
  let source = smol_bytes_0_2::compact::Bytes::from(DOC);
  assert_eq!(syntactic_reads!(&source), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_shared_utf8_bytes() {
  let source = smol_bytes_0_2::Utf8Bytes::from(DOC);
  assert_eq!(syntactic_reads!(&source), over_str());
}

#[cfg(feature = "smol-bytes_0_2")]
#[test]
fn the_syntactic_layer_reads_compact_utf8_bytes() {
  let source = smol_bytes_0_2::compact::Utf8Bytes::from(DOC);
  assert_eq!(syntactic_reads!(&source), over_str());
}

#[test]
fn bytes_that_are_not_utf8_are_rejected_by_the_syntactic_layer() {
  let bad: &[u8] = b"type Query { hero: String }\xFF";
  let mut lent = Vec::new();
  let Err(rejected) = Syntactic::new().type_system_document(bad, &mut lent) else {
    panic!("bytes that are not UTF-8 are not a document");
  };
  assert!(rejected.error_count() >= 1);
  assert_eq!(lent.len(), rejected.error_count());
}

// ── The sinks ────────────────────────────────────────────────────────────────────────────────

#[test]
fn the_sink_that_keeps_nothing() {
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut ()) else {
    panic!("a field with no type is a syntax error");
  };
  assert!(rejected.error_count() >= 1);
}

#[test]
fn an_untyped_vec_keeps_a_clone_of_each_error() {
  let mut errors = Vec::new();
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut errors) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(errors.len(), rejected.error_count());
  assert!(
    errors
      .iter()
      .all(|error: &GraphqlxError<&str>| error.span().end() <= BROKEN.len())
  );
}

#[test]
fn a_closure_that_reads_the_error_writes_its_type_on_the_parameter() {
  let mut kinds = Vec::new();
  let mut sink = from_fn(|error: &GraphqlxError<&str>| {
    kinds.push(error.kind());
    ControlFlow::Continue(())
  });
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut sink) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(kinds.len(), rejected.error_count());
}

#[test]
fn a_closure_that_reads_the_error_writes_its_type_as_a_turbofish() {
  let mut spans = Vec::new();
  let mut sink = from_fn::<GraphqlxError<&str>, _>(|error| {
    spans.push(error.span());
    ControlFlow::Continue(())
  });
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut sink) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(spans.len(), rejected.error_count());
}

#[test]
fn a_closure_that_only_counts_writes_no_type() {
  let mut count = 0usize;
  let mut sink = from_fn(|_| {
    count += 1;
    ControlFlow::Continue(())
  });
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut sink) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(count, rejected.error_count());
}

/// A sink of the caller's own, for every error a layer can lend: it keeps each one's message.
struct Messages(Vec<String>);

impl<E: Diagnose> Report<E> for Messages {
  fn error(&mut self, error: &E) -> ControlFlow<()> {
    self.0.push(error.to_string());
    ControlFlow::Continue(())
  }
}

#[test]
fn a_sink_of_the_callers_own() {
  let mut messages = Messages(Vec::new());
  let Err(rejected) = Syntactic::new().type_system_document(BROKEN, &mut messages) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(messages.0.len(), rejected.error_count());
  assert!(
    messages
      .0
      .iter()
      .all(|message: &String| !message.is_empty())
  );
}

#[test]
fn a_borrowed_sink_and_an_erased_one() {
  let mut messages = Messages(Vec::new());
  let mut forwarded = &mut messages;
  let Err(borrowed) = Syntactic::new().type_system_document(BROKEN, &mut forwarded) else {
    panic!("a field with no type is a syntax error");
  };
  let erased: &mut dyn Report<GraphqlxError<&str>> = &mut messages;
  let Err(through) = Syntactic::new().type_system_document(BROKEN, erased) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(
    messages.0.len(),
    borrowed.error_count() + through.error_count()
  );
}

#[test]
fn a_sink_that_breaks_stops_the_hand_over_and_nothing_else() {
  let mut taken = 0usize;
  let mut sink = from_fn(|_: &GraphqlxError<&str>| {
    taken += 1;
    ControlFlow::Break(())
  });
  let Err(stopped) = Syntactic::new().type_system_document(BROKEN, &mut sink) else {
    panic!("a field with no type is a syntax error");
  };
  let Err(whole) = Syntactic::new().type_system_document(BROKEN, &mut ()) else {
    panic!("a field with no type is a syntax error");
  };
  assert_eq!(taken, 1);
  assert_eq!(stopped, whole);
}

// ── Consumers ────────────────────────────────────────────────────────────────────────────────

/// One reading, written once over the layer: the top-level item count, the recovery, the verdict,
/// the error count, and how many errors the layer lent.
fn read<'s, P: Layer<'s>>(
  parser: &P,
  text: &'s str,
) -> Option<(usize, Recovery, bool, usize, usize)> {
  let mut lent = 0usize;
  let mut sink = from_fn(|_: &P::Error| {
    lent += 1;
    ControlFlow::Continue(())
  });
  let parsed = parser.type_system_document(text, &mut sink).ok()?;
  Some((
    parsed.document().definitions().len(),
    parsed.recovery(),
    parsed.is_valid(),
    parsed.error_count(),
    lent,
  ))
}

#[test]
fn one_reading_serves_both_layers() {
  let (items, recovery, valid, errors, lent) =
    read(&Syntactic::new(), DOC).expect("the syntactic layer reads the document");
  assert_eq!((items, valid, errors, lent), (2, true, 0, 0));
  assert!(recovery.is_complete());
  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  {
    let (items, recovery, valid, errors, lent) =
      read(&Lossless::new(), DOC).expect("the lossless layer reads the document");
    assert_eq!((items, valid, errors, lent), (2, true, 0, 0));
    assert!(recovery.is_complete());
  }
}

/// A function that only names the layer, to keep a value of it: its declaration asks nothing of
/// the source.
#[allow(
  clippy::extra_unused_type_parameters,
  reason = "`Src` is named only in the layer's bound, which is the call site under test"
)]
fn keep<'a, Src: ?Sized, P: Layer<'a, Src>>(parser: P) -> P {
  parser
}

#[test]
fn naming_the_layer_asks_nothing_of_the_source() {
  let over_text = keep::<str, _>(Syntactic::new());
  let over_bytes = keep::<[u8], _>(Syntactic::new());
  assert_eq!(over_text.limits(), over_bytes.limits());
  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  {
    let lossless = keep::<str, _>(Lossless::new());
    assert_eq!(lossless.limits(), Lossless::new().limits());
  }
}

/// A consumer that needs one layer for every lifetime: it reads a text it owns itself.
fn every<P: for<'x> Layer<'x>>(parser: &P) -> bool {
  let text = String::from(DOC);
  let parsed = parser.type_system_document(text.as_str(), &mut ());
  parsed.is_ok()
}

#[test]
fn one_layer_for_every_lifetime() {
  assert!(every(&Syntactic::new()));
  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  assert!(every(&Lossless::new()));
}

// ── The lossless layer ───────────────────────────────────────────────────────────────────────

#[cfg(all(feature = "rowan_0_17", feature = "std"))]
mod lossless {
  use smear::parser::graphqlx::{Layer, Lossless};

  use super::DOC;

  #[test]
  fn the_lossless_layer_reads_a_str() {
    let parsed = Lossless::new()
      .type_system_document(DOC, &mut ())
      .expect("short enough for a tree");
    assert!(parsed.is_valid());
    assert_eq!(parsed.error_count(), 0);
    assert!(!parsed.tree().has_errors());
    assert_eq!(parsed.document().definitions().len(), 2);
  }

  #[test]
  fn the_lossless_layer_reads_an_owned_string_through_as_str() {
    let owned = String::from(DOC);
    let from_owned = Lossless::new()
      .type_system_document(owned.as_str(), &mut ())
      .expect("short enough for a tree");
    let from_str = Lossless::new()
      .type_system_document(DOC, &mut ())
      .expect("short enough for a tree");
    assert_eq!(from_owned.document(), from_str.document());
  }

  // The next two calls compile only while `str` is the one source the lossless layer reads: the
  // compiler takes the source from that layer's single impl, and then coerces the argument as at
  // a `&str` parameter. A second source for the layer makes the source ambiguous again and both
  // stop compiling, which is their purpose: they are deleted with that change, and no
  // documentation shows either call. A holder of a `String` writes `as_str()`.

  #[test]
  fn a_string_reference_is_read_while_the_lossless_layer_has_one_impl() {
    let owned = String::from(DOC);
    let parsed = Lossless::new()
      .type_system_document(&owned, &mut ())
      .expect("short enough for a tree");
    assert!(parsed.is_valid());
  }

  #[test]
  fn as_ref_names_a_source_while_the_lossless_layer_has_one_impl() {
    let owned = String::from(DOC);
    let parsed = Lossless::new()
      .type_system_document(owned.as_ref(), &mut ())
      .expect("short enough for a tree");
    assert!(parsed.is_valid());
  }

  #[test]
  fn the_lossless_layer_through_the_trait() {
    let lossless = Lossless::new();
    let parsed =
      Layer::type_system_document(&lossless, DOC, &mut ()).expect("short enough for a tree");
    assert!(parsed.is_valid());
  }

  #[test]
  fn bytes_reach_the_lossless_layer_decoded() {
    let text = core::str::from_utf8(DOC.as_bytes()).expect("the bytes are UTF-8");
    let parsed = Lossless::new()
      .type_system_document(text, &mut ())
      .expect("short enough for a tree");
    assert!(parsed.is_valid());
  }

  #[test]
  fn a_broken_text_answers_its_well_formed_part() {
    let text = "type Query { hero: String }\ntype Half { f: }";
    let parsed = Lossless::new()
      .type_system_document(text, &mut ())
      .expect("short enough for a tree");
    assert!(!parsed.is_valid());
    assert!(parsed.error_count() >= 1);
    assert!(parsed.tree().has_errors());
    assert_eq!(parsed.document().definitions().len(), 1);
    assert!(!parsed.recovery().is_complete());
  }
}

// ── The analyzer ─────────────────────────────────────────────────────────────────────────────

#[cfg(feature = "analysis")]
mod analysis {
  use smear::{
    analysis::graphqlx::{
      Analysis, Layout, Limits, Package, SourceFile, SourceText, Tier, Trees, analyze, analyze_with,
    },
    parser::graphqlx::{Layer, Syntactic},
  };

  use super::DOC;

  /// T8's analysis, written once over the layer.
  fn analysis<'a, P: Layer<'a>, S: SourceText>(
    parser: &P,
    root: &'a Package<S>,
    dependencies: &'a [Package<S>],
  ) -> Analysis {
    analyze_with(parser, root, dependencies, &Limits::new()).0
  }

  /// A package of one file holding `text`.
  fn package(text: &str) -> Package<&str> {
    Package::new("app", Layout::Flat, Tier::Std)
      .with_files([SourceFile::new("schema.graphqlx", text)])
  }

  #[test]
  fn one_analysis_serves_both_layers() {
    let root = package(DOC);
    let syntactic = analysis(&Syntactic::new(), &root, &[]);
    assert!(syntactic.is_ok());
    assert_eq!(
      syntactic.diagnostics().len(),
      analyze(&root, &[], &Limits::new()).diagnostics().len()
    );
    #[cfg(all(feature = "rowan_0_17", feature = "std"))]
    {
      let lossless = analysis(&smear::parser::graphqlx::Lossless::new(), &root, &[]);
      assert!(lossless.is_ok());
    }
  }

  #[test]
  fn the_syntactic_layer_keeps_nothing() {
    let root = package(DOC);
    let (answer, trees): (Analysis, Trees<()>) =
      analyze_with(&Syntactic::new(), &root, &[], &Limits::new());
    assert!(answer.is_ok());
    assert_eq!(trees.len(), 1);
  }

  /// The trees of an analysis over packages this function builds and drops: they borrow nothing
  /// of them.
  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  fn lossless_trees() -> Trees<smear::parser::graphqlx::lossless::Parse> {
    let text = String::from(DOC);
    let root = package(text.as_str());
    let (answer, trees) = analyze_with(
      &smear::parser::graphqlx::Lossless::new(),
      &root,
      &[],
      &Limits::new(),
    );
    assert!(answer.is_ok());
    drop(root);
    drop(text);
    trees
  }

  #[cfg(all(feature = "rowan_0_17", feature = "std"))]
  #[test]
  fn the_lossless_trees_outlive_the_packages() {
    use smear::{analysis::graphqlx::SourceId, diagnostic::Location, lexer::tokora::SimpleSpan};

    let trees = lossless_trees();
    assert_eq!(trees.len(), 1);
    assert!(!trees.is_empty());
    let (source, tree) = trees.iter().next().expect("the one file has a tree");
    assert!(!tree.has_errors());
    assert!(trees.get(source).is_some());
    assert!(
      trees
        .node(Location::new(source.get(), SimpleSpan::new(0, 4)))
        .is_some()
    );
    assert!(trees.node(Location::entire(source.get())).is_none());

    let mut by_reference: Vec<SourceId> = Vec::new();
    for (id, _) in &trees {
      by_reference.push(id);
    }
    assert_eq!(by_reference, [source]);
    let by_value = trees
      .into_iter()
      .map(|(id, _): (SourceId, smear::parser::graphqlx::lossless::Parse)| id)
      .collect::<Vec<SourceId>>();
    assert_eq!(by_value, [source]);
  }
}

// ── Both dialects in one scope ───────────────────────────────────────────────────────────────

#[cfg(feature = "graphql")]
mod both_dialects {
  use smear::parser::{
    graphql::{self, Layer as GraphqlLayer},
    graphqlx::{self, Layer as GraphqlxLayer},
  };

  use super::DOC;

  /// A valid standard GraphQL query.
  const QUERY: &str = "query Q { hero }";

  #[test]
  fn the_inherent_methods_resolve_with_both_traits_in_scope() {
    let schema = graphqlx::Syntactic::new()
      .type_system_document(DOC, &mut ())
      .expect("the syntactic layer reads the document");
    let query = graphql::Syntactic::new()
      .executable_document(QUERY, &mut ())
      .expect("the syntactic layer reads the query");
    assert!(schema.is_valid() && query.is_valid());
  }

  #[test]
  fn a_path_call_reaches_each_trait() {
    let schema = GraphqlxLayer::type_system_document(&graphqlx::Syntactic::new(), DOC, &mut ())
      .expect("the syntactic layer reads the document");
    let query = GraphqlLayer::executable_document(&graphql::Syntactic::new(), QUERY, &mut ())
      .expect("the syntactic layer reads the query");
    assert!(schema.is_valid() && query.is_valid());
  }
}

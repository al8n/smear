#![cfg(feature = "validator")]

//! Draft §5: a description is documentation, and nothing a validation rule can see.
//!
//! The current draft gives `OperationDefinition`, `FragmentDefinition` and `VariableDefinition`
//! each a `Description?`, and states the obligation that comes with them: descriptions in
//! executable documents *"MUST NOT affect the execution, validation, or response of a GraphQL
//! document. It is safe to remove all descriptions and comments from executable documents without
//! changing their behavior or results."*
//!
//! That is a claim about behaviour, so it is measured rather than assumed. Two directions have to
//! hold at once and a test that only checks one of them proves half the property:
//!
//! - no rule may **fire** because a description is there;
//! - no rule may be **suppressed** because a description is there.
//!
//! # The twin is blanked, not shortened, and that is what makes the comparison total
//!
//! Every fixture is written once, with descriptions, and its undescribed twin is derived by
//! replacing each description literal with **the same number of spaces**. A twin that merely
//! deleted the description would be a shorter document, every later token would move, and the
//! comparison would have to be weakened to "the same rules fired" — which is exactly the weaker
//! claim this file exists not to settle for. Blanking keeps every other byte where it was, so
//! [`Diagnostic`]'s own [`PartialEq`] — over rule, span, related span, subject slice and
//! schema-side context — is the assertion, `Vec` against `Vec`, order included.
//!
//! GraphQL whitespace is ignored between tokens, so the blanked twin is the same document with
//! the descriptions removed, at the same offsets.
//!
//! # Both doors
//!
//! The syntactic door validates an AST; the lossless door projects a CST and validates that. A
//! description is a **node** in the CST, so the two doors reach the property through different
//! machinery and the lossless leg is where a description could plausibly shift a span. It runs
//! behind `feature = "rowan"`, like every other lossless gate.
//!
//! # The positive control
//!
//! A pair that agrees because neither half produces anything proves nothing, so
//! [`FIRING_FLOOR`] pins how many diagnostics the sweep must have compared and how many distinct
//! rules they must reach. An all-valid corpus fails here.

#![allow(missing_docs)]

use std::{collections::BTreeSet, string::String, vec::Vec};

use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{Budget, Collect, Diagnostic, RuleSet, Schema, Scratch, validate_executable_with},
};

#[allow(dead_code)]
#[path = "support/validator_corpus.rs"]
mod corpus;

use corpus::SCHEMA;

/// The smallest number of diagnostics the sweep is allowed to have compared, and the smallest
/// number of distinct rules they may reach.
///
/// Measured on the day this was written: ten diagnostics over nine distinct rules. The first
/// fixture is a valid document and contributes nothing; the rest are what makes the equality say
/// something.
const FIRING_FLOOR: (usize, usize) = (10, 9);

/// One document, its description literals, and what it is for.
///
/// Each literal listed here is blanked to build the twin, so a literal that is *not* a
/// description — a string argument, say — must never appear in this column.
struct Fixture {
  what: &'static str,
  source: &'static str,
  descriptions: &'static [&'static str],
}

const FIXTURES: &[Fixture] = &[
  Fixture {
    what: "a valid document described at all three positions",
    source: concat!(
      "\"\"\"the operation\"\"\" query Q(\"\"\"the variable\"\"\" $atOtherHomes: Boolean) {\n",
      "  dog { ...info isHouseTrained(atOtherHomes: $atOtherHomes) }\n",
      "}\n",
      "\"\"\"the fragment\"\"\" fragment info on Dog { name nickname }\n",
    ),
    descriptions: &[
      "\"\"\"the operation\"\"\"",
      "\"\"\"the variable\"\"\"",
      "\"\"\"the fragment\"\"\"",
    ],
  },
  Fixture {
    what: "a described operation selecting a field the type does not have",
    source: "\"\"\"the operation\"\"\" query Q { dog { nope } }\n",
    descriptions: &["\"\"\"the operation\"\"\""],
  },
  Fixture {
    what: "a described fragment on a type the schema does not have",
    source: concat!(
      "query Q { dog { ...info } }\n",
      "\"\"\"the fragment\"\"\" fragment info on Nope { name }\n",
    ),
    descriptions: &["\"\"\"the fragment\"\"\""],
  },
  Fixture {
    what: "a described variable the operation never uses",
    source: "\"the operation\" query Q(\"the variable\" $unused: Boolean) { dog { name } }\n",
    descriptions: &["\"the operation\"", "\"the variable\""],
  },
  Fixture {
    what: "a described variable declared twice",
    source: concat!(
      "query Q(\"first\" $x: Boolean, \"second\" $x: Boolean) {\n",
      "  dog { isHouseTrained(atOtherHomes: $x) }\n",
      "}\n",
    ),
    descriptions: &["\"first\"", "\"second\""],
  },
  Fixture {
    what: "a described operation using a variable it never declared",
    source: concat!(
      "\"the operation\" query Q(\"the variable\" $x: Boolean) {\n",
      "  dog { isHouseTrained(atOtherHomes: $y) }\n",
      "}\n",
    ),
    descriptions: &["\"the operation\"", "\"the variable\""],
  },
  Fixture {
    what: "a described fragment nothing spreads",
    source: concat!(
      "\"the operation\" query Q { dog { name } }\n",
      "\"the fragment\" fragment unused on Dog { name }\n",
    ),
    descriptions: &["\"the operation\"", "\"the fragment\""],
  },
  Fixture {
    what: "two described operations sharing a name",
    source: concat!(
      "\"the first\" query Q { dog { name } }\n",
      "\"the second\" query Q { dog { nickname } }\n",
    ),
    descriptions: &["\"the first\"", "\"the second\""],
  },
  Fixture {
    what: "described fragments that spread each other",
    source: concat!(
      "query Q { dog { ...a } }\n",
      "\"the first\" fragment a on Dog { name ...b }\n",
      "\"the second\" fragment b on Dog { nickname ...a }\n",
    ),
    descriptions: &["\"the first\"", "\"the second\""],
  },
  Fixture {
    what: "a described variable given a type the argument cannot take",
    source: concat!(
      "\"the operation\" query Q(\"the variable\" $v: Int) {\n",
      "  dog { isHouseTrained(atOtherHomes: $v) }\n",
      "}\n",
    ),
    descriptions: &["\"the operation\"", "\"the variable\""],
  },
];

/// `source` with each of `descriptions` replaced by an equal-length run of spaces.
///
/// Panics rather than silently blanking nothing: a fixture that lists a literal its source does
/// not contain would otherwise compare a document against itself and pass forever.
fn blanked(source: &str, descriptions: &[&str]) -> String {
  let mut out = String::from(source);
  for description in descriptions {
    let at = out.find(description).unwrap_or_else(|| {
      panic!("the fixture does not contain the description {description:?}\n---\n{source}")
    });
    let blanks: String = core::iter::repeat_n(' ', description.len()).collect();
    out.replace_range(at..at + description.len(), &blanks);
  }
  assert_eq!(
    out.len(),
    source.len(),
    "blanking moved a byte, so the twins are no longer comparable by span\n---\n{source}"
  );
  assert_ne!(
    out, source,
    "the fixture carries no description, so its twin proves nothing\n---\n{source}"
  );
  out
}

fn build(sdl: &str) -> Schema {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(sdl)
  .unwrap_or_else(|errors| panic!("fixture SDL does not parse: {errors:?}"));
  Schema::build(&document).unwrap_or_else(|errors| panic!("fixture SDL is not a schema:\n{errors}"))
}

/// The syntactic door: parse to an AST, validate the AST.
fn syntactic<'a>(schema: &Schema, source: &'a str) -> Vec<Diagnostic<&'a str>> {
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    ExecutableDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(executable_document)
  .parse_str(source)
  .unwrap_or_else(|errors| panic!("the syntactic parser rejects it: {errors:?}\n---\n{source}"));

  let mut scratch = Scratch::new();
  let mut collected = Vec::new();
  let mut sink = Collect::new(&mut collected);
  let verdict = validate_executable_with(
    schema,
    &document,
    &mut scratch,
    &Budget::default(),
    RuleSet::ALL,
    &mut sink,
  );
  assert_eq!(
    verdict.is_err(),
    !collected.is_empty(),
    "the verdict and the diagnostics disagree\n---\n{source}"
  );
  collected
}

#[test]
fn a_description_changes_no_diagnostic_the_syntactic_door_produces() {
  let schema = build(SCHEMA);
  let mut compared = 0usize;
  let mut rules = BTreeSet::new();

  for fixture in FIXTURES {
    let undescribed = blanked(fixture.source, fixture.descriptions);
    let with = syntactic(&schema, fixture.source);
    let without = syntactic(&schema, &undescribed);

    assert_eq!(
      with.len(),
      without.len(),
      "{}: the described twin produced a different number of diagnostics\n---\n{}",
      fixture.what,
      fixture.source
    );
    for (with, without) in with.iter().zip(without.iter()) {
      assert_eq!(
        with.rule(),
        without.rule(),
        "{}: a description changed which rule fired",
        fixture.what
      );
      assert_eq!(
        with.span(),
        without.span(),
        "{}: a description moved a diagnostic's span",
        fixture.what
      );
      assert_eq!(
        with, without,
        "{}: a description changed a diagnostic",
        fixture.what
      );
      compared += 1;
      rules.insert(with.rule());
    }
  }

  assert!(
    compared >= FIRING_FLOOR.0,
    "the sweep compared {compared} diagnostics, below the floor of {}; an all-valid corpus \
     agrees with itself and proves nothing",
    FIRING_FLOOR.0
  );
  assert!(
    rules.len() >= FIRING_FLOOR.1,
    "the compared diagnostics reached {} rules, below the floor of {}",
    rules.len(),
    FIRING_FLOOR.1
  );
}

/// Both directions of the obligation, stated separately from the equality above.
///
/// The equality would still hold if descriptions made *every* document invalid, or if they made
/// every document valid. These two assertions are what rule that out.
#[test]
fn a_description_neither_fires_a_rule_nor_suppresses_one() {
  let schema = build(SCHEMA);

  // The valid fixture stays valid with its descriptions: nothing fires because of one.
  let valid = FIXTURES.first().expect("the valid fixture");
  assert!(
    syntactic(&schema, valid.source).is_empty(),
    "a description made a valid document invalid"
  );
  assert!(
    syntactic(&schema, &blanked(valid.source, valid.descriptions)).is_empty(),
    "the blanked twin of the valid fixture is not valid, so the fixture is mis-filed"
  );

  // Every invalid fixture stays invalid with its descriptions: nothing is suppressed by one.
  for fixture in FIXTURES.iter().skip(1) {
    assert!(
      !syntactic(&schema, fixture.source).is_empty(),
      "{}: a description suppressed every rule the document trips",
      fixture.what
    );
  }
}

#[cfg(feature = "rowan")]
mod lossless {
  use super::{BTreeSet, Diagnostic, FIRING_FLOOR, FIXTURES, SCHEMA, Vec, blanked, build};

  use smear::{
    parser::graphql::lossless::parse_executable_document,
    validator::{Budget, Collect, RuleSet, Schema, Scratch, validate_executable_lossless_with},
  };

  /// The lossless door: parse to a CST, hand the CST and its text to the validator.
  ///
  /// A `Description` is a node in that tree, so this is the leg where one could plausibly shift a
  /// span — the syntactic door reads a description that the AST has already lifted out.
  fn lossless<'a>(schema: &Schema, source: &'a str) -> Vec<Diagnostic<&'a str>> {
    let parse = parse_executable_document(source);
    assert!(
      !parse.has_errors(),
      "the lossless parser rejects it\n---\n{source}"
    );
    let mut scratch = Scratch::new();
    let mut collected = Vec::new();
    let mut sink = Collect::new(&mut collected);
    let verdict = validate_executable_lossless_with(
      schema,
      &parse,
      source,
      &mut scratch,
      &Budget::default(),
      RuleSet::ALL,
      &mut sink,
    );
    assert_eq!(
      verdict.is_err(),
      !collected.is_empty(),
      "the lossless verdict and its diagnostics disagree\n---\n{source}"
    );
    collected
  }

  #[test]
  fn a_description_changes_no_diagnostic_the_lossless_door_produces() {
    let schema = build(SCHEMA);
    let mut compared = 0usize;
    let mut rules = BTreeSet::new();

    for fixture in FIXTURES {
      let undescribed = blanked(fixture.source, fixture.descriptions);
      let with = lossless(&schema, fixture.source);
      let without = lossless(&schema, &undescribed);

      assert_eq!(
        with.len(),
        without.len(),
        "{}: the described twin produced a different number of diagnostics\n---\n{}",
        fixture.what,
        fixture.source
      );
      for (with, without) in with.iter().zip(without.iter()) {
        assert_eq!(
          with, without,
          "{}: a description changed a diagnostic through the lossless door",
          fixture.what
        );
        compared += 1;
        rules.insert(with.rule());
      }
    }

    assert!(
      compared >= FIRING_FLOOR.0,
      "the lossless sweep compared {compared} diagnostics, below the floor of {}",
      FIRING_FLOOR.0
    );
    assert!(
      rules.len() >= FIRING_FLOOR.1,
      "the lossless sweep reached {} rules, below the floor of {}",
      rules.len(),
      FIRING_FLOOR.1
    );
  }

  /// The two doors agree with each other on the described half, not only each with itself.
  ///
  /// Without this, both legs could drift the same way and every equality above would still hold.
  #[test]
  fn the_two_doors_agree_on_a_described_document() {
    let schema = build(SCHEMA);
    for fixture in FIXTURES {
      assert_eq!(
        super::syntactic(&schema, fixture.source),
        lossless(&schema, fixture.source),
        "{}: the two doors disagree about a described document",
        fixture.what
      );
    }
  }
}

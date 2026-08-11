//! The cross-dialect keyword-prefix pin.
//!
//! `graphqlx::ContextualKeyword` is `graphql::ContextualKeyword`'s 38 variants **verbatim, in
//! order**, plus six (`Import`, `From`, `As`, `Where`, `Set`, `Map`). Measured by diffing the two
//! enum bodies: exactly one hunk, six pure additions at the tail, nothing changed and nothing
//! reordered.
//!
//! # Why this is a unit-test module and not `tests/keyword_prefix.rs`
//!
//! Both `KEYWORDS` lists are `#[cfg(test)] pub(crate)`, and an integration test cannot see either.
//! The alternative was to promote them to `#[doc(hidden)] pub` on a published crate — permanently
//! widening `smear-lexer`'s surface so that a consistency check between two of its *internal*
//! enums could be written one directory over. The check is internal, so it lives inside.
//!
//! # Why it matters even though nothing depends on the discriminants
//!
//! Phase B measured a token-kind prefix and rejected it as a shared-production enabler. What it did
//! *not* reject is the prefix as a **drift alarm**: the two enums are hand-maintained, the parser's
//! two directive-location predicates are generated from one shared variant-name list
//! (`smear::parser::lossless::directive_location_predicate`), and that generation is silently wrong
//! the day a name exists in one dialect and not the other.

use crate::{graphql::ContextualKeyword as G, graphqlx::ContextualKeyword as X};

/// The 38 spellings both dialects share, in graphql's declaration order.
///
/// Written out rather than derived, because deriving it from either enum is what makes the check
/// vacuous: a list generated from graphqlx cannot notice graphqlx reordering. This array is the
/// third party the two enums are both compared against.
const SHARED: &[(&str, G, X)] = &[
  ("type", G::Type, X::Type),
  ("interface", G::Interface, X::Interface),
  ("union", G::Union, X::Union),
  ("enum", G::Enum, X::Enum),
  ("input", G::Input, X::Input),
  ("scalar", G::Scalar, X::Scalar),
  ("extend", G::Extend, X::Extend),
  ("schema", G::Schema, X::Schema),
  ("directive", G::Directive, X::Directive),
  ("fragment", G::Fragment, X::Fragment),
  ("query", G::Query, X::Query),
  ("mutation", G::Mutation, X::Mutation),
  ("subscription", G::Subscription, X::Subscription),
  ("implements", G::Implements, X::Implements),
  ("repeatable", G::Repeatable, X::Repeatable),
  ("on", G::On, X::On),
  ("true", G::True, X::True),
  ("false", G::False, X::False),
  ("null", G::Null, X::Null),
  ("QUERY", G::QueryLocation, X::QueryLocation),
  ("MUTATION", G::MutationLocation, X::MutationLocation),
  (
    "SUBSCRIPTION",
    G::SubscriptionLocation,
    X::SubscriptionLocation,
  ),
  ("FIELD", G::FieldLocation, X::FieldLocation),
  (
    "FRAGMENT_DEFINITION",
    G::FragmentDefinitionLocation,
    X::FragmentDefinitionLocation,
  ),
  (
    "FRAGMENT_SPREAD",
    G::FragmentSpreadLocation,
    X::FragmentSpreadLocation,
  ),
  (
    "INLINE_FRAGMENT",
    G::InlineFragmentLocation,
    X::InlineFragmentLocation,
  ),
  (
    "VARIABLE_DEFINITION",
    G::VariableDefinitionLocation,
    X::VariableDefinitionLocation,
  ),
  ("SCHEMA", G::SchemaLocation, X::SchemaLocation),
  ("SCALAR", G::ScalarLocation, X::ScalarLocation),
  ("OBJECT", G::ObjectLocation, X::ObjectLocation),
  (
    "FIELD_DEFINITION",
    G::FieldDefinitionLocation,
    X::FieldDefinitionLocation,
  ),
  (
    "ARGUMENT_DEFINITION",
    G::ArgumentDefinitionLocation,
    X::ArgumentDefinitionLocation,
  ),
  ("INTERFACE", G::InterfaceLocation, X::InterfaceLocation),
  ("UNION", G::UnionLocation, X::UnionLocation),
  ("ENUM", G::EnumLocation, X::EnumLocation),
  ("ENUM_VALUE", G::EnumValueLocation, X::EnumValueLocation),
  (
    "INPUT_OBJECT",
    G::InputObjectLocation,
    X::InputObjectLocation,
  ),
  (
    "INPUT_FIELD_DEFINITION",
    G::InputFieldDefinitionLocation,
    X::InputFieldDefinitionLocation,
  ),
];

/// The six GraphQLx adds, and the fact that it adds exactly six.
const EXTRA: &[(&str, X)] = &[
  ("import", X::Import),
  ("from", X::From),
  ("as", X::As),
  ("where", X::Where),
  ("set", X::Set),
  ("map", X::Map),
];

/// Every shared spelling exists in both dialects and spells the same word.
#[test]
fn the_shared_keyword_spellings_agree() {
  assert_eq!(SHARED.len(), 38, "the shared list is no longer 38 entries");
  for (word, g, x) in SHARED {
    assert_eq!(
      g.as_str(),
      *word,
      "graphql {g:?} spells {:?}, expected {word:?}",
      g.as_str()
    );
    assert_eq!(
      x.as_str(),
      *word,
      "graphqlx {x:?} spells {:?}, expected {word:?}",
      x.as_str()
    );
  }
}

/// The shared list is graphql's declaration order, not merely its contents.
///
/// Position matters because the whole claim is that graphqlx is graphql's list *verbatim, in
/// order*, plus a tail. A set comparison would pass over a reordering, which is precisely the
/// drift a hand-maintained pair of enums produces.
#[test]
fn the_shared_list_is_both_dialects_declaration_order() {
  let graphql: Vec<G> = crate::graphql::keyword::KEYWORDS.to_vec();
  let graphqlx: Vec<X> = crate::graphqlx::keyword::KEYWORDS.to_vec();

  for (index, (word, g, x)) in SHARED.iter().enumerate() {
    assert_eq!(
      graphql[index], *g,
      "graphql's {index}th keyword is {:?}, but the shared list says {word:?}",
      graphql[index]
    );
    assert_eq!(
      graphqlx[index], *x,
      "graphqlx's {index}th keyword is {:?}, but the shared list says {word:?}",
      graphqlx[index]
    );
  }
}

/// GraphQLx adds exactly six spellings, at the tail, and graphql has none of them.
#[test]
fn graphqlx_adds_exactly_six_keywords_at_the_tail() {
  assert_eq!(EXTRA.len(), 6);
  let shared: std::collections::BTreeSet<&str> = SHARED.iter().map(|(w, _, _)| *w).collect();
  let graphqlx = crate::graphqlx::keyword::KEYWORDS;

  for (offset, (word, x)) in EXTRA.iter().enumerate() {
    assert_eq!(x.as_str(), *word);
    assert!(!shared.contains(word), "{word:?} is in both lists");
    assert_eq!(
      graphqlx[SHARED.len() + offset],
      *x,
      "graphqlx's extra keywords are not in the order EXTRA states"
    );
  }
}

/// Neither enum has grown a spelling the lists above do not know about.
///
/// **This is the assertion that makes the two above non-vacuous.** Both dialects' `KEYWORDS` are
/// arrays, so each pins its own length at compile time; what this pins is that the two lengths and
/// the two hand-written lists still agree. Without it, a 39th graphql keyword or a 45th graphqlx
/// one passes every test in this file.
#[test]
fn neither_enum_has_grown_behind_the_lists() {
  assert_eq!(
    crate::graphql::keyword::KEYWORDS.len(),
    SHARED.len(),
    "graphql's keyword count moved; update SHARED and say why"
  );
  assert_eq!(
    crate::graphqlx::keyword::KEYWORDS.len(),
    SHARED.len() + EXTRA.len(),
    "graphqlx's keyword count moved; update SHARED or EXTRA and say which"
  );
}

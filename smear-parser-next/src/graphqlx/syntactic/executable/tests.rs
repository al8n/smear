//! GraphQLx executable-definition production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family. Fixture `0016_operation_with_generics` is
//! inlined verbatim as the document-level oracle; `0022_complex_fragments`
//! supplies the generic-fragment and where-clause rows (the full-corpus fixture
//! run lands with the Wave 8b entry runner).

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser, utils::cmp::Equivalent};

use super::{
  described_executable_definition, executable_definition, executable_document, fragment_definition,
  import_or_executable_definition, operation_definition, operation_type, variable_definition,
  variables_definition,
};
use crate::graphqlx::ast::{ExecutableDefinition, OperationDefinition};

use crate::graphqlx::error::GraphqlxErrors;

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlxErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlxErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlxErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlxErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlxErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlxErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlxErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_bytes(input)
}

/// Runs `parser` over `src` as `str`, `[u8]`, and (behind the feature) `Bytes`,
/// applying the generic `check` fn to each accepted AST — the source matrix.
macro_rules! accept_all {
  ($parser:expr, $src:expr, $check:path) => {{
    $check(drive_str($parser, $src).expect(concat!("str accept: ", $src)));
    $check(drive_slice($parser, $src.as_bytes()).expect(concat!("slice accept: ", $src)));
    #[cfg(feature = "bytes")]
    {
      let owned = ::bytes::Bytes::from_static($src.as_bytes());
      $check(drive_bytes($parser, &owned).expect(concat!("bytes accept: ", $src)));
    }
  }};
}

/// Asserts `parser` rejects `src` over both `str` and `[u8]`.
macro_rules! reject_all {
  ($parser:expr, $src:expr) => {{
    assert!(
      drive_str(|inp| $parser(inp).map(|_| ()), $src).is_err(),
      "str should reject: {:?}",
      $src
    );
    assert!(
      drive_slice(|inp| $parser(inp).map(|_| ()), $src.as_bytes()).is_err(),
      "slice should reject: {:?}",
      $src
    );
  }};
}

// ─── variable_definition / variables_definition ──────────────────────────────

#[test]
fn variable_definition_basic_and_full() {
  fn basic<S: AsRef<[u8]>>(d: crate::graphqlx::ast::DescribedVariableDefinition<S>) {
    assert!(d.description().is_none());
    let def = d.node();
    assert!("id".equivalent(def.variable().name().source_ref()));
    assert!(def.default_value().is_none());
  }
  accept_all!(variable_definition, "$id: ID!", basic);

  fn full<S: AsRef<[u8]>>(d: crate::graphqlx::ast::DescribedVariableDefinition<S>) {
    assert!(d.description().is_some());
    let def = d.node();
    assert!(def.default_value().is_some());
    assert!(def.directives().is_some());
  }
  accept_all!(
    variable_definition,
    "\"the count\" $n: Int = 10 @range(max: 100)",
    full
  );
}

#[test]
fn variable_definition_graphqlx_types_and_defaults() {
  // The type slot is the full GraphQLx `ty`; the default the GraphQLx const
  // family (fixture `0019`'s map default, at the variable position).
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::DescribedVariableDefinition<S>) {
    let def = d.node();
    assert!(def.ty().is_map());
    assert!(def.default_value().is_some());
  }
  accept_all!(
    variable_definition,
    "$config: <String => Int> = map { \"a\" => 1 }",
    check
  );
}

#[test]
fn variables_definition_list_reject_empty_and_decline() {
  fn two<S: AsRef<[u8]>>(v: Option<crate::graphqlx::ast::VariablesDefinition<S>>) {
    let v = v.expect("variables present");
    assert_eq!(v.variable_definitions().len(), 2);
  }
  accept_all!(variables_definition, "($a: Int, $b: user::Profile!)", two);

  // Amendment 2 (the W3 site carried to GraphQLx): `( VariableDefinition+ )`.
  reject_all!(variables_definition, "()");

  // No `(` ahead: decline without consuming.
  let out = drive_str(
    |inp| {
      let vars = variables_definition(inp)?;
      assert!(vars.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "plain",
  );
  assert_eq!(out.ok(), Some("plain"));
}

// ─── operation_type ──────────────────────────────────────────────────────────

#[test]
fn operation_type_all_three_and_reject() {
  fn is_query(t: crate::graphqlx::ast::OperationType) {
    assert!(t.is_query());
  }
  accept_all!(operation_type, "query", is_query);
  fn is_mutation(t: crate::graphqlx::ast::OperationType) {
    assert!(t.is_mutation());
  }
  accept_all!(operation_type, "mutation", is_mutation);
  fn is_subscription(t: crate::graphqlx::ast::OperationType) {
    assert!(t.is_subscription());
  }
  accept_all!(operation_type, "subscription", is_subscription);
  reject_all!(operation_type, "fragment");
}

// ─── operation_definition ────────────────────────────────────────────────────

#[test]
fn operation_shorthand_and_named() {
  fn shorthand<S: AsRef<[u8]>>(o: OperationDefinition<S>) {
    assert!(o.is_shorthand());
  }
  accept_all!(operation_definition, "{ id name }", shorthand);

  // Fixture `0016`: `query GetData<T> { items { ...ItemFragment<T> } }`.
  fn named<S: AsRef<[u8]>>(o: OperationDefinition<S>) {
    let named = o.unwrap_named();
    let name = named.name().expect("name present");
    assert!("GetData".equivalent(name.name().source_ref()));
    let generics = name.generics().expect("generic parameters present");
    assert_eq!(generics.params_slice().len(), 1);
    assert!(named.selection_set().where_clause().is_none());
  }
  accept_all!(
    operation_definition,
    "query GetData<T> { items { ...ItemFragment<T> } }",
    named
  );
}

#[test]
fn operation_with_variables_directives_and_where_clause() {
  fn check<S: AsRef<[u8]>>(o: OperationDefinition<S>) {
    let named = o.unwrap_named();
    assert!(named.variable_definitions().is_some());
    assert!(named.directives().is_some());
    let constrained = named.selection_set();
    let wc = constrained.where_clause().expect("where clause present");
    assert_eq!(wc.predicates_slice().len(), 1);
    assert_eq!(constrained.target().selections().len(), 1);
  }
  accept_all!(
    operation_definition,
    "query Q<T>($x: T!) @traced where T: Node { f }",
    check
  );
}

#[test]
fn operation_named_where_parses_greedily() {
  // Adjudicated soft-keyword precedence (module deviation note): in
  // `query where { x }` the identifier `where` is the operation NAME — never an
  // empty where clause. An anonymous where-constrained operation is semantically
  // void (no name, no type parameters), so nothing representable is lost.
  fn check<S: AsRef<[u8]>>(o: OperationDefinition<S>) {
    let named = o.unwrap_named();
    let name = named.name().expect("name present");
    assert!("where".equivalent(name.name().source_ref()));
    assert!(named.selection_set().where_clause().is_none());
  }
  accept_all!(operation_definition, "query where { x }", check);

  // With both spelled out, the first `where` is the name and the second opens
  // the clause.
  fn both<S: AsRef<[u8]>>(o: OperationDefinition<S>) {
    let named = o.unwrap_named();
    assert!("where".equivalent(named.name().expect("name present").name().source_ref()));
    assert!(named.selection_set().where_clause().is_some());
  }
  accept_all!(
    operation_definition,
    "query where where T: Node { x }",
    both
  );
}

#[test]
fn operation_rejects_truncations() {
  reject_all!(operation_definition, "query");
  reject_all!(operation_definition, "query Q(");
  // The empty variables list breaches `( VariableDefinition+ )` (Amendment 2).
  reject_all!(operation_definition, "query Q() { f }");
}

// ─── fragment_definition ─────────────────────────────────────────────────────

#[test]
fn fragment_plain_and_generic() {
  fn plain<S: AsRef<[u8]>>(f: crate::graphqlx::ast::FragmentDefinition<S>) {
    let (impl_generics, name) = (f.name().first(), f.name().second());
    assert!(impl_generics.is_none());
    assert!("PostPreview".equivalent(name.ident().source_ref()));
    assert!(name.generics().is_none());
    assert!(f.selection_set().where_clause().is_none());
  }
  accept_all!(
    fragment_definition,
    "fragment PostPreview on Post { id }",
    plain
  );

  // Fixture `0016`: `fragment<T> ItemFragment<T> on Item<T> { id data }`.
  fn generic<S: AsRef<[u8]>>(f: crate::graphqlx::ast::FragmentDefinition<S>) {
    let impl_generics = f.name().first().as_ref().expect("impl generics present");
    assert_eq!(impl_generics.params_slice().len(), 1);
    let name = f.name().second();
    assert!("ItemFragment".equivalent(name.ident().source_ref()));
    assert!(name.generics().is_some());
    let tc = f.type_condition();
    assert!("Item".equivalent(tc.name().path().segments_slice()[0].source_ref()));
    assert!(tc.name().type_generics().is_some());
  }
  accept_all!(
    fragment_definition,
    "fragment<T> ItemFragment<T> on Item<T> { id data }",
    generic
  );
}

#[test]
fn fragment_with_where_clause() {
  // Fixture `0022`: `fragment<T> NodeFragment<T> on Node<T> where T: Timestamped
  // & Serializable { … }`.
  fn check<S: AsRef<[u8]>>(f: crate::graphqlx::ast::FragmentDefinition<S>) {
    let wc = f
      .selection_set()
      .where_clause()
      .expect("where clause present");
    assert_eq!(wc.predicates_slice().len(), 1);
    assert_eq!(wc.predicates_slice()[0].bounds_slice().len(), 2);
    assert_eq!(f.selection_set().target().selections().len(), 2);
  }
  accept_all!(
    fragment_definition,
    "fragment<T> NodeFragment<T> on Node<T>\n  where T: Timestamped & Serializable\n{ id createdAt }",
    check
  );
}

#[test]
fn fragment_named_on_error_per_spec() {
  // `FragmentName : Name but not on` — the shared-grammar exclusion carried to
  // GraphQLx through `executable_definition_name`.
  reject_all!(fragment_definition, "fragment on on Item { id }");
}

// ─── executable_definition / described / import-or ───────────────────────────

#[test]
fn executable_definition_dispatches_both_arms() {
  fn frag<S: AsRef<[u8]>>(d: ExecutableDefinition<S>) {
    assert!(d.is_fragment());
  }
  accept_all!(executable_definition, "fragment F on T { id }", frag);

  fn op<S: AsRef<[u8]>>(d: ExecutableDefinition<S>) {
    assert!(d.is_operation());
  }
  accept_all!(executable_definition, "mutation M { save }", op);
  accept_all!(executable_definition, "{ shorthand }", op);
}

#[test]
fn described_executable_definition_carries_the_description() {
  // Fixture `0016`'s leading description string.
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::DescribedExecutableDefinition<S>) {
    assert!(d.description().is_some());
    assert!(d.node().is_operation());
  }
  accept_all!(
    described_executable_definition,
    "\"Query operation with generic fragment\"\nquery GetData<T> { items { id } }",
    check
  );

  fn none<S: AsRef<[u8]>>(d: crate::graphqlx::ast::DescribedExecutableDefinition<S>) {
    assert!(d.description().is_none());
    assert!(d.node().is_fragment());
  }
  accept_all!(
    described_executable_definition,
    "fragment F on T { id }",
    none
  );
}

#[test]
fn import_or_executable_definition_dispatch() {
  fn import<S: AsRef<[u8]>>(i: crate::graphqlx::ast::ImportOrExecutableDefinition<S>) {
    assert!(i.is_import());
  }
  accept_all!(
    import_or_executable_definition,
    "import { User } from \"./types.graphqlx\"",
    import
  );

  fn exec<S: AsRef<[u8]>>(i: crate::graphqlx::ast::ImportOrExecutableDefinition<S>) {
    assert!(i.is_executable());
  }
  accept_all!(import_or_executable_definition, "{ id }", exec);
}

// ─── executable_document ─────────────────────────────────────────────────────

/// Fixture `0016_operation_with_generics`, inlined verbatim.
const FIXTURE_0016: &str = "\"Query operation with generic fragment\"\n\nquery GetData<T> {\n  items {\n    ...ItemFragment<T>\n  }\n}\n\nfragment<T> ItemFragment<T> on Item<T> {\n  id\n  data\n}\n\n";

#[test]
fn executable_document_fixture_0016() {
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ExecutableDocument<S>) {
    let items = d.definitions();
    assert_eq!(items.len(), 2);
    // The described operation, its description attached.
    let op = match &items[0] {
      crate::graphqlx::ast::ImportOrExecutableDefinition::Executable(e) => e,
      _ => panic!("expected an executable item"),
    };
    assert!(op.description().is_some());
    assert!(op.node().is_operation());
    // The generic fragment.
    let frag = match &items[1] {
      crate::graphqlx::ast::ImportOrExecutableDefinition::Executable(e) => e,
      _ => panic!("expected an executable item"),
    };
    assert!(frag.description().is_none());
    assert!(frag.node().is_fragment());
  }
  check(drive_str(executable_document, FIXTURE_0016).expect("str accept: fixture 0016"));
  check(
    drive_slice(executable_document, FIXTURE_0016.as_bytes()).expect("slice accept: fixture 0016"),
  );
  #[cfg(feature = "bytes")]
  {
    let owned = ::bytes::Bytes::from_static(FIXTURE_0016.as_bytes());
    check(drive_bytes(executable_document, &owned).expect("bytes accept: fixture 0016"));
  }
}

#[test]
fn executable_document_imports_and_operations_interleave() {
  // Import-only documents (fixtures `0001`–`0003`) are valid — imports count as
  // items — and imports mix with operations.
  fn import_only<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ExecutableDocument<S>) {
    assert_eq!(d.definitions().len(), 1);
    assert!(d.definitions()[0].is_import());
  }
  accept_all!(
    executable_document,
    "import * from \"./types.graphqlx\"",
    import_only
  );

  fn mixed<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ExecutableDocument<S>) {
    assert_eq!(d.definitions().len(), 2);
    assert!(d.definitions()[0].is_import());
    assert!(d.definitions()[1].is_executable());
  }
  accept_all!(
    executable_document,
    "import { User } from \"./u.graphqlx\"\nquery Q { user }",
    mixed
  );
}

#[test]
fn executable_document_empty_rejects_per_cardinality() {
  // Amendment 2 (the GraphQL `Document : Definition+` parallel).
  reject_all!(executable_document, "");
  reject_all!(executable_document, "   ");
}

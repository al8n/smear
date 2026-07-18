//! GraphQLx generic-parameter production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject cases
//! assert the error family. The frozen `smear-parser` generic parsers no longer
//! exist, so the `ok_*` corpus fixtures and the scaffold node shapes are the
//! arbiter (plan Wave 8a).

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{
  definition_name, definition_type_param, executable_definition_name, extension_type_param,
  try_definition_type_generics, try_executable_definition_type_generics,
  try_extension_type_generics, try_where_clause, where_predicate,
};
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

/// Views a slice (`&str` or `&[u8]`) as bytes, so one assertion body reads across
/// every source representation.
fn bytes<S: AsRef<[u8]>>(slice: &S) -> &[u8] {
  slice.as_ref()
}

// ─── definition_type_param ───────────────────────────────────────────────────

#[test]
fn definition_type_param_without_default() {
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::DefinitionTypeParam<S>) {
    assert_eq!(bytes(p.ident().source_ref()), b"T");
    assert!(p.default().is_none());
  }
  accept_all!(definition_type_param, "T", check);
}

#[test]
fn definition_type_param_with_default_type() {
  // Fixture `0010_generics_with_default`: `Response<T = String>`.
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::DefinitionTypeParam<S>) {
    assert_eq!(bytes(p.ident().source_ref()), b"T");
    let default = p.default().expect("default type present");
    let path = default.unwrap_path_ref();
    assert_eq!(
      bytes(path.path().segments_slice()[0].source_ref()),
      b"String"
    );
  }
  accept_all!(definition_type_param, "T = String", check);
}

#[test]
fn definition_type_param_with_composite_default() {
  // The default is a full `Type`: a non-null list of non-null names works.
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::DefinitionTypeParam<S>) {
    let default = p.default().expect("default type present");
    assert!(default.unwrap_list_ref().required());
  }
  accept_all!(definition_type_param, "T = [Int!]!", check);
}

#[test]
fn definition_type_param_equal_without_type_errors() {
  // The `=` commits to a default type; a missing one is an error, never a decline.
  reject_all!(definition_type_param, "T =");
}

// ─── try_definition_type_generics ────────────────────────────────────────────

/// The parameter list of a `DefinitionTypeGenerics` accept case, or a panic when
/// the production declined.
fn unwrap_def_generics<S>(
  g: Option<crate::graphqlx::ast::DefinitionTypeGenerics<S>>,
) -> crate::graphqlx::ast::DefinitionTypeGenerics<S> {
  g.expect("generics present")
}

#[test]
fn definition_type_generics_single_param() {
  fn check<S: AsRef<[u8]>>(g: Option<crate::graphqlx::ast::DefinitionTypeGenerics<S>>) {
    let g = unwrap_def_generics(g);
    let params = g.params_slice();
    assert_eq!(params.len(), 1);
    assert_eq!(bytes(params[0].ident().source_ref()), b"T");
  }
  accept_all!(try_definition_type_generics, "<T>", check);
}

#[test]
fn definition_type_generics_multiple_params_with_default() {
  // Fixtures `0005` (multiple params) + `0010` (defaults): `<K, V = String>`.
  fn check<S: AsRef<[u8]>>(g: Option<crate::graphqlx::ast::DefinitionTypeGenerics<S>>) {
    let g = unwrap_def_generics(g);
    let params = g.params_slice();
    assert_eq!(params.len(), 2);
    assert_eq!(bytes(params[0].ident().source_ref()), b"K");
    assert!(params[0].default().is_none());
    assert_eq!(bytes(params[1].ident().source_ref()), b"V");
    assert!(params[1].default().is_some());
  }
  accept_all!(try_definition_type_generics, "<K, V = String>", check);
}

#[test]
fn definition_type_generics_declines_without_angle() {
  // No `<` ahead: the production declines without consuming, leaving the next
  // token in place.
  let out = drive_str(
    |inp| {
      let generics = try_definition_type_generics(inp)?;
      assert!(generics.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "plain",
  );
  assert_eq!(out.ok(), Some("plain"));
}

#[test]
fn definition_type_generics_empty_rejects_per_cardinality() {
  // Amendment 2: `< DefinitionTypeParam+ >` demands one-or-more, so `<>` errors.
  reject_all!(try_definition_type_generics, "<>");
}

// ─── try_extension_type_generics ─────────────────────────────────────────────

#[test]
fn extension_type_generics_params() {
  // Fixture `0015_extend_with_generics`: `extend type Box<T>`.
  fn check<S: AsRef<[u8]>>(g: Option<crate::graphqlx::ast::ExtensionTypeGenerics<S>>) {
    let g = g.expect("generics present");
    let params = g.params_slice();
    assert_eq!(params.len(), 2);
    assert_eq!(bytes(params[0].ident().source_ref()), b"T");
    assert_eq!(bytes(params[1].ident().source_ref()), b"U");
  }
  accept_all!(try_extension_type_generics, "<T, U>", check);
}

#[test]
fn extension_type_generics_reject_empty_and_defaults() {
  // `<>` breaks the `+` cardinality; `= Type` defaults exist only on definitions.
  reject_all!(try_extension_type_generics, "<>");
  reject_all!(try_extension_type_generics, "<T = String>");
}

#[test]
fn extension_type_param_is_a_bare_name() {
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::ExtensionTypeParam<S>) {
    assert_eq!(bytes(p.ident().source_ref()), b"T");
  }
  accept_all!(extension_type_param, "T", check);
}

// ─── try_executable_definition_type_generics ─────────────────────────────────

#[test]
fn executable_definition_type_generics_params() {
  // Fixture `0022_complex_fragments`: `fragment<K, V> ConnectionFields<K, V>`.
  fn check<S: AsRef<[u8]>>(g: Option<crate::graphqlx::ast::ExecutableDefinitionTypeGenerics<S>>) {
    let g = g.expect("generics present");
    let params = g.params_slice();
    assert_eq!(params.len(), 2);
    assert_eq!(bytes(params[0].source_ref()), b"K");
    assert_eq!(bytes(params[1].source_ref()), b"V");
  }
  accept_all!(try_executable_definition_type_generics, "<K, V>", check);
}

#[test]
fn executable_definition_type_generics_reject_empty_and_defaults() {
  // `<>` breaks the `+` cardinality; the parameters are bare names, so a default
  // rejects (scaffold `ExecutableDefinitionTypeGenerics<Ident>` carries none —
  // the type, not its doc example, is the authority).
  reject_all!(try_executable_definition_type_generics, "<>");
  reject_all!(try_executable_definition_type_generics, "<T = String>");
}

#[test]
fn executable_definition_type_generics_declines_without_angle() {
  let out = drive_str(
    |inp| {
      let generics = try_executable_definition_type_generics(inp)?;
      assert!(generics.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "Frag",
  );
  assert_eq!(out.ok(), Some("Frag"));
}

// ─── definition_name / executable_definition_name ────────────────────────────

#[test]
fn definition_name_plain_and_generic() {
  fn plain<S: AsRef<[u8]>>(n: crate::graphqlx::ast::DefinitionName<S>) {
    assert_eq!(bytes(n.name().source_ref()), b"Response");
    assert!(n.generics().is_none());
  }
  accept_all!(definition_name, "Response", plain);

  // Fixture `0010_generics_with_default`: `Response<T = String>`.
  fn generic<S: AsRef<[u8]>>(n: crate::graphqlx::ast::DefinitionName<S>) {
    assert_eq!(bytes(n.name().source_ref()), b"Response");
    let generics = n.generics().expect("generics present");
    assert_eq!(generics.params_slice().len(), 1);
    assert!(generics.params_slice()[0].default().is_some());
  }
  accept_all!(definition_name, "Response<T = String>", generic);
}

#[test]
fn executable_definition_name_plain_and_generic() {
  // Fixture `0016_operation_with_generics`: `ItemFragment<T>`.
  fn generic<S: AsRef<[u8]>>(n: crate::graphqlx::ast::ExecutableDefinitionName<S>) {
    assert_eq!(bytes(n.ident().source_ref()), b"ItemFragment");
    let generics = n.generics().expect("generics present");
    assert_eq!(generics.params_slice().len(), 1);
    assert_eq!(bytes(generics.params_slice()[0].source_ref()), b"T");
  }
  accept_all!(executable_definition_name, "ItemFragment<T>", generic);

  fn plain<S: AsRef<[u8]>>(n: crate::graphqlx::ast::ExecutableDefinitionName<S>) {
    assert_eq!(bytes(n.ident().source_ref()), b"PostPreview");
    assert!(n.generics().is_none());
  }
  accept_all!(executable_definition_name, "PostPreview", plain);
}

#[test]
fn executable_definition_name_rejects_on_per_spec() {
  // `FragmentName : Name but not on` — the shared-grammar exclusion carried to
  // GraphQLx (the carrier's only grammar position is a fragment's name).
  reject_all!(executable_definition_name, "on");
}

// ─── where_predicate ─────────────────────────────────────────────────────────

#[test]
fn where_predicate_single_bound() {
  // Fixture `0006_where_clause_simple`: `where T: Node`.
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::WherePredicate<S>) {
    assert_eq!(
      bytes(p.bounded_type().path().segments_slice()[0].source_ref()),
      b"T"
    );
    let bounds = p.bounds_slice();
    assert_eq!(bounds.len(), 1);
    assert_eq!(
      bytes(bounds[0].path().segments_slice()[0].source_ref()),
      b"Node"
    );
  }
  accept_all!(where_predicate, "T: Node", check);
}

#[test]
fn where_predicate_multiple_bounds() {
  // Fixture `0007_where_clause_multiple_bounds`: `where T: Node & Timestamped`.
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::WherePredicate<S>) {
    let bounds = p.bounds_slice();
    assert_eq!(bounds.len(), 2);
    assert_eq!(
      bytes(bounds[0].path().segments_slice()[0].source_ref()),
      b"Node"
    );
    assert_eq!(
      bytes(bounds[1].path().segments_slice()[0].source_ref()),
      b"Timestamped"
    );
  }
  accept_all!(where_predicate, "T: Node & Timestamped", check);
}

#[test]
fn where_predicate_generic_and_pathed_shapes() {
  // Both sides of a predicate are full type paths: bounded `u::V<W>`, bound `X<Y>`.
  fn check<S: AsRef<[u8]>>(p: crate::graphqlx::ast::WherePredicate<S>) {
    assert_eq!(p.bounded_type().path().segments_slice().len(), 2);
    assert!(p.bounded_type().type_generics().is_some());
    assert!(p.bounds_slice()[0].type_generics().is_some());
  }
  accept_all!(where_predicate, "u::V<W>: X<Y>", check);
}

#[test]
fn where_predicate_rejects_headless_and_boundless() {
  reject_all!(where_predicate, "T");
  reject_all!(where_predicate, "T:");
  reject_all!(where_predicate, "T: Node &");
  reject_all!(where_predicate, ": Node");
}

// ─── try_where_clause ────────────────────────────────────────────────────────

#[test]
fn where_clause_single_predicate() {
  fn check<S: AsRef<[u8]>>(c: Option<crate::graphqlx::ast::WhereClause<S>>) {
    let c = c.expect("clause present");
    assert_eq!(c.predicates_slice().len(), 1);
  }
  accept_all!(try_where_clause, "where T: Node", check);
}

#[test]
fn where_clause_multiple_predicates_without_separators() {
  // Fixture `0020_where_multiple_predicates`: predicates stack with no separator.
  fn check<S: AsRef<[u8]>>(c: Option<crate::graphqlx::ast::WhereClause<S>>) {
    let c = c.expect("clause present");
    let preds = c.predicates_slice();
    assert_eq!(preds.len(), 2);
    assert_eq!(
      bytes(preds[0].bounded_type().path().segments_slice()[0].source_ref()),
      b"T"
    );
    assert_eq!(preds[0].bounds_slice().len(), 2);
    assert_eq!(
      bytes(preds[1].bounded_type().path().segments_slice()[0].source_ref()),
      b"U"
    );
  }
  accept_all!(
    try_where_clause,
    "where\n    T: Node & Timestamped\n    U: Serializable",
    check
  );
}

#[test]
fn where_clause_declines_without_keyword() {
  // No `where` ahead: decline without consuming — the next token stays readable.
  let out = drive_str(
    |inp| {
      let clause = try_where_clause(inp)?;
      assert!(clause.is_none());
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "wherever",
  );
  assert_eq!(out.ok(), Some("wherever"));
}

#[test]
fn where_clause_stops_before_a_non_predicate_identifier() {
  // The probe ends the clause at `type Bar` — an identifier that heads no
  // `TypePath ':'` — leaving it unconsumed for the enclosing construct (the
  // W8b document-stream case: `scalar Foo<T> where T: X` followed by another
  // definition).
  let out = drive_str(
    |inp| {
      let clause = try_where_clause(inp)?.expect("clause present");
      assert_eq!(clause.predicates_slice().len(), 1);
      crate::combinator::ident(inp).map(|id| *id.source_ref())
    },
    "where T: Node type",
  );
  assert_eq!(out.ok(), Some("type"));
}

#[test]
fn where_clause_probe_commits_across_pathed_predicate_heads() {
  // A variable-length predicate head (`u::V<W>:`) continues the clause — the
  // probe is not a fixed-width peek.
  fn check<S: AsRef<[u8]>>(c: Option<crate::graphqlx::ast::WhereClause<S>>) {
    let c = c.expect("clause present");
    assert_eq!(c.predicates_slice().len(), 2);
  }
  accept_all!(try_where_clause, "where T: A & B u::V<W>: X", check);
}

#[test]
fn where_clause_rejects_empty_and_boundless_per_cardinality() {
  // Amendment 2: `where WherePredicate+` — a bare `where` errors on its committed
  // first predicate; a committed probe head with no bound (`U:`) stays an error.
  reject_all!(try_where_clause, "where");
  reject_all!(try_where_clause, "where {");
  reject_all!(try_where_clause, "where T: A U:");
}

//! GraphQLx selection production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family. Fixtures `0016` and `0022` drive the
//! GraphQLx-specific rows: generic fragment spreads and generic type conditions.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{field, selection, selection_set};
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

// ─── field ───────────────────────────────────────────────────────────────────

#[test]
fn field_plain_aliased_and_full() {
  fn plain<S: AsRef<[u8]>>(f: crate::graphqlx::ast::Field<S>) {
    assert!(f.alias().is_none());
    assert_eq!(bytes(f.name().source_ref()), b"id");
    assert!(f.arguments().is_none());
    assert!(f.selection_set().is_none());
  }
  accept_all!(field, "id", plain);

  // Fixture `0022`: `typename: __typename` — the two-name alias lookahead.
  fn aliased<S: AsRef<[u8]>>(f: crate::graphqlx::ast::Field<S>) {
    let alias = f.alias().expect("alias present");
    assert_eq!(bytes(alias.name().source_ref()), b"typename");
    assert_eq!(bytes(f.name().source_ref()), b"__typename");
  }
  accept_all!(field, "typename: __typename", aliased);

  fn full<S: AsRef<[u8]>>(f: crate::graphqlx::ast::Field<S>) {
    assert!(f.alias().is_some());
    assert!(f.arguments().is_some());
    assert!(f.directives().is_some());
    let ss = f.selection_set().expect("selection set present");
    assert_eq!(ss.selections().len(), 1);
  }
  accept_all!(field, "u: user(id: $id) @cached { name }", full);
}

#[test]
fn field_alias_requires_a_second_name() {
  reject_all!(field, "alias: {");
  reject_all!(field, "alias:");
}

// ─── selection dispatch ──────────────────────────────────────────────────────

#[test]
fn selection_dispatches_all_three_arms() {
  // One row per alternative (dispatcher coverage): field, spread, inline.
  fn is_field<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    assert!(s.is_field());
  }
  accept_all!(selection, "name", is_field);

  fn is_spread<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    assert!(s.is_fragment_spread());
  }
  accept_all!(selection, "...UserFields", is_spread);

  fn is_inline<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    assert!(s.is_inline_fragment());
  }
  accept_all!(selection, "... on User { id }", is_inline);
}

#[test]
fn fragment_spread_with_generic_arguments() {
  // Fixture `0022`: `...ConnectionFields<String, Int>` — generic application on
  // the spread's type path.
  fn check<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    let spread = s.unwrap_fragment_spread();
    let name = spread.name();
    assert_eq!(
      bytes(name.path().segments_slice()[0].source_ref()),
      b"ConnectionFields"
    );
    let generics = name.type_generics().expect("generic arguments present");
    assert_eq!(generics.params().len(), 2);
  }
  accept_all!(selection, "...ConnectionFields<String, Int>", check);
}

#[test]
fn fragment_spread_with_pathed_name_and_directives() {
  fn check<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    let spread = s.unwrap_fragment_spread();
    assert_eq!(spread.name().path().segments_slice().len(), 2);
    assert!(spread.directives().is_some());
  }
  accept_all!(selection, "...shared::UserFields @skip(if: $flag)", check);
}

#[test]
fn inline_fragment_generic_type_condition() {
  // Fixture `0022`: `... on Document<T> { … }` — a generic application in the
  // type condition.
  fn check<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    let inline = s.unwrap_inline_fragment();
    let tc = inline.type_condition().expect("type condition present");
    assert_eq!(
      bytes(tc.name().path().segments_slice()[0].source_ref()),
      b"Document"
    );
    assert!(tc.name().type_generics().is_some());
  }
  accept_all!(selection, "... on Document<T> { title }", check);
}

#[test]
fn inline_fragment_bare_and_with_directives() {
  // Fixture `0022`'s bare `... { … }` plus the `@`-headed bare form.
  fn bare<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    let inline = s.unwrap_inline_fragment();
    assert!(inline.type_condition().is_none());
    assert!(inline.directives().is_none());
  }
  accept_all!(selection, "... { inlineField }", bare);

  fn directed<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    let inline = s.unwrap_inline_fragment();
    assert!(inline.type_condition().is_none());
    assert!(inline.directives().is_some());
  }
  accept_all!(selection, "... @defer { slowField }", directed);
}

#[test]
fn fragment_spread_named_on_is_unrepresentable() {
  // `... on …` always commits to the inline-fragment arm, so a spread named `on`
  // cannot be written (the shared grammar's FragmentName exclusion, carried to
  // GraphQLx): with a selection set it is an inline fragment on type `on`…
  fn check<S: AsRef<[u8]>>(s: crate::graphqlx::ast::Selection<S>) {
    assert!(s.is_inline_fragment());
  }
  accept_all!(selection, "... on on { id }", check);
  // … and without one it errors (the inline arm demands a type path + selection
  // set — never a spread of a fragment named `on`).
  reject_all!(selection, "... on @skip { id }");
}

// ─── selection_set ───────────────────────────────────────────────────────────

#[test]
fn selection_set_mixed_selections() {
  // Fixture `0022`'s field + spread + inline mix, commas-as-trivia.
  fn check<S: AsRef<[u8]>>(ss: crate::graphqlx::ast::SelectionSet<S>) {
    let sels = ss.selections();
    assert_eq!(sels.len(), 3);
    assert!(sels[0].is_field());
    assert!(sels[1].is_fragment_spread());
    assert!(sels[2].is_inline_fragment());
  }
  accept_all!(
    selection_set,
    "{ id ...NodeFragment<T> ... on Media<T> { url } }",
    check
  );
}

#[test]
fn selection_set_nests_recursively() {
  fn check<S: AsRef<[u8]>>(ss: crate::graphqlx::ast::SelectionSet<S>) {
    let outer = ss.selections();
    assert_eq!(outer.len(), 1);
    let field = match &outer[0] {
      crate::graphqlx::ast::Selection::Field(f) => f,
      _ => panic!("expected a field"),
    };
    let inner = field.selection_set().expect("nested selection set");
    assert_eq!(inner.selections().len(), 2);
  }
  accept_all!(selection_set, "{ user { id name } }", check);
}

#[test]
fn selection_set_empty_braces_error_per_spec() {
  // Amendment 2 (the W3 site carried to GraphQLx): `{ Selection+ }`.
  reject_all!(selection_set, "{}");
  reject_all!(selection_set, "{ }");
}

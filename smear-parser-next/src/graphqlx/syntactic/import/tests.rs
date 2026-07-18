//! GraphQLx `import` production tests.
//!
//! Every case is driven end to end over the real GraphQLx syntactic lexer under a
//! `Fatal<GraphqlxErrors>` context, matching `ty`'s harness. Accept cases run the
//! full source matrix (`str`, `[u8]`, and `Bytes` behind the feature); reject
//! cases assert the error family. The `ok_*` import fixtures (`0001`–`0003`,
//! `0013`) are the grammar arbiter.

use smear_lexer::graphqlx::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{import_clause, import_definition, import_list, named_specifier, wildcard_specifier};
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

// ─── named_specifier / wildcard_specifier ────────────────────────────────────

#[test]
fn named_specifier_plain_and_aliased() {
  fn plain<S: AsRef<[u8]>>(s: crate::graphqlx::ast::NamedSpecifier<S>) {
    assert_eq!(bytes(s.name().source_ref()), b"User");
    assert!(s.alias().is_none());
  }
  accept_all!(named_specifier, "User", plain);

  // Fixture `0003_import_with_alias`: `User as UserType`; the alias is a full
  // `::`-path.
  fn aliased<S: AsRef<[u8]>>(s: crate::graphqlx::ast::NamedSpecifier<S>) {
    assert_eq!(bytes(s.name().source_ref()), b"User");
    let alias = s.alias().expect("alias present");
    let segs = alias.segments_slice();
    assert_eq!(segs.len(), 2);
    assert_eq!(bytes(segs[0].source_ref()), b"types");
    assert_eq!(bytes(segs[1].source_ref()), b"UserType");
  }
  accept_all!(named_specifier, "User as types::UserType", aliased);
}

#[test]
fn named_specifier_as_without_path_errors() {
  // The soft `as` commits to an alias path; a missing one is an error.
  reject_all!(named_specifier, "User as");
  reject_all!(named_specifier, "User as *");
}

#[test]
fn wildcard_specifier_plain_and_aliased() {
  fn plain<S: AsRef<[u8]>>(s: crate::graphqlx::ast::WildcardSpecifier<S>) {
    assert!(s.alias().is_none());
  }
  accept_all!(wildcard_specifier, "*", plain);

  // Fixture `0013_complex_import`: `* as utils`.
  fn aliased<S: AsRef<[u8]>>(s: crate::graphqlx::ast::WildcardSpecifier<S>) {
    let alias = s.alias().expect("alias present");
    assert_eq!(bytes(alias.segments_slice()[0].source_ref()), b"utils");
  }
  accept_all!(wildcard_specifier, "* as utils", aliased);
}

// ─── import_list / import_clause ─────────────────────────────────────────────

#[test]
fn import_list_mixed_members() {
  // Fixture `0013_complex_import`: `{ User, Post, * as utils }` mixes named and
  // wildcard members; commas are trivia.
  fn check<S: AsRef<[u8]>>(l: crate::graphqlx::ast::ImportList<S>) {
    let members = l.items_slice();
    assert_eq!(members.len(), 3);
    assert!(members[0].is_named());
    assert!(members[1].is_named());
    assert!(members[2].is_wildcard());
  }
  accept_all!(import_list, "{ User, Post, * as utils }", check);
}

#[test]
fn import_list_empty_rejects_per_cardinality() {
  // Amendment 2 + the Wave 6 substrate grammar (`ImportMember+`): `{}` errors.
  reject_all!(import_list, "{}");
}

#[test]
fn import_clause_dispatches_list_and_wildcard() {
  fn list<S: AsRef<[u8]>>(c: crate::graphqlx::ast::ImportClause<S>) {
    assert!(c.is_list());
  }
  accept_all!(import_clause, "{ User }", list);

  fn wildcard<S: AsRef<[u8]>>(c: crate::graphqlx::ast::ImportClause<S>) {
    assert!(c.is_wildcard());
  }
  accept_all!(import_clause, "* as helpers", wildcard);

  // Anything else begins no clause.
  reject_all!(import_clause, "User");
  reject_all!(import_clause, "\"./types.graphqlx\"");
}

// ─── import_definition ───────────────────────────────────────────────────────

#[test]
fn import_definition_named_fixture() {
  // Fixture `0001_import_named`: `import { User, Post } from "./types.graphqlx"`.
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ImportDefinition<S>) {
    let clause = d.clause();
    assert!(clause.is_list());
    assert_eq!(bytes(d.file_path().source_ref()), b"\"./types.graphqlx\"");
  }
  accept_all!(
    import_definition,
    "import { User, Post } from \"./types.graphqlx\"",
    check
  );
}

#[test]
fn import_definition_wildcard_fixtures() {
  // Fixtures `0002` / `0013`: bare and aliased wildcard clauses.
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ImportDefinition<S>) {
    assert!(d.clause().is_wildcard());
  }
  accept_all!(
    import_definition,
    "import * from \"./types.graphqlx\"",
    check
  );
  accept_all!(
    import_definition,
    "import * as helpers from \"./helpers.graphqlx\"",
    check
  );
}

#[test]
fn import_definition_aliased_members_fixture() {
  // Fixture `0003_import_with_alias`.
  fn check<S: AsRef<[u8]>>(d: crate::graphqlx::ast::ImportDefinition<S>) {
    let list = match d.clause() {
      crate::graphqlx::ast::ImportClause::List(l) => l,
      crate::graphqlx::ast::ImportClause::Wildcard(_) => panic!("expected a list clause"),
    };
    let members = list.items_slice();
    assert_eq!(members.len(), 2);
    for member in members {
      assert!(member.alias().is_some());
    }
  }
  accept_all!(
    import_definition,
    "import { User as UserType, Post as BlogPost } from \"./types.graphqlx\"",
    check
  );
}

#[test]
fn import_definition_rejects_malformed_shapes() {
  // Missing `from`, missing path, and the empty-list cardinality breach.
  reject_all!(import_definition, "import { User }");
  reject_all!(import_definition, "import { User } \"./a.graphqlx\"");
  reject_all!(import_definition, "import {} from \"./a.graphqlx\"");
  reject_all!(import_definition, "import from \"./a.graphqlx\"");
}

#[test]
fn import_definition_rejects_block_string_path() {
  // The file path is keyed by `InlineStringValue` (frozen model): a block string
  // is not a file path (Deviations Register — type-shape authority).
  reject_all!(
    import_definition,
    "import * from \"\"\"./types.graphqlx\"\"\""
  );
}

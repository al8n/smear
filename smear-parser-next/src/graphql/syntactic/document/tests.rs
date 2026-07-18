//! Document production tests — the top-level `definition` dispatch, the
//! definition-or-extension forks, and the `document` / `type_system_document` roots.
//!
//! Mirrors the `extension` harness. The frozen-parity oracle pins the frozen
//! `smear-parser` verdicts, EXCLUDING the empty-input rows (parser-next rejects them
//! per the spec-cardinality rule, plan Amendment 2) and the rows the Wave-3/4/5
//! deviations already own (registered at their productions).

use smear_lexer::graphql::syntactic::SyntacticLexer;
use tokora::{FatalContext, InputRef, Parse, Parser};

use super::{
  definition, definition_or_extension, document, type_system_definition_or_extension,
  type_system_document,
};
use crate::graphql::{
  ast::{
    Definition, DefinitionOrExtension, Document, TypeSystemDefinitionOrExtension,
    TypeSystemDocument,
  },
  error::GraphqlErrors,
};

/// The fatal context a `str`-sourced parse runs under.
type StrCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, str>, GraphqlErrors<&'inp str>>;
/// The fatal context a `[u8]`-sourced parse runs under.
type SliceCtx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, [u8]>, GraphqlErrors<&'inp [u8]>>;

/// Drives `f` over a `str` source under `Fatal<GraphqlErrors<&str>>`.
fn drive_str<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp str>>,
  input: &'inp str,
) -> Result<O, GraphqlErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

/// Drives `f` over a `[u8]` source under `Fatal<GraphqlErrors<&[u8]>>`.
fn drive_slice<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp [u8],
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
  Parser::with_parser(f).parse_slice(input)
}

#[cfg(feature = "bytes")]
fn drive_bytes<'inp, O>(
  f: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, [u8]>, SliceCtx<'inp>>,
  ) -> Result<O, GraphqlErrors<&'inp [u8]>>,
  input: &'inp ::bytes::Bytes,
) -> Result<O, GraphqlErrors<&'inp [u8]>> {
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

/// Asserts the first error's data is end-of-input (the family the empty-document
/// cardinality deviations report).
fn first_is_end_of_input<'inp>(
  parser: impl for<'c> FnMut(
    &mut InputRef<'inp, 'c, SyntacticLexer<'inp, str>, StrCtx<'inp>>,
  ) -> Result<(), GraphqlErrors<&'inp str>>,
  src: &'inp str,
) -> bool {
  match drive_str(parser, src) {
    Err(errs) => errs
      .into_iter()
      .next()
      .is_some_and(|e| e.data().is_end_of_input()),
    Ok(()) => false,
  }
}

// ─── definition dispatch ─────────────────────────────────────────────────────

#[test]
fn definition_dispatches_each_arm() {
  fn is_ts<S: AsRef<[u8]>>(d: Definition<S>) {
    assert!(d.is_type_system());
  }
  fn is_exec<S: AsRef<[u8]>>(d: Definition<S>) {
    assert!(d.is_executable());
  }
  // Type-system arms: schema, directive, and each type-definition keyword.
  accept_all!(definition, "schema { query: Q }", is_ts);
  accept_all!(definition, "directive @a on FIELD", is_ts);
  accept_all!(definition, "scalar DateTime", is_ts);
  accept_all!(definition, "type User { id: ID }", is_ts);
  accept_all!(definition, "interface Node { id: ID }", is_ts);
  accept_all!(definition, "union U = A | B", is_ts);
  accept_all!(definition, "enum E { A }", is_ts);
  accept_all!(definition, "input In { x: Int }", is_ts);
  // Executable arms: fragment, named operation, query-shorthand.
  accept_all!(definition, "fragment F on T { f }", is_exec);
  accept_all!(definition, "query Q { f }", is_exec);
  accept_all!(definition, "mutation { f }", is_exec);
  accept_all!(definition, "{ f }", is_exec);
}

#[test]
fn definition_rejects_malformed() {
  reject_all!(definition, "bogus X");
  reject_all!(definition, "extend type T @d");
  reject_all!(definition, "");
}

// ─── definition_or_extension ─────────────────────────────────────────────────

#[test]
fn definition_or_extension_forks_on_extend() {
  fn is_ext<S: AsRef<[u8]>>(d: DefinitionOrExtension<S>) {
    assert!(d.is_extension());
  }
  fn is_def<S: AsRef<[u8]>>(d: DefinitionOrExtension<S>) {
    assert!(d.is_definition());
  }
  accept_all!(definition_or_extension, "extend type User @dir", is_ext);
  accept_all!(definition_or_extension, "extend schema @dir", is_ext);
  accept_all!(definition_or_extension, "type User { id: ID }", is_def);
  accept_all!(definition_or_extension, "query Q { f }", is_def);
}

#[test]
fn definition_or_extension_carries_description() {
  fn described<S: AsRef<[u8]>>(d: DefinitionOrExtension<S>) {
    match d {
      DefinitionOrExtension::Definition(described) => {
        assert!(described.description().is_some());
      }
      DefinitionOrExtension::Extension(_) => panic!("expected a described definition"),
    }
  }
  // The description rides the document-level Described wrapper on EVERY
  // definition arm (frozen parity), including the executable ones.
  accept_all!(
    definition_or_extension,
    "\"a user\" type User { id: ID }",
    described
  );
  accept_all!(
    definition_or_extension,
    "\"\"\"the op\"\"\" query Q { f }",
    described
  );
}

#[test]
fn definition_or_extension_rejects_described_extension() {
  // A description is legal before definitions only — `extend` refuses one (frozen
  // parity: the extension arm dispatches before the description is attempted).
  reject_all!(definition_or_extension, "\"doc\" extend type User @dir");
}

// ─── type_system_definition_or_extension ─────────────────────────────────────

#[test]
fn type_system_definition_or_extension_arms() {
  fn is_ext<S: AsRef<[u8]>>(d: TypeSystemDefinitionOrExtension<S>) {
    assert!(d.is_extension());
  }
  fn is_def<S: AsRef<[u8]>>(d: TypeSystemDefinitionOrExtension<S>) {
    assert!(d.is_definition());
  }
  accept_all!(
    type_system_definition_or_extension,
    "extend enum E { A }",
    is_ext
  );
  accept_all!(
    type_system_definition_or_extension,
    "schema { query: Q }",
    is_def
  );
  accept_all!(
    type_system_definition_or_extension,
    "directive @a on FIELD",
    is_def
  );
  accept_all!(
    type_system_definition_or_extension,
    "\"doc\" type User { id: ID }",
    is_def
  );
}

#[test]
fn type_system_definition_or_extension_rejects_executables() {
  // This root is schema-only: operations and fragments REJECT (frozen parity —
  // its non-extension arm dispatches schema/directive/type only).
  reject_all!(type_system_definition_or_extension, "query Q { f }");
  reject_all!(type_system_definition_or_extension, "{ f }");
  reject_all!(type_system_definition_or_extension, "fragment F on T { f }");
  reject_all!(type_system_definition_or_extension, "");
}

// ─── document ────────────────────────────────────────────────────────────────

#[test]
fn document_accepts_mixed_entries() {
  fn count3<S: AsRef<[u8]>>(d: Document<S>) {
    assert_eq!(d.definitions().len(), 3);
  }
  accept_all!(
    document,
    "type User { id: ID }\nextend type User @dir\nquery Q { user }",
    count3
  );
}

#[test]
fn document_accepts_described_definitions() {
  fn count2<S: AsRef<[u8]>>(d: Document<S>) {
    assert_eq!(d.definitions().len(), 2);
    match &d.definitions()[0] {
      DefinitionOrExtension::Definition(described) => {
        assert!(described.description().is_some());
      }
      DefinitionOrExtension::Extension(_) => panic!("expected a described definition"),
    }
  }
  accept_all!(
    document,
    "\"the schema\" schema { query: Q } scalar DateTime",
    count2
  );
}

#[test]
fn document_empty_input_error_per_spec() {
  // Spec-cardinality rule (plan Amendment 2): `Definition+` demands one-or-more,
  // so an empty document errors — a documented deviation from the frozen parser,
  // whose unenforced `+` accepted it.
  reject_all!(document, "");
  assert!(first_is_end_of_input(|inp| document(inp).map(|_| ()), ""));
}

#[test]
fn document_rejects_trailing_garbage() {
  // The entry list runs to end of input: a malformed tail is an error, not a stop.
  reject_all!(document, "type User { id: ID } bogus!");
}

// ─── type_system_document ────────────────────────────────────────────────────

#[test]
fn type_system_document_accepts_definitions_and_extensions() {
  fn count3<S: AsRef<[u8]>>(d: TypeSystemDocument<S>) {
    assert_eq!(d.definitions().len(), 3);
    assert!(d.definitions()[0].is_definition());
    assert!(d.definitions()[2].is_extension());
  }
  accept_all!(
    type_system_document,
    "schema { query: Q }\n\"doc\" type Q { f: Int }\nextend type Q @dir",
    count3
  );
}

#[test]
fn type_system_document_rejects_executables_and_empty() {
  reject_all!(type_system_document, "query Q { f }");
  reject_all!(type_system_document, "type Q { f: Int } query Q { f }");
  // Spec-cardinality rule (plan Amendment 2): `TypeSystemDefinitionOrExtension+`
  // demands one-or-more, so an empty document errors — a documented deviation from
  // the frozen parser.
  reject_all!(type_system_document, "");
  assert!(first_is_end_of_input(
    |inp| type_system_document(inp).map(|_| ()),
    ""
  ));
}

// ─── frozen-parity oracle (table-driven) ─────────────────────────────────────

/// Accept/reject verdicts for `document`. The empty-input row is EXCLUDED (the
/// Amendment-2 deviation pinned by `document_empty_input_error_per_spec`), as are
/// the rows owned by earlier registered deviations (empty `()`/`{}` lists, enum
/// `true`/`false`/`null`, bare `extend scalar`, implements-only extensions).
const DOCUMENT_ORACLE: &[(&str, bool)] = &[
  ("{ f }", true),
  ("query Q($x: Int = 1) { f(a: $x) }", true),
  ("fragment F on T { f } query { ...F }", true),
  ("schema { query: Q }", true),
  ("\"doc\" schema @d { query: Q }", true),
  ("scalar DateTime @tag", true),
  ("type User implements Node { id: ID! }", true),
  ("interface Node { id: ID }", true),
  ("union U = | A | B", true),
  ("enum E { A B }", true),
  ("input In { x: Int = 3 }", true),
  ("directive @a(x: Int) repeatable on FIELD | OBJECT", true),
  ("extend schema { mutation: M }", true),
  ("extend scalar S @d", true),
  ("extend type T { a: Int }", true),
  ("extend interface I @d", true),
  ("extend union U = A", true),
  ("extend enum E { A }", true),
  ("extend input In @d", true),
  (
    "type A { a: Int } extend type A @d { b: Int } query { a }",
    true,
  ),
  ("bogus X", false),
  ("type", false),
  ("query Q", false),
  ("extend", false),
  ("type User { id: ID } }", false),
];

#[test]
fn document_matches_frozen_verdicts() {
  for (src, accept) in DOCUMENT_ORACLE {
    assert_eq!(
      drive_str(|inp| document(inp).map(|_| ()), src).is_ok(),
      *accept,
      "str document({src:?})"
    );
    assert_eq!(
      drive_slice(|inp| document(inp).map(|_| ()), src.as_bytes()).is_ok(),
      *accept,
      "slice document({src:?})"
    );
  }
}

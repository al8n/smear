//! A public enum's attribute set is part of its contract: `#[non_exhaustive]` forces every
//! downstream exhaustive match to grow a wildcard arm, and a derive lost from the list can drop a
//! trait bound a consumer already relied on. Neither shows up in a normal review pass over the
//! variant list, because neither one touches a variant.
//!
//! **So is a variant's payload type, and this census watches that too — because the first version
//! of it did not, and the very next change to the enum it was written for walked past it.**
//! `feat/parser-materialised-i32` reshaped `ErrorData::IntOverflow(S)` into
//! `IntOverflow(IntOverflow<S>)`: an exhaustive match arm that binds the payload and uses it as
//! `S` stops compiling, as does the derived `unwrap_int_overflow`, in every configuration
//! including the one with `materialized-numbers` off. That is the same class of defect the
//! attribute axis was written against — an unreviewed shape change to a public enum — one axis
//! over, and this gate reported green on it. A gate written against the last defect reads the
//! surface that defect used; the repair is to name the axes rather than the cases, so [`AXES`]
//! is a list and adding a third is adding a row.
//!
//! `feat/parser-materialised-values` added `#[non_exhaustive]` to
//! [`ErrorData`](smear_parser::graphql::error::ErrorData) without saying so. The doc paragraph
//! introduced beside it argued the attribute belonged there, and a later round read that
//! justification as evidence the attribute predated the branch — it did not: `ErrorData` was
//! exhaustive at `c3e35b8`, the base this branch was cut from, and Codex found the gap after the
//! justification had already made it read as settled. Both the attribute and the paragraph came
//! straight back off, and this test is the gate that mistake argues for: **on every axis below,
//! every public enum in the files this branch touches must read as it did at the merge base, or
//! the difference must be recorded with the value it was reviewed at.**
//!
//! **`ErrorData` carries the attribute again, and the difference between the two rounds is this
//! table.** Removing it was the right action on an unreviewed change and it settled nothing; the
//! question was then argued on its own — `7b9b293`'s rule, the measured downstream cost, and what
//! `#[non_exhaustive]` takes from a consumer — and the attribute won. So there is an entry in
//! [`RECORDED_DIFFERENCES`] below carrying the exact four attribute lines the review approved, and
//! that entry is the *only* difference between an attribute that was decided and one that was not.
//! The gate did not go quiet because the argument was persuasive; it went quiet because the value
//! was written down — and both directions out of that value were planted to prove it is a value
//! and not a silence. Taking the attribute back off fails with *its attribute set is back to the
//! merge base's, but a recorded difference still claims it changed*; adding a second attribute
//! beside it fails with *a SECOND change has landed under the first one's justification*.
//!
//! **The merge base moved and these values did not.** #157 merged into `feat/proto`, so the branch
//! now sits on `6e0baf0`; `git diff --name-only c3e35b8 6e0baf0 -- smear-parser` is **empty**, so
//! every string in the two baseline tables is what the current merge base says too. Re-read them
//! from the new base only if that diff ever stops being empty — a table whose provenance commit is
//! no longer reachable from the base is a table nobody can re-derive.
//!
//! # Scope: the files this branch's diff touches
//!
//! Not the whole crate. The defect this test guards was a diff-review blind spot on a branch's
//! own changes, and that is what [`FILES`] enumerates — `git diff --name-only c3e35b8 bd5d52b`
//! filtered to the files that can declare an `enum`. A public enum elsewhere in `smear-parser` can
//! still gain `#[non_exhaustive]` unnoticed; widening this list to the whole crate is future work
//! for whoever decides the cost of auditing every other enum's history is worth it, not a silent
//! limitation of this one.
//!
//! **A stacked branch adds to this list rather than inheriting it, and it takes back what it
//! removes.** `feat/parser-materialised-i32` added `src/graphql/ast/materialized32.rs`, because a
//! file that declares two public enums and is absent from [`FILES`] is a scan that reports green
//! over a surface it never read — the same absence-shaped defect one level out.
//! `refactor/materialised-generic` deletes that file, so the entry goes and four [`Recorded`] rows
//! go with it. Both directions fail loudly rather than quietly: the count assertions move whether
//! or not the file is listed, and a `Recorded` row naming an enum this scan can no longer find is
//! reported as a stale entry by [`audit`].
//!
//! # Why a line scan and not `syn`
//!
//! `ci/source_census` reads source as text with `syn` because it has to see inside expressions —
//! a `match` arm buried in a closure, a type nested three levels deep. This census only has to
//! read the attribute lines stacked directly above a `pub enum` line and the variant lines
//! inside it, and every one of those in these six files is a single `cargo fmt`-kept line.
//! [`find_public_enums`] and [`read_variants`] assert that rather than assuming it — a wrapped
//! attribute, a wrapped enum header, a multi-line variant and a struct variant each panic with
//! the line that caused it and the words "the census needs widening", so a line that stops being
//! readable fails loudly here instead of being silently misread. That is what keeps this test
//! free of a `syn` dev-dependency `smear-parser` has no other reason to carry.
//!
//! # Two axes, four tables, and what each failure means
//!
//! [`MERGE_BASE_ATTRS`] is what `git show c3e35b8:<path>` said for every enum that already
//! existed there, read once and copied here verbatim; [`MERGE_BASE_VARIANTS`] is the same
//! reading of the variant signatures. [`RECORDED_DIFFERENCES`] and
//! [`RECORDED_VARIANT_DIFFERENCES`] are every enum whose current value on that axis does not
//! match — because it is new since the merge base, or because it changed and the change was
//! reviewed on its own merits. An enum in neither table for an axis, or one whose current value
//! differs from the baseline without a matching recorded entry, fails the test with both values
//! printed. That is the shape `#[non_exhaustive]` on `ErrorData` took the first time: no recorded
//! entry, because the round that added it believed it was already there. It has one now.
//!
//! **The exception lists are per axis and not per enum**, which is the one structural decision in
//! here worth arguing. A single entry per enum would let a justification written for one contract
//! change wave through the other: `ast/value.rs::ConstInputValue` has a recorded *attribute*
//! difference and no recorded payload one, so a payload change to it still fails — under a shared
//! entry it would not. An enum that is genuinely new needs a line in both.
//!
//! A table entry matching nothing this scan still finds fails too, the same way
//! `ci/source_census`'s own exemption table does: a stale entry hides the next real one exactly
//! as an unrecorded change does.
//!
//! # What each of the two axes was planted against
//!
//! A gate is worth what its plant proved, so both were run against this file and against the
//! version of it that shipped before the payload axis existed.
//!
//! | plant | old gate | this gate |
//! |---|---|---|
//! | `ErrorData::InvalidEnumValue(S)` → `InvalidEnumValue(UnexpectedKeyword<S>)` | green | fails |
//! | `#[non_exhaustive]` on `ast/value.rs::ConstInputValue` | green | fails |
//!
//! The first is the payload axis, planted on a variant that has nothing to do with the change
//! this branch makes — `IntOverflow` would have proved only that the census can read the one line
//! already in a table. The second is the `expected` field on [`Recorded`], and it went green on
//! the old gate for a *different* reason worth naming: `ConstInputValue` already had a recorded
//! attribute difference, and a reason-only entry approved the enum rather than the change, so a
//! second, unrelated attribute change inherited the first one's justification. Nothing about the
//! payload axis caused that hole; generalising the table is what exposed it.
//!
//! # What the payload axis cannot see, and the blind spot that closed
//!
//! It compares the payload as **written**, not as resolved, so a change made inside a
//! module-local `pub type` changes no line it reads.
//!
//! That used to bite exactly here. `materialized::InputValue::Int` and
//! `materialized32::InputValue::Int` both read `Int(IntValue)` and were different types, because
//! each module declared its own `IntValue` alias — one at `i64`, one at `i32`. Two enums whose
//! recorded variant lists were byte-identical and whose meanings were not is the worst case a text
//! census can be handed, and the entry for the second one said so.
//!
//! **`refactor/materialised-generic` closed it by deleting the second enum.** There is one
//! materialised tree now and the width is a parameter on it, so the line this census reads is
//! `Int(IntValue<I>)` — the payload names the parameter that decides it, and two widths are two
//! instantiations of one recorded list rather than two lists that happen to match. The census
//! still cannot resolve `IntValue<I>` to `super::IntValue<I, SimpleSpan>`, and that limit is real;
//! what is gone is the case where resolving it was the only way to tell two enums apart.
//! `every_value_tree_declares_the_same_variants` and the round trips in
//! `syntactic::value::materialized::tests` hold the resolved axis.

use std::collections::BTreeSet;

/// One file this branch's diff touches, embedded at compile time so the census needs no
/// filesystem access beyond what the compiler already resolved.
struct SourceFile {
  /// Path relative to this crate's `Cargo.toml` — the root `git diff --stat` reports from.
  path: &'static str,
  text: &'static str,
}

const FILES: &[SourceFile] = &[
  SourceFile {
    path: "src/graphql/error.rs",
    text: include_str!("../src/graphql/error.rs"),
  },
  SourceFile {
    path: "src/graphql/ast/value.rs",
    text: include_str!("../src/graphql/ast/value.rs"),
  },
  SourceFile {
    path: "src/graphql/ast/materialized.rs",
    text: include_str!("../src/graphql/ast/materialized.rs"),
  },
  SourceFile {
    path: "src/graphql/syntactic/value/mod.rs",
    text: include_str!("../src/graphql/syntactic/value/mod.rs"),
  },
  SourceFile {
    path: "src/graphql/syntactic/value/numbers.rs",
    text: include_str!("../src/graphql/syntactic/value/numbers.rs"),
  },
];

/// A `pub enum`, found by the scan, with the attribute lines stacked directly above it and the
/// variant signatures inside it. Doc comments are excluded from both — this census is about the
/// enum's contract, not its prose.
#[derive(Debug)]
struct FoundEnum {
  file: &'static str,
  name: String,
  attrs: Vec<String>,
  /// Each variant as written, trailing comma stripped: `"EndOfInput"`,
  /// `"IntOverflow(IntOverflow<S>)"`. Name and payload together, because the pair is what a
  /// downstream pattern binds.
  variants: Vec<String>,
}

/// Reads the variant signatures out of the enum body that starts at `header` (the `pub enum …`
/// line, index into `lines`).
///
/// # Indentation, not brace counting, and the difference is load-bearing
///
/// A depth counter over raw lines is wrong here and silently so: `Expectation`'s doc comments
/// contain a literal `` `{` `` and `` `}` ``, and [`Unclosed`]'s say `missing `}``. A counter that
/// reads them loses `Expectation::LBrace` and `Unclosed::Object` from the census — two variants
/// that would then be free to change payload unwatched. This walks to the `}` at the enum
/// header's own indentation instead, which no comment can forge, and asserts every other
/// assumption it makes rather than trusting `cargo fmt` to have held.
///
/// [`Unclosed`]: smear_parser::graphql::error::Unclosed
fn read_variants(file: &SourceFile, lines: &[&str], header: usize) -> Vec<String> {
  let header_line = lines[header];
  let indent = header_line.len() - header_line.trim_start().len();
  assert!(
    header_line.trim_end().ends_with('{'),
    "{}:{}: an enum header this scan cannot read whole: {header_line:?} — the generics or the \
     where-clause wrap, and the census needs widening before it can trust what it read",
    file.path,
    header + 1,
  );

  let mut variants = Vec::new();
  for (offset, line) in lines.iter().enumerate().skip(header + 1) {
    let trimmed = line.trim();
    if trimmed == "}" && line.len() - line.trim_start().len() == indent {
      return variants;
    }
    if trimmed.is_empty() || trimmed.starts_with("//") || trimmed.starts_with("#[") {
      continue;
    }

    let line_no = offset + 1;
    assert_eq!(
      line.len() - line.trim_start().len(),
      indent + 2,
      "{}:{line_no}: a line inside an enum body at an indentation this scan does not expect: \
       {trimmed:?}",
      file.path,
    );
    assert!(
      trimmed.ends_with(','),
      "{}:{line_no}: a variant this scan cannot read whole: {trimmed:?} — it spans more than \
       one line, and the census needs widening before it can trust what it read",
      file.path,
    );
    assert!(
      !trimmed.contains('{'),
      "{}:{line_no}: a struct variant, which this scan cannot read: {trimmed:?} — its fields \
       are the payload and they are on other lines, so the census needs widening",
      file.path,
    );
    assert_eq!(
      trimmed.matches('(').count(),
      trimmed.matches(')').count(),
      "{}:{line_no}: a variant whose parentheses do not balance on one line: {trimmed:?}",
      file.path,
    );
    variants.push(trimmed.trim_end_matches(',').to_string());
  }

  panic!(
    "{}:{}: no `}}` at the enum header's own indentation — this scan cannot tell where the body \
     ends",
    file.path,
    header + 1,
  );
}

/// Walks `file.text` line by line and returns every `pub enum` this scan can read, each paired
/// with the attribute lines immediately above it and the variant signatures inside it.
/// `pub(crate)` and private enums are skipped — they cannot break a downstream match, which is
/// the property this gate exists to protect.
fn find_public_enums(file: &SourceFile) -> Vec<FoundEnum> {
  let lines: Vec<&str> = file.text.lines().collect();
  let mut found = Vec::new();

  for (i, line) in lines.iter().enumerate() {
    let Some(rest) = line.trim_start().strip_prefix("pub enum ") else {
      continue;
    };
    let name: String = rest
      .chars()
      .take_while(|c| c.is_alphanumeric() || *c == '_')
      .collect();
    assert!(
      !name.is_empty(),
      "{}:{}: `pub enum` with no name this scan can read: {line:?}",
      file.path,
      i + 1
    );

    // Walk upward collecting attribute lines, stopping at the first line that is not one — a
    // doc comment, a blank line, or code from the previous item.
    let mut attrs = Vec::new();
    let mut j = i;
    while j > 0 {
      j -= 1;
      let above = lines[j].trim();
      if above.starts_with("#[") {
        assert!(
          above.ends_with(']'),
          "{}:{}: an attribute line this scan cannot read whole: {above:?} — it spans more \
           than one line, and the census needs widening before it can trust what it read",
          file.path,
          j + 1
        );
        attrs.push(above.to_string());
      } else {
        break;
      }
    }
    attrs.reverse();
    let variants = read_variants(file, &lines, i);
    found.push(FoundEnum {
      file: file.path,
      name,
      attrs,
      variants,
    });
  }

  found
}

/// The attribute set each currently-existing public enum had at `c3e35b8`, this branch's merge
/// base into `feat/proto` — read once from `git show c3e35b8:<path>` and copied here verbatim.
///
/// `(file, enum name, attribute lines in source order)`.
#[rustfmt::skip]
const MERGE_BASE_ATTRS: &[(&str, &str, &[&str])] = &[
  ("src/graphql/error.rs", "VariableValueHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "ObjectFieldValueHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "SchemaExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "UnionTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "InputObjectTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "ObjectTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "InterfaceTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "EnumTypeExtensionHint", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "Expectation", &[
    "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]",
    "#[non_exhaustive]",
  ]),
  ("src/graphql/error.rs", "Unclosed", &[
    "#[derive(Debug, Copy, Clone, IsVariant)]",
  ]),
  ("src/graphql/error.rs", "ErrorData", &[
    "#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
  ("src/graphql/ast/value.rs", "InputValue", &[
    "#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
  ("src/graphql/ast/value.rs", "ConstInputValue", &[
    "#[derive(Debug, Clone, PartialEq, Eq, IsVariant, Unwrap, TryUnwrap)]",
    "#[unwrap(ref, ref_mut)]",
    "#[try_unwrap(ref, ref_mut)]",
  ]),
];

/// The variant signatures each currently-existing public enum had at `c3e35b8` — name and payload
/// together, in source order, derived from `git show c3e35b8:<path>` by the same rules
/// [`read_variants`] applies to the current text, so a mismatch is a real difference and not two
/// readers disagreeing.
///
/// `(file, enum name, variant signatures in source order)`.
#[rustfmt::skip]
const MERGE_BASE_VARIANTS: &[(&str, &str, &[&str])] = &[
  ("src/graphql/error.rs", "VariableValueHint", &[
    "Name",
    "Dollar",
  ]),
  ("src/graphql/error.rs", "ObjectFieldValueHint", &[
    "Colon",
    "Value",
    "Name",
  ]),
  ("src/graphql/error.rs", "SchemaExtensionHint", &[
    "Directives",
    "RootOperationTypesDefinition",
    "Extend",
    "Schema",
    "DirectivesOrRootOperationTypesDefinition",
  ]),
  ("src/graphql/error.rs", "UnionTypeExtensionHint", &[
    "Directives",
    "UnionMemberTypes",
    "Name",
    "Extend",
    "Union",
    "DirectivesOrUnionMemberTypes",
  ]),
  ("src/graphql/error.rs", "InputObjectTypeExtensionHint", &[
    "Directives",
    "InputFieldsDefinition",
    "Name",
    "Extend",
    "Input",
    "DirectivesOrInputFieldsDefinition",
  ]),
  ("src/graphql/error.rs", "ObjectTypeExtensionHint", &[
    "Implements",
    "Directives",
    "FieldsDefinition",
    "Name",
    "Extend",
    "Type",
    "ImplementsOrDirectivesOrFieldsDefinition",
  ]),
  ("src/graphql/error.rs", "InterfaceTypeExtensionHint", &[
    "Implements",
    "Directives",
    "FieldsDefinition",
    "Name",
    "Extend",
    "Interface",
    "ImplementsOrDirectivesOrFieldsDefinition",
  ]),
  ("src/graphql/error.rs", "EnumTypeExtensionHint", &[
    "Directives",
    "EnumValuesDefinition",
    "Name",
    "Extend",
    "Enum",
    "DirectivesOrEnumValuesDefinition",
  ]),
  ("src/graphql/error.rs", "Expectation", &[
    "InlineString",
    "BlockString",
    "Dollar",
    "LParen",
    "RParen",
    "Spread",
    "Colon",
    "Equal",
    "At",
    "LBracket",
    "RBracket",
    "LBrace",
    "RBrace",
    "Pipe",
    "Bang",
    "Ampersand",
    "ConstInputValue",
    "InputValue",
    "Type",
    "Selection",
    "VariableDefinition",
    "OperationType",
    "ExecutableDefinition",
    "OperationTypeOrFragment",
    "FragmentName",
    "Name",
    "OperationName",
    "DirectiveLocation",
    "FragmentSpreadOrInlineFragment",
    "IntValue",
    "BooleanValue",
    "FloatValue",
    "NullValue",
    "EnumValue",
    "StringValue",
    "Keyword(&'static str)",
  ]),
  ("src/graphql/error.rs", "Unclosed", &[
    "Parentheses",
    "List",
    "Object",
  ]),
  ("src/graphql/error.rs", "ErrorData", &[
    "Lexer(LexerErrors<Char, StateError>)",
    "IntOverflow(S)",
    "FloatOverflow(S)",
    "InvalidEnumValue(S)",
    "InvalidBooleanValue(S)",
    "InvalidNullValue(S)",
    "InvalidFragmentName(S)",
    "Unclosed(Unclosed)",
    "UnexpectedToken(UnexpectedToken<T, Exp>)",
    "UnexpectedKeyword(UnexpectedKeyword<S>)",
    "UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>)",
    "UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>)",
    "UnknownDirectiveLocation(S)",
    "UnknownOperationType(S)",
    "UnexpectedEndOfObjectExtension(UnexpectedEnd<ObjectTypeExtensionHint>)",
    "UnexpectedEndOfInterfaceExtension(UnexpectedEnd<InterfaceTypeExtensionHint>)",
    "UnexpectedEndOfEnumExtension(UnexpectedEnd<EnumTypeExtensionHint>)",
    "UnexpectedEndOfInputObjectExtension(UnexpectedEnd<InputObjectTypeExtensionHint>)",
    "UnexpectedEndOfUnionExtension(UnexpectedEnd<UnionTypeExtensionHint>)",
    "UnexpectedEndOfSchemaExtension(UnexpectedEnd<SchemaExtensionHint>)",
    "EndOfInput",
    "Other(std::borrow::Cow<'static, str>)",
  ]),
  ("src/graphql/ast/value.rs", "InputValue", &[
    "Variable(VariableValue<S>)",
    "Boolean(BooleanValue<S>)",
    "String(StringValue<S>)",
    "Float(FloatValue<S>)",
    "Int(IntValue<S>)",
    "Enum(EnumValue<S>)",
    "Null(NullValue<S>)",
    "List(List<S>)",
    "Object(Object<S>)",
  ]),
  ("src/graphql/ast/value.rs", "ConstInputValue", &[
    "Boolean(BooleanValue<S>)",
    "String(StringValue<S>)",
    "Float(FloatValue<S>)",
    "Int(IntValue<S>)",
    "Enum(EnumValue<S>)",
    "Null(NullValue<S>)",
    "List(ConstList<S>)",
    "Object(ConstObject<S>)",
  ]),
];

/// One reviewed difference from the merge base, on one axis, for one enum.
///
/// # `expected` is the whole point, and the first draft of this table did not have it
///
/// A recorded difference used to be a *reason* and nothing else, so the entry said "this enum is
/// allowed to differ" rather than "this enum differs in exactly this way". That is an exemption
/// with no upper bound: `ast/value.rs::ConstInputValue` is recorded for gaining `From` in its
/// derive list, and under a reason-only entry it could also gain `#[non_exhaustive]` and stay
/// green — the second change riding in on the first one's justification, which is the shape this
/// whole census exists to refuse. That is the second row of the plant table in the module header,
/// and it was green before this field existed.
///
/// So an entry carries the value the review approved. Anything else — a further change, or the
/// enum reverting to its merge-base value — fails and prints all three readings.
struct Recorded {
  file: &'static str,
  name: &'static str,
  /// What the reviewed change made this axis, exactly as the scan reads it.
  expected: &'static [&'static str],
  reason: &'static str,
}

/// Every enum whose current attribute set does not equal `MERGE_BASE_ATTRS` — new since the
/// merge base, or changed since it — each with the reviewed value and the reason. This is the
/// table `#[non_exhaustive]` on `ErrorData` should have gone in and did not: an unreviewed
/// attribute change is exactly an entry missing from both this table and `MERGE_BASE_ATTRS`. The
/// first entry below is that attribute, on the second attempt, in the table.
const RECORDED_DIFFERENCES: &[Recorded] = &[
  Recorded {
    file: "src/graphql/error.rs",
    name: "ErrorData",
    expected: &[
      "#[derive(Debug, Clone, From, IsVariant, Unwrap, TryUnwrap)]",
      "#[unwrap(ref, ref_mut)]",
      "#[try_unwrap(ref, ref_mut)]",
      "#[non_exhaustive]",
    ],
    reason: "ONE attribute added: `#[non_exhaustive]`. SOURCE-BREAKING, ARGUED AND ACCEPTED — the \
       section of that name on `ErrorData` is the argument, and this is the entry the round that \
       added it the first time did not write. What decided it is `7b9b293`'s own rule: the \
       attribute belongs on vocabulary SMEAR OWNS and not on vocabulary the GRAPHQL \
       SPECIFICATION ENUMERATES, and `ErrorData` transcribes no production — its `Other` variant \
       exists because the list is open, its two overflow variants arrived with a feature, and its \
       six extension-hint variants are one grammatical situation cut six ways. The four enums \
       `7b9b293` freed are the other side of the same rule and stay exhaustive, as do `Unclosed` \
       and `IntWidth` in this very file. MEASURED COST: `cargo check --workspace --all-features \
       --all-targets --keep-going` with the attribute applied reports exactly one `E0004` in the \
       whole workspace, `smear-smoke`'s out-of-crate probe, which is replaced rather than deleted \
       by `error_data_variant_tag`. The three derive lines above are the merge base's, unedited, \
       and repeating them is the point — this entry approves one attribute, not the enum.",
  },
  Recorded {
    file: "src/graphql/ast/value.rs",
    name: "ConstInputValue",
    expected: &[
      "#[derive(Debug, Clone, PartialEq, Eq, From, IsVariant, Unwrap, TryUnwrap)]",
      "#[unwrap(ref, ref_mut)]",
      "#[try_unwrap(ref, ref_mut)]",
    ],
    reason: "gained `From` in its derive list (merge base had none). Additive: `InputValue` beside it \
       has derived `From` since it was written, this is the one change the materialisation axis \
       makes to this file, and the doc comment directly above the derive says why — a shared \
       value-tree production converts the leaf it just read, so one body builds both trees.",
  },
  Recorded {
    file: "src/graphql/ast/materialized.rs",
    name: "InputValue",
    expected: &[
      "#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]",
      "#[unwrap(ref, ref_mut)]",
      "#[try_unwrap(ref, ref_mut)]",
    ],
    reason: "new enum: the materialised-number value tree's twin of `ast::InputValue`, gated behind \
       `materialized-numbers`. No merge-base entry exists because the file did not. `Eq` is \
       absent where the slice tree has it, because a materialised `Float` leaf is an `f64`.",
  },
  Recorded {
    file: "src/graphql/ast/materialized.rs",
    name: "ConstInputValue",
    expected: &[
      "#[derive(Debug, Clone, PartialEq, From, IsVariant, Unwrap, TryUnwrap)]",
      "#[unwrap(ref, ref_mut)]",
      "#[try_unwrap(ref, ref_mut)]",
    ],
    reason: "new enum, same reason as `materialized::InputValue` above.",
  },
  Recorded {
    file: "src/graphql/error.rs",
    name: "IntWidth",
    expected: &[
      "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, derive_more::Display)]",
    ],
    reason: "new enum: which signed width a materialised `Int` leaf was read at, carried by \
       `ErrorData::IntOverflow` now that `materialized-numbers` ships two readings of `Int`. \
       EXHAUSTIVE deliberately — the enum's own doc argues it, and the argument is that a \
       consumer reads this precisely to branch on the two, so `#[non_exhaustive]` would force a \
       wildcard over a closed two-element set. Attribute sets are contracts and this one is a \
       choice, which is the entry this table exists for.",
  },
];

/// Every enum whose current variant signatures do not equal [`MERGE_BASE_VARIANTS`] — new since
/// the merge base, or reshaped since it — each with the reviewed variant list and the reason.
///
/// **A separate list from [`RECORDED_DIFFERENCES`], deliberately.** One entry per enum covering
/// both axes would mean an exemption written for one contract change silently covers the other:
/// `ast/value.rs::ConstInputValue` is recorded above for gaining `From` in its derive list, and
/// under a shared entry a payload change to any of its eight variants would ride in on that
/// justification. An enum that is new needs a line in both tables; an enum whose attributes
/// changed needs a line here only if its payloads changed too — which `ConstInputValue`'s
/// absence from this table asserts.
const RECORDED_VARIANT_DIFFERENCES: &[Recorded] = &[
  Recorded {
    file: "src/graphql/error.rs",
    name: "ErrorData",
    expected: &[
      "Lexer(LexerErrors<Char, StateError>)",
      "IntOverflow(IntOverflow<S>)",
      "FloatOverflow(S)",
      "InvalidEnumValue(S)",
      "InvalidBooleanValue(S)",
      "InvalidNullValue(S)",
      "InvalidFragmentName(S)",
      "Unclosed(Unclosed)",
      "UnexpectedToken(UnexpectedToken<T, Exp>)",
      "UnexpectedKeyword(UnexpectedKeyword<S>)",
      "UnexpectedEndOfVariableValue(UnexpectedEnd<VariableValueHint>)",
      "UnexpectedEndOfObjectFieldValue(UnexpectedEnd<ObjectFieldValueHint>)",
      "UnknownDirectiveLocation(S)",
      "UnknownOperationType(S)",
      "UnexpectedEndOfObjectExtension(UnexpectedEnd<ObjectTypeExtensionHint>)",
      "UnexpectedEndOfInterfaceExtension(UnexpectedEnd<InterfaceTypeExtensionHint>)",
      "UnexpectedEndOfEnumExtension(UnexpectedEnd<EnumTypeExtensionHint>)",
      "UnexpectedEndOfInputObjectExtension(UnexpectedEnd<InputObjectTypeExtensionHint>)",
      "UnexpectedEndOfUnionExtension(UnexpectedEnd<UnionTypeExtensionHint>)",
      "UnexpectedEndOfSchemaExtension(UnexpectedEnd<SchemaExtensionHint>)",
      "EndOfInput",
      "Other(std::borrow::Cow<'static, str>)",
    ],
    reason: "ONE variant changed: `IntOverflow(S)` became `IntOverflow(IntOverflow<S>)`. \
       SOURCE-BREAKING, ACCEPTED AND DECLARED — see the section of that name on `ErrorData` for \
       what breaks, what it buys and why the alternatives are worse. The variant predates the \
       merge base, so this reshapes an old name rather than adding a new one: an existing match \
       arm that binds the payload and uses it as `S` stops compiling, as does a call to the \
       derived `unwrap_int_overflow`, in EVERY configuration including the one with \
       `materialized-numbers` off. The other 21 lines above are the merge base's, unedited, and \
       repeating them is the point — this entry approves one reshaping, not the enum.",
  },
  Recorded {
    file: "src/graphql/error.rs",
    name: "IntWidth",
    expected: &["I32", "I64"],
    reason: "new enum: the two readings of `Int` the `materialized-numbers` feature ships. No \
       merge-base entry exists because the enum did not.",
  },
  Recorded {
    file: "src/graphql/ast/materialized.rs",
    name: "InputValue",
    expected: &[
      "Variable(AstVariableValue<S>)",
      "Boolean(AstBooleanValue<S>)",
      "String(AstStringValue<S>)",
      "Float(FloatValue)",
      "Int(IntValue<I>)",
      "Enum(AstEnumValue<S>)",
      "Null(AstNullValue<S>)",
      "List(List<S, I>)",
      "Object(Object<S, I>)",
    ],
    reason: "new enum: the materialised-number value tree's twin of `ast::InputValue`, variant for \
       variant with the slice tree and differing in the two numeric leaves. No merge-base entry \
       exists because the file did not. THE PAYLOAD PARAMETER IS PART OF THIS LIST: `Int`, `List` \
       and `Object` name `I`, and `Float` does not, which is the census reading off the \
       declaration that the width reaches the integer leaf and the containers over it and nothing \
       else. Draft §3.5.2 makes `Float` IEEE 754 double precision at every reading of `Int`, so an \
       `I` appearing on that line would be a defect this row fails on.",
  },
  Recorded {
    file: "src/graphql/ast/materialized.rs",
    name: "ConstInputValue",
    expected: &[
      "Boolean(AstBooleanValue<S>)",
      "String(AstStringValue<S>)",
      "Float(FloatValue)",
      "Int(IntValue<I>)",
      "Enum(AstEnumValue<S>)",
      "Null(AstNullValue<S>)",
      "List(ConstList<S, I>)",
      "Object(ConstObject<S, I>)",
    ],
    reason: "new enum, same reason as `materialized::InputValue` above, payload parameter included.",
  },
];

/// One contract axis this census reads off a `pub enum`, with the two tables that account for it.
struct Axis {
  /// What a failure message calls this axis.
  name: &'static str,
  /// What the scan found on this axis, for one enum.
  actual: fn(&FoundEnum) -> &[String],
  baseline: &'static [(&'static str, &'static str, &'static [&'static str])],
  recorded: &'static [Recorded],
}

const AXES: &[Axis] = &[
  Axis {
    name: "attribute set",
    actual: |enum_| &enum_.attrs,
    baseline: MERGE_BASE_ATTRS,
    recorded: RECORDED_DIFFERENCES,
  },
  Axis {
    name: "variant signature list",
    actual: |enum_| &enum_.variants,
    baseline: MERGE_BASE_VARIANTS,
    recorded: RECORDED_VARIANT_DIFFERENCES,
  },
];

fn lines_match(expected: &[&str], actual: &[String]) -> bool {
  expected.len() == actual.len() && expected.iter().zip(actual).all(|(e, a)| *e == a.as_str())
}

/// Audits one axis over every enum the scan found, appending a line to `failures` for each enum
/// this axis cannot account for and for each table entry that no longer matches anything.
///
/// An enum passes on an axis when it reads exactly as the merge base did, or exactly as the
/// [`Recorded`] entry approving its difference says. Reading as neither fails, and so does
/// reading as the merge base while still carrying a recorded difference — a stale exemption
/// hides the next real change as effectively as an unrecorded one.
fn audit(axis: &Axis, found: &[FoundEnum], failures: &mut Vec<String>) {
  let mut seen_baseline: BTreeSet<(&str, &str)> = BTreeSet::new();
  let mut seen_recorded: BTreeSet<(&str, &str)> = BTreeSet::new();

  for enum_ in found {
    let actual = (axis.actual)(enum_);
    let baseline = axis
      .baseline
      .iter()
      .find(|entry| entry.0 == enum_.file && entry.1 == enum_.name);
    let recorded = axis
      .recorded
      .iter()
      .find(|entry| entry.file == enum_.file && entry.name == enum_.name);

    if let Some(entry) = baseline {
      seen_baseline.insert((entry.0, entry.1));
    }
    if let Some(entry) = recorded {
      seen_recorded.insert((entry.file, entry.name));
    }

    let matches_baseline = baseline.is_some_and(|entry| lines_match(entry.2, actual));
    let matches_recorded = recorded.is_some_and(|entry| lines_match(entry.expected, actual));

    if matches_recorded {
      continue;
    }
    if matches_baseline {
      if recorded.is_some() {
        failures.push(format!(
          "{}::{} — its {} is back to the merge base's, but a recorded difference still claims \
           it changed. Delete the entry, the difference it named is gone",
          enum_.file, enum_.name, axis.name,
        ));
      }
      continue;
    }

    match (baseline, recorded) {
      (Some(b), Some(r)) => failures.push(format!(
        "{}::{} — its {} matches neither the merge base nor the difference recorded for it. A \
         SECOND change has landed under the first one's justification, which is the thing the \
         `expected` column exists to refuse\n    merge base: {:?}\n    recorded:   {:?}\n    \
         now:        {:?}\n    what was approved: {}",
        enum_.file, enum_.name, axis.name, b.2, r.expected, actual, r.reason,
      )),
      (Some(b), None) => failures.push(format!(
        "{}::{} — its {} changed since the merge base and no difference is recorded for that \
         axis\n    merge base: {:?}\n    now:        {:?}",
        enum_.file, enum_.name, axis.name, b.2, actual,
      )),
      (None, Some(r)) => failures.push(format!(
        "{}::{} — recorded as differing on its {}, but it does not read as the entry says\n    \
         recorded: {:?}\n    now:      {:?}\n    what was approved: {}",
        enum_.file, enum_.name, axis.name, r.expected, actual, r.reason,
      )),
      (None, None) => failures.push(format!(
        "{}::{} — no merge-base entry for its {} and not recorded as new\n    now: {:?}",
        enum_.file, enum_.name, axis.name, actual,
      )),
    }
  }

  failures.extend(
    axis
      .baseline
      .iter()
      .filter(|entry| !seen_baseline.contains(&(entry.0, entry.1)))
      .map(|entry| {
        format!(
          "{}::{} — a merge-base {} entry matching no public enum this scan still finds \
           (renamed, removed, or the scan stopped finding them)",
          entry.0, entry.1, axis.name,
        )
      }),
  );

  failures.extend(
    axis
      .recorded
      .iter()
      .filter(|entry| !seen_recorded.contains(&(entry.file, entry.name)))
      .map(|entry| {
        format!(
          "{}::{} — a recorded {} difference whose enum this scan no longer finds. Delete the \
           line, the difference it named is gone",
          entry.file, entry.name, axis.name,
        )
      }),
  );
}

/// The gate: on **both** axes, every public enum found in [`FILES`] is either unchanged since the
/// merge base or its difference is named. Plant a new `#[non_exhaustive]` (or any other attribute
/// change) on a tracked enum with no matching entry, and this test names it and fails; plant a
/// payload change on any variant of one, and it does the same.
#[test]
fn public_enum_attributes_and_payloads_match_the_merge_base_or_are_named() {
  let mut found = Vec::new();
  for file in FILES {
    found.extend(find_public_enums(file));
  }

  // Non-vacuity, and it takes two counts because the two axes fail vacuously in different ways.
  // A scan that stops finding enums has nothing left to check; a scan that finds them and reads
  // no variants out of them is accounted for on the attribute axis and silent on the payload one.
  //
  // 16 enums = 12 in error.rs + 2 in ast/value.rs + 2 in ast/materialized.rs. 139 variants =
  // 105 + 17 + 17 across the same three files. Update either count only after checking why it
  // moved.
  //
  // It moved once, and this is the reading: `refactor/materialised-generic` deleted
  // ast/materialized32.rs, whose two enums were ast/materialized.rs's with `IntValue` resolving
  // to a different width. 18 -> 16 and 156 -> 139 is that file leaving, and nothing else.
  assert_eq!(
    found.len(),
    16,
    "the scan found {} public enum(s) across {} files, expected 16 — a file gained or lost one, \
     or the line-matching in `find_public_enums` stopped seeing the surface",
    found.len(),
    FILES.len(),
  );
  let variants_read: usize = found.iter().map(|enum_| enum_.variants.len()).sum();
  assert_eq!(
    variants_read, 139,
    "the scan read {variants_read} variant(s) out of 16 enums, expected 139 — an enum gained or \
     lost one, or `read_variants` stopped seeing them",
  );

  let mut failures = Vec::new();
  for axis in AXES {
    audit(axis, &found, &mut failures);
  }

  assert!(
    failures.is_empty(),
    "{} public enum contract(s) not accounted for:\n{}",
    failures.len(),
    failures.join("\n"),
  );
}

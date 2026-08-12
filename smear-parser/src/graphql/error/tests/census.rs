//! The `ErrorData` producibility census.

use smear_lexer::graphql::{error::LexerErrors, syntactic::SyntacticTokenKind};
use tokora::SimpleSpan as Span;

use crate::graphql::error::{
  EnumTypeExtensionHint, ErrorData, Expectation, GraphqlError, GraphqlErrors,
  InputObjectTypeExtensionHint, InterfaceTypeExtensionHint, ObjectFieldValueHint,
  ObjectTypeExtensionHint, SchemaExtensionHint, UnionTypeExtensionHint, VariableValueHint,
};

/// The error this census is written over: `S = &str`, with the token, char and expectation
/// parameters the dialect pins.
type Data = ErrorData<&'static str, SyntacticTokenKind>;

/// Census: every [`ErrorData`] variant **this configuration declares** must be producible
/// without naming the variant.
///
/// Three things make it non-vacuous, and they are different things.
///
/// * **One variant list drives both the exhaustive `match` and the assertions.** A variant added
///   without an entry is an `E0004` in the generated `tag`, and an entry cannot be written
///   without a sample expression beside it — so there is no state in which a variant is declared,
///   listed and unproven. This is `smear-lexer`'s `census!` shape, chosen for exactly that
///   property; a version of this test with a hand-written match and a separate sample list let a
///   variant slip through with a match arm and no sample, and a hard-coded variant count did not
///   notice.
/// * **`ErrorData` is `#[non_exhaustive]`**, which binds other crates and not this one, so the
///   exhaustiveness check still runs here.
/// * **Every sample is built through a public constructor or a `From` conversion**, never by
///   writing `ErrorData::Variant(…)`. That is the rule with teeth: `IntOverflow` and
///   `FloatOverflow` sat in this enum with no constructor and no construction site anywhere in
///   the workspace, and a census whose samples could name the variant directly would have
///   reported them green. Same defect as tokora's `FinishError::InvalidDialectKind` and
///   `smear-lexer`'s `LosslessTokenKind::Boolean` (28 declared, 27 producible).
///
/// **Per configuration, not over the union of them.** The two conversion variants exist only
/// under `materialized-numbers`, so the variant, its constructor and its entry here carry the
/// same `#[cfg]`, and each build checks exactly the enum it compiled. A census written over the
/// union — the shape `ci/source_census`'s D1–D4 necessarily has, since it reads the source as
/// text with `syn` and strips only `#[cfg(test)]` — cannot tell "no configuration can produce
/// this" from "this configuration cannot", and would answer the first question when asked the
/// second.
///
/// The two conversion variants are additionally produced **by a real parse** in
/// `graphql::syntactic::value::materialized::tests`. A constructor existing is the weaker claim;
/// both are worth making.
#[test]
fn error_data_variant_census() {
  macro_rules! census {
    ($($(#[$attr:meta])* $variant:ident => $sample:expr),+ $(,)?) => {
      // Wildcard-free and generated from the same list as the samples below.
      fn tag(data: &Data) -> &'static str {
        match data {
          $($(#[$attr])* ErrorData::$variant { .. } => stringify!($variant),)+
        }
      }

      $(
        $(#[$attr])*
        {
          let sample: Data = $sample;
          assert_eq!(
            tag(&sample),
            stringify!($variant),
            "the sample recorded for {} produced a different variant",
            stringify!($variant),
          );
        }
      )+
    };
  }

  let span = Span::new(0, 1);

  census! {
    Lexer => GraphqlError::from_lexer_errors(LexerErrors::default(), span).into_data(),

    #[cfg(feature = "materialized-numbers")]
    IntOverflow =>
      GraphqlError::<&str>::int_overflow("99999999999999999999999999", span).into_data(),

    #[cfg(feature = "materialized-numbers")]
    FloatOverflow => GraphqlError::<&str>::float_overflow("1e400", span).into_data(),

    InvalidEnumValue => GraphqlError::<&str>::invalid_enum_value("x", span).into_data(),

    InvalidBooleanValue => GraphqlError::<&str>::invalid_boolean_value("x", span).into_data(),

    InvalidNullValue => GraphqlError::<&str>::invalid_null_value("x", span).into_data(),

    InvalidFragmentName => GraphqlError::<&str>::invalid_fragment_name("on", span).into_data(),

    Unclosed => GraphqlError::<&str>::unclosed_list(span).into_data(),

    UnexpectedToken =>
      GraphqlError::<&str>::unexpected_token(SyntacticTokenKind::Colon, Expectation::Name, span)
        .into_data(),

    UnexpectedKeyword =>
      GraphqlError::<&str>::unexpected_keyword("quary", "query", span).into_data(),

    UnexpectedEndOfVariableValue =>
      GraphqlError::<&str>::unexpected_end_of_variable_value(VariableValueHint::Name, span)
        .into_data(),

    UnexpectedEndOfObjectFieldValue =>
      GraphqlError::<&str>::unexpected_end_of_object_field_value(
        ObjectFieldValueHint::Name,
        span,
      )
      .into_data(),

    UnknownDirectiveLocation =>
      GraphqlError::<&str>::unknown_directive_location("NOWHERE", span).into_data(),

    UnknownOperationType =>
      GraphqlError::<&str>::unknown_operation_type("quary", span).into_data(),

    UnexpectedEndOfObjectExtension =>
      GraphqlError::<&str>::unexpected_end_of_object_extension(
        span,
        ObjectTypeExtensionHint::Name,
      )
      .into_data(),

    UnexpectedEndOfInterfaceExtension =>
      GraphqlError::<&str>::unexpected_end_of_interface_extension(
        span,
        InterfaceTypeExtensionHint::Name,
      )
      .into_data(),

    UnexpectedEndOfEnumExtension =>
      GraphqlError::<&str>::unexpected_end_of_enum_extension(span, EnumTypeExtensionHint::Name)
        .into_data(),

    UnexpectedEndOfInputObjectExtension =>
      GraphqlError::<&str>::unexpected_end_of_input_object_extension(
        span,
        InputObjectTypeExtensionHint::Name,
      )
      .into_data(),

    UnexpectedEndOfUnionExtension =>
      GraphqlError::<&str>::unexpected_end_of_union_extension(
        span,
        UnionTypeExtensionHint::Name,
      )
      .into_data(),

    UnexpectedEndOfSchemaExtension =>
      GraphqlError::<&str>::unexpected_end_of_schema_extension(
        span,
        SchemaExtensionHint::Schema,
      )
      .into_data(),

    EndOfInput => GraphqlError::<&str>::unexpected_end_of_input(span).into_data(),

    // Not a constructor: the only producers of `Other` are the six `From` conversions in
    // `error.rs`, and this is the cheapest of them to mint.
    Other => GraphqlErrors::<&str>::from(LexerErrors::<char, ()>::default())[0]
      .clone()
      .into_data(),
  }
}

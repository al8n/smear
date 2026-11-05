//! What a message says was expected: every [`Expectation`], rendered as the object of
//! "expected …".

use std::string::ToString;

use crate::graphql::error::Expectation;

/// Every expectation renders as the object of "expected …": punctuation and keywords in backticks,
/// every other class as a noun phrase, a production as the phrase it carries, and `Unspecified` as
/// nothing at all, which is what drops a message's "expected" clause. The table is copied from the
/// impl row for row, so rewording an expectation is a change this test has to be shown.
#[test]
fn every_expectation_renders_as_the_object_of_expected() {
  for (expectation, text) in [
    (Expectation::InlineString, "a string"),
    (Expectation::BlockString, "a block string"),
    (Expectation::Dollar, "`$`"),
    (Expectation::LParen, "`(`"),
    (Expectation::RParen, "`)`"),
    (Expectation::Spread, "`...`"),
    (Expectation::Colon, "`:`"),
    (Expectation::Equal, "`=`"),
    (Expectation::At, "`@`"),
    (Expectation::LBracket, "`[`"),
    (Expectation::RBracket, "`]`"),
    (Expectation::LBrace, "`{`"),
    (Expectation::RBrace, "`}`"),
    (Expectation::Pipe, "`|`"),
    (Expectation::Bang, "`!`"),
    (Expectation::Ampersand, "`&`"),
    (Expectation::ConstInputValue, "a constant value"),
    (Expectation::InputValue, "a value"),
    (Expectation::Type, "a type"),
    (Expectation::Selection, "a selection"),
    (Expectation::VariableDefinition, "a variable definition"),
    (Expectation::OperationType, "an operation type"),
    (
      Expectation::ExecutableDefinition,
      "an executable definition",
    ),
    (
      Expectation::OperationTypeOrFragment,
      "an operation type or `fragment`",
    ),
    (Expectation::FragmentName, "a fragment name"),
    (Expectation::Name, "a name"),
    (Expectation::OperationName, "an operation name"),
    (Expectation::DirectiveLocation, "a directive location"),
    (
      Expectation::FragmentSpreadOrInlineFragment,
      "a fragment spread or an inline fragment",
    ),
    (Expectation::IntValue, "an integer"),
    (Expectation::BooleanValue, "a boolean"),
    (Expectation::FloatValue, "a float"),
    (Expectation::NullValue, "`null`"),
    (Expectation::EnumValue, "an enum value"),
    (Expectation::StringValue, "a string"),
    (Expectation::Keyword("on"), "`on`"),
    (
      Expectation::Production("a type-system definition"),
      "a type-system definition",
    ),
    (Expectation::Unspecified, ""),
  ] {
    assert_eq!(expectation.to_string(), text, "{expectation:?}");
  }
}

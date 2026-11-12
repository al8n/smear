//! What a message says was expected: every [`Expectation`], rendered as the object of
//! "expected …".

use std::string::ToString;

use crate::graphqlx::error::Expectation;

/// Every expectation renders as the object of "expected …", as GraphQL's twin pins it, over this
/// dialect's own vocabulary: the angle pair, `*`, `+`, `-`, `::`, the three root sigils and `=>`,
/// paths, variables, and a use tree's keywords and parts. The table is copied from the impl row
/// for row.
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
    (Expectation::LAngle, "`<`"),
    (Expectation::RAngle, "`>`"),
    (Expectation::LBracket, "`[`"),
    (Expectation::RBracket, "`]`"),
    (Expectation::LBrace, "`{`"),
    (Expectation::RBrace, "`}`"),
    (Expectation::Asterisk, "`*`"),
    (Expectation::Plus, "`+`"),
    (Expectation::Minus, "`-`"),
    (Expectation::PathSeparator, "`::`"),
    (Expectation::Tilde, "`~`"),
    (Expectation::DotDot, "`..`"),
    (Expectation::Caret, "`^`"),
    (Expectation::FatArrow, "`=>`"),
    (Expectation::Pipe, "`|`"),
    (Expectation::Bang, "`!`"),
    (Expectation::Ampersand, "`&`"),
    (Expectation::Name, "a name"),
    (Expectation::Path, "a path"),
    (Expectation::IntValue, "an integer"),
    (Expectation::FloatValue, "a float"),
    (Expectation::StringValue, "a string"),
    (Expectation::BooleanValue, "a boolean"),
    (Expectation::NullValue, "`null`"),
    (Expectation::EnumValue, "an enum value"),
    (Expectation::VariableValue, "a variable"),
    (Expectation::Type, "a type"),
    (Expectation::InputValue, "a value"),
    (Expectation::ConstInputValue, "a constant value"),
    (Expectation::UseTree, "a use tree"),
    (Expectation::NameGlobOrGroup, "a name, `*` or `{`"),
    (Expectation::Use, "`use`"),
    (Expectation::As, "`as`"),
    (
      Expectation::OperationTypeOrFragment,
      "an operation type or `fragment`",
    ),
    (Expectation::FragmentName, "a fragment name"),
    (Expectation::Keyword("extend"), "`extend`"),
    (
      Expectation::Production("a type extension"),
      "a type extension",
    ),
    (Expectation::Unspecified, ""),
  ] {
    assert_eq!(expectation.to_string(), text, "{expectation:?}");
  }
}

/// The abstract syntax tree (AST) definitions and parsers for GraphQLx language.
pub mod ast;

/// The concrete syntax tree (CST) definitions and parsers for GraphQLx language.
pub mod cst;

mod error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Expectation {
  /// An inline string was expected.
  InlineString,
  /// A block string was expected.
  BlockString,
  /// A `$` was expected.
  Dollar,
  /// A `(` was expected.
  LParen,
  /// A `)` was expected.
  RParen,
  /// A `...` was expected.
  Spread,
  /// A `:` was expected.
  Colon,
  /// A `=` was expected.
  Equal,
  /// A `@` was expected.
  At,
  /// A `<` was expected.
  LAngle,
  /// A `>` was expected.
  RAngle,
  /// A `[` was expected.
  LBracket,
  /// A `]` was expected.
  RBracket,
  /// A `{` was expected.
  LBrace,
  /// A `}` was expected.
  RBrace,
  /// An `*` was expected.
  Asterisk,
  /// A `+` was expected.
  Plus,
  /// A `-` was expected.
  Minus,
  /// A path separator (`::`) was expected.
  PathSeparator,
  /// A fat arrow (`=>`) was expected.
  FatArrow,
  /// A `|` was expected.
  Pipe,
  /// A `!` was expected.
  Bang,
  /// A `&` was expected.
  Ampersand,

  /// Const input value was expected.
  ConstInputValue,
  /// Input value was expected.
  InputValue,
  /// Fragment name was expected.
  FragmentName,
  /// An identifier was expected.
  Identifier,
  /// An operation name was expected.
  OperationName,
  /// An directive location was expected.
  DirectiveLocation,
  /// Either a fragment spread or an inline fragment was expected.
  FragmentSpreadOrInlineFragment,
  /// A number was expected.
  IntValue,
  /// A boolean was expected.
  BooleanValue,
  /// A float was expected.
  FloatValue,
  /// A null value was expected.
  NullValue,
  /// An enum value was expected.
  EnumValue,
  /// A string value was expected.
  StringValue,
  /// A keyword was expected.
  Keyword(&'static str),
}

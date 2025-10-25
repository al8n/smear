/// Errors for GraphQL parsers
pub mod error;

pub use impls::*;

mod impls;

/// The abstract syntax tree (AST) definitions and parsers for GraphQL language.
pub mod ast;

/// The concrete syntax tree (CST) definitions and parsers for GraphQL language.
pub mod cst;

/// Expectations for the GraphQL parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
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
  /// A `[` was expected.
  LBracket,
  /// A `]` was expected.
  RBracket,
  /// A `{` was expected.
  LBrace,
  /// A `}` was expected.
  RBrace,
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
  /// A name was expected.
  Name,
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
  /// One or more keywords were expected.
  Keyword(&'static [&'static str]),
  /// A type definition was expected.
  TypeDefinition,
  /// A type extension was expected.
  TypeExtension,
  /// A type system definition was expected.
  TypeSystemDefinition,
  /// A type system extension was expected.
  TypeSystemExtension,
  /// A type system definition or extension was expected.
  TypeSystemDefinitionOrExtension,
  /// An executable definition was expected.
  ExecutableDefinition,
  /// A definition was expected.
  Definition,
  /// A definition or extension was expected.
  DefinitionOrExtension,
}

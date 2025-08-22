use chumsky::{container::Container, extra::ParserExtra, prelude::*};
use either::Either;

use super::{
  super::{
    convert::*,
    source::{Char, Slice, Source},
  },
  ignored,
  punct::{Colon, Equal, LAngle},
};

/// Groto input value and const input value parsers
pub mod groto;

pub use boolean::*;
pub use enum_::*;
pub use float::*;
pub use int::*;
pub use list::*;
pub use map::*;
pub use null::*;
pub use object::*;
pub use set::*;
pub use string::*;
pub use uint::*;
pub use variable::*;

mod boolean;
mod enum_;
mod float;
mod int;
mod list;
mod map;
mod null;
mod object;
mod set;
mod string;
mod uint;
mod variable;

/// Marker trait for GraphQL input value types.
///
/// This trait identifies types that represent valid GraphQL input values and
/// indicates whether they are constant (compile-time evaluable) or variable
/// (runtime-dependent). This distinction is crucial for GraphQL's type system
/// and execution model.
///
/// ## GraphQL Input Value Hierarchy
///
/// GraphQL defines a specific set of types that can be used as input values:
/// - **Scalars**: `Int`, `Float`, `String`, `Boolean`, `ID`
/// - **Enums**: User-defined enumeration values
/// - **Input Objects**: Structured input with named fields
/// - **Lists**: Ordered collections of input values
/// - **Null**: The absence of a value
/// - **Variables**: Runtime-provided values (only in non-constant contexts)
///
/// ## Constant vs Variable Values
///
/// GraphQL distinguishes between two categories of input values:
///
/// ### Constant Values (`CONST = true`)
/// Values that can be determined at query parse time and don't depend on
/// variables or runtime context:
/// - **Literals**: `42`, `"hello"`, `true`, `null`, `ENUM_VALUE`
/// - **Literal objects**: `{ name: "John", age: 30 }`
/// - **Literal lists**: `[1, 2, 3]`, `["a", "b"]`
///
/// ### Variable Values (`CONST = false`)
/// Values that depend on runtime-provided variables:
/// - **Variables**: `$userId`, `$filter`, `$limit`
/// - **Objects with variables**: `{ id: $userId, name: "John" }`
/// - **Lists with variables**: `[$id1, $id2]`, `[1, $dynamicValue]`
///
/// ## Usage Contexts
///
/// Different GraphQL contexts have different requirements for input values:
///
/// ### Constant-Only Contexts (require `CONST = true`)
/// - **Default values**: `field(arg: Type = defaultValue)`
/// - **Directive arguments**: `@deprecated(reason: "Use newField instead")`
/// - **Schema definitions**: Input object field defaults
///
/// ### Variable-Allowed Contexts (accept `CONST = false`)
/// - **Query arguments**: `user(id: $userId)`
/// - **Mutation inputs**: `createUser(input: $userInput)`
/// - **Fragment arguments**: Field arguments in queries
///
/// ## Type-Level Validation
///
/// This trait enables compile-time validation of GraphQL context requirements:
///
/// ## Schema Validation
///
/// The `CONST` flag is used during schema validation to ensure:
/// - Default values contain only constant expressions
/// - Directive arguments use only constant values
/// - Variable usage is appropriate for the context
///
/// ## Execution Implications
///
/// - **Constant values**: Can be pre-evaluated and cached
/// - **Variable values**: Must be evaluated for each execution
/// - **Mixed structures**: Require partial evaluation strategies
///
/// ## Error Prevention
///
/// This trait helps prevent common GraphQL errors:
/// - Using variables in constant-only contexts
/// - Attempting to serialize variable values as literals
/// - Invalid default value specifications
/// - Incorrect directive argument types
///
/// Spec: [Input Values](https://spec.graphql.org/draft/#sec-Input-Values)
pub trait InputValue<const CONST: bool> {}

/// A GraphQL default value assignment for input parameters.
///
/// Represents the default value assignment syntax used in GraphQL variable
/// declarations, field arguments, and input type definitions. Default values
/// provide fallback values when no explicit value is provided, following
/// GraphQL's default value semantics and constant expression requirements.
///
/// ## Specification Rules
///
/// GraphQL default values follow strict formatting and semantic rules:
/// - **Equals syntax**: Must use `=` to assign the default value
/// - **Constant requirement**: Default values must be constant expressions (no variables)
/// - **Type compatibility**: Default value type must match the declared type
/// - **Nullability handling**: Non-null types can have null defaults (making them effectively nullable)
/// - **Whitespace flexibility**: Optional whitespace around the `=` token
///
/// ## Grammar
///
/// ```text
/// DefaultValue ::= '=' Value
/// ```
#[derive(Debug, Clone, Copy)]
pub struct DefaultInputValue<Value, Span> {
  span: Span,
  eq: Equal<Span>,
  value: Value,
}

impl<Value, Span> DefaultInputValue<Value, Span> {
  /// Returns the source span of the entire default value assignment.
  ///
  /// This span covers from the `=` token through the last character of the
  /// default value, providing the complete source location for error reporting
  /// and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the equals assignment token.
  ///
  /// This provides access to the `=` character that introduces the default
  /// value assignment, including its exact source position. Useful for syntax
  /// highlighting and precise error reporting.
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    &self.eq
  }

  /// Returns the default value expression.
  ///
  /// This provides access to the constant expression that serves as the
  /// default value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for default value assignments with constant validation.
  ///
  /// This parser handles the complete default value syntax including the equals
  /// token, optional whitespace, and the default value expression. It enforces
  /// GraphQL's requirement that default values must be constant expressions
  /// through compile-time type constraints.
  ///
  ///
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::Span<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone,
    Value: InputValue<true>,
  {
    Equal::parser()
      .then_ignore(ignored::ignored())
      .then(value)
      .map_with(|(eq, value), sp| Self {
        span: Span::from_map_extra(sp),
        eq,
        value,
      })
  }
}

impl<Value, Span> AsRef<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span> IntoSpan<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span> IntoComponents for DefaultInputValue<Value, Span> {
  type Components = (Span, Equal<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.eq, self.value)
  }
}

/// Returns a parser which can parse either a set or a map.
pub fn map_or_set_parser<'src, I, E, Key, Value, Item, KP, VP, IP, CS, CM>(
  key_parser: impl Fn() -> KP,
  value_parser: impl Fn() -> VP,
  item_parser: impl Fn() -> IP,
) -> impl Parser<'src, I, Either<Set<Item, I::Span, CS>, Map<Key, Value, I::Span, CM>>, E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  I::Span: crate::source::Span<'src, I, E>,
  E: ParserExtra<'src, I>,
  KP: Parser<'src, I, Key, E> + Clone,
  VP: Parser<'src, I, Value, E> + Clone,
  IP: Parser<'src, I, Item, E> + Clone,
  CM: Container<MapEntry<Key, Value, I::Span>>,
  CS: Container<Item>,
{
  // Non-consuming guard:
  // after '<' and ws, either a ':' (i.e. "<:>") or "value ':'" => it's a Map.
  let map_guard = LAngle::<I::Span>::parser()
    .ignore_then(ignored())
    .ignore_then(choice((
      Colon::<I::Span>::parser().to(()), // "<:>"
      key_parser()
        .then_ignore(ignored())
        .then_ignore(Colon::<I::Span>::parser())
        .to(()), // "< value :"
    )))
    .rewind();

  map_guard
    .ignore_then(Map::<Key, Value, _, CM>::parser_with(
      key_parser(),
      value_parser(),
    ))
    .map(Either::Right)
    .or(Set::<Item, _, CS>::parser_with(item_parser()).map(Either::Left))
}

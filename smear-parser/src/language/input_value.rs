use chumsky::{extra::ParserExtra, prelude::*};

use super::{
  super::{char::Char, language::punct::Equal, source::Source, spanned::Spanned, convert::*},
  ignored,
};

/// Groto input value and const input value parsers
pub mod groto;

pub use angle::*;
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

mod angle;
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

/// Default input value
#[derive(Debug, Clone)]
pub struct DefaultInputValue<Value, Span> {
  span: Span,
  eq: Equal<Span>,
  value: Value,
}

impl<Value, Span> DefaultInputValue<Value, Span> {
  /// Returns the span of the default input value
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the equal token
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    &self.eq
  }

  /// Returns a reference to the value of the default input value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns a parser of default input value.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    E: ParserExtra<'src, I>,
    Span: Spanned<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone,
    Value: InputValue<true>,
  {
    Equal::parser()
      .then_ignore(ignored::ignored())
      .then(value)
      .map_with(|(eq, value), sp| Self {
        span: Spanned::from_map_extra(sp),
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

impl<Value, Span> IntoSpanned<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn into_spanned(self) -> Span {
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

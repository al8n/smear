use derive_more::{Display, IsVariant};

pub use smear_lexer::hints::*;
pub use smear_scaffold::hints::*;

/// Hints for parsing a variable value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, IsVariant, Display)]
#[non_exhaustive]
pub enum VariableValueHint {
  /// A [`Name`](crate::parser::ast::Name) was expected.
  #[display("name")]
  Name,
  /// A [`Dollar`](crate::parser::ast::Dollar) was expected.
  #[display("dollar")]
  Dollar,
}

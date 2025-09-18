use logosky::utils::Span;
use smear_parser::{definitions, lang};

use crate::{lexer::token::fast::{Token, TokenKind}, parser::ast::StringValue};

use super::{FastTokenErrors, FastTokenStream};

pub use argument::*;
pub use directives::*;
pub use field::*;
pub use fragment::*;
pub use list::*;
pub use object::*;
pub use selection_set::*;
pub use value::*;

mod argument;
mod boolean_value;
mod directives;
mod enum_value;
mod field;
mod float;
mod fragment;
mod int;
mod keyword;
mod list;
mod name;
mod null_value;
mod object;
mod punctuator;
mod selection_set;
mod string;
mod value;
mod variable;

pub type ArgumentsDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>, InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>> = definitions::v2::ArgumentsDefinition<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>, InputValueContainer>;

pub type InputValueDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>> = definitions::v2::InputValueDefinition<
  StringValue<S>,
  lang::Name<S>,
  definitions::v2::RcType<lang::Name<S>>,
  DefaultInputValue<S>,
  Directives<S, ArgumentContainer, DirectiveContainer>
>;

pub type InputFieldsDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>, InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>> = definitions::v2::InputFieldsDefinition<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>, InputValueContainer>;

pub type FieldDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>, InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>> = definitions::v2::FieldDefinition<
  StringValue<S>,
  lang::Name<S>,
  ArgumentsDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>,
  definitions::v2::RcType<lang::Name<S>>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

pub type FieldsDefinition<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>, InputValueContainer = Vec<InputValueDefinition<S, ArgumentContainer, DirectiveContainer>>, FieldContainer = Vec<FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>>> = definitions::v2::FieldsDefinition<FieldDefinition<S, ArgumentContainer, DirectiveContainer, InputValueContainer>, FieldContainer>;

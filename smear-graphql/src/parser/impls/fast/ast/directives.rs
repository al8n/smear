use crate::parser::{ast::Name, fast::{Argument, Arguments, ConstArgument, ConstArguments}};
use smear_parser::lang::v2;

pub type Directive<S, ArgumentsContainer = Vec<Argument<S>>> = v2::Directive<Name<S>, Arguments<S, ArgumentsContainer>>;
pub type Directives<S, ArgumentsContainer = Vec<Argument<S>>, Container = Vec<Directive<S, ArgumentsContainer>>> = v2::Directives<Directive<S, ArgumentsContainer>, Container>;

pub type ConstDirective<S, ArgumentsContainer = Vec<ConstArgument<S>>> = v2::Directive<Name<S>, ConstArguments<S, ArgumentsContainer>>;
pub type ConstDirectives<S, ArgumentsContainer = Vec<ConstArgument<S>>, Container = Vec<ConstDirective<S, ArgumentsContainer>>> = v2::Directives<ConstDirective<S, ArgumentsContainer>, Container>;

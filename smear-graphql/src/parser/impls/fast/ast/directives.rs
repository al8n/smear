use crate::parser::{ast::{self, Name}, fast::{Arguments, ConstArguments}};

pub type Directive<S> = ast::Directive<Name<S>, Arguments<S>>;
pub type Directives<S, Container = Vec<Directive<S>>> = ast::Directives<Directive<S>, Container>;

pub type ConstDirective<S> = ast::Directive<Name<S>, ConstArguments<S>>;
pub type ConstDirectives<S, Container = Vec<ConstDirective<S>>> = ast::Directives<ConstDirective<S>, Container>;

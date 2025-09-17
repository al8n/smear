use crate::parser::ast::Name;

pub type Directive<Args, S> = smear_parser::lang::v2::Directive<Name<S>, Args>;
pub type Directives<Arg, Container = Vec<Arg>> = smear_parser::lang::v2::Directives<Arg, Container>;

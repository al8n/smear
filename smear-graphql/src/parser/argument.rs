use crate::parser::ast::Name;


pub type Argument<V, S> = smear_parser::lang::v2::Argument<Name<S>, V>;
pub type Arguments<Arg, Container = Vec<Arg>> = smear_parser::lang::v2::Arguments<Arg, Container>;

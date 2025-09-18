use smear_parser::lang::{v2::{FragmentName, Name}};
use super::{Argument, Arguments, Directive, Directives};

use crate::parser::{fast::{Alias, TypeCondition}, ast};

pub type Selection<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>> = ast::Selection<Alias<S>, Name<S>, FragmentName<S>, TypeCondition<S>, Arguments<S, ArgumentContainer>, Directives<S, ArgumentContainer, DirectiveContainer>>;

pub type SelectionSet<S, ArgumentContainer = Vec<Argument<S>>, DirectiveContainer = Vec<Directive<S, ArgumentContainer>>> = ast::SelectionSet<Alias<S>, Name<S>, FragmentName<S>, TypeCondition<S>, Arguments<S, ArgumentContainer>, Directives<S, ArgumentContainer, DirectiveContainer>>;

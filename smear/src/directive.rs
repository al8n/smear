use apollo_parser::ast::Directive;

use crate::{error::DirectiveError, Diagnosticable, NamedDiagnosticable};

pub trait DiagnosticableDirective:
  Diagnosticable<Node = Directive, Error = DirectiveError> + NamedDiagnosticable
{
  fn avaliable_argument_names() -> &'static [&'static str];
}

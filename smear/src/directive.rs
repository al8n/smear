use apollo_parser::ast::Directive;

use crate::{Diagnosticable, NamedDiagnosticable, error::DirectiveError};


pub trait DiagnosticableDirective: Diagnosticable<Node = Directive, Error = DirectiveError> + NamedDiagnosticable {
  fn avaliable_argument_names() -> &'static [&'static str];
}


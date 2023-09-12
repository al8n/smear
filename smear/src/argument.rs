use apollo_parser::ast::Argument;

use crate::{error::ArgumentError, Diagnosticable, NamedDiagnosticable};

pub trait DiagnosticableArgument:
  Diagnosticable<Node = Argument, Error = ArgumentError> + NamedDiagnosticable
{
}

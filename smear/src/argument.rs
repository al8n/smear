use apollo_parser::ast::Argument;

use crate::{NamedDiagnosticable, Diagnosticable, error::ArgumentError};

pub trait DiagnosticableArgument: Diagnosticable<Node = Argument, Error = ArgumentError> + NamedDiagnosticable {}

use apollo_parser::ast::Directive;

use crate::{error::DirectiveError, Diagnosticable, NamedDiagnosticable, value::ValueDescriptor};

mod location;
pub use location::*;

pub trait DiagnosticableDirective:
  Diagnosticable<Node = Directive, Error = DirectiveError> + NamedDiagnosticable
{
  fn avaliable_argument_names() -> &'static [&'static str];
}

#[viewit::viewit(setters(skip))]
pub struct ArgumentDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  optional: bool,
  value_descriptor: &'static ValueDescriptor,
}

#[viewit::viewit(setters(skip))]
pub struct DirectiveDescriptor { 
  locations: &'static [DirectiveLocation],
  available_arguments: &'static [&'static ArgumentDescriptor],
  required_arguments: &'static [&'static ArgumentDescriptor],
  optional_arguments: &'static [&'static ArgumentDescriptor],
}

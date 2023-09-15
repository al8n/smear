use crate::value::ValueDescriptor;

mod location;
pub use location::*;

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Copy, Clone)]
pub struct ArgumentDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  value_descriptor: &'static ValueDescriptor,
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Copy, Clone)]
pub struct ArgumentsDescriptor {
  available_arguments: &'static [ArgumentDescriptor],
  required_arguments: &'static [ArgumentDescriptor],
  optional_arguments: &'static [ArgumentDescriptor],
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Copy, Clone)]
pub struct DirectiveDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  available_names: &'static [&'static str],
  arguments: ArgumentsDescriptor,
  locations: &'static [DirectiveLocation],
}

impl DirectiveDescriptor {
  pub fn contains_name(&self, name: &'static str) -> bool {
    self.available_names.contains(&name)
  }
}

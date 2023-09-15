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
  available_arguments: &'static [&'static ArgumentDescriptor],
  required_arguments: &'static [&'static ArgumentDescriptor],
  optional_arguments: &'static [&'static ArgumentDescriptor],
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Copy, Clone)]
pub struct DirectiveDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  arguments: &'static ArgumentsDescriptor,
  locations: &'static [DirectiveLocation],
}

impl DirectiveDescriptor {
  pub fn contains_name(&self, name: &'static str) -> bool {
    self.name == name
      || self.aliases.contains(&name)
      || self
        .short
        .map(|ch| name.len() == 1 && name.chars().next().unwrap() == ch)
        .unwrap_or(false)
  }
}

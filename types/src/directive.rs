use apollo_encoder::{DirectiveDefinition, InputValueDefinition, Type_};
use tabled::{builder::Builder, settings::Style};

use crate::{value::ValueDescriptor, Deprecated};

mod location;
pub use location::*;

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Debug, Copy, Clone)]
pub struct ArgumentDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  available_names: &'static [&'static str],
  value_descriptor: &'static ValueDescriptor,
  // default: Option<>,
  description: Option<&'static str>,
  deprecated: Option<Deprecated>,
}

impl ArgumentDescriptor {
  pub fn contains_name(&self, name: &'static str) -> bool {
    self.aliases.contains(&name)
  }
}

impl From<&ArgumentDescriptor> for InputValueDefinition {
  fn from(descriptor: &ArgumentDescriptor) -> Self {
    let mut def = InputValueDefinition::new(
      descriptor.name.to_string(),
      descriptor.value_descriptor.into(),
    );
    def.description(descriptor.to_string());
    def
  }
}

impl core::fmt::Display for ArgumentDescriptor {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    const FIELDS: usize = 6;
    let mut headers = Vec::with_capacity(FIELDS);
    let mut record = Vec::with_capacity(FIELDS);

    headers.push("name");
    record.push(self.name.to_string());

    let val: Type_ = self.value_descriptor.into();
    headers.push("type");
    record.push(val.to_string());

    if let Some(d) = self.deprecated {
      headers.push("deprecated");
      record.push(d.to_string());
    }

    if let Some(short) = self.short {
      headers.push("short");
      record.push(short.to_string());
    }

    if !self.aliases.is_empty() {
      headers.push("aliases");
      record.push(self.aliases.join(", "));
    }

    if let Some(des) = self.description {
      headers.push("description");
      record.push(des.to_string());
    }

    let mut builder = Builder::default();
    builder.set_header(headers);
    builder.push_record(record);
    let mut table = builder.build();
    table.with(Style::markdown());
    write!(f, "{}", table)
  }
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
  #[viewit(getter(style = "ref"))]
  arguments: ArgumentsDescriptor,
  locations: &'static [DirectiveLocation],
  description: Option<&'static str>,
  deprecated: Option<Deprecated>,
}

impl DirectiveDescriptor {
  pub fn contains_name(&self, name: &'static str) -> bool {
    self.available_names.contains(&name)
  }
}

impl core::fmt::Display for DirectiveDescriptor {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    const FIELDS: usize = 8;
    let mut headers = Vec::with_capacity(FIELDS);
    let mut record = Vec::with_capacity(FIELDS);

    headers.push("name");
    record.push(self.name.to_string());

    headers.push("locations");
    record.push(
      self
        .locations
        .iter()
        .map(|l| l.as_str())
        .collect::<Vec<_>>()
        .join(", "),
    );

    if let Some(d) = self.deprecated {
      headers.push("deprecated");
      record.push(d.to_string());
    }

    if let Some(short) = self.short {
      headers.push("short");
      record.push(short.to_string());
    }

    if !self.aliases.is_empty() {
      headers.push("aliases");
      record.push(self.aliases.join(", "));
    }

    if !self.arguments.required_arguments.is_empty() {
      headers.push("required_arguments");
      record.push(
        self
          .arguments
          .required_arguments
          .iter()
          .map(|a| a.name)
          .collect::<Vec<_>>()
          .join(", "),
      );
    }

    if !self.arguments.optional_arguments.is_empty() {
      headers.push("optional_arguments");
      record.push(
        self
          .arguments
          .optional_arguments
          .iter()
          .map(|a| a.name)
          .collect::<Vec<_>>()
          .join(", "),
      );
    }

    if let Some(des) = self.description {
      headers.push("description");
      record.push(des.to_string());
    }

    let mut builder = Builder::default();
    builder.set_header(headers);
    builder.push_record(record);
    let mut table = builder.build();
    table.with(Style::markdown());
    write!(f, "{}", table)
  }
}

impl From<&DirectiveDescriptor> for DirectiveDefinition {
  fn from(descriptor: &DirectiveDescriptor) -> Self {
    let mut arguments: Vec<InputValueDefinition> =
      Vec::with_capacity(descriptor.arguments.available_arguments.len());

    for arg in descriptor.arguments.required_arguments {
      arguments.push(arg.into());
    }

    for arg in descriptor.arguments.optional_arguments {
      arguments.push(arg.into());
    }

    let mut def = DirectiveDefinition::new(descriptor.name.to_string());
    for arg in arguments.iter() {
      def.arg(arg.clone());
    }

    def.description(descriptor.to_string());

    for loc in descriptor.locations.iter().map(|l| l.as_str().to_string()) {
      def.location(loc);
    }
    def
  }
}

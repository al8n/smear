use std::str::FromStr;

use apollo_parser::ast::Directive;

use crate::{error::DirectiveError, Diagnosticable, NamedDiagnosticable};

pub trait DiagnosticableDirective:
  Diagnosticable<Node = Directive, Error = DirectiveError> + NamedDiagnosticable
{
  fn avaliable_argument_names() -> &'static [&'static str];
}

pub struct ArgumentDescriptor {}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum DirectiveLocation {
  Query,
  Mutation,
  Subscription,
  Field,
  FragmentDefinition,
  FragmentSpread,
  InlineFragment,
  VariableDefinition,
  Schema,
  Scalar,
  Object,
  FieldDefinition,
  ArgumentDefinition,
  Interface,
  Union,
  Enum,
  EnumValue,
  InputObject,
  InputFieldDefinition,
}

impl DirectiveLocation {
  pub const fn as_str(&self) -> &'static str {
    match self {
      Self::Query => "QUERY",
      Self::Mutation => "MUTATION",
      Self::Subscription => "SUBSCRIPTION",
      Self::Field => "FIELD",
      Self::FragmentDefinition => "FRAGMENT_DEFINITION",
      Self::FragmentSpread => "FRAGMENT_SPREAD",
      Self::InlineFragment => "INLINE_FRAGMENT",
      Self::VariableDefinition => "VARIABLE_DEFINITION",
      Self::Schema => "SCHEMA",
      Self::Scalar => "SCALAR",
      Self::Object => "OBJECT",
      Self::FieldDefinition => "FIELD_DEFINITION",
      Self::ArgumentDefinition => "ARGUMENT_DEFINITION",
      Self::Interface => "INTERFACE",
      Self::Union => "UNION",
      Self::Enum => "ENUM",
      Self::EnumValue => "ENUM_VALUE",
      Self::InputObject => "INPUT_OBJECT",
      Self::InputFieldDefinition => "INPUT_FIELD_DEFINITION",
    }
  }

  const fn available_locations() -> &'static [&'static str] {
    &[
      "QUERY",
      "MUTATION",
      "SUBSCRIPTION",
      "FIELD",
      "FRAGMENT_DEFINITION",
      "FRAGMENT_SPREAD",
      "INLINE_FRAGMENT",
      "VARIABLE_DEFINITION",
      "SCHEMA",
      "SCALAR",
      "OBJECT",
      "FIELD_DEFINITION",
      "ARGUMENT_DEFINITION",
      "INTERFACE",
      "UNION",
      "ENUM",
      "ENUM_VALUE",
      "INPUT_OBJECT",
      "INPUT_FIELD_DEFINITION" 
    ]
  }
}

impl From<DirectiveLocation> for String {
  fn from(dir_loc: DirectiveLocation) -> Self {
    dir_loc.as_str().to_string()
  }
}

#[derive(Debug, Clone)]
pub struct UnknownDirectiveLocation {
  src: String,
  suggestion: Option<String>,
}

impl UnknownDirectiveLocation {
  fn new(src: String) -> Self {
    Self { suggestion: crate::utils::did_you_mean(src.as_str(), DirectiveLocation::available_locations()), src } 
  }
}

impl core::fmt::Display for UnknownDirectiveLocation {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match &self.suggestion {
      Some(suggestion) => write!(f, "Unknown directive location: {}, did you mean {}?", self.src, suggestion),
      None => write!(f, "Unknown directive location: {}, available locations are `[{}]`", self.src, DirectiveLocation::available_locations().join(", ")),
    }
  }
}

impl std::error::Error for UnknownDirectiveLocation {}

impl FromStr for DirectiveLocation {
  type Err = UnknownDirectiveLocation;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let loc = match s.trim() {
      "QUERY" | "query" => Self::Query,
      "MUTATION" | "mutation" => Self::Mutation,
      "SUBSCRIPTION" | "subscription" => Self::Subscription,
      "FIELD" | "field" => Self::Field,
      "FRAGMENT_DEFINITION" | "fragment_definition" => Self::FragmentDefinition,
      "FRAGMENT_SPREAD" | "fragment_spread" => Self::FragmentSpread,
      "INLINE_FRAGMENT" | "inline_fragment" => Self::InlineFragment,
      "VARIABLE_DEFINITION" | "variable_definition" => Self::VariableDefinition,
      "SCHEMA" | "schema" => Self::Schema,
      "SCALAR" | "scalar" => Self::Scalar,
      "OBJECT" | "object" => Self::Object,
      "FIELD_DEFINITION" | "field_definition" => Self::FieldDefinition,
      "ARGUMENT_DEFINITION" | "argument_definition" => Self::ArgumentDefinition,
      "INTERFACE" | "interface" => Self::Interface,
      "UNION" | "union" => Self::Union,
      "ENUM" | "enum" => Self::Enum,
      "ENUM_VALUE" | "enum_value" => Self::EnumValue,
      "INPUT_OBJECT" | "input_object" => Self::InputObject,
      "INPUT_FIELD_DEFINITION" | "input_field_definition" => Self::InputFieldDefinition,
      val => return Err(UnknownDirectiveLocation::new(val.to_string())),
    };
    Ok(loc)
  }
}

#[viewit::viewit(setters(skip))]
pub struct DirectiveDescriptor {
  name: &'static str,
  short: Option<char>,
  aliases: &'static [&'static str],
  locations: &'static [DirectiveLocation],
  optional: bool,
  available_arguments: &'static [&'static ArgumentDescriptor],
  required_arguments: &'static [&'static ArgumentDescriptor],
  optional_arguments: &'static [&'static ArgumentDescriptor],
}

impl DirectiveDescriptor {

}
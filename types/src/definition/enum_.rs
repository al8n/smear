use crate::{directive::DirectiveDescriptor, Deprecated};

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Default, Copy, Clone)]
pub struct EnumValueDescriptor {
  name: &'static str,
  description: Option<&'static str>,
  deprecated: Option<&'static Deprecated>,
  directives: &'static [DirectiveDescriptor],
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Default, Copy, Clone)]
pub struct EnumDescriptor {
  name: &'static str,
  description: Option<&'static str>,
  deprecated: Option<&'static Deprecated>,
  directives: &'static [&'static DirectiveDescriptor],
  values: &'static [EnumValueDescriptor],
}

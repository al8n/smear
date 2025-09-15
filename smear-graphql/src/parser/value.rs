use crate::parser::ast::{EnumValue, Float, Int, List, StringValue, Variable};

pub enum InputValue<S> {
  Variable(Variable<S>),
  String(StringValue<S>),
  Float(Float<S>),
  Int(Int<S>),
  Enum(EnumValue<S>),
  List(List<InputValue<S>>),
  Object(),
}

pub enum ConstInputValue<S> {
  String(StringValue<S>),
  Float(Float<S>),
  Int(Int<S>),
  Enum(EnumValue<S>),
  List(List<ConstInputValue<S>>),
  Object(),
}

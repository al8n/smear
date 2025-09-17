pub use impls::*;

mod boolean_value;
mod enum_value;
mod float;
mod impls;
mod int;
mod list;
mod name;
mod null;
mod object;
mod string;
mod variable;
mod argument;

pub mod ast {
  pub use super::{
    argument::*,
    boolean_value::BooleanValue, enum_value::EnumValue, float::FloatValue, int::IntValue,
    list::List, name::Name, null::NullValue, object::Object, string::StringValue,
    variable::Variable,
  };
  pub use smear_parser::lang::punctuator::*;
}

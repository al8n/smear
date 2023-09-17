use crate::{directive::DirectiveDescriptor, Deprecated};

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Default, Copy, Clone)]
pub struct EnumValueDescriptor {
  name: &'static str,
  description: Option<&'static str>,
  deprecated: Option<&'static Deprecated>,
  required_directives: &'static [&'static DirectiveDescriptor],
  optional_directives: &'static [&'static DirectiveDescriptor],
  available_directives: &'static [&'static DirectiveDescriptor],
}

#[viewit::viewit(setters(skip), getters(style = "move"))]
#[derive(Default, Copy, Clone)]
pub struct EnumDescriptor {
  name: &'static str,
  description: Option<&'static str>,
  deprecated: Option<&'static Deprecated>,
  required_directives: &'static [&'static DirectiveDescriptor],
  optional_directives: &'static [&'static DirectiveDescriptor],
  available_directives: &'static [&'static DirectiveDescriptor],
  values: &'static [EnumValueDescriptor],
}

fn parse(node: apollo_parser::ast::EnumTypeDefinition) {
  match node.name() {
    Some(name) => {
      let name = name.text().to_string();
      // match node.directives() {
      //   Some(directives) => {
      //     for directive in directives.directives() {
      //       if let Some(n) = directive.name() {
      //         match () {
      //           () if n.text().as_str() => {
                  
      //           }
      //           _ => {}
      //         }
      //       } else {
              
      //       }
      //     }
      //   }
      //   None => {}
      // }
      for v in node.enum_values_definition() {
        for x in v.enum_value_definitions() {
          if let Some(ev) = x.enum_value() {
            
          }
        }
      }
    },
    None => {
    },
  }
}


fn parse1(node: apollo_parser::ast::Value) {
  // if let Some(val) = node.enum_value() {
  //   val.text()
  // }
    match node {
        apollo_parser::ast::Value::Variable(_) => todo!(),
        apollo_parser::ast::Value::StringValue(_) => todo!(),
        apollo_parser::ast::Value::FloatValue(_) => todo!(),
        apollo_parser::ast::Value::IntValue(_) => todo!(),
        apollo_parser::ast::Value::BooleanValue(_) => todo!(),
        apollo_parser::ast::Value::NullValue(_) => todo!(),
        apollo_parser::ast::Value::EnumValue(val) => todo!(),
        apollo_parser::ast::Value::ListValue(_) => todo!(),
        apollo_parser::ast::Value::ObjectValue(_) => todo!(),
    }
}
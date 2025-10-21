use core::fmt::Debug;

use crate::cst::{DefaultInputValue, Object, ObjectField, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Value> DefaultInputValue<Value, GraphQLxLanguage> => DefaultValue(DefaultInputValue::new)
  where Value: Debug
}

impl_graphqlx_node! {
  for<Name, Value> ObjectField<Name, Value, GraphQLxLanguage> => ObjectField(ObjectField::new)
  where
    Name: Debug,
    Value: Debug
}

impl_graphqlx_node! {
  for<Name, Value> Object<Name, Value, GraphQLxLanguage> => ObjectValue(Object::new)
  where
    Name: Debug,
    Value: Debug
}

mod list;
mod map;
mod set;

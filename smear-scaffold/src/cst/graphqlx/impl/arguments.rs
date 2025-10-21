use core::fmt::Debug;

use crate::cst::{Argument, Arguments, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Name, Value> Argument<Name, Value, GraphQLxLanguage> => Argument(Argument::new)
  where
    Name: Debug,
    Value: Debug
}

impl_graphqlx_node! {
  for<Arg> Arguments<Arg, GraphQLxLanguage> => Arguments(Arguments::new)
  where Arg: Debug
}

use core::fmt::Debug;

use crate::cst::{Argument, Arguments};

use super::GraphQLLanguage;


impl_graphql_node! {
  for<Name, Value> Argument<Name, Value, GraphQLLanguage> => Argument(Argument::new)
  where
    Name: Debug,
    Value: Debug
}

impl_graphql_node! {
  for<Arg> Arguments<Arg, GraphQLLanguage> => Arguments(Arguments::new)
  where Arg: Debug
}

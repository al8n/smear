use core::fmt::Debug;

use crate::cst::{Directive, Directives};

use super::GraphQLLanguage;

impl_graphql_node! {
  for<Name, Arguments> Directive<Name, Arguments, GraphQLLanguage> => Directive(Directive::new)
  where
    Name: Debug,
    Arguments: Debug
}

impl_graphql_node! {
  for<Dir> Directives<Dir, GraphQLLanguage> => Directives(Directives::new)
  where Dir: Debug
}

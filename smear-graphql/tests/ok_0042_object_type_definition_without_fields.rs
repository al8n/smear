use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
"A type with no fields"
type AnObjectTypeWithoutFields

extend type AnObjectTypeWithoutFields {
  id: ID!
}
"###;

#[test]
fn object_type_definition_without_fields() {}

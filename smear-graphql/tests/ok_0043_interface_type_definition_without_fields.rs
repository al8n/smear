use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
"An interface with no fields"
interface AnInterfaceWithoutFields

extend interface AnInterfaceWithoutFields {
  id: ID!
}

"###;

#[test]
fn interface_type_definition_without_fields() {
  let definition =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<'_, char>>>(ALL)
      .unwrap();
}

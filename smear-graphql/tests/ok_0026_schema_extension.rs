use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
extend schema @skip @example {
    query: MyExtendedQueryType
}
"###;

#[test]
fn schema_extension() {
  let extension = SchemaExtension::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap();

  let directives = extension.directives().unwrap();
  assert_eq!(directives.directives().len(), 2);
  let mut directives = directives.directives().iter();
  {
    let skip = directives.next().unwrap();
    assert_eq!(skip.name().span().source(), &"skip");
    assert!(skip.arguments().is_none());
  }
  {
    let example = directives.next().unwrap();
    assert_eq!(example.name().span().source(), &"example");
    assert!(example.arguments().is_none());
  }

  let operation_types = extension.root_operation_types_definition().unwrap();
  let operation_types = operation_types.root_operation_type_definitions();
  assert_eq!(operation_types.len(), 1);
  let operation_type = operation_types.first().unwrap();
  assert_eq!(operation_type.operation_type().span().source(), &"query");
  assert_eq!(
    operation_type.name().span().source(),
    &"MyExtendedQueryType"
  );
}

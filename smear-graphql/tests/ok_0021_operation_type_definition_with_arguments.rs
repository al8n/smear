use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{ast::*, parse::*, WithSource};

const ALL: &str = r###"
query myQuery($var: input $varOther: otherInput){
    animal
    treat
}
"###;

#[test]
fn operation_type_definition_with_arguments() {
  let definition = OperationDefinition::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
    extra::Err<Rich<char>>,
  >(ALL)
  .unwrap()
  .unwrap_named();
  assert_eq!(definition.name().unwrap().span().source(), &"myQuery");
  assert_eq!(definition.operation_type().span().source(), &"query");

  let args = definition.variable_definitions().unwrap();
  assert_eq!(args.variable_definitions().len(), 2);
  let mut args = args.variable_definitions().iter();
  {
    let var = args.next().unwrap();
    assert_eq!(var.variable().name().span().source(), &"var");
    let ty = var.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"input");
    assert!(ty.bang().is_none());
  }

  {
    let var_other = args.next().unwrap();
    assert_eq!(var_other.variable().name().span().source(), &"varOther");
    let ty = var_other.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"otherInput");
    assert!(ty.bang().is_none());
  }

  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 2);
  let mut selections = selection_set.clone().into_selections().into_iter();

  {
    let animal = selections.next().unwrap().unwrap_field();
    assert_eq!(animal.name().span().source(), &"animal");
    assert!(animal.alias().is_none());
    assert!(animal.arguments().is_none());
    assert!(animal.directives().is_none());
    assert!(animal.selection_set().is_none());
  }

  {
    let treat = selections.next().unwrap().unwrap_field();
    assert_eq!(treat.name().span().source(), &"treat");
    assert!(treat.alias().is_none());
    assert!(treat.arguments().is_none());
    assert!(treat.directives().is_none());
    assert!(treat.selection_set().is_none());
  }
}

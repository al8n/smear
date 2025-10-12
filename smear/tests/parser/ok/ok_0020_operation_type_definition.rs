
const ALL: &str = include_str!("../../fixtures/parser/ok/0020_operation_type_definition.graphql");

#[test]
fn operation_type_definition() {
  use smear::parser::graphql::ast::{OperationDefinition, ParseStr};
  
  let definition = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();
  assert_eq!(definition.name().unwrap().source(), "myQuery");
  assert_eq!(definition.operation_type().as_str(), "query");

  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
  let mut selections = selection_set.clone().into_selections().into_iter();

  {
    let cat = selections.next().unwrap().unwrap_field();
    assert_eq!(cat.name().source(), "cat");
    assert_eq!(cat.alias().unwrap().name().source(), "animal");
    assert!(cat.arguments().is_none());
    assert!(cat.directives().is_none());
    assert!(cat.selection_set().is_none());
  }

  {
    let dog = selections.next().unwrap().unwrap_field();
    assert_eq!(dog.name().source(), "dog");
    assert!(dog.alias().is_none());
    assert!(dog.arguments().is_none());
    assert!(dog.directives().is_none());
    let dog_selection_set = dog.selection_set().unwrap();
    assert_eq!(dog_selection_set.selections().len(), 1);
    let mut dog_selections = dog_selection_set.clone().into_selections().into_iter();

    {
      let panda = dog_selections.next().unwrap().unwrap_field();
      assert_eq!(panda.name().source(), "panda");
      assert!(panda.alias().is_none());
      assert!(panda.arguments().is_none());
      assert!(panda.directives().is_none());
      let panda_selection_set = panda.selection_set().unwrap();
      assert_eq!(panda_selection_set.selections().len(), 1);
      let mut panda_selections = panda_selection_set.clone().into_selections().into_iter();

      {
        let another_cat = panda_selections.next().unwrap().unwrap_field();
        assert_eq!(another_cat.name().source(), "anotherCat");
        assert!(another_cat.alias().is_none());
        assert!(another_cat.arguments().is_none());
        let directives = another_cat.directives().cloned().unwrap();
        assert_eq!(directives.directives().len(), 1);
        let directive = directives.directives().first().unwrap();
        assert_eq!(directive.name().source(), "deprecated");
        assert!(directive.arguments().is_none());
        assert!(another_cat.selection_set().is_none());
      }
    }
  }

  {
    let lion = selections.next().unwrap().unwrap_field();
    assert_eq!(lion.name().source(), "lion");
    assert!(lion.alias().is_none());
    assert!(lion.arguments().is_none());
    assert!(lion.directives().is_none());
    assert!(lion.selection_set().is_none());
  }
}

#[test]
fn graphqlx_operation_type_definition() {
  use smear::parser::graphqlx::ast::{OperationDefinition, ParseStr};

  let definition = OperationDefinition::<&str>::parse_str(ALL)
  .unwrap()
  .unwrap_named();
  assert_eq!(definition.name().unwrap(), "myQuery");
  assert_eq!(definition.operation_type().as_str(), "query");

  let selection_set = definition.selection_set();
  assert_eq!(selection_set.selections().len(), 3);
  let mut selections = selection_set.clone().into_selections().into_iter();

  {
    let cat = selections.next().unwrap().unwrap_field();
    assert_eq!(cat.name(), "cat");
    assert_eq!(cat.alias().unwrap().name(), "animal");
    assert!(cat.arguments().is_none());
    assert!(cat.directives().is_none());
    assert!(cat.selection_set().is_none());
  }

  {
    let dog = selections.next().unwrap().unwrap_field();
    assert_eq!(dog.name(), "dog");
    assert!(dog.alias().is_none());
    assert!(dog.arguments().is_none());
    assert!(dog.directives().is_none());
    let dog_selection_set = dog.selection_set().unwrap();
    assert_eq!(dog_selection_set.selections().len(), 1);
    let mut dog_selections = dog_selection_set.clone().into_selections().into_iter();

    {
      let panda = dog_selections.next().unwrap().unwrap_field();
      assert_eq!(panda.name(), "panda");
      assert!(panda.alias().is_none());
      assert!(panda.arguments().is_none());
      assert!(panda.directives().is_none());
      let panda_selection_set = panda.selection_set().unwrap();
      assert_eq!(panda_selection_set.selections().len(), 1);
      let mut panda_selections = panda_selection_set.clone().into_selections().into_iter();

      {
        let another_cat = panda_selections.next().unwrap().unwrap_field();
        assert_eq!(another_cat.name(), "anotherCat");
        assert!(another_cat.alias().is_none());
        assert!(another_cat.arguments().is_none());
        let directives = another_cat.directives().cloned().unwrap();
        assert_eq!(directives.directives().len(), 1);
        let directive = directives.directives().first().unwrap();
        assert_eq!(directive.name(), "deprecated");
        assert!(directive.arguments().is_none());
        assert!(another_cat.selection_set().is_none());
      }
    }
  }

  {
    let lion = selections.next().unwrap().unwrap_field();
    assert_eq!(lion.name(), "lion");
    assert!(lion.alias().is_none());
    assert!(lion.arguments().is_none());
    assert!(lion.directives().is_none());
    assert!(lion.selection_set().is_none());
  }
}

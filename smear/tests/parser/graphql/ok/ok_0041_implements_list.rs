const ALL: &str = include_str!("../../../fixtures/parser/graphql/ok/0041_implements_list.graphql");

#[test]
#[cfg(feature = "graphql")]
fn implements_list() {
  use smear::parser::graphql::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();

    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "One");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "Just one interface"
    );
    let impls = object.implements().unwrap();
    let first = impls.interfaces().first().unwrap();
    assert_eq!(first.source(), "A");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "Two");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "Several interfaces"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(impls.interfaces().len(), 3);
    assert_eq!(ifs[0].source(), "A");
    assert_eq!(ifs[1].source(), "B");
    assert_eq!(ifs[2].source(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "Three");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "&-prefixed"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(ifs.len(), 3);
    assert_eq!(ifs[0].source(), "A");
    assert_eq!(ifs[1].source(), "B");
    assert_eq!(ifs[2].source(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().source(), "Int");
      assert!(ty.required());
    }
  }
}

#[test]
#[cfg(feature = "graphqlx")]
fn graphqlx_implements_list() {
  use smear::parser::graphqlx::ast::{Document, ParseStr};

  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();

    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "One");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "Just one interface"
    );
    let impls = object.implements().unwrap();
    let first = impls.interfaces().first().unwrap();
    assert_eq!(first.path(), "A");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "Two");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "Several interfaces"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(impls.interfaces().len(), 3);
    assert_eq!(ifs[0].path(), "A");
    assert_eq!(ifs[1].path(), "B");
    assert_eq!(ifs[2].path(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().source(), "Three");
    assert_eq!(
      described.description().unwrap().source().trim_matches('"'),
      "&-prefixed"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(ifs.len(), 3);
    assert_eq!(ifs[0].path(), "A");
    assert_eq!(ifs[1].path(), "B");
    assert_eq!(ifs[2].path(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().source(), "field");
      let ty = field.ty().unwrap_path_ref();
      assert_eq!(ty.path(), "Int");
      assert!(ty.required());
    }
  }
}

use smear_graphql::parser::ast::{raw::Document, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0041_implements_list.graphql");

#[test]
fn implements_list() {
  let document =
    Document::<&str>::parse_str(ALL)
      .unwrap();

  let mut types = document.definitions().iter();

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref();

    let object = described
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().slice(), "One");
    assert_eq!(
      described.description().unwrap().content(),
      "Just one interface"
    );
    let impls = object.implements().unwrap();
    let first = impls.interfaces().first().unwrap();
    assert_eq!(first.slice(), "A");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().slice(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().slice(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().slice(), "Two");
    assert_eq!(
      described.description().unwrap().content(),
      "Several interfaces"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(impls.interfaces().len(), 3);
    assert_eq!(ifs[0].slice(), "A");
    assert_eq!(ifs[1].slice(), "B");
    assert_eq!(ifs[2].slice(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().slice(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().slice(), "Int");
      assert!(ty.required());
    }
  }

  {
    let described = types
      .next()
      .unwrap()
      .unwrap_type_system_ref()
      .unwrap_definition_ref();
    let object = described
      .unwrap_type_ref()
      .unwrap_object_ref();

    assert_eq!(object.name().slice(), "Three");
    assert_eq!(
      described.description().unwrap().content(),
      "&-prefixed"
    );
    let impls = object.implements().unwrap();
    let ifs = impls.interfaces();

    assert_eq!(ifs.len(), 3);
    assert_eq!(ifs[0].slice(), "A");
    assert_eq!(ifs[1].slice(), "B");
    assert_eq!(ifs[2].slice(), "C");

    {
      let selections = object.fields_definition().unwrap().field_definitions();
      assert_eq!(selections.len(), 1);
      let field = &selections[0];
      assert_eq!(field.name().slice(), "field");
      let ty = field.ty().unwrap_name_ref();
      assert_eq!(ty.name().slice(), "Int");
      assert!(ty.required());
    }
  }
}

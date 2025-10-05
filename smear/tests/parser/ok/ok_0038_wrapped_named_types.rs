use smear::parser::graphql::ast::{{Document, ObjectTypeDefinition}, ParseStr};

const ALL: &str = include_str!("../../fixtures/parser/ok/0038_wrapped_named_types.graphql");

#[test]
fn wrapped_named_types() {
  let document =
    Document::<&str>::parse_str(ALL).unwrap();

  let definitions = document.definitions();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter().map(|d| {
    d.unwrap_definition_ref()
      .unwrap_type_system_ref()
      .unwrap_type_ref()
      .unwrap_object_ref()
  });

  {
    let def = definitions.next().unwrap();
    check_object_def(def, "ObjectDef");
  }

  {
    let def = definitions.next().unwrap();
    check_object_def(def, "ObjectDefTwo");
  }
}

fn check_object_def(def: &ObjectTypeDefinition<&str>, name: &str) {
  assert_eq!(def.name().source(), name);
  let fields = def.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 5);

  let mut iter = fields.iter();

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().source(), "a");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "String");
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().source(), "b");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().source(), "Int");
    assert!(ty.required());
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().source(), "c");
    let ty = field.ty().unwrap_list_ref();
    assert!(ty.required());
    {
      let inner = ty.ty().unwrap_name_ref();
      assert_eq!(inner.name().source(), "Int");
      assert!(inner.required());
    }
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().source(), "d");
    let ty = field.ty().unwrap_list_ref();
    assert!(!ty.required());
    {
      let level1 = ty.ty().unwrap_list_ref();
      assert!(!level1.required());

      {
        let level2 = level1.ty().unwrap_list_ref();
        assert!(!level2.required());

        {
          let level3 = level2.ty().unwrap_list_ref();
          assert!(!level3.required());

          {
            let level4 = level3.ty().unwrap_list_ref();
            assert!(!level4.required());

            {
              let level5 = level4.ty().unwrap_name_ref();
              assert_eq!(level5.name().source(), "Int");
              assert!(!level5.required());
            }
          }
        }
      }
    }
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().source(), "d");
    let ty = field.ty().unwrap_list_ref();
    assert!(ty.required());
    {
      let level1 = ty.ty().unwrap_list_ref();
      assert!(level1.required());

      {
        let level2 = level1.ty().unwrap_list_ref();
        assert!(level2.required());

        {
          let level3 = level2.ty().unwrap_list_ref();
          assert!(level3.required());

          {
            let level4 = level3.ty().unwrap_list_ref();
            assert!(level4.required());

            {
              let level5 = level4.ty().unwrap_name_ref();
              assert_eq!(level5.name().source(), "Int");
              assert!(level5.required());
            }
          }
        }
      }
    }
  }
}

use chumsky::{error::Rich, extra, span::SimpleSpan};
use smear_graphql::{cst::*, parse::*, WithSource};

const ALL: &str = r###"
type ObjectDef {
    a: String
    b: Int!
    c: [Int!]!
    d: [[[[[Int]]]]]
    d: [[[[[Int!]!]!]!]!]!
}

type ObjectDefTwo {
    a: String,
    b: Int!,
    c: [Int!]!,
    d: [[[[[Int]]]]],
    d: [[[[[Int!]!]!]!]!]!,
}
"###;

#[test]
fn wrapped_named_types() {
  let document =
    Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<extra::Err<Rich<char>>>(ALL)
      .unwrap();

  let definitions = document.content();
  assert_eq!(definitions.len(), 2);

  let mut definitions = definitions.iter().map(|d| {
    d.unwrap_type_system_ref()
      .unwrap_definition_ref()
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

fn check_object_def(def: &ObjectTypeDefinition<WithSource<&str, SimpleSpan>>, name: &str) {
  assert_eq!(def.name().span().source(), &name);
  let fields = def.fields_definition().unwrap().field_definitions();
  assert_eq!(fields.len(), 5);

  let mut iter = fields.iter();

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().span().source(), &"a");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"String");
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().span().source(), &"b");
    let ty = field.ty().unwrap_name_ref();
    assert_eq!(ty.name().span().source(), &"Int");
    assert!(ty.bang().is_some());
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().span().source(), &"c");
    let ty = field.ty().unwrap_list_ref();
    assert!(ty.bang().is_some());
    {
      let inner = ty.ty().unwrap_name_ref();
      assert_eq!(inner.name().span().source(), &"Int");
      assert!(inner.bang().is_some());
    }
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().span().source(), &"d");
    let ty = field.ty().unwrap_list_ref();
    assert!(ty.bang().is_none());
    {
      let level1 = ty.ty().unwrap_list_ref();
      assert!(level1.bang().is_none());

      {
        let level2 = level1.ty().unwrap_list_ref();
        assert!(level2.bang().is_none());

        {
          let level3 = level2.ty().unwrap_list_ref();
          assert!(level3.bang().is_none());

          {
            let level4 = level3.ty().unwrap_list_ref();
            assert!(level4.bang().is_none());

            {
              let level5 = level4.ty().unwrap_name_ref();
              assert_eq!(level5.name().span().source(), &"Int");
              assert!(level5.bang().is_none());
            }
          }
        }
      }
    }
  }

  {
    let field = iter.next().unwrap();
    assert_eq!(field.name().span().source(), &"d");
    let ty = field.ty().unwrap_list_ref();
    assert!(ty.bang().is_some());
    {
      let level1 = ty.ty().unwrap_list_ref();
      assert!(level1.bang().is_some());

      {
        let level2 = level1.ty().unwrap_list_ref();
        assert!(level2.bang().is_some());

        {
          let level3 = level2.ty().unwrap_list_ref();
          assert!(level3.bang().is_some());

          {
            let level4 = level3.ty().unwrap_list_ref();
            assert!(level4.bang().is_some());

            {
              let level5 = level4.ty().unwrap_name_ref();
              assert_eq!(level5.name().span().source(), &"Int");
              assert!(level5.bang().is_some());
            }
          }
        }
      }
    }
  }
}

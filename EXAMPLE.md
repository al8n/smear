
```rust
#[derive(smear_derive::Directive)]
#[smear(
  short,
)]
struct Derive {
  #[smear(
    validator = "path/to/validator",
    parser = "path/to/parser",
    default,
    optional
  )]
  field1: String,
}

#[derive(smear_derive::ObjectDefinition)]
#[smear(
  rename_all = "xxx",
  directive(
    type = "FooDirective",
    short,
    name = "foo",
    aliases(myfoo, yourfoo),
    optional
  ),
  directive(
    type = "BarDirective",
    short,
    name = "bar",
    aliases(mybar, yourbar),
  )
)]
struct Object {
  #[smear(
    rename = "xxx1",
    directive(
      type = "FooDirective",
      short,
      name = "foo",
      aliases(myfoo, yourfoo),
      optional
    ),
    directive(
      type = "BarDirective",
      short,
      name = "bar",
      aliases(mybar, yourbar),
    )
  )]
  f1: String,
}

smear::document! {
  objects: [{
    name: FooObjectDefinition,
    directives: [FooDirective, BarDirective],
  }],
  enums: [{
    name: 
  }]
}

#[derive(smear_derive::Document)]
#[smear(
  rename_all = "xxx",
  objects(

  ),
  enums(

  ),
)]
struct Object {
  #[smear(name = "xxx", )]
  f1: FooFieldDefinition,
}

#[derive(smear_derive::UnitEnum)]
#[smear(
  suggestion,
  rename_all = "camel_case",
  parser(
    ignore_case,
    possible_types(string, enum)
  ),
)]
enum DeriveEnum {
  Copy,
  Clone,
}

#[derive(smear_derive::Directive)]
#[smear(
  name = "",
  default = true,
  repeat(op = "last"), // "first", "last" and "merge"
  alias(name1, name2, name3),
  arguments(
    derive = ,
    xxx = {
    
    }
  ),
)]
struct Attr;


#[derive(smear_derive::Field)]
#[smear(
  type = "",
  directives(Indexed, ),
)]
struct Field1 {

}

```

# generated

```rust
struct IndexedDirective(bool);

impl IndexedDirective {
  pub fn parse() -> Result<Self, >
}

enum InvalidIndexedDirective {
  Err1,
  Err2,
}

enum DeriveArg {
  Enum()
}
```

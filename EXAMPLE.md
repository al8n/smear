
```rust
#[derive(smear_derive::BooleanDirective)]
#[smear(
  short,
  long = "indexed",
  default = true,
  alias(name1, name2, name3),
)]
struct Indexed;

#[derive(smear_derive::BooleanDirective)]
#[smear(
  short = 'f',
  default = false,
  alias(name1, name2, name3),
)]
struct Fixed;

#[derive(smear_derive::Directive)]
#[smear(
  short,
)]
struct Derive {
  #[smear(
    validator = "path/to/validator",
    parser = "path/to/parser",
    default,
    required,
  )]
  field1: StringArgument,
}


#[derive(smear_derive::FieldDefinition)]
#[smear(
  rename_all = "xxx"
)]
struct Field {
  #[smear(
    validator = "path/to/validator",
    parser = "path/to/parser",
    default,
    required,
  )]
  d1: FooDirective,

  d2: BarDirective,
}

#[derive(smear_derive::ObjectDefinition)]
#[smear(
  rename_all = "xxx",
  directives(

  )
)]
struct Object {
  #[smear(name = "xxx", )]
  f1: FooFieldDefinition,
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

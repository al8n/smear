
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

#[derive(smear_derive::Argument)]
#[smear(
  short,
  parser = "path/to/parse_fn",
)]
struct Derive;


#[derive(smear_derive::ObjectArgument)]
#[smear(
  short,
  fields(
    foo(U64Parser),
    bar(),
  ),
  parser = "path/to/parse_fn",
)]
struct Object;

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

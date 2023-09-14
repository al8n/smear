
#[smear::hashmap(
  key(String),
  value(String)
)]
struct Foo;


#[smear::btreemap(
  key(String),
  value(String)
)]
struct Bar;


#[smear::hashmap(
  key = String,
  value("String"),
  attributes(derive(Clone))
)]
struct Baz;

#[smear::btreemap(
  key = "String",
  value(String)
)]
struct Bazz;

fn main() {}
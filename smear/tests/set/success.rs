
#[smear::hashset(
  value(String)
)]
struct Foo;


#[smear::btreeset(
  value(String)
)]
struct Bar;


#[smear::hashset(
  value("String"),
  attributes(derive(Clone))
)]
struct Baz;

#[smear::btreeset(
  value(String)
)]
struct Bazz;

fn main() {}
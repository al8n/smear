
#[smear::hashmap(
  key(String),
  value(String)
)]
enum Foo {

}


#[smear::btreemap(
  key(String),
  value(String)
)]
struct Bar(f64, u32);


#[smear::hashmap(
  key = String,
  value("String"),
  attributes(derive(Clone))
)]
struct Baz {
  f1: u64,
  f2: u32,
}

#[smear::btreemap(
  key = "String",
  value(String)
)]
struct Bazz(u64);

fn main() {}

#[smear::hashset(
  value(String)
)]
enum Foo {

}


#[smear::btreeset(
  value(String)
)]
struct Bar(f64, u32);


#[smear::hashset(
  value("String"),
  attributes(derive(Clone))
)]
struct Baz {
  f1: u64,
  f2: u32,
}

#[smear::btreeset(
  value(String)
)]
struct Bazz(u64);

fn main() {}
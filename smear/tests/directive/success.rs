
#[derive(::smear::Directive)]
#[smear(
  short = 'f',
  aliases(bar),
  on(field_definition, object)
)]
struct Foo {
  #[smear(
    short = 's',
  )]
  small: u64,
  #[smear(
    short = 'b',
  )]
  big: u32,
}

fn main() {}
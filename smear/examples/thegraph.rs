use apollo_encoder::DirectiveDefinition;

#[derive(::smear::Directive)]
#[smear(
  short = 'f',
  aliases(bar),
  on(field_definition, object),
  description = "A foo directive",
  deprecated(version = "0.1.0")
)]
struct Foo {
  #[smear(short = 's', deprecated)]
  small: u64,
  #[smear(short, aliases(bigger, biggest))]
  big: u32,
}

fn main() {
  use apollo_encoder::InputValueDefinition;
  use smear_types::Diagnosticable;

  let def = DirectiveDefinition::from(FooDirective::descriptor());
  println!("{}", def);
}

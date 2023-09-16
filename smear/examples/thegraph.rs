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
  #[smear(aliases("rwd"), deprecated, default)]
  required_with_default: u64,
  #[smear(short, aliases(ro), default = 16)]
  required_only: u32,
  #[smear(short(o), optional, default(default_optional_with_default))]
  optional_with_default: f64,
  #[smear(short = 'y', optional)]
  optional_only: String,
}

fn default_optional_with_default() -> f64 {
  1.0
}

fn main() {
  use smear_types::Encodable;

  let def = FooDirective::encode();
  println!("{}", def);
}

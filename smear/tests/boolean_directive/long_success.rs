#[derive(smear::BooleanDirective)]
#[smear(
  long = "_6"
)]
struct StartWithUnderscored;

#[derive(smear::BooleanDirective)]
#[smear(long)]
struct LongEmpty;

fn main() {}
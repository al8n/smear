#[derive(smear::BooleanDirective)]
#[smear(
  long = "6n"
)]
struct StartWithNumber;

#[derive(smear::BooleanDirective)]
#[smear(
  long = "i"
)]
struct TooShort;


#[derive(smear::BooleanDirective)]
#[smear(
  long = ""
)]
struct EmptyString;

#[derive(smear::BooleanDirective)]
#[smear(
  long = "👍"
)]
struct Unicode;

#[derive(smear::BooleanDirective)]
#[smear(
  long = "invalid-character"
)]
struct InvalidCharacter;

fn main() {}
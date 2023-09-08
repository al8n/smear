#[derive(smear::BooleanDirective)]
#[smear(aliases())]
struct EmptyAliases;

#[derive(smear::BooleanDirective)]
#[smear(aliases)]
struct EmptyAliases1;

#[derive(smear::BooleanDirective)]
#[smear(aliases("", "a", "1b", "_", "1", "👍", 1, 2))]
struct InvalidAliasName;

fn main() {}
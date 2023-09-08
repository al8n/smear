#[derive(smear::BooleanDirective)]
#[smear(short = '9')]
struct NumberChar;

#[derive(smear::BooleanDirective)]
#[smear(short = '_')]
struct UnderscoreChar;

#[derive(smear::BooleanDirective)]
#[smear(short = '-')]
struct InvalidChar;

#[derive(smear::BooleanDirective)]
#[smear(short = 9)]
struct InvalidLit;

#[derive(smear::BooleanDirective)]
#[smear(short())]
struct InvalidMeta;

fn main() {}
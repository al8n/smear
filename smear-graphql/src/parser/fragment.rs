use smear_parser::lang::v2::{self, Name, FragmentName};

pub type FragmentSpread<Directives, S> = v2::FragmentSpread<FragmentName<S>, Directives>;

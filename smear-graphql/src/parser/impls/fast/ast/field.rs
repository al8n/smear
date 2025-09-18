use smear_parser::lang::v2::{self, FragmentName, Name};

use crate::parser::ast;

use super::*;

pub type Alias<S> = v2::Alias<Name<S>>;
pub type Field<
  S,
  ArgumentContainer = Vec<Argument<S>>,
  DirectiveContainer = Vec<Directive<S, ArgumentContainer>>,
> = ast::Field<
  Alias<S>,
  Name<S>,
  FragmentName<S>,
  TypeCondition<S>,
  Arguments<S, ArgumentContainer>,
  Directives<S, ArgumentContainer, DirectiveContainer>,
>;

#[cfg(test)]
mod tests {
  use chumsky::Parser;
  use logosky::{Parseable, utils::Span};

  use crate::parser::fast::{FastParserExtra, FastTokenStream};

  use super::*;

  #[test]
  fn test_alias_parser() {
    let parser = Alias::parser::<FastParserExtra<&str>>();
    let input = r#"foo:"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "foo");
    assert_eq!(parsed.span(), &Span::new(0, 4));
  }
}

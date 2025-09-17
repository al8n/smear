use crate::parser::{ast, fast::{ConstInputValue, InputValue}};

pub type Argument<S> = ast::Argument<InputValue<S>, S>;
pub type Arguments<S, Container = Vec<Argument<S>>> = ast::Arguments<Argument<S>, Container>;

pub type ConstArgument<S> = ast::Argument<ConstInputValue<S>, S>;
pub type ConstArguments<S, Container = Vec<ConstArgument<S>>> = ast::Arguments<ConstArgument<S>, Container>;

#[cfg(test)]
mod tests {
  use logosky::{utils::Span, Parseable};
  use chumsky::Parser;

  use crate::parser::fast::{FastParserExtra, FastTokenStream};

  use super::*;

  #[test]
  fn test_argument_parser() {
    let parser = Argument::<&str>::parser::<FastParserExtra>();
    let input = r#"argName: 123"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "argName");
    assert_eq!(parsed.name().span(), &Span::new(0, 7));
    assert_eq!(parsed.colon().span(), &Span::new(7, 8));
    match parsed.value() {
      InputValue::Int(int_val) => {
        assert_eq!(*int_val.source(), "123");
        assert_eq!(int_val.span(), &Span::new(9, 12));
      }
      _ => panic!("Expected IntValue"),
    }
  }

  #[test]
  fn test_const_argument_parser() {
    let parser = ConstArgument::<&str>::parser::<FastParserExtra>();
    let input = r#"argName: 123"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(*parsed.name().source(), "argName");
    assert_eq!(parsed.name().span(), &Span::new(0, 7));
    assert_eq!(parsed.colon().span(), &Span::new(7, 8));
    match parsed.value() {
      ConstInputValue::Int(int_val) => {
        assert_eq!(*int_val.source(), "123");
        assert_eq!(int_val.span(), &Span::new(9, 12)); // Note: ConstInputValue should have the same span as InputValue
      }
      _ => panic!("Expected IntValue"),
    }
  }

  #[test]
  fn test_arguments_parser() {
    let parser = Arguments::<&str>::parser::<FastParserExtra>();
    let input = r#"(arg1: 123, arg2: "value")"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.arguments().len(), 2);
    assert_eq!(*parsed.arguments()[0].name().source(), "arg1");
    assert_eq!(*parsed.arguments()[1].name().source(), "arg2");
  }

  #[test]
  fn test_const_arguments_parser() {
    let parser = ConstArguments::<&str>::parser::<FastParserExtra>();
    let input = r#"(arg1: 123, arg2: "value")"#;
    let parsed = parser.parse(FastTokenStream::new(input)).unwrap();
    assert_eq!(parsed.arguments().len(), 2);
    assert_eq!(*parsed.arguments()[0].name().source(), "arg1");
    assert_eq!(*parsed.arguments()[1].name().source(), "arg2");
  }
}

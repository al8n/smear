use chumsky::{
  container::Container, extra::ParserExtra, input::StrInput, label::LabelError, prelude::*,
  text::TextExpected, util::MaybeRef,
};
use either::Either;

use crate::parser::{
  language::input_value::{Map, MapEntry, Set},
  Char,
};

/// Returns a parser which can parse either a set or a map.
pub fn angle_parser_with<'src, I, E, P, T, CS, CM>(
  value: P,
) -> impl Parser<'src, I, Either<Set<T, I::Slice, I::Span, CS>, Map<T, I::Slice, I::Span, CM>>, E> + Clone
where
  I: StrInput<'src>,
  I::Token: Char + 'src,
  I::Slice: 'src,
  I::Span: 'src,
  E: ParserExtra<'src, I>,
  E::Error:
    LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
  P: Parser<'src, I, T, E> + Clone,
  CM: Container<MapEntry<T, I::Slice, I::Span>>,
  CS: Container<T>,
{
  let ws = super::ignored::ignored();
  let colon = just(I::Token::COLON);

  // Non-consuming guard:
  // after '<' and ws, either a ':' (i.e. "<:>") or "value ':'" => it's a Map.
  let map_guard = just(I::Token::LESS_THAN)
    .ignore_then(ws.clone())
    .ignore_then(choice((
      colon.clone().to(()), // "<:>"
      value
        .clone()
        .then_ignore(ws.clone())
        .then_ignore(colon.clone())
        .to(()), // "< value :"
    )))
    .rewind();

  map_guard
    .ignore_then(Map::<T, _, _, CM>::parser_with(value.clone()))
    .map(Either::Right)
    .or(Set::<T, _, _, CS>::parser_with(value).map(Either::Left))
}

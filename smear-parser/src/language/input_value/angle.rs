use chumsky::{container::Container, extra::ParserExtra, prelude::*};
use either::Either;

use super::super::{
  super::{
    source::{Char, Slice, Source},
    spanned::Spanned,
  },
  ignored::ignored,
  input_value::{Map, MapEntry, Set},
  punct::{Colon, LAngle},
};

/// Returns a parser which can parse either a set or a map.
pub fn map_or_set_parser<'src, I, E, Key, Value, Item, KP, VP, IP, CS, CM>(
  key_parser: impl Fn() -> KP,
  value_parser: impl Fn() -> VP,
  item_parser: impl Fn() -> IP,
) -> impl Parser<'src, I, Either<Set<Item, I::Span, CS>, Map<Key, Value, I::Span, CM>>, E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  E: ParserExtra<'src, I>,
  I::Span: Spanned<'src, I, E>,
  IP: Parser<'src, I, Item, E> + Clone,
  KP: Parser<'src, I, Key, E> + Clone,
  VP: Parser<'src, I, Value, E> + Clone,
  CM: Container<MapEntry<Key, Value, I::Span>>,
  CS: Container<Item>,
{
  // Non-consuming guard:
  // after '<' and ws, either a ':' (i.e. "<:>") or "value ':'" => it's a Map.
  let map_guard = LAngle::<I::Span>::parser()
    .ignore_then(ignored())
    .ignore_then(choice((
      Colon::<I::Span>::parser().to(()), // "<:>"
      key_parser()
        .then_ignore(ignored())
        .then_ignore(Colon::<I::Span>::parser())
        .to(()), // "< value :"
    )))
    .rewind();

  map_guard
    .ignore_then(Map::<Key, Value, _, CM>::parser_with(
      key_parser(),
      value_parser(),
    ))
    .map(Either::Right)
    .or(Set::<Item, _, CS>::parser_with(item_parser()).map(Either::Left))
}

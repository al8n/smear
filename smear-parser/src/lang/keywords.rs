word!(
  /// `repeatable` keyword
  "repeatable": Repeatable: [I::Token::r, I::Token::e, I::Token::p, I::Token::e, I::Token::a, I::Token::t, I::Token::a, I::Token::b, I::Token::l, I::Token::e],
  /// `directive` keyword
  "directive": Directive: [I::Token::d, I::Token::i, I::Token::r, I::Token::e, I::Token::c, I::Token::t, I::Token::i, I::Token::v, I::Token::e],
  /// `on` keyword
  "on": On: [I::Token::o, I::Token::n],
  /// `extend` keyword
  "extend": Extend: [I::Token::e, I::Token::x, I::Token::t, I::Token::e, I::Token::n, I::Token::d],
  /// `scalar` keyword
  "scalar": Scalar: [I::Token::s, I::Token::c, I::Token::a, I::Token::l, I::Token::a, I::Token::r],
  /// `enum` keyword
  "enum": Enum: [I::Token::e, I::Token::n, I::Token::u, I::Token::m],
  /// `input` keyword
  "input": Input: [I::Token::i, I::Token::n, I::Token::p, I::Token::u, I::Token::t],
  /// `union` keyword
  "union": Union: [I::Token::u, I::Token::n, I::Token::i, I::Token::o, I::Token::n],
  /// `interface` keyword
  "interface": Interface: [I::Token::i, I::Token::n, I::Token::t, I::Token::e, I::Token::r, I::Token::f, I::Token::a, I::Token::c, I::Token::e],
  /// `type` keyword
  "type": Type: [I::Token::t, I::Token::y, I::Token::p, I::Token::e],
  /// `fragment` keyword
  "fragment": Fragment: [I::Token::f, I::Token::r, I::Token::a, I::Token::g, I::Token::m, I::Token::e, I::Token::n, I::Token::t],
  /// `schema` keyword
  "schema": Schema: [I::Token::s, I::Token::c, I::Token::h, I::Token::e, I::Token::m, I::Token::a],
  /// `implements` keyword
  "implements": Implements: [I::Token::i, I::Token::m, I::Token::p, I::Token::l, I::Token::e, I::Token::m, I::Token::e, I::Token::n, I::Token::t, I::Token::s],
  /// `query` keyword
  "query": Query: [I::Token::q, I::Token::u, I::Token::e, I::Token::r, I::Token::y],
  /// `mutation` keyword
  "mutation": Mutation: [I::Token::m, I::Token::u, I::Token::t, I::Token::a, I::Token::t, I::Token::i, I::Token::o, I::Token::n],
  /// `subscription` keyword
  "subscription": Subscription: [I::Token::s, I::Token::u, I::Token::b, I::Token::s, I::Token::c, I::Token::r, I::Token::i, I::Token::p, I::Token::t, I::Token::i, I::Token::o, I::Token::n],
);

// /// A macro to define concrete keyword types
// #[macro_export]
// macro_rules! keywords {
//   ($(
//     $(#[$meta:meta])*
//     $label:literal
//   ),+$(,)?) => {
//     paste::paste! {
//       $(
//         $(#[$meta])*
//         #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
//         pub struct [< $label:upper:camel >]<T>(T);

//         impl<T> ::core::convert::AsRef<T> for [< $label:upper:camel >]<T> {
//           #[inline]
//           fn as_ref(&self) -> &T {
//             ::core::ops::Deref::deref(self)
//           }
//         }

//         impl<T> ::core::ops::Deref for [< $label:upper:camel >]<T> {
//           type Target = T;

//           #[inline]
//           fn deref(&self) -> &T {
//             &self.0
//           }
//         }

//         impl<T> $crate::__private::IntoComponents for [< $label:upper:camel >]<T> {
//           type Components = T;

//           #[inline]
//           fn into_components(self) -> Self::Components {
//             self.0
//           }
//         }

//         impl<T> [< $label:upper:camel >]<T> {
//           #[doc = "Returns the parser for the `" $label "` keyword."]
//           pub fn parser<'src, I, E>() -> impl $crate::__private::chumsky::prelude::Parser<'src, I, Self, E> + ::core::clone::Clone
//           where
//             I: $crate::__private::lexer::Tokenizer<'src>,
//             <I as $crate::__private::chumsky::input::Input<'src>>::Token: $crate::__private::lexer::Token<
//               'src,
//               <I as $crate::__private::lexer::Tokenizer<'src>>::Text,
//               <I as $crate::__private::lexer::Tokenizer<'src>>::State,
//             >
//             + $crate::__private::lexer::Require<
//                 'src,
//                 <I as $crate::__private::lexer::Tokenizer<'src>>::Text,
//                 <I as $crate::__private::chumsky::input::Input<'src>>::Token,
//                 <I as $crate::__private::lexer::Tokenizer<'src>>::State,
//                 $crate::__private::lexer::token::kind::Keyword<'src>,
//                 Output = T,
//               >,
//             E: $crate::__private::chumsky::extra::ParserExtra<'src, I>,
//             <E as $crate::__private::chumsky::extra::ParserExtra<'src, I>>::Error: ::core::convert::From<
//               $crate::__private::lexer::Error<
//                 'src,
//                 <I as $crate::__private::lexer::Tokenizer<'src>>::Text,
//                 <I as $crate::__private::chumsky::input::Input<'src>>::Token,
//                 <I as $crate::__private::lexer::Tokenizer<'src>>::State,
//               >
//             >,
//           {
//             use $crate::__private::chumsky::prelude::Parser as _;

//             $crate::__private::lexer::token::keyword($label).map(Self)
//           }
//         }
//       )*
//     }
//   };
// }

// mod keywords2 {
//   keywords!(
//     /// `repeatable` keyword
//     "repeatable",
//     /// `directive` keyword
//     "directive",
//     /// `on` keyword
//     "on",
//     /// `extend` keyword
//     "extend",
//     /// `scalar` keyword
//     "scalar",
//     /// `enum` keyword
//     "enum",
//     /// `input` keyword
//     "input",
//     /// `type` keyword
//     "type",
//     /// `interface` keyword
//     "interface",
//     /// `implements` keyword
//     "implements",
//     /// `union` keyword
//     "union",
//     /// `fragment` keyword
//     "fragment",
//     /// `schema` keyword
//     "schema",
//     /// `query` keyword
//     "query",
//     /// `mutation` keyword
//     "mutation",
//     /// `subscription` keyword
//     "subscription",
//   );
// }

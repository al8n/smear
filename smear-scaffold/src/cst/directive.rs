// use core::marker::PhantomData;
// use logosky::{
//   Logos, Parseable, Source, Token, Tokenizer,
//   chumsky::{self, IterParser, Parser, extra::ParserExtra},
//   utils::{AsSpan, IntoComponents, IntoSpan, Span},
// };

// use smear_lexer::syntactic::punctuator::At;
// use rowan::{SyntaxNode, Language};


// /// Represents a single directive in a GraphQL-style syntax.
// ///
// /// A directive consists of an `@` symbol followed by a name and optional arguments.
// /// For example: `@deprecated`, `@include(if: true)`, `@customDirective(arg1: "value", arg2: 42)`
// ///
// /// Spec: [Directive](https://spec.graphql.org/draft/#Directive)
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct Directive<Name, Args, Lang>
// where
//   Lang: Language,
// {
//   span: Span,
//   // name: Name,
//   // arguments: Option<Args>,
//   node: SyntaxNode<Lang>,
//   _name: PhantomData<Name>,
//   _args: PhantomData<Args>, 
// }

// impl<Name, Args, Lang> Directive<Name, Args, Lang>
// where
//   Lang: Language,
// {
//   /// Returns a reference to the span covering the entire directive.
//   ///
//   /// The span includes the `@` symbol, name, and arguments (if present).
//   #[inline]
//   pub const fn span(&self) -> &Span {
//     &self.span
//   }

//   /// Creates a parser for a directive using the provided name and arguments parsers.
//   ///
//   /// The `name_parser` is used to parse the directive's name, while the `args_parser`
//   /// is used to parse the optional arguments.
//   ///
//   /// The resulting parser will recognize the `@` symbol, followed by the name and optional arguments,
//   /// constructing a `Directive` instance with the parsed components.
//   pub fn parser_with<'a, I, T, Error, E, NP, AP>(
//     name_parser: NP,
//     args_parser: AP,
//   ) -> impl Parser<'a, I, Self, E> + Clone
//   where
//     T: Token<'a>,
//     <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
//     I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     Error: 'a,
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//     Args: Parseable<'a, I, T, Error>,
//     At: Parseable<'a, I, T, Error>,
//     NP: Parser<'a, I, Name, E> + Clone,
//     AP: Parser<'a, I, Args, E> + Clone,
//   {
//     At::parser()
//       .ignore_then(name_parser)
//       .then(args_parser.or_not())
//       .map_with(|(name, arguments), exa| {
//         let mut builder = rowan::GreenNodeBuilder::new();
//         builder.start_node(kind);
//         builder.token(kind, text);
//         Self {
//           span: exa.span(),
//           node: SyntaxNode::new_root(builder.finish()),
//           _name: PhantomData,
//           _args: PhantomData,
//         }
//       })
//   }
// }

// impl<'a, Name, Args, Lang, I, T, Error> Parseable<'a, I, T, Error> for Directive<Name, Args, Lang>
// where
//   Args: Parseable<'a, I, T, Error>,
//   At: Parseable<'a, I, T, Error>,
//   Name: Parseable<'a, I, T, Error>,
//   Lang: Language,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
//   where
//     Self: Sized + 'a,
//     E: ParserExtra<'a, I, Error = Error> + 'a,
//     T: Token<'a>,
//     I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     Error: 'a,
//   {
//     Self::parser_with(Name::parser(), Args::parser())
//   }
// }


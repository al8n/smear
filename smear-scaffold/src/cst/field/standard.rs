use logosky::cst::Node;
use rowan::{Language, SyntaxNode, TextRange};

use super::super::{Field, FragmentSpread, InlineFragment, SelectionSet};

/// A standard field in a GraphQL selection set.
///
/// This is a type alias for a Field that uses StandardSelectionSet.
pub type StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang> =
  Field<
    Alias,
    Name,
    Arguments,
    Directives,
    StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>,
    Lang,
  >;

/// A standard selection set in GraphQL.
///
/// This is a type alias for a SelectionSet that contains StandardSelection.
pub type StandardSelectionSet<
  Alias,
  Name,
  FragmentName,
  TypeCondition,
  Arguments,
  Directives,
  Lang,
> = SelectionSet<
  StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>,
  Lang,
>;

/// Represents a standard selection in a GraphQL selection set.
///
/// A selection can be one of:
/// - A field selection
/// - A fragment spread
/// - An inline fragment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>
where
  Lang: Language,
{
  /// A field selection.
  Field(StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>),
  /// A fragment spread selection.
  FragmentSpread(FragmentSpread<FragmentName, Directives, Lang>),
  /// An inline fragment selection.
  InlineFragment(
    InlineFragment<
      TypeCondition,
      Directives,
      StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>,
      Lang,
    >,
  ),
}

impl<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>
  StandardSelection<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>
where
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: Node<Language = Lang>,
{
  /// Tries to create a `StandardSelection` from the given syntax node.
  pub fn try_new(
    syntax: SyntaxNode<Lang>,
  ) -> Result<Self, super::super::error::SyntaxNodeMismatch<Self>> {
    Self::try_cast(syntax)
  }

  /// Returns the source span of the selection.
  pub fn span(&self) -> TextRange {
    self.syntax().text_range()
  }

  /// Returns the syntax node of the selection.
  pub fn syntax(&self) -> &SyntaxNode<Lang> {
    match self {
      Self::Field(f) => f.syntax(),
      Self::FragmentSpread(fs) => fs.syntax(),
      Self::InlineFragment(iff) => iff.syntax(),
    }
  }
}

// impl<
//   'a,
//   Alias: 'a,
//   Name: 'a,
//   FragmentName: 'a,
//   TypeCondition: 'a,
//   Arguments: 'a,
//   Directives: 'a,
//   Lang,
//   I,
//   T,
//   Error,
// > Parseable<'a, I, T, Error>
//   for StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>
// where
//   On<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   Spread<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   LBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   RBrace<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   TypeCondition: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   Alias: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   Name: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   FragmentName: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   Arguments: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   Directives: Parseable<'a, I, T, Error, Language = Lang> + 'a,
//   InlineFragment<TypeCondition, Directives, StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>, Lang>: Node<Language = Lang> + 'a,
//   FragmentSpread<FragmentName, Directives, Lang>: Node<Language = Lang> + 'a,
//   StandardSelectionSet<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>: Node<Language = Lang> + 'a,
//   StandardField<Alias, Name, FragmentName, TypeCondition, Arguments, Directives, Lang>: Node<Language = Lang> + 'a,
//   Lang: Language,
//   Lang::Kind: Into<rowan::SyntaxKind>,
// {
//   type Language = Lang;

//   fn parser<E>(
//     builder: &'a SyntaxTreeBuilder<Self::Language>,
//   ) -> impl chumsky::Parser<'a, I, (), E> + Clone
//   where
//     I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
//     T: LosslessToken<'a>,
//     <<T>::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
//     Error: 'a,
//     E: chumsky::extra::ParserExtra<'a, I, Error = Error> + 'a {
//     recursive(|field_parser| {
//       // Inner fixpoint: build a `StandardSelection<Span>` parser by using the recursive `field_parser`.
//       let selection = recursive(|selection| {
//         // StandardSelectionSet needs a `StandardSelection` parser
//         let selection_set = crate::cst::StandardSelectionSet::parser_with(builder, selection.clone());

//         let spread = FragmentSpread::parser(builder).map(|fs| StandardSelection::FragmentSpread(fs));

//         let inline = crate::cst::InlineFragment::parser_with(
//           builder,
//           TypeCondition::parser,
//           Directives::parser,
//           selection_set.clone(),
//         )
//         .map(|f| StandardSelection::InlineFragment(f));

//         choice((field_parser.map(StandardSelection::Field), spread, inline))
//       });

//       // Pass the selection parser to the selection set
//       let selection_set = crate::cst::StandardSelectionSet::parser_with(builder, selection);

//       crate::cst::Field::parser_with(Arguments::parser, Directives::parser, selection_set).map(Self)
//     })
//   }
// }

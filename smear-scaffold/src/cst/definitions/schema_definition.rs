use logosky::{
  Logos, LosslessToken, Source, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  cst::{CstElement, CstNode, Parseable, SyntaxTreeBuilder, error::SyntaxError},
};
use rowan::{Language, SyntaxNode, SyntaxToken, TextRange};

use smear_lexer::keywords::Schema;

/// Represents a schema definition in the CST.
#[derive(Debug, Clone)]
pub struct SchemaDefinition<Directives, RootOperations, Lang>
where
  Lang: Language,
  RootOperations: CstNode<Language = Lang>,
{
  syntax: SyntaxNode<Lang>,
  schema_kw: Schema<TextRange, SyntaxToken<Lang>>,
  directives: Option<Directives>,
  operations: RootOperations,
}

impl<Directives, RootOperations, Lang> SchemaDefinition<Directives, RootOperations, Lang>
where
  Lang: Language,
  RootOperations: CstNode<Language = Lang>,
{
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(in crate::cst) const fn new(
    syntax: SyntaxNode<Lang>,
    schema_kw: Schema<TextRange, SyntaxToken<Lang>>,
    directives: Option<Directives>,
    operations: RootOperations,
  ) -> Self {
    Self {
      syntax,
      schema_kw,
      directives,
      operations,
    }
  }

  #[inline]
  pub fn try_new(syntax: SyntaxNode<Lang>) -> Result<Self, SyntaxError<Self>>
  where
    Self: CstNode<Language = Lang>,
  {
    Self::try_cast_node(syntax)
  }

  #[inline]
  pub fn span(&self) -> TextRange {
    self.syntax.text_range()
  }

  #[inline]
  pub const fn schema_keyword(&self) -> &Schema<TextRange, SyntaxToken<Lang>> {
    &self.schema_kw
  }

  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  #[inline]
  pub const fn root_operation_types(&self) -> &RootOperations {
    &self.operations
  }
}

impl<'a, Directives, RootOperations, Lang, I, T, Error> Parseable<'a, I, T, Error>
  for SchemaDefinition<Directives, RootOperations, Lang>
where
  Schema<TextRange, SyntaxToken<Lang>>: Parseable<'a, I, T, Error, Language = Lang>,
  Directives: Parseable<'a, I, T, Error, Language = Lang>,
  RootOperations: Parseable<'a, I, T, Error, Language = Lang> + CstNode<Language = Lang>,
  Lang: Language,
  Lang::Kind: Into<rowan::SyntaxKind>,
  Self: CstNode<Language = Lang>,
{
  type Language = Lang;

  #[inline]
  fn parser<E>(builder: &'a SyntaxTreeBuilder<Self::Language>) -> impl Parser<'a, I, (), E> + Clone
  where
    I: Tokenizer<'a, T, Slice = <<<T>::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    T: LosslessToken<'a>,
    <T::Logos as Logos<'a>>::Source: Source<Slice<'a> = &'a str>,
    Error: 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
  {
    builder.start_node(Self::KIND);
    Schema::parser(builder)
      .ignore_then(Directives::parser(builder).or_not())
      .ignore_then(RootOperations::parser(builder))
      .map(|_| {
        builder.finish_node();
      })
  }
}

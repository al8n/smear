//! Complete GraphQLx document productions.
//!
//! The document dispatcher classifies every top-level head once, then enters
//! the selected committed parser tail.  It combines GraphQL executable and
//! type-system definitions with GraphQLx imports, generic headers, qualified
//! paths, `where` clauses, and type-system extensions.

use std::vec::Vec;

use smear_lexer::graphqlx::{ContextualKeyword, syntactic::SyntacticTokenKind};
use tokora::{
  Accumulator, Branch, Lexer, ParseChoice, ParseInput, SimpleSpan, Slice, Source, Token,
  cache::{Peeked, PeekedTokenExt},
  error::{Unclosed, UnexpectedEot, token::UnexpectedToken},
  parser::Action,
  punct::{Angle, Brace, Bracket, Paren},
  span::Spanned,
  utils::{DowncastRef, typenum::U1},
};

use super::{
  GraphqlxError, GraphqlxInput, GraphqlxLexer, GraphqlxSlice, GraphqlxToken,
  definition::{type_system_definition_after_keyword, type_system_extension_after_extend},
  executable::{ExecutableDefinitionHead, OperationHead, executable_definition_after_head},
  import::import_definition_after_keyword,
  keyword_of, unexpected_here,
};
use crate::{
  combinator::ParseCtx,
  graphqlx::{
    GraphQLx,
    ast::{Definition, Described, Document, ImportOrDefinitionOrExtension, StringValue},
    error::{Expectation, GraphqlxError as DialectGraphqlxError},
  },
};

macro_rules! document_parser {
  ($(#[$meta:meta])* $visibility:vis $name:ident, $input:ident, $output:ty, $body:block) => {
    $(#[$meta])*
    $visibility fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
        + DowncastRef<ContextualKeyword>,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
        + From<
          UnexpectedToken<
            'inp,
            GraphqlxToken<'inp, Src>,
            <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
            SimpleSpan,
            GraphQLx,
          >,
        >
        + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
        + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
        + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
        + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
        + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

/// A pre-classified complete GraphQLx document head.
#[derive(Debug, Copy, Clone)]
enum DocumentHead {
  Description,
  Import,
  Extension,
  TypeSystem(ContextualKeyword),
  Executable(ExecutableDefinitionHead),
}

#[inline]
fn is_type_system_keyword(keyword: ContextualKeyword) -> bool {
  matches!(
    keyword,
    ContextualKeyword::Schema
      | ContextualKeyword::Directive
      | ContextualKeyword::Scalar
      | ContextualKeyword::Type
      | ContextualKeyword::Interface
      | ContextualKeyword::Union
      | ContextualKeyword::Enum
      | ContextualKeyword::Input
  )
}

/// Classifies one complete document entry without consuming its head.
#[inline]
fn classify_document_head<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<Option<DocumentHead>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  let mut peeked = inp.peek::<U1>()?;
  Ok(peeked.pop_front().and_then(|token| match token.token() {
    GraphqlxToken::<'inp, Src>::LitInlineStr(_) | GraphqlxToken::<'inp, Src>::LitBlockStr(_) => {
      Some(DocumentHead::Description)
    }
    GraphqlxToken::<'inp, Src>::LBrace => Some(DocumentHead::Executable(
      ExecutableDefinitionHead::Shorthand,
    )),
    token => match keyword_of(token) {
      Some(ContextualKeyword::Import) => Some(DocumentHead::Import),
      Some(ContextualKeyword::Extend) => Some(DocumentHead::Extension),
      Some(keyword) if is_type_system_keyword(keyword) => Some(DocumentHead::TypeSystem(keyword)),
      Some(ContextualKeyword::Query) => Some(DocumentHead::Executable(
        ExecutableDefinitionHead::Operation(OperationHead::Query),
      )),
      Some(ContextualKeyword::Mutation) => Some(DocumentHead::Executable(
        ExecutableDefinitionHead::Operation(OperationHead::Mutation),
      )),
      Some(ContextualKeyword::Subscription) => Some(DocumentHead::Executable(
        ExecutableDefinitionHead::Operation(OperationHead::Subscription),
      )),
      Some(ContextualKeyword::Fragment) => {
        Some(DocumentHead::Executable(ExecutableDefinitionHead::Fragment))
      }
      _ => None,
    },
  }))
}

/// Consumes a string literal after deterministic document-head dispatch.
#[inline]
fn take_classified_description<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<StringValue<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitInlineStr(value),
    }) => Ok(StringValue::new(span, value.into())),
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::LitBlockStr(value),
    }) => Ok(StringValue::new(span, value.into())),
    Some(Spanned { span, data: token }) => Err(
      DialectGraphqlxError::unexpected_token(token.kind(), Expectation::StringValue, span).into(),
    ),
    None => unexpected_here(inp, Expectation::StringValue),
  }
}

/// Consumes an identifier after deterministic document-head dispatch.
#[inline]
fn take_classified_identifier<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  expected: Expectation,
) -> Result<SimpleSpan, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match inp.next()? {
    Some(Spanned {
      span,
      data: GraphqlxToken::<'inp, Src>::Identifier(_),
    }) => Ok(span),
    Some(Spanned { span, data: token }) => {
      Err(DialectGraphqlxError::unexpected_token(token.kind(), expected, span).into())
    }
    None => unexpected_here(inp, expected),
  }
}

#[inline]
fn described_definition<S>(
  description: Option<StringValue<S>>,
  definition: Definition<S>,
) -> Described<Definition<S>, S> {
  let start = description.as_ref().map_or_else(
    || definition.span().start(),
    |description| description.span().start(),
  );
  let end = definition.span().end();
  Described::new(SimpleSpan::new(start, end), description, definition)
}

/// Enters a complete document definition after a leading description.
///
/// A description can only decorate an executable or type-system definition;
/// GraphQLx imports and extensions deliberately remain undescribed.
fn entry_after_description<'inp, Src, Ctx>(
  description: StringValue<GraphqlxSlice<'inp, Src>>,
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<ImportOrDefinitionOrExtension<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlxToken<'inp, Src>,
        <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQLx,
      >,
    > + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
    + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
    + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
    + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
    + From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let definition = match classify_document_head(inp)? {
    Some(DocumentHead::TypeSystem(keyword)) => {
      let span = take_classified_identifier(inp, Expectation::Keyword("type-system definition"))?;
      type_system_definition_after_keyword(inp, keyword, span.start())
        .map(Definition::TypeSystem)?
    }
    Some(DocumentHead::Executable(head)) => {
      executable_definition_after_head(head, inp).map(Definition::Executable)?
    }
    _ => return unexpected_here(inp, Expectation::Keyword("definition after description")),
  };
  Ok(ImportOrDefinitionOrExtension::Definition(
    described_definition(Some(description), definition),
  ))
}

document_parser!(
  /// Parses one complete GraphQLx top-level entry with fused head dispatch.
  ///
  /// Imports and type-system extensions are GraphQLx additions. Definitions
  /// retain the standard GraphQL executable or type-system shape, while a
  /// leading description may decorate either definition family.
  ///
  /// See [GraphQL Document](https://spec.graphql.org/draft/#Document),
  /// [Executable Definition](https://spec.graphql.org/draft/#ExecutableDefinition),
  /// and [Type System Definition](https://spec.graphql.org/draft/#TypeSystemDefinition).
  pub import_or_definition_or_extension,
  inp,
  ImportOrDefinitionOrExtension<GraphqlxSlice<'inp, Src>>,
  {
    let mut type_system_keyword = None;
    let mut executable_head = None;
    let branch: Branch<4> = match classify_document_head(inp)? {
      Some(DocumentHead::Description) => Branch::B0,
      Some(DocumentHead::Import) => Branch::B1,
      Some(DocumentHead::Extension) => Branch::B2,
      Some(DocumentHead::TypeSystem(keyword)) => {
        type_system_keyword = Some(keyword);
        Branch::B3
      }
      Some(DocumentHead::Executable(head)) => {
        executable_head = Some(head);
        Branch::B4
      }
      None => return unexpected_here(inp, Expectation::Keyword("document entry")),
    };

    let mut tails = (
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        take_classified_description(inp).and_then(|description| entry_after_description(description, inp))
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let span = take_classified_identifier(inp, Expectation::Import)?;
        import_definition_after_keyword(inp, span.start()).map(ImportOrDefinitionOrExtension::Import)
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let span = take_classified_identifier(inp, Expectation::Keyword("extend"))?;
        type_system_extension_after_extend(inp, span.start())
          .map(ImportOrDefinitionOrExtension::Extension)
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let keyword = type_system_keyword
          .expect("type-system document branch retains its classified keyword");
        let span = take_classified_identifier(inp, Expectation::Keyword("type-system definition"))?;
        type_system_definition_after_keyword(inp, keyword, span.start()).map(|definition| {
          ImportOrDefinitionOrExtension::Definition(described_definition(
            None,
            Definition::TypeSystem(definition),
          ))
        })
      },
      |inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
        let head = executable_head.expect("executable document branch retains its classified head");
        executable_definition_after_head(head, inp).map(|definition| {
          ImportOrDefinitionOrExtension::Definition(described_definition(
            None,
            Definition::Executable(definition),
          ))
        })
      },
    );
    tails.parse_choice(inp, &branch)
  }
);

fn decide_document_tail<'inp, Src, Ctx>(
  mut peeked: Peeked<'_, 'inp, GraphqlxLexer<'inp, Src>, U1>,
  _: &mut Ctx::Emitter,
) -> Result<Action, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
{
  Ok(if peeked.pop_front().is_some() {
    Action::Continue
  } else {
    Action::Stop
  })
}

document_parser!(
  /// Parses a nonempty complete GraphQLx document.
  ///
  /// The root accepts imports, executable definitions, type-system
  /// definitions, and type-system extensions in source order. Imports,
  /// generic syntax, qualified paths, and `where` clauses are GraphQLx
  /// extensions; standard definition forms follow GraphQL's document grammar.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  pub document,
  inp,
  Document<GraphqlxSlice<'inp, Src>>,
  {
    let Spanned { span, data: entries }: Spanned<
      Vec<ImportOrDefinitionOrExtension<GraphqlxSlice<'inp, Src>>>,
      SimpleSpan,
    > = import_or_definition_or_extension
      .repeated_while::<_, U1>(decide_document_tail::<Src, Ctx>)
      .at_least(1)
      .collect_with(Vec::new())
      .map(|entries: Vec<ImportOrDefinitionOrExtension<GraphqlxSlice<'inp, Src>>>| entries)
      .spanned()
      .parse_input(inp)?;
    Ok(Document::new(span, entries))
  }
);

macro_rules! impl_document_api {
  ($(#[$meta:meta])* $slice:ident, $node:ty, $parser:ident) => {
    impl<$slice> $node {
      $(#[$meta])*
      /// The lexer source is inferred from `inp`.
      pub fn graphqlx<'inp, Src, Ctx>(
        inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      ) -> Result<Self, GraphqlxError<'inp, Src, Ctx>>
      where
        Src: Source<usize, Slice<'inp> = $slice> + ?Sized,
        $slice: Slice<'inp> + Clone + 'inp,
        GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind>
          + DowncastRef<ContextualKeyword>,
        GraphqlxLexer<'inp, Src>: Lexer<
          'inp,
          Source = Src,
          Token = GraphqlxToken<'inp, Src>,
          Span = SimpleSpan,
          Offset = usize,
        >,
        Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
        GraphqlxError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQLx>>
          + From<
            UnexpectedToken<
              'inp,
              GraphqlxToken<'inp, Src>,
              <GraphqlxToken<'inp, Src> as Token<'inp>>::Kind,
              SimpleSpan,
              GraphQLx,
            >,
          >
          + From<Unclosed<Paren, SimpleSpan, GraphQLx>>
          + From<Unclosed<Bracket, SimpleSpan, GraphQLx>>
          + From<Unclosed<Brace, SimpleSpan, GraphQLx>>
          + From<Unclosed<Angle, SimpleSpan, GraphQLx>>
          + From<DialectGraphqlxError<$slice>>,
      {
        $parser(inp)
      }
    }
  };
}

impl_document_api!(
  /// Parses one complete GraphQLx document entry.
  ///
  /// Imports and extensions are GraphQLx-only entries; executable and
  /// type-system definitions retain their corresponding GraphQL grammar.
  ///
  /// See [GraphQL Document](https://spec.graphql.org/draft/#Document),
  /// [Executable Definition](https://spec.graphql.org/draft/#ExecutableDefinition),
  /// and [Type System Definition](https://spec.graphql.org/draft/#TypeSystemDefinition).
  S,
  ImportOrDefinitionOrExtension<S>,
  import_or_definition_or_extension
);
impl_document_api!(
  /// Parses a nonempty complete GraphQLx document.
  ///
  /// GraphQLx adds imports, type-system extensions, generic syntax, qualified
  /// paths, and `where` clauses; no separate GraphQL specification anchor
  /// exists for those extensions.
  ///
  /// See the [GraphQL Document specification](https://spec.graphql.org/draft/#Document).
  S,
  Document<S>,
  document
);

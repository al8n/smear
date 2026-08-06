//! GraphQLx type-system extension productions.

use super::*;
use crate::parser::type_system::{
  EnumTypeExtensionData, InputObjectTypeExtensionData, InterfaceTypeExtensionData,
  ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

macro_rules! extension_tail {
  ($name:ident, $output:ty, [contextual], |$input:ident, $start:ident| $body:block) => {
    extension_tail!(
      @impl
      $name,
      $output,
      $input,
      $start,
      [GraphqlxToken<'inp, Src>: DowncastRef<ContextualKeyword>,],
      $body
    );
  };
  (@impl $name:ident, $output:ty, $input:ident, $start:ident, [$($bounds:tt)*], $body:block) => {
    fn $name<'inp, Src, Ctx>(
      $input: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
      $start: usize,
    ) -> Result<$output, GraphqlxError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
      GraphqlxLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlxToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
      $($bounds)*
      GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
    $body
  };
}

extension_tail!(
  scalar_extension_after_keyword,
  ScalarTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let directives = match optional_const_directives(inp)? {
      Some(directives) => directives,
      None => return expected_definition_phase(inp, Expectation::At),
    };
    Ok(ScalarTypeExtension::new(
      SimpleSpan::new(start, directives.span().end()),
      name,
      directives,
    ))
  }
);

fn constrained_extension_fields<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<
    Constrained<FieldsDefinition<GraphqlxSlice<'inp, Src>>, WhereClause<GraphqlxSlice<'inp, Src>>>,
  >,
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  match (where_clause, try_fields_definition(inp)?) {
    (Some(where_clause), ParseAttempt::Accept(fields)) => Ok(Some(Constrained::new(
      SimpleSpan::new(where_clause.span().start(), fields.span().end()),
      Some(where_clause),
      fields,
    ))),
    (Some(_), ParseAttempt::Decline) => expected_definition_phase(inp, Expectation::LBrace),
    (None, ParseAttempt::Accept(fields)) => {
      let span = *fields.span();
      Ok(Some(Constrained::new(span, None, fields)))
    }
    (None, ParseAttempt::Decline) => Ok(None),
  }
}

extension_tail!(
  object_extension_after_keyword,
  ObjectTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let implements: Option<ImplementInterfaces<GraphqlxSlice<'inp, Src>>> =
      try_implements(inp)?.into();
    let directives = optional_const_directives(inp)?;
    let fields_definition = constrained_extension_fields(inp)?;
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives.as_ref().map_or_else(
          || {
            implements
              .as_ref()
              .map_or(start, |implements| implements.span().end())
          },
          |directives| directives.span().end(),
        )
      },
      |fields| fields.span().end(),
    );
    let data = match (implements, directives, fields_definition) {
      (implements, directives, Some(fields_definition)) => ObjectTypeExtensionData::Fields {
        implements,
        directives,
        fields_definition,
      },
      (implements, Some(directives), None) => ObjectTypeExtensionData::Directives {
        implements,
        directives,
      },
      (Some(implements), None, None) => ObjectTypeExtensionData::Implements(implements),
      (None, None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("implements, directives, or fields definition"),
        );
      }
    };
    Ok(ObjectTypeExtension::new(
      SimpleSpan::new(start, end),
      name,
      data,
    ))
  }
);

extension_tail!(
  interface_extension_after_keyword,
  InterfaceTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let implements: Option<ImplementInterfaces<GraphqlxSlice<'inp, Src>>> =
      try_implements(inp)?.into();
    let directives = optional_const_directives(inp)?;
    let fields_definition = constrained_extension_fields(inp)?;
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives.as_ref().map_or_else(
          || {
            implements
              .as_ref()
              .map_or(start, |implements| implements.span().end())
          },
          |directives| directives.span().end(),
        )
      },
      |fields| fields.span().end(),
    );
    let data = match (implements, directives, fields_definition) {
      (implements, directives, Some(fields_definition)) => InterfaceTypeExtensionData::Fields {
        implements,
        directives,
        fields_definition,
      },
      (implements, Some(directives), None) => InterfaceTypeExtensionData::Directives {
        implements,
        directives,
      },
      (Some(implements), None, None) => InterfaceTypeExtensionData::Implements(implements),
      (None, None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("implements, directives, or fields definition"),
        );
      }
    };
    Ok(InterfaceTypeExtension::new(
      SimpleSpan::new(start, end),
      name,
      data,
    ))
  }
);

fn constrained_extension_members<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<
    Constrained<UnionMemberTypes<GraphqlxSlice<'inp, Src>>, WhereClause<GraphqlxSlice<'inp, Src>>>,
  >,
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let members = try_union_members(inp)?;
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => {
      if matches!(&members, ParseAttempt::Decline) {
        return expected_definition_phase(inp, Expectation::Equal);
      }
      Some(where_clause)
    }
    ParseAttempt::Decline => None,
  };
  match (members, where_clause) {
    (ParseAttempt::Accept(members), Some(where_clause)) => Ok(Some(Constrained::new(
      SimpleSpan::new(members.span().start(), where_clause.span().end()),
      Some(where_clause),
      members,
    ))),
    (ParseAttempt::Accept(members), None) => {
      let span = *members.span();
      Ok(Some(Constrained::new(span, None, members)))
    }
    (ParseAttempt::Decline, None) => Ok(None),
    (ParseAttempt::Decline, Some(_)) => unreachable!("a where clause requires union members"),
  }
}

extension_tail!(
  union_extension_after_keyword,
  UnionTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let member_types = constrained_extension_members(inp)?;
    let end = member_types.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |directives| directives.span().end())
      },
      |members| members.span().end(),
    );
    let data = match (directives, member_types) {
      (directives, Some(member_types)) => UnionTypeExtensionData::Members {
        directives,
        member_types,
      },
      (Some(directives), None) => UnionTypeExtensionData::Directives(directives),
      (None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("directives or union member types"),
        );
      }
    };
    Ok(UnionTypeExtension::new(
      SimpleSpan::new(start, end),
      name,
      data,
    ))
  }
);

extension_tail!(
  enum_extension_after_keyword,
  EnumTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let enum_values_definition: Option<EnumValuesDefinition<GraphqlxSlice<'inp, Src>>> =
      try_enum_values_definition(inp)?.into();
    let end = enum_values_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |directives| directives.span().end())
      },
      |values| values.span().end(),
    );
    let data = match (directives, enum_values_definition) {
      (directives, Some(enum_values_definition)) => EnumTypeExtensionData::Values {
        directives,
        enum_values_definition,
      },
      (Some(directives), None) => EnumTypeExtensionData::Directives(directives),
      (None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("directives or enum values definition"),
        );
      }
    };
    Ok(EnumTypeExtension::new(
      SimpleSpan::new(start, end),
      name,
      data,
    ))
  }
);

fn constrained_extension_input_fields<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<
    Constrained<
      InputFieldsDefinition<GraphqlxSlice<'inp, Src>>,
      WhereClause<GraphqlxSlice<'inp, Src>>,
    >,
  >,
  GraphqlxError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let where_clause = match try_where_clause(inp)? {
    ParseAttempt::Accept(where_clause) => Some(where_clause),
    ParseAttempt::Decline => None,
  };
  match (where_clause, try_input_fields_definition(inp)?) {
    (Some(where_clause), ParseAttempt::Accept(fields)) => Ok(Some(Constrained::new(
      SimpleSpan::new(where_clause.span().start(), fields.span().end()),
      Some(where_clause),
      fields,
    ))),
    (Some(_), ParseAttempt::Decline) => expected_definition_phase(inp, Expectation::LBrace),
    (None, ParseAttempt::Accept(fields)) => {
      let span = *fields.span();
      Ok(Some(Constrained::new(span, None, fields)))
    }
    (None, ParseAttempt::Decline) => Ok(None),
  }
}

extension_tail!(
  input_object_extension_after_keyword,
  InputObjectTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let name = extension_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let fields_definition = constrained_extension_input_fields(inp)?;
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |directives| directives.span().end())
      },
      |fields| fields.span().end(),
    );
    let data = match (directives, fields_definition) {
      (directives, Some(fields_definition)) => InputObjectTypeExtensionData::Fields {
        directives,
        fields_definition,
      },
      (Some(directives), None) => InputObjectTypeExtensionData::Directives(directives),
      (None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("directives or input fields definition"),
        );
      }
    };
    Ok(InputObjectTypeExtension::new(
      SimpleSpan::new(start, end),
      name,
      data,
    ))
  }
);

extension_tail!(
  schema_extension_after_keyword,
  SchemaExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  |inp, start| {
    let directives = optional_const_directives(inp)?;
    let root_operation_types_definition: Option<
      RootOperationTypesDefinition<GraphqlxSlice<'inp, Src>>,
    > = super::schema::try_root_operation_types_definition(inp)?.into();
    let end = root_operation_types_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |directives| directives.span().end())
      },
      |operations| operations.span().end(),
    );
    let data = match (directives, root_operation_types_definition) {
      (directives, Some(root_operation_types_definition)) => SchemaExtensionData::Operations {
        directives,
        root_operation_types_definition,
      },
      (Some(directives), None) => SchemaExtensionData::Directives(directives),
      (None, None) => {
        return expected_definition_phase(
          inp,
          Expectation::Keyword("directives or root operation types definition"),
        );
      }
    };
    Ok(SchemaExtension::new(SimpleSpan::new(start, end), data))
  }
);

/// Enters a named extension tail after both `extend` and its shape keyword
/// were consumed by fused dispatch.
pub(super) fn type_extension_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeExtension<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  match keyword {
    ContextualKeyword::Scalar => {
      scalar_extension_after_keyword(inp, start).map(TypeExtension::Scalar)
    }
    ContextualKeyword::Type => {
      object_extension_after_keyword(inp, start).map(TypeExtension::Object)
    }
    ContextualKeyword::Interface => {
      interface_extension_after_keyword(inp, start).map(TypeExtension::Interface)
    }
    ContextualKeyword::Union => union_extension_after_keyword(inp, start).map(TypeExtension::Union),
    ContextualKeyword::Enum => enum_extension_after_keyword(inp, start).map(TypeExtension::Enum),
    ContextualKeyword::Input => {
      input_object_extension_after_keyword(inp, start).map(TypeExtension::InputObject)
    }
    _ => expected_definition_phase(inp, Expectation::Keyword("type extension")),
  }
}

/// Consumes and classifies the extension shape after an already consumed
/// `extend` keyword.
pub(super) fn type_extension_after_extend<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<TypeExtension<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let identifier_head =
    |Spanned { span, data: token }: Spanned<GraphqlxToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
      let kind = token.kind();
      let keyword = keyword_of(&token);
      match token {
        GraphqlxToken::<'inp, Src>::Identifier(_) => match keyword {
          Some(
            keyword @ (ContextualKeyword::Scalar
            | ContextualKeyword::Type
            | ContextualKeyword::Interface
            | ContextualKeyword::Union
            | ContextualKeyword::Enum
            | ContextualKeyword::Input),
          ) => type_extension_after_keyword(inp, keyword, start),
          _ => Err(
            DialectGraphqlxError::unexpected_token(
              kind,
              Expectation::Keyword("type extension"),
              span,
            )
            .into(),
          ),
        },
        _ => unreachable!("fused GraphQLx type-extension arm received a non-identifier token"),
      }
    };
  match (identifier_head,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(extension) => Ok(extension),
    ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("type extension")),
  }
}

/// Enters a type-system-extension tail after a fused dispatcher consumed
/// `extend` exactly once.
pub(crate) fn type_system_extension_after_extend<'inp, Src, Ctx>(
  inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<TypeSystemExtension<GraphqlxSlice<'inp, Src>>, GraphqlxError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlxSlice<'inp, Src>: Slice<'inp> + Clone + 'inp,
  GraphqlxToken<'inp, Src>: Token<'inp, Kind = SyntacticTokenKind> + DowncastRef<ContextualKeyword>,
  GraphqlxLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlxToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: ParseCtx<'inp, GraphqlxLexer<'inp, Src>, GraphQLx>,
  GraphqlxError<'inp, Src, Ctx>: From<DialectGraphqlxError<GraphqlxSlice<'inp, Src>>>,
{
  let identifier_head = |Spanned { span, data: token }: Spanned<
    GraphqlxToken<'inp, Src>,
    SimpleSpan,
  >,
                         inp: &mut GraphqlxInput<'inp, '_, Src, Ctx>| {
    let kind = token.kind();
    let keyword = keyword_of(&token);
    match token {
      GraphqlxToken::<'inp, Src>::Identifier(_) => match keyword {
        Some(ContextualKeyword::Schema) => {
          schema_extension_after_keyword(inp, start).map(TypeSystemExtension::Schema)
        }
        Some(
          keyword @ (ContextualKeyword::Scalar
          | ContextualKeyword::Type
          | ContextualKeyword::Interface
          | ContextualKeyword::Union
          | ContextualKeyword::Enum
          | ContextualKeyword::Input),
        ) => type_extension_after_keyword(inp, keyword, start).map(TypeSystemExtension::Type),
        _ => Err(
          DialectGraphqlxError::unexpected_token(
            kind,
            Expectation::Keyword("type-system extension"),
            span,
          )
          .into(),
        ),
      },
      _ => unreachable!("fused GraphQLx type-system-extension arm received a non-identifier token"),
    }
  };
  match (identifier_head,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(extension) => Ok(extension),
    ParseAttempt::Decline => {
      expected_definition_phase(inp, Expectation::Keyword("type-system extension"))
    }
  }
}

definition_parser!(
  /// Parses a GraphQLx scalar type extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub scalar_type_extension,
  inp,
  ScalarTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Scalar)?;
    scalar_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx object type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub object_type_extension,
  inp,
  ObjectTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Type)?;
    object_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx interface type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub interface_type_extension,
  inp,
  InterfaceTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Interface)?;
    interface_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx union type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub union_type_extension,
  inp,
  UnionTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Union)?;
    union_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx enum type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub enum_type_extension,
  inp,
  EnumTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Enum)?;
    enum_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx input object type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub input_object_type_extension,
  inp,
  InputObjectTypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Input)?;
    input_object_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx schema extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub schema_extension,
  inp,
  SchemaExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Schema)?;
    schema_extension_after_keyword(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx named type extension with fused single-consumption dispatch.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub type_extension,
  inp,
  TypeExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    type_extension_after_extend(inp, start)
  }
);
definition_parser!(
  /// Parses a GraphQLx type-system extension with fused single-consumption dispatch.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub type_system_extension,
  inp,
  TypeSystemExtension<GraphqlxSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    type_system_extension_after_extend(inp, start)
  }
);

impl_definition_api!(
  /// Parses a GraphQLx scalar type extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  ScalarTypeExtension<S>,
  scalar_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx object type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  ObjectTypeExtension<S>,
  object_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx interface type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  InterfaceTypeExtension<S>,
  interface_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx union type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  UnionTypeExtension<S>,
  union_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx enum type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  EnumTypeExtension<S>,
  enum_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx input object type extension.
  ///
  /// GraphQLx adds generic arguments, qualified paths, and `where` clauses.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  InputObjectTypeExtension<S>,
  input_object_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx schema extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  SchemaExtension<S>,
  schema_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx named type extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  TypeExtension<S>,
  type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a GraphQLx type-system extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  TypeSystemExtension<S>,
  type_system_extension,
  [contextual]
);

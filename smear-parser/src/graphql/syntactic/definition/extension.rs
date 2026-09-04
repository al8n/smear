//! GraphQL SDL type-system extension productions.

use super::*;

use crate::type_system::{
  EnumTypeExtensionData, InputObjectTypeExtensionData, InterfaceTypeExtensionData,
  ObjectTypeExtensionData, SchemaExtensionData, UnionTypeExtensionData,
};

macro_rules! extension_tail {
  ($name:ident, $inp:ident, $start:ident, $output:ty, $body:block) => {
    fn $name<'inp, Src, Ctx>(
      $inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
      $start: usize,
    ) -> Result<$output, GraphqlError<'inp, Src, Ctx>>
    where
      Src: Source<usize> + ?Sized,
      GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
      GraphqlLexer<'inp, Src>: Lexer<
        'inp,
        Source = Src,
        Token = GraphqlToken<'inp, Src>,
        Span = SimpleSpan,
        Offset = usize,
      >,
      GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
      Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
      GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
    $body
  };
}

extension_tail!(
  scalar_extension_after_keyword,
  inp,
  start,
  ScalarTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
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

extension_tail!(
  object_extension_after_keyword,
  inp,
  start,
  ObjectTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
    let implements = match try_implements(inp)? {
      ParseAttempt::Accept(implements) => Some(implements),
      ParseAttempt::Decline => None,
    };
    let directives = optional_const_directives(inp)?;
    let fields_definition = match try_fields_definition(inp)? {
      ParseAttempt::Accept(fields) => Some(fields),
      ParseAttempt::Decline => None,
    };
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives.as_ref().map_or_else(
          || {
            implements
              .as_ref()
              .map_or(start, |value| value.span().end())
          },
          |value| value.span().end(),
        )
      },
      |value| value.span().end(),
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
  inp,
  start,
  InterfaceTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
    let implements = match try_implements(inp)? {
      ParseAttempt::Accept(implements) => Some(implements),
      ParseAttempt::Decline => None,
    };
    let directives = optional_const_directives(inp)?;
    let fields_definition = match try_fields_definition(inp)? {
      ParseAttempt::Accept(fields) => Some(fields),
      ParseAttempt::Decline => None,
    };
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives.as_ref().map_or_else(
          || {
            implements
              .as_ref()
              .map_or(start, |value| value.span().end())
          },
          |value| value.span().end(),
        )
      },
      |value| value.span().end(),
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

extension_tail!(
  union_extension_after_keyword,
  inp,
  start,
  UnionTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let member_types = match try_union_members(inp)? {
      ParseAttempt::Accept(member_types) => Some(member_types),
      ParseAttempt::Decline => None,
    };
    let end = member_types.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |value| value.span().end())
      },
      |value| value.span().end(),
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
  inp,
  start,
  EnumTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let enum_values_definition = match try_enum_values_definition(inp)? {
      ParseAttempt::Accept(values) => Some(values),
      ParseAttempt::Decline => None,
    };
    let end = enum_values_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |value| value.span().end())
      },
      |value| value.span().end(),
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

extension_tail!(
  input_object_extension_after_keyword,
  inp,
  start,
  InputObjectTypeExtension<GraphqlSlice<'inp, Src>>,
  {
    let name = take_name(inp)?;
    let directives = optional_const_directives(inp)?;
    let fields_definition = match try_input_fields_definition(inp)? {
      ParseAttempt::Accept(fields) => Some(fields),
      ParseAttempt::Decline => None,
    };
    let end = fields_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |value| value.span().end())
      },
      |value| value.span().end(),
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
  inp,
  start,
  SchemaExtension<GraphqlSlice<'inp, Src>>,
  {
    let directives = optional_const_directives(inp)?;
    let root_operation_types_definition = match root_operation_types_definition
      .peek_then_try::<_, U1>(decide_lbrace_opener::<Src, Ctx>)
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(definition) => Some(definition),
      ParseAttempt::Decline => None,
    };
    let end = root_operation_types_definition.as_ref().map_or_else(
      || {
        directives
          .as_ref()
          .map_or(start, |value| value.span().end())
      },
      |value| value.span().end(),
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

/// Enters a type-extension tail after both `extend` and its shape keyword were
/// consumed by a fused dispatcher.
pub(super) fn type_extension_after_keyword<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  keyword: ContextualKeyword,
  start: usize,
) -> Result<TypeExtension<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
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

/// Consumes and classifies the type-extension shape following a consumed `extend`.
pub(super) fn type_extension_after_extend<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<TypeExtension<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let identifier_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let kind = token.kind();
      let keyword = token.downcast_ref();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
          Some(
            keyword @ (ContextualKeyword::Scalar
            | ContextualKeyword::Type
            | ContextualKeyword::Interface
            | ContextualKeyword::Union
            | ContextualKeyword::Enum
            | ContextualKeyword::Input),
          ) => type_extension_after_keyword(inp, keyword, start),
          _ => Err(
            DialectGraphqlError::unexpected_token(
              kind,
              Expectation::Keyword("type extension"),
              span,
            )
            .into(),
          ),
        },
        _ => unreachable!("fused type-extension arm received a non-identifier token"),
      }
    };
  match (identifier_head_arm,)
    .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
    .try_parse_input(inp)?
  {
    ParseAttempt::Accept(extension) => Ok(extension),
    ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("type extension")),
  }
}

/// Enters a type-system-extension tail after `extend` was consumed by fused
/// dispatch, consuming and classifying its next identifier exactly once.
pub(crate) fn type_system_extension_after_extend<'inp, Src, Ctx>(
  inp: &mut GraphqlInput<'inp, '_, Src, Ctx>,
  start: usize,
) -> Result<TypeSystemExtension<GraphqlSlice<'inp, Src>>, GraphqlError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlSlice<'inp, Src>: Slice<'inp> + Clone + 'inp + crate::value::Leaf,
  GraphqlLexer<'inp, Src>:
    Lexer<'inp, Source = Src, Token = GraphqlToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  GraphqlToken<'inp, Src>: DowncastRef<ContextualKeyword>,
  Ctx: ParseCtx<'inp, GraphqlLexer<'inp, Src>, GraphQL>,
  GraphqlError<'inp, Src, Ctx>: From<DialectGraphqlError<GraphqlSlice<'inp, Src>>>,
{
  let identifier_head_arm =
    |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
     inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
      let kind = token.kind();
      let keyword = token.downcast_ref();
      match token {
        GraphqlToken::<'inp, Src>::Identifier(_) => match keyword {
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
            DialectGraphqlError::unexpected_token(
              kind,
              Expectation::Keyword("type-system extension"),
              span,
            )
            .into(),
          ),
        },
        _ => unreachable!("fused type-system-extension arm received a non-identifier token"),
      }
    };
  match (identifier_head_arm,)
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
  /// Parses a scalar type extension.
  ///
  /// See the [GraphQL Scalar Type Extension specification](https://spec.graphql.org/draft/#ScalarTypeExtension).
  pub scalar_type_extension,
  inp,
  ScalarTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Scalar)?;
    scalar_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an object type extension.
  ///
  /// See the [GraphQL Object Type Extension specification](https://spec.graphql.org/draft/#ObjectTypeExtension).
  pub object_type_extension,
  inp,
  ObjectTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Type)?;
    object_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an interface type extension.
  ///
  /// See the [GraphQL Interface Type Extension specification](https://spec.graphql.org/draft/#InterfaceTypeExtension).
  pub interface_type_extension,
  inp,
  InterfaceTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Interface)?;
    interface_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses a union type extension.
  ///
  /// See the [GraphQL Union Type Extension specification](https://spec.graphql.org/draft/#UnionTypeExtension).
  pub union_type_extension,
  inp,
  UnionTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Union)?;
    union_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an enum type extension.
  ///
  /// See the [GraphQL Enum Type Extension specification](https://spec.graphql.org/draft/#EnumTypeExtension).
  pub enum_type_extension,
  inp,
  EnumTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Enum)?;
    enum_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses an input object type extension.
  ///
  /// See the [GraphQL Input Object Type Extension specification](https://spec.graphql.org/draft/#InputObjectTypeExtension).
  pub input_object_type_extension,
  inp,
  InputObjectTypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Input)?;
    input_object_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses a schema extension.
  ///
  /// See the [GraphQL Schema Extension specification](https://spec.graphql.org/draft/#SchemaExtension).
  pub schema_extension,
  inp,
  SchemaExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let start = take_contextual_keyword(inp, ContextualKeyword::Extend)?.start();
    take_contextual_keyword(inp, ContextualKeyword::Schema)?;
    schema_extension_after_keyword(inp, start)
  }
);

definition_parser!(
  /// Parses a named type extension with fused single-consumption dispatch.
  ///
  /// See the [GraphQL Type Extension specification](https://spec.graphql.org/draft/#TypeExtension).
  pub type_extension,
  inp,
  TypeExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) if keyword == Some(ContextualKeyword::Extend) => {
            type_extension_after_extend(inp, span.start())
          }
          GraphqlToken::<'inp, Src>::Identifier(_) => Err(
            DialectGraphqlError::unexpected_token(kind, Expectation::Keyword("extend"), span).into(),
          ),
          _ => unreachable!("fused type-extension opener received a non-identifier token"),
        }
      };
    match (identifier_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(extension) => Ok(extension),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("extend")),
    }
  }
);

definition_parser!(
  /// Parses a schema or named type extension with fused single-consumption dispatch.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  pub type_system_extension,
  inp,
  TypeSystemExtension<GraphqlSlice<'inp, Src>>,
  [contextual],
  {
    let identifier_head_arm =
      |Spanned { span, data: token }: Spanned<GraphqlToken<'inp, Src>, SimpleSpan>,
       inp: &mut GraphqlInput<'inp, '_, Src, Ctx>| {
        let kind = token.kind();
        let keyword = token.downcast_ref();
        match token {
          GraphqlToken::<'inp, Src>::Identifier(_) if keyword == Some(ContextualKeyword::Extend) => {
            type_system_extension_after_extend(inp, span.start())
          }
          GraphqlToken::<'inp, Src>::Identifier(_) => Err(
            DialectGraphqlError::unexpected_token(kind, Expectation::Keyword("extend"), span).into(),
          ),
          _ => unreachable!("fused type-system-extension opener received a non-identifier token"),
        }
      };
    match (identifier_head_arm,)
      .fused_dispatch_on_kind(&[SyntacticTokenKind::Identifier])
      .try_parse_input(inp)?
    {
      ParseAttempt::Accept(extension) => Ok(extension),
      ParseAttempt::Decline => expected_definition_phase(inp, Expectation::Keyword("extend")),
    }
  }
);

impl_definition_api!(
  /// Parses a scalar type extension.
  ///
  /// See the [GraphQL Scalar Type Extension specification](https://spec.graphql.org/draft/#ScalarTypeExtension).
  S,
  ScalarTypeExtension<S>,
  scalar_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses an object type extension.
  ///
  /// See the [GraphQL Object Type Extension specification](https://spec.graphql.org/draft/#ObjectTypeExtension).
  S,
  ObjectTypeExtension<S>,
  object_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses an interface type extension.
  ///
  /// See the [GraphQL Interface Type Extension specification](https://spec.graphql.org/draft/#InterfaceTypeExtension).
  S,
  InterfaceTypeExtension<S>,
  interface_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a union type extension.
  ///
  /// See the [GraphQL Union Type Extension specification](https://spec.graphql.org/draft/#UnionTypeExtension).
  S,
  UnionTypeExtension<S>,
  union_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses an enum type extension.
  ///
  /// See the [GraphQL Enum Type Extension specification](https://spec.graphql.org/draft/#EnumTypeExtension).
  S,
  EnumTypeExtension<S>,
  enum_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses an input object type extension.
  ///
  /// See the [GraphQL Input Object Type Extension specification](https://spec.graphql.org/draft/#InputObjectTypeExtension).
  S,
  InputObjectTypeExtension<S>,
  input_object_type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a schema extension.
  ///
  /// See the [GraphQL Schema Extension specification](https://spec.graphql.org/draft/#SchemaExtension).
  S,
  SchemaExtension<S>,
  schema_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a named type extension.
  ///
  /// See the [GraphQL Type Extension specification](https://spec.graphql.org/draft/#TypeExtension).
  S,
  TypeExtension<S>,
  type_extension,
  [contextual]
);
impl_definition_api!(
  /// Parses a schema or named type extension.
  ///
  /// See the [GraphQL Type System Extension specification](https://spec.graphql.org/draft/#TypeSystemExtension).
  S,
  TypeSystemExtension<S>,
  type_system_extension,
  [contextual]
);

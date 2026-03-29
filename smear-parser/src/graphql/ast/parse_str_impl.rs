//! Implementation of [`ParseStr`] for all GraphQL AST types.
//!
//! This module provides the recursive-descent parser that bridges the tokit
//! `Parser::with_parser().parse_str()` API with the GraphQL AST types.

use std::boxed::Box;
use std::vec;
use std::vec::Vec;

use smear_lexer::tokit::{
  FatalContext, InputRef, Parse, Parser,
  SimpleSpan as Span,
  span::Spanned,
};

use smear_scaffold::ast::{self as scaffold};

use crate::lexer::graphql::syntactic::{
  SyntacticLexer, SyntacticToken, SyntacticTokenKind,
};
use crate::graphql::Expectation;

use super::{
  SyntacticTokenError, SyntacticTokenErrors, next_token, ParseStr,
  parse_name, parse_input_value, parse_const_input_value,
  operation_type::parse_operation_type,
  keyword::*,
  location::parse_location,
  default::*,
  value::*,
  name::Name,
  ty::Type,
  fragment::*,
  type_system::*,
};

/// Type alias for the parse context used in ParseStr implementations.
type Ctx<'inp> = FatalContext<'inp, SyntacticLexer<'inp, &'inp str>, SyntacticTokenErrors<&'inp str>>;

/// Type alias for the InputRef used in ParseStr implementations.
type Inp<'inp, 'c> = InputRef<'inp, 'c, SyntacticLexer<'inp, &'inp str>, Ctx<'inp>>;

/// Helper: peek at the next token kind without consuming it.
/// Uses try_expect with a predicate that always returns false to peek without consuming.
fn peek_kind<'inp>(input: &mut Inp<'inp, '_>) -> Option<SyntacticTokenKind> {
  // Use try_expect to peek: the predicate inspects but rejects (returns false)
  // so the token stays unconsumed. We capture the kind in a cell.
  let mut kind = None;
  let _ = input.try_expect(|spanned| {
    kind = Some(spanned.data().kind());
    false // never consume
  });
  kind
}

/// Helper: check if a peeked identifier matches a keyword.
fn peek_keyword<'inp>(input: &mut Inp<'inp, '_>, kw: &str) -> bool {
  let mut found = false;
  let _ = input.try_expect(|spanned| {
    found = matches!(spanned.data(), SyntacticToken::Identifier(s) if *s == kw);
    false // never consume
  });
  found
}

/// Helper: try to consume a specific token kind, return None if not matched.
fn try_token<'inp>(
  input: &mut Inp<'inp, '_>,
  expected: SyntacticTokenKind,
) -> Result<Option<Spanned<SyntacticToken<&'inp str>>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) == Some(expected) {
    Ok(Some(next_token(input)?))
  } else {
    Ok(None)
  }
}

/// Helper: expect and consume a specific token kind.
fn expect_token<'inp>(
  input: &mut Inp<'inp, '_>,
  expected: SyntacticTokenKind,
) -> Result<Spanned<SyntacticToken<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  let tok = next_token(input)?;
  if tok.data().kind() == expected {
    Ok(tok)
  } else {
    Err(SyntacticTokenError::unexpected_token(
      tok.data().clone(),
      Expectation::from(expected),
      tok.span(),
    ).into())
  }
}

// ─── Name ────────────────────────────────────────────────────────────────────

fn p_name<'inp>(input: &mut Inp<'inp, '_>) -> Result<Name<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_name(input)
}

// ─── StringValue (Description) ───────────────────────────────────────────────

fn p_description<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<StringValue<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  match peek_kind(input) {
    Some(SyntacticTokenKind::InlineString | SyntacticTokenKind::BlockString) => {
      let tok = next_token(input)?;
      let span = tok.span();
      match tok.into_data() {
        SyntacticToken::LitInlineStr(s) => Ok(Some(StringValue::new(span, s.into()))),
        SyntacticToken::LitBlockStr(s) => Ok(Some(StringValue::new(span, s.into()))),
        _ => unreachable!(),
      }
    }
    _ => Ok(None),
  }
}

// ─── Type ────────────────────────────────────────────────────────────────────

fn p_type<'inp>(input: &mut Inp<'inp, '_>) -> Result<Type<Name<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  match peek_kind(input) {
    Some(SyntacticTokenKind::LBracket) => {
      next_token(input)?; // consume [
      let inner = p_type(input)?;
      expect_token(input, SyntacticTokenKind::RBracket)?;
      let required = try_token(input, SyntacticTokenKind::Bang)?.is_some();
      let span = input.span_since(&cursor);
      Ok(Type::List(Box::new(scaffold::ListType::new(span, inner, required))))
    }
    Some(SyntacticTokenKind::Identifier) => {
      let name = p_name(input)?;
      let required = try_token(input, SyntacticTokenKind::Bang)?.is_some();
      let span = input.span_since(&cursor);
      Ok(Type::Name(scaffold::NamedType::new(span, name, required)))
    }
    _ => {
      let tok = next_token(input)?;
      Err(SyntacticTokenError::unexpected_token(tok.data().clone(), Expectation::Name, tok.span()).into())
    }
  }
}

// ─── Const Directives ────────────────────────────────────────────────────────

fn p_const_argument<'inp>(input: &mut Inp<'inp, '_>) -> Result<ConstArgument<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let name = p_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let value = parse_const_input_value(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Argument::new(span, name, value))
}

fn p_const_arguments<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<ConstArguments<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  let l = smear_lexer::punctuator::LParen::new(next_token(input)?.into_span());
  let mut args = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RParen) {
    args.push(p_const_argument(input)?);
  }
  let r = smear_lexer::punctuator::RParen::new(next_token(input)?.into_span());
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Arguments::new(span, l, args, r)))
}

fn p_const_directive<'inp>(input: &mut Inp<'inp, '_>) -> Result<ConstDirective<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::At)?;
  let name = p_name(input)?;
  let args = p_const_arguments(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Directive::new(span, name, args))
}

fn p_const_directives<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<ConstDirectives<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::At) { return Ok(None); }
  let cursor = input.cursor().clone();
  let mut ds = Vec::new();
  while peek_kind(input) == Some(SyntacticTokenKind::At) { ds.push(p_const_directive(input)?); }
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Directives::new(span, ds)))
}

// ─── Executable Arguments & Directives ───────────────────────────────────────

fn p_argument<'inp>(input: &mut Inp<'inp, '_>) -> Result<Argument<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let name = p_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let value = parse_input_value(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Argument::new(span, name, value))
}

fn p_arguments<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<Arguments<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  let l = smear_lexer::punctuator::LParen::new(next_token(input)?.into_span());
  let mut args = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RParen) { args.push(p_argument(input)?); }
  let r = smear_lexer::punctuator::RParen::new(next_token(input)?.into_span());
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Arguments::new(span, l, args, r)))
}

fn p_directives<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<Directives<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::At) { return Ok(None); }
  let cursor = input.cursor().clone();
  let mut ds = Vec::new();
  while peek_kind(input) == Some(SyntacticTokenKind::At) {
    let c2 = input.cursor().clone();
    expect_token(input, SyntacticTokenKind::At)?;
    let name = p_name(input)?;
    let args = p_arguments(input)?;
    ds.push(scaffold::Directive::new(input.span_since(&c2), name, args));
  }
  let span = input.span_since(&cursor);
  Ok(Some(scaffold::Directives::new(span, ds)))
}

// ─── Selection Set ───────────────────────────────────────────────────────────

fn p_selection<'inp>(input: &mut Inp<'inp, '_>) -> Result<Selection<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  match peek_kind(input) {
    Some(SyntacticTokenKind::Spread) => {
      let cursor = input.cursor().clone();
      next_token(input)?; // consume ...
      if peek_keyword(input, "on") {
        next_token(input)?;
        let tn = p_name(input)?;
        let tc = TypeCondition::new(input.span_since(&cursor), tn);
        let dirs = p_directives(input)?;
        let ss = p_selection_set(input)?;
        Ok(Selection::InlineFragment(scaffold::InlineFragment::new(input.span_since(&cursor), Some(tc), dirs, ss)))
      } else if peek_kind(input) == Some(SyntacticTokenKind::LBrace) || peek_kind(input) == Some(SyntacticTokenKind::At) {
        let dirs = p_directives(input)?;
        let ss = p_selection_set(input)?;
        Ok(Selection::InlineFragment(scaffold::InlineFragment::new(input.span_since(&cursor), None, dirs, ss)))
      } else {
        let name = p_name(input)?;
        let fname = scaffold::FragmentName::new(name.span().clone(), name.source());
        let dirs = p_directives(input)?;
        Ok(Selection::FragmentSpread(scaffold::FragmentSpread::new(input.span_since(&cursor), fname, dirs)))
      }
    }
    _ => p_field(input).map(|f| Selection::Field(f.into())),
  }
}

fn p_field<'inp>(input: &mut Inp<'inp, '_>) -> Result<scaffold::Field<Alias<&'inp str>, Name<&'inp str>, Arguments<&'inp str>, Directives<&'inp str>, SelectionSet<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let name1 = p_name(input)?;
  let (alias, name) = if peek_kind(input) == Some(SyntacticTokenKind::Colon) {
    next_token(input)?;
    let n2 = p_name(input)?;
    (Some(scaffold::Alias::new(input.span_since(&cursor), name1)), n2)
  } else {
    (None, name1)
  };
  let args = p_arguments(input)?;
  let dirs = p_directives(input)?;
  let ss = if peek_kind(input) == Some(SyntacticTokenKind::LBrace) { Some(p_selection_set(input)?) } else { None };
  Ok(scaffold::Field::new(input.span_since(&cursor), alias, name, args, dirs, ss))
}

fn p_selection_set<'inp>(input: &mut Inp<'inp, '_>) -> Result<SelectionSet<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut sels = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) && peek_kind(input).is_some() {
    sels.push(p_selection(input)?);
  }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(scaffold::SelectionSet::new(input.span_since(&cursor), sels))
}

// ─── Input Value / Arguments Definitions ─────────────────────────────────────

fn p_default_value<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<DefaultInputValue<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::Equal) { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  let value = parse_const_input_value(input)?;
  Ok(Some(scaffold::DefaultInputValue::new(input.span_since(&cursor), value)))
}

fn p_input_value_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<InputValueDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let desc = p_description(input)?;
  let name = p_name(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = p_type(input)?;
  let dv = p_default_value(input)?;
  let dirs = p_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::InputValueDefinition::new(span, name, ty, dv, dirs)))
}

fn p_args_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<ArgumentsDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LParen)?;
  let mut defs = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RParen) { defs.push(p_input_value_def(input)?); }
  expect_token(input, SyntacticTokenKind::RParen)?;
  Ok(scaffold::ArgumentsDefinition::new(input.span_since(&cursor), defs))
}

fn p_opt_args_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<ArgumentsDefinition<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) == Some(SyntacticTokenKind::LParen) { Ok(Some(p_args_def(input)?)) } else { Ok(None) }
}

// ─── Field / Fields Definition ───────────────────────────────────────────────

fn p_field_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<FieldDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let desc = p_description(input)?;
  let name = p_name(input)?;
  let args = p_opt_args_def(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = p_type(input)?;
  let dirs = p_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::FieldDefinition::new(span, name, args, ty, dirs)))
}

fn p_fields_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<FieldsDefinition<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut fs = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) { fs.push(p_field_def(input)?); }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::FieldsDefinition::new(input.span_since(&cursor), fs)))
}

fn p_input_fields_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<InputFieldsDefinition<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut fs = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) { fs.push(p_input_value_def(input)?); }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::InputFieldsDefinition::new(input.span_since(&cursor), fs)))
}

// ─── Implements / Union Members / Directive Locations / Enum Values ──────────

fn p_implements<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<scaffold::ImplementInterfaces<Name<&'inp str>>>, SyntacticTokenErrors<&'inp str>> {
  if !peek_keyword(input, "implements") { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  let _ = try_token(input, SyntacticTokenKind::Ampersand)?;
  let mut ns = vec![p_name(input)?];
  while peek_kind(input) == Some(SyntacticTokenKind::Ampersand) { next_token(input)?; ns.push(p_name(input)?); }
  Ok(Some(scaffold::ImplementInterfaces::new(input.span_since(&cursor), ns)))
}

fn p_union_members<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<scaffold::UnionMemberTypes<Name<&'inp str>>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::Equal) { return Ok(None); }
  let cursor = input.cursor().clone();
  next_token(input)?;
  let _ = try_token(input, SyntacticTokenKind::Pipe)?;
  let mut ms = vec![p_name(input)?];
  while peek_kind(input) == Some(SyntacticTokenKind::Pipe) { next_token(input)?; ms.push(p_name(input)?); }
  Ok(Some(scaffold::UnionMemberTypes::new(input.span_since(&cursor), ms)))
}

fn p_directive_locations<'inp>(input: &mut Inp<'inp, '_>) -> Result<scaffold::DirectiveLocations<scaffold::Location>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let _ = try_token(input, SyntacticTokenKind::Pipe)?;
  let mut locs = vec![parse_location(input)?];
  while peek_kind(input) == Some(SyntacticTokenKind::Pipe) { next_token(input)?; locs.push(parse_location(input)?); }
  Ok(scaffold::DirectiveLocations::new(input.span_since(&cursor), locs))
}

fn p_enum_value_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<EnumValueDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let desc = p_description(input)?;
  let name = p_name(input)?;
  let dirs = p_const_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::EnumValueDefinition::new(span, name, dirs)))
}

fn p_enum_values_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<EnumValuesDefinition<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LBrace) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut vs = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) { vs.push(p_enum_value_def(input)?); }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(Some(scaffold::EnumValuesDefinition::new(input.span_since(&cursor), vs)))
}

// ─── Variable / Variables Definition ─────────────────────────────────────────

fn p_variable_value<'inp>(input: &mut Inp<'inp, '_>) -> Result<VariableValue<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::Dollar)?;
  let name = p_name(input)?;
  Ok(VariableValue::new(input.span_since(&cursor), name))
}

fn p_var_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<DescribedVariableDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let desc = p_description(input)?;
  let var = p_variable_value(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let ty = p_type(input)?;
  let dv = p_default_value(input)?;
  let dirs = p_directives(input)?;
  let span = input.span_since(&cursor);
  Ok(scaffold::Described::new(span, desc, scaffold::VariableDefinition::new(span, var, ty, dirs, dv)))
}

fn p_vars_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<Option<VariablesDefinition<&'inp str>>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) != Some(SyntacticTokenKind::LParen) { return Ok(None); }
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LParen)?;
  let mut vs = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RParen) { vs.push(p_var_def(input)?); }
  expect_token(input, SyntacticTokenKind::RParen)?;
  Ok(Some(scaffold::VariablesDefinition::new(input.span_since(&cursor), vs)))
}

// ─── Root Operation Types ────────────────────────────────────────────────────

fn p_root_op_type_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<RootOperationTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let op = parse_operation_type(input)?;
  expect_token(input, SyntacticTokenKind::Colon)?;
  let name = p_name(input)?;
  Ok(scaffold::RootOperationTypeDefinition::new(input.span_since(&cursor), op, name))
}

fn p_root_ops_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<RootOperationTypesDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  expect_token(input, SyntacticTokenKind::LBrace)?;
  let mut ds = Vec::new();
  while peek_kind(input) != Some(SyntacticTokenKind::RBrace) { ds.push(p_root_op_type_def(input)?); }
  expect_token(input, SyntacticTokenKind::RBrace)?;
  Ok(scaffold::RootOperationTypesDefinition::new(input.span_since(&cursor), ds))
}

// ─── Type System Definitions ─────────────────────────────────────────────────

fn p_scalar_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<ScalarTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_scalar(input)?;
  let name = p_name(input)?;
  let dirs = p_const_directives(input)?;
  Ok(scaffold::ScalarTypeDefinition::new(input.span_since(&cursor), name, dirs))
}

fn p_object_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<ObjectTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_type_kw(input)?;
  let name = p_name(input)?;
  let impls = p_implements(input)?;
  let dirs = p_const_directives(input)?;
  let fields = p_fields_def(input)?;
  Ok(scaffold::ObjectTypeDefinition::new(input.span_since(&cursor), name, impls, dirs, fields))
}

fn p_interface_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<InterfaceTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_interface(input)?;
  let name = p_name(input)?;
  let impls = p_implements(input)?;
  let dirs = p_const_directives(input)?;
  let fields = p_fields_def(input)?;
  Ok(scaffold::InterfaceTypeDefinition::new(input.span_since(&cursor), name, impls, dirs, fields))
}

fn p_union_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<UnionTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_union(input)?;
  let name = p_name(input)?;
  let dirs = p_const_directives(input)?;
  let members = p_union_members(input)?;
  Ok(scaffold::UnionTypeDefinition::new(input.span_since(&cursor), name, dirs, members))
}

fn p_enum_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<EnumTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_enum(input)?;
  let name = p_name(input)?;
  let dirs = p_const_directives(input)?;
  let vals = p_enum_values_def(input)?;
  Ok(scaffold::EnumTypeDefinition::new(input.span_since(&cursor), name, dirs, vals))
}

fn p_input_object_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<InputObjectTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_input_kw(input)?;
  let name = p_name(input)?;
  let dirs = p_const_directives(input)?;
  let fields = p_input_fields_def(input)?;
  Ok(scaffold::InputObjectTypeDefinition::new(input.span_since(&cursor), name, dirs, fields))
}

fn p_directive_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<DirectiveDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_directive_kw(input)?;
  expect_token(input, SyntacticTokenKind::At)?;
  let name = p_name(input)?;
  let args = p_opt_args_def(input)?;
  let rep = peek_keyword(input, "repeatable");
  if rep { next_token(input)?; }
  parse_on(input)?;
  let locs = p_directive_locations(input)?;
  Ok(scaffold::DirectiveDefinition::new(input.span_since(&cursor), name, args, rep, locs))
}

fn p_schema_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<SchemaDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_schema(input)?;
  let dirs = p_const_directives(input)?;
  let ops = p_root_ops_def(input)?;
  Ok(scaffold::SchemaDefinition::new(input.span_since(&cursor), dirs, ops))
}

fn p_type_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<TypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_keyword(input, "scalar") { return p_scalar_def(input).map(TypeDefinition::Scalar); }
  if peek_keyword(input, "type") { return p_object_def(input).map(TypeDefinition::Object); }
  if peek_keyword(input, "interface") { return p_interface_def(input).map(TypeDefinition::Interface); }
  if peek_keyword(input, "union") { return p_union_def(input).map(TypeDefinition::Union); }
  if peek_keyword(input, "enum") { return p_enum_def(input).map(TypeDefinition::Enum); }
  if peek_keyword(input, "input") { return p_input_object_def(input).map(TypeDefinition::InputObject); }
  let tok = next_token(input)?;
  Err(SyntacticTokenError::unexpected_token(tok.data().clone(), Expectation::Name, tok.span()).into())
}

// ─── Extensions ──────────────────────────────────────────────────────────────

fn p_type_ext<'inp>(input: &mut Inp<'inp, '_>) -> Result<TypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_keyword(input, "scalar") {
    let c = input.cursor().clone();
    parse_scalar(input)?;
    let name = p_name(input)?;
    let dirs = p_const_directives(input)?;
    let span = input.span_since(&c);
    return Ok(TypeExtension::Scalar(scaffold::ScalarTypeExtension::new(span, name, dirs.unwrap_or_else(|| scaffold::Directives::new(span, Vec::new())))));
  }
  if peek_keyword(input, "type") {
    let c = input.cursor().clone();
    parse_type_kw(input)?;
    let name = p_name(input)?;
    let impls = p_implements(input)?;
    let dirs = p_const_directives(input)?;
    let fields = p_fields_def(input)?;
    let span = input.span_since(&c);
    let data = match (impls, dirs, fields) {
      (i, d, Some(f)) => scaffold::ObjectTypeExtensionData::Fields { implements: i, directives: d, fields: f },
      (i, Some(d), None) => scaffold::ObjectTypeExtensionData::Directives { implements: i, directives: d },
      _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeExtension::Object(scaffold::ObjectTypeExtension::new(span, name, data)));
  }
  if peek_keyword(input, "interface") {
    let c = input.cursor().clone();
    parse_interface(input)?;
    let name = p_name(input)?;
    let impls = p_implements(input)?;
    let dirs = p_const_directives(input)?;
    let fields = p_fields_def(input)?;
    let span = input.span_since(&c);
    let data = match (impls, dirs, fields) {
      (i, d, Some(f)) => scaffold::InterfaceTypeExtensionData::Fields { implements: i, directives: d, fields: f },
      (i, Some(d), None) => scaffold::InterfaceTypeExtensionData::Directives { implements: i, directives: d },
      _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeExtension::Interface(scaffold::InterfaceTypeExtension::new(span, name, data)));
  }
  if peek_keyword(input, "union") {
    let c = input.cursor().clone();
    parse_union(input)?;
    let name = p_name(input)?;
    let dirs = p_const_directives(input)?;
    let members = p_union_members(input)?;
    let span = input.span_since(&c);
    let data = match (dirs, members) {
      (d, Some(m)) => scaffold::UnionTypeExtensionData::Members { directives: d, members: m },
      (Some(d), None) => scaffold::UnionTypeExtensionData::Directives(d),
      _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeExtension::Union(scaffold::UnionTypeExtension::new(span, name, data)));
  }
  if peek_keyword(input, "enum") {
    let c = input.cursor().clone();
    parse_enum(input)?;
    let name = p_name(input)?;
    let dirs = p_const_directives(input)?;
    let vals = p_enum_values_def(input)?;
    let span = input.span_since(&c);
    let data = match (dirs, vals) {
      (d, Some(v)) => scaffold::EnumTypeExtensionData::Values { directives: d, values: v },
      (Some(d), None) => scaffold::EnumTypeExtensionData::Directives(d),
      _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeExtension::Enum(scaffold::EnumTypeExtension::new(span, name, data)));
  }
  if peek_keyword(input, "input") {
    let c = input.cursor().clone();
    parse_input_kw(input)?;
    let name = p_name(input)?;
    let dirs = p_const_directives(input)?;
    let fields = p_input_fields_def(input)?;
    let span = input.span_since(&c);
    let data = match (dirs, fields) {
      (d, Some(f)) => scaffold::InputObjectTypeExtensionData::Fields { directives: d, fields: f },
      (Some(d), None) => scaffold::InputObjectTypeExtensionData::Directives(d),
      _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeExtension::InputObject(scaffold::InputObjectTypeExtension::new(span, name, data)));
  }
  let tok = next_token(input)?;
  Err(SyntacticTokenError::unexpected_token(tok.data().clone(), Expectation::Name, tok.span()).into())
}

fn p_type_system_ext<'inp>(input: &mut Inp<'inp, '_>) -> Result<TypeSystemExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(input)?;
  if peek_keyword(input, "schema") {
    let c = input.cursor().clone();
    parse_schema(input)?;
    let dirs = p_const_directives(input)?;
    let ops = if peek_kind(input) == Some(SyntacticTokenKind::LBrace) { Some(p_root_ops_def(input)?) } else { None };
    let span = input.span_since(&c);
    let data = match (dirs, ops) {
      (Some(d), Some(o)) => scaffold::SchemaExtensionData::Operations { directives: Some(d), definitions: o },
      (Some(d), None) => scaffold::SchemaExtensionData::Directives(d),
      (None, Some(o)) => scaffold::SchemaExtensionData::Operations { directives: None, definitions: o },
      (None, None) => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
    };
    return Ok(TypeSystemExtension::Schema(scaffold::SchemaExtension::new(span, data)));
  }
  p_type_ext(input).map(TypeSystemExtension::Type)
}

// ─── Operation / Fragment / Document ─────────────────────────────────────────

fn p_operation_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<OperationDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_kind(input) == Some(SyntacticTokenKind::LBrace) {
    return Ok(scaffold::OperationDefinition::Shorthand(p_selection_set(input)?));
  }
  let cursor = input.cursor().clone();
  let op = parse_operation_type(input)?;
  let name = if peek_kind(input) == Some(SyntacticTokenKind::Identifier) && !peek_keyword(input, "on") {
    Some(p_name(input)?)
  } else { None };
  let vars = p_vars_def(input)?;
  let dirs = p_directives(input)?;
  let ss = p_selection_set(input)?;
  Ok(scaffold::OperationDefinition::Named(
    scaffold::NamedOperationDefinition::new(input.span_since(&cursor), op, name, vars, dirs, ss),
  ))
}

fn p_fragment_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<FragmentDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  parse_fragment_kw(input)?;
  let name = p_name(input)?;
  let fname = scaffold::FragmentName::new(name.span().clone(), name.source());
  parse_on(input)?;
  let tn = p_name(input)?;
  let tc = TypeCondition::new(tn.span().clone(), tn);
  let dirs = p_directives(input)?;
  let ss = p_selection_set(input)?;
  Ok(scaffold::FragmentDefinition::new(input.span_since(&cursor), fname, tc, dirs, ss))
}

fn p_executable_def<'inp>(input: &mut Inp<'inp, '_>) -> Result<ExecutableDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_keyword(input, "fragment") {
    p_fragment_def(input).map(scaffold::ExecutableDefinition::Fragment)
  } else {
    p_operation_def(input).map(scaffold::ExecutableDefinition::Operation)
  }
}

fn p_definition<'inp>(input: &mut Inp<'inp, '_>) -> Result<Definition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let is_ts = peek_keyword(input, "schema") || peek_keyword(input, "scalar") || peek_keyword(input, "type") ||
    peek_keyword(input, "interface") || peek_keyword(input, "union") || peek_keyword(input, "enum") ||
    peek_keyword(input, "input") || peek_keyword(input, "directive");
  if is_ts {
    let def = if peek_keyword(input, "schema") { scaffold::TypeSystemDefinition::Schema(p_schema_def(input)?) }
    else if peek_keyword(input, "directive") { scaffold::TypeSystemDefinition::Directive(p_directive_def(input)?) }
    else { scaffold::TypeSystemDefinition::Type(p_type_def(input)?) };
    Ok(scaffold::Definition::TypeSystem(def))
  } else {
    p_executable_def(input).map(scaffold::Definition::Executable)
  }
}

fn p_def_or_ext<'inp>(input: &mut Inp<'inp, '_>) -> Result<DefinitionOrExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_keyword(input, "extend") {
    p_type_system_ext(input).map(scaffold::DefinitionOrExtension::Extension)
  } else {
    let cursor = input.cursor().clone();
    let desc = p_description(input)?;
    let def = p_definition(input)?;
    let span = input.span_since(&cursor);
    Ok(scaffold::DefinitionOrExtension::Definition(scaffold::Described::new(span, desc, def)))
  }
}

fn p_ts_def_or_ext<'inp>(input: &mut Inp<'inp, '_>) -> Result<TypeSystemDefinitionOrExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  if peek_keyword(input, "extend") {
    p_type_system_ext(input).map(scaffold::TypeSystemDefinitionOrExtension::Extension)
  } else {
    let cursor = input.cursor().clone();
    let desc = p_description(input)?;
    let def = if peek_keyword(input, "schema") { scaffold::TypeSystemDefinition::Schema(p_schema_def(input)?) }
    else if peek_keyword(input, "directive") { scaffold::TypeSystemDefinition::Directive(p_directive_def(input)?) }
    else { scaffold::TypeSystemDefinition::Type(p_type_def(input)?) };
    let span = input.span_since(&cursor);
    Ok(scaffold::TypeSystemDefinitionOrExtension::Definition(scaffold::Described::new(span, desc, def)))
  }
}

fn p_document<'inp, T>(input: &mut Inp<'inp, '_>, parse_item: fn(&mut Inp<'inp, '_>) -> Result<T, SyntacticTokenErrors<&'inp str>>) -> Result<scaffold::Document<T>, SyntacticTokenErrors<&'inp str>> {
  let cursor = input.cursor().clone();
  let mut defs = Vec::new();
  while peek_kind(input).is_some() { defs.push(parse_item(input)?); }
  Ok(scaffold::Document::new(input.span_since(&cursor), defs))
}

// ─── ParseStr implementations ────────────────────────────────────────────────

/// Helper function used to run a parse closure as a ParseStr implementation.
fn run_parser<'inp, T>(
  input: &'inp str,
  f: fn(&mut Inp<'inp, '_>) -> Result<T, SyntacticTokenErrors<&'inp str>>,
) -> Result<T, SyntacticTokenErrors<&'inp str>> {
  Parser::with_parser(f).parse_str(input)
}

macro_rules! impl_parse_str {
  ($ty:ty, $parse_fn:ident) => {
    impl<'a> ParseStr<'a> for $ty {
      fn parse_str(input: &'a str) -> Result<Self, SyntacticTokenErrors<&'a str>> {
        run_parser(input, $parse_fn)
      }
    }
  };
}

// Named wrapper functions for document types
fn p_doc<'inp>(inp: &mut Inp<'inp, '_>) -> Result<Document<&'inp str>, SyntacticTokenErrors<&'inp str>> { p_document(inp, p_def_or_ext) }
fn p_ts_doc<'inp>(inp: &mut Inp<'inp, '_>) -> Result<TypeSystemDocument<&'inp str>, SyntacticTokenErrors<&'inp str>> { p_document(inp, p_ts_def_or_ext) }
fn p_exec_doc<'inp>(inp: &mut Inp<'inp, '_>) -> Result<ExecutableDocument<&'inp str>, SyntacticTokenErrors<&'inp str>> { p_document(inp, p_executable_def) }

// Named wrapper functions for described types
fn p_described_object_def<'inp>(inp: &mut Inp<'inp, '_>) -> Result<DescribedObjectTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let c = inp.cursor().clone(); let d = p_description(inp)?; let def = p_object_def(inp)?;
  Ok(scaffold::Described::new(inp.span_since(&c), d, def))
}
fn p_described_interface_def<'inp>(inp: &mut Inp<'inp, '_>) -> Result<DescribedInterfaceTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let c = inp.cursor().clone(); let d = p_description(inp)?; let def = p_interface_def(inp)?;
  Ok(scaffold::Described::new(inp.span_since(&c), d, def))
}
fn p_described_enum_def<'inp>(inp: &mut Inp<'inp, '_>) -> Result<DescribedEnumTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let c = inp.cursor().clone(); let d = p_description(inp)?; let def = p_enum_def(inp)?;
  Ok(scaffold::Described::new(inp.span_since(&c), d, def))
}
fn p_described_input_object_def<'inp>(inp: &mut Inp<'inp, '_>) -> Result<DescribedInputObjectTypeDefinition<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  let c = inp.cursor().clone(); let d = p_description(inp)?; let def = p_input_object_def(inp)?;
  Ok(scaffold::Described::new(inp.span_since(&c), d, def))
}

// Named wrapper functions for extension types
fn p_object_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<ObjectTypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_type_kw(inp)?; let name = p_name(inp)?; let impls = p_implements(inp)?; let dirs = p_const_directives(inp)?; let fields = p_fields_def(inp)?; let span = inp.span_since(&c);
  let data = match (impls, dirs, fields) {
    (i, d, Some(f)) => scaffold::ObjectTypeExtensionData::Fields { implements: i, directives: d, fields: f },
    (i, Some(d), None) => scaffold::ObjectTypeExtensionData::Directives { implements: i, directives: d },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::ObjectTypeExtension::new(span, name, data))
}
fn p_interface_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<InterfaceTypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_interface(inp)?; let name = p_name(inp)?; let impls = p_implements(inp)?; let dirs = p_const_directives(inp)?; let fields = p_fields_def(inp)?; let span = inp.span_since(&c);
  let data = match (impls, dirs, fields) {
    (i, d, Some(f)) => scaffold::InterfaceTypeExtensionData::Fields { implements: i, directives: d, fields: f },
    (i, Some(d), None) => scaffold::InterfaceTypeExtensionData::Directives { implements: i, directives: d },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::InterfaceTypeExtension::new(span, name, data))
}
fn p_enum_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<EnumTypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_enum(inp)?; let name = p_name(inp)?; let dirs = p_const_directives(inp)?; let vals = p_enum_values_def(inp)?; let span = inp.span_since(&c);
  let data = match (dirs, vals) {
    (d, Some(v)) => scaffold::EnumTypeExtensionData::Values { directives: d, values: v },
    (Some(d), None) => scaffold::EnumTypeExtensionData::Directives(d),
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::EnumTypeExtension::new(span, name, data))
}
fn p_input_object_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<InputObjectTypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_input_kw(inp)?; let name = p_name(inp)?; let dirs = p_const_directives(inp)?; let fields = p_input_fields_def(inp)?; let span = inp.span_since(&c);
  let data = match (dirs, fields) {
    (d, Some(f)) => scaffold::InputObjectTypeExtensionData::Fields { directives: d, fields: f },
    (Some(d), None) => scaffold::InputObjectTypeExtensionData::Directives(d),
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::InputObjectTypeExtension::new(span, name, data))
}
fn p_scalar_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<ScalarTypeExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_scalar(inp)?; let name = p_name(inp)?; let dirs = p_const_directives(inp)?; let span = inp.span_since(&c);
  Ok(scaffold::ScalarTypeExtension::new(span, name, dirs.unwrap_or_else(|| scaffold::Directives::new(span, Vec::new()))))
}
fn p_schema_ext<'inp>(inp: &mut Inp<'inp, '_>) -> Result<SchemaExtension<&'inp str>, SyntacticTokenErrors<&'inp str>> {
  parse_extend(inp)?; let c = inp.cursor().clone(); parse_schema(inp)?; let dirs = p_const_directives(inp)?; let ops = if peek_kind(inp) == Some(SyntacticTokenKind::LBrace) { Some(p_root_ops_def(inp)?) } else { None }; let span = inp.span_since(&c);
  let data = match (dirs, ops) {
    (Some(d), Some(o)) => scaffold::SchemaExtensionData::Operations { directives: Some(d), definitions: o },
    (Some(d), None) => scaffold::SchemaExtensionData::Directives(d),
    (None, Some(o)) => scaffold::SchemaExtensionData::Operations { directives: None, definitions: o },
    _ => return Err(SyntacticTokenError::unexpected_end_of_input(span).into()),
  };
  Ok(scaffold::SchemaExtension::new(span, data))
}

impl_parse_str!(Document<&'a str>, p_doc);
impl_parse_str!(TypeSystemDocument<&'a str>, p_ts_doc);
impl_parse_str!(ExecutableDocument<&'a str>, p_exec_doc);
impl_parse_str!(SelectionSet<&'a str>, p_selection_set);
impl_parse_str!(OperationDefinition<&'a str>, p_operation_def);
impl_parse_str!(FragmentDefinition<&'a str>, p_fragment_def);
impl_parse_str!(DirectiveDefinition<&'a str>, p_directive_def);
impl_parse_str!(SchemaDefinition<&'a str>, p_schema_def);
impl_parse_str!(ObjectTypeDefinition<&'a str>, p_object_def);
impl_parse_str!(ArgumentsDefinition<&'a str>, p_args_def);
impl_parse_str!(DescribedObjectTypeDefinition<&'a str>, p_described_object_def);
impl_parse_str!(DescribedInterfaceTypeDefinition<&'a str>, p_described_interface_def);
impl_parse_str!(DescribedEnumTypeDefinition<&'a str>, p_described_enum_def);
impl_parse_str!(DescribedInputObjectTypeDefinition<&'a str>, p_described_input_object_def);
impl_parse_str!(ObjectTypeExtension<&'a str>, p_object_ext);
impl_parse_str!(InterfaceTypeExtension<&'a str>, p_interface_ext);
impl_parse_str!(EnumTypeExtension<&'a str>, p_enum_ext);
impl_parse_str!(InputObjectTypeExtension<&'a str>, p_input_object_ext);
impl_parse_str!(ScalarTypeExtension<&'a str>, p_scalar_ext);
impl_parse_str!(SchemaExtension<&'a str>, p_schema_ext);

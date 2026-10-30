//! Rendering a draft §4 introspection result as the SDL that describes the same schema.
//!
//! # This is the whole door
//!
//! Everything downstream of this file is [`Schema::build`](super::super::Schema::build), unchanged
//! and unaware that a response was involved. That is the entire architecture of the introspection
//! feature and the reason the two construction doors cannot drift: there is no second builder, no
//! second interner, no second draft §3 pass — only a translation into the input the one builder
//! already takes.
//!
//! # What the renderer emits, and what it leaves to the build
//!
//! It emits **only what the response adds**. The introspection meta-schema, the five built-in
//! scalars and the five specified directives are injected by every build, so re-emitting them
//! would be wrong in three different ways: `__`-prefixed definitions collide
//! ([`RedefinedBuiltInType`]), and a re-emitted `Int` or `@skip` would arrive as a *user*
//! definition and come out of the build with [`TypeDef::is_built_in`] false where the SDL door
//! leaves it true. Dropping them is what makes the two doors produce the same flags, and it costs
//! nothing: the build puts back exactly what was dropped.
//!
//! A built-in scalar or a specified directive is dropped only while that last clause holds. One
//! the response reports other than as the build injects it says something the injected one would
//! lose — for a scalar, a `specifiedByURL`, a `null` description or a description other than the
//! built-in's own; for a directive, any of its description, locations, arguments (with their
//! descriptions, types and defaults) or repeatability — so it is written out as a definition of its
//! own. That is how the SDL door reads a document that spelled the built-in out, flag, text and
//! place among its siblings all three, so the two doors agree on it. A server whose built-in texts
//! are not graphql-js's has its built-ins written out the same way, keeping its own.
//!
//! It emits **nothing it has not checked**, and most of the checking happened before it ran.
//! [`decode`](super::decode) admits a name only if it spells one, and resolves type kinds and
//! directive locations to their enums, so what reaches this file is already drawn from the closed
//! vocabularies. What is left here is everything a *use site* decides — that a union member may
//! not be a wrapper, that an enum value may not be `true` — and the default value, which is the
//! one fragment no vocabulary covers and which goes through the parser itself. The only bytes that
//! reach the SDL parser unexamined are the renderer's own punctuation.
//!
//! **Prose is the one fragment it writes rather than checks.** A description, a deprecation reason
//! and a `specifiedByURL` are text the response chose, so each is written as the string literal
//! graphql-js's `printString` would write for it — quoted, with every quote, backslash and control
//! character escaped — and comes back out of the build's decoder as the same text. Nothing in it
//! can close the literal it sits in, so nothing in it can reach the parser as syntax.
//!
//! [`RedefinedBuiltInType`]: super::super::SchemaErrorKind::RedefinedBuiltInType
//! [`TypeDef::is_built_in`]: super::super::TypeDef::is_built_in

use std::string::String;

use tokora::{Parse as _, Parser};

use smear_parser::graphql::{
  GraphQL,
  ast::{
    ConstInputValue, TypeDefinition, TypeSystemDefinition, TypeSystemDefinitionOrExtension,
    TypeSystemDocument,
  },
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, type_system_document},
};

use super::{
  super::{
    // The builder's own escaping, so a description renders as the literal the build decodes back,
    // and its own printing, so a default compares as the build would keep it.
    builder::print::{canonical, escape},
    builtin,
    // The SDL door's own owner-path helper, so `Query.hero.first` reads the same whichever door
    // refused it.
    error::owner_path as path,
    repr::{DirectiveLocation, DirectiveLocations, TypeKind},
  },
  decode,
  error::{ResponseError, ResponseErrorKind},
  json::Text,
  model::{
    Deprecated, IntrospectedDirective, IntrospectedEnumValue, IntrospectedField,
    IntrospectedInputValue, IntrospectedKind, IntrospectedSchema, IntrospectedType, Name, TypeRef,
  },
};

type Rendered<T = ()> = Result<T, ResponseError>;

/// Reads a response and renders the SDL it describes.
pub(super) fn render(response: &str) -> Rendered<String> {
  let schema = decode::read(response)?;
  Renderer::default().schema(&schema)
}

// ---------------------------------------------------------------------------------------------
// rendering
// ---------------------------------------------------------------------------------------------

#[derive(Debug, Default)]
struct Renderer {
  out: String,
}

impl Renderer {
  fn schema(mut self, schema: &IntrospectedSchema<'_>) -> Rendered<String> {
    self.roots(schema)?;
    for ty in &schema.types {
      self.ty(ty)?;
    }
    for directive in &schema.directives {
      self.directive(directive)?;
    }
    Ok(self.out)
  }

  /// The `schema` block, always written explicitly.
  ///
  /// A response names its roots and never relies on the `Query`/`Mutation`/`Subscription` naming
  /// convention, so the renderer does not either. Writing the block unconditionally also means a
  /// schema whose query root is called something else round-trips, which the convention would
  /// quietly break.
  fn roots(&mut self, schema: &IntrospectedSchema<'_>) -> Rendered {
    self.description(schema.description.as_ref(), "");
    self.out.push_str("schema {\n");
    let query = named_root(schema.query_type.name, "query")?;
    self.out.push_str("  query: ");
    self.out.push_str(query);
    self.out.push('\n');

    for (slot, root) in [
      ("mutation", schema.mutation_type.as_ref()),
      ("subscription", schema.subscription_type.as_ref()),
    ] {
      // A null root slot is the ordinary case — most schemas have no subscription — and is not the
      // same as a root present with a null name, which `named_root` refuses.
      let Some(root) = root else { continue };
      let name = named_root(root.name, slot)?;
      self.out.push_str("  ");
      self.out.push_str(slot);
      self.out.push_str(": ");
      self.out.push_str(name);
      self.out.push('\n');
    }
    self.out.push_str("}\n\n");
    Ok(())
  }

  fn ty(&mut self, ty: &IntrospectedType<'_>) -> Rendered {
    let name = match ty.name {
      Some(name) => name.as_str(),
      None => {
        return Err(ResponseError::new(
          ResponseErrorKind::UnnamedType,
          "a member of `__Schema.types`",
        ));
      }
    };

    let kind = named_kind(ty.kind, name, None)?;

    // The meta-schema is injected by the build, unconditionally, so re-emitting it is a
    // redefinition. Only the eight names the specification defines are dropped: a server exposing
    // some other `__`-prefixed type is exposing something this schema does not know about, and
    // `ReservedTypeName` says so rather than the door swallowing it.
    if builtin::INTROSPECTION_TYPES.contains(&name) {
      return Ok(());
    }
    // Likewise the five built-in scalars — but only when the response agrees they are scalars, and
    // only when it says nothing about one that the build's own would not. A response that calls
    // `Int` an enum is describing a schema in which `Int` is an enum, and dropping it would build a
    // different schema than the one described. A response that gives `String` a description of its
    // own, a `null` one or a `specifiedByURL` says what a document that spelled `String` out says,
    // and the SDL door keeps that spelling as the document's own definition, so this door writes it
    // out too.
    if matches!(kind, TypeKind::Scalar) && is_the_built_in_scalar(ty, name) {
      return Ok(());
    }

    self.description(ty.description.as_ref().and_then(Option::as_ref), "");
    match kind {
      TypeKind::Scalar => {
        self.out.push_str("scalar ");
        self.out.push_str(name);
        if let Some(url) = ty.specified_by_url.as_ref() {
          self.out.push_str(" @specifiedBy(url: ");
          quote(url.as_str(), &mut self.out);
          self.out.push(')');
        }
        self.out.push_str("\n\n");
      }
      TypeKind::Object | TypeKind::Interface => {
        self.out.push_str(if matches!(kind, TypeKind::Object) {
          "type "
        } else {
          "interface "
        });
        self.out.push_str(name);
        self.implements(ty, name)?;
        self.fields(ty.fields.as_deref(), name)?;
      }
      TypeKind::Union => {
        self.out.push_str("union ");
        self.out.push_str(name);
        self.members(ty.possible_types.as_deref(), name)?;
      }
      TypeKind::Enum => {
        self.out.push_str("enum ");
        self.out.push_str(name);
        self.enum_values(ty.enum_values.as_deref(), name)?;
      }
      TypeKind::InputObject => {
        self.out.push_str("input ");
        self.out.push_str(name);
        // The one applied directive that survives into `Schema`, as `TypeFlags::ONE_OF`.
        if ty.is_one_of == Some(true) {
          self.out.push_str(" @oneOf");
        }
        self.input_fields(ty.input_fields.as_deref(), name)?;
      }
    }
    Ok(())
  }

  /// `implements A & B`, or nothing.
  ///
  /// An empty `interfaces` list is written as no clause at all: `implements` with nothing after it
  /// is a syntax error, and the two mean the same thing.
  fn implements(&mut self, ty: &IntrospectedType<'_>, owner: &str) -> Rendered {
    let Some(interfaces) = ty.interfaces.as_deref() else {
      return Ok(());
    };
    if interfaces.is_empty() {
      return Ok(());
    }
    self.out.push_str(" implements ");
    for (index, interface) in interfaces.iter().enumerate() {
      if index > 0 {
        self.out.push_str(" & ");
      }
      let name = base_name(interface, owner, "implements")?;
      self.out.push_str(name);
    }
    Ok(())
  }

  /// A field block, or nothing at all when the type declares no fields.
  ///
  /// "No fields" is left to say itself: draft §3.6.1 refuses an object or interface without them,
  /// and `type Foo` with no block is how SDL spells that, so the build reports
  /// `EmptyFieldsDefinition` for the response exactly as it would for the document.
  fn fields(&mut self, fields: Option<&[IntrospectedField<'_>]>, owner: &str) -> Rendered {
    let fields = fields.unwrap_or(&[]);
    if fields.is_empty() {
      self.out.push_str("\n\n");
      return Ok(());
    }
    self.out.push_str(" {\n");
    for field in fields {
      let name = field.name.as_str();
      self.description(field.description.as_ref(), "  ");
      self.out.push_str("  ");
      self.out.push_str(name);
      self.arguments(&field.args, &path(&[owner, name]))?;
      self.out.push_str(": ");
      let rendered = type_reference(&field.ty, owner, name)?;
      self.out.push_str(&rendered);
      self.deprecated(&field.deprecated);
      self.out.push('\n');
    }
    self.out.push_str("}\n\n");
    Ok(())
  }

  fn arguments(&mut self, args: &[IntrospectedInputValue<'_>], owner: &str) -> Rendered {
    if args.is_empty() {
      return Ok(());
    }
    self.out.push('(');
    for (index, arg) in args.iter().enumerate() {
      if index > 0 {
        self.out.push_str(", ");
      }
      self.input_value(arg, owner)?;
    }
    self.out.push(')');
    Ok(())
  }

  fn input_fields(
    &mut self,
    fields: Option<&[IntrospectedInputValue<'_>]>,
    owner: &str,
  ) -> Rendered {
    let fields = fields.unwrap_or(&[]);
    if fields.is_empty() {
      self.out.push_str("\n\n");
      return Ok(());
    }
    self.out.push_str(" {\n");
    for field in fields {
      self.out.push_str("  ");
      self.input_value(field, owner)?;
      self.out.push('\n');
    }
    self.out.push_str("}\n\n");
    Ok(())
  }

  /// `name: Type` with an optional `= default`, shared by arguments and input-object fields, its
  /// description written in front of it on the same line.
  fn input_value(&mut self, value: &IntrospectedInputValue<'_>, owner: &str) -> Rendered {
    let name = value.name.as_str();
    if let Some(Some(description)) = &value.description {
      quote(description.as_str(), &mut self.out);
      self.out.push(' ');
    }
    self.out.push_str(name);
    self.out.push_str(": ");
    let rendered = type_reference(&value.ty, owner, name)?;
    self.out.push_str(&rendered);
    if let Some(default) = value.default_value.as_ref() {
      let default = default.as_str();
      check_default_value(default, owner, name)?;
      self.out.push_str(" = ");
      self.out.push_str(default);
    }
    self.deprecated(&value.deprecated);
    Ok(())
  }

  /// `= A | B`, or nothing — the same "let the build say it" treatment as an empty field block.
  fn members(&mut self, members: Option<&[TypeRef<'_>]>, owner: &str) -> Rendered {
    let members = members.unwrap_or(&[]);
    if members.is_empty() {
      self.out.push_str("\n\n");
      return Ok(());
    }
    self.out.push_str(" = ");
    for (index, member) in members.iter().enumerate() {
      if index > 0 {
        self.out.push_str(" | ");
      }
      let name = base_name(member, owner, "possibleTypes")?;
      self.out.push_str(name);
    }
    self.out.push_str("\n\n");
    Ok(())
  }

  fn enum_values(&mut self, values: Option<&[IntrospectedEnumValue<'_>]>, owner: &str) -> Rendered {
    let values = values.unwrap_or(&[]);
    if values.is_empty() {
      self.out.push_str("\n\n");
      return Ok(());
    }
    self.out.push_str(" {\n");
    for value in values {
      let name = value.name.as_str();
      // The grammar's `EnumValue` is a `Name` but not `true`, `false` or `null`, so a response
      // carrying one of the three describes an enum no document can write. It is a rule about
      // where the name sits rather than about how it is spelled, which is why it is here and not
      // in the reader with the rest of the name check.
      if matches!(name, "true" | "false" | "null") {
        return Err(ResponseError::new(ResponseErrorKind::InvalidEnumValue, name).owned_by(owner));
      }
      self.description(value.description.as_ref(), "  ");
      self.out.push_str("  ");
      self.out.push_str(name);
      self.deprecated(&value.deprecated);
      self.out.push('\n');
    }
    self.out.push_str("}\n\n");
    Ok(())
  }

  /// A description on a line of its own, in front of the element it describes.
  fn description(&mut self, description: Option<&Text<'_>>, indent: &str) {
    if let Some(description) = description {
      self.out.push_str(indent);
      quote(description.as_str(), &mut self.out);
      self.out.push('\n');
    }
  }

  /// ` @deprecated(reason: …)` behind a member the response reports deprecated, its reason quoted
  /// or `null` as the response wrote it — the one applied directive besides `@oneOf` and
  /// `@specifiedBy` that introspection carries.
  fn deprecated(&mut self, deprecated: &Deprecated<'_>) {
    let Some(reason) = deprecated else {
      return;
    };
    self.out.push_str(" @deprecated(reason: ");
    match reason {
      Some(reason) => quote(reason.as_str(), &mut self.out),
      None => self.out.push_str("null"),
    }
    self.out.push(')');
  }

  fn directive(&mut self, directive: &IntrospectedDirective<'_>) -> Rendered {
    let name = directive.name.as_str();
    // As with the built-in scalars: injected by the build, so re-emitting one the build would
    // inject anyway would flip `DirectiveDef::is_built_in` for a definition the two doors are
    // supposed to agree on — and one the response reports otherwise is the document's own, which
    // the SDL door keeps as such, so it is written out.
    if is_the_specified_directive(directive) {
      return Ok(());
    }
    if directive.locations.is_empty() {
      // `on` with nothing after it does not parse, and a directive valid nowhere is not a
      // directive. There is no draft §3 rule for it, so the door is where it stops. Checked before
      // anything is written, so a refusal never leaves a half-rendered definition behind.
      return Err(
        ResponseError::new(
          ResponseErrorKind::UnknownDirectiveLocation,
          "`locations` is empty",
        )
        .owned_by(name),
      );
    }

    self.description(directive.description.as_ref().and_then(Option::as_ref), "");
    self.out.push_str("directive @");
    self.out.push_str(name);
    self.arguments(&directive.args, name)?;
    if directive.is_repeatable {
      self.out.push_str(" repeatable");
    }
    self.out.push_str(" on ");
    for (index, location) in directive.locations.iter().enumerate() {
      if index > 0 {
        self.out.push_str(" | ");
      }
      self.out.push_str(location.as_str());
    }
    self.out.push_str("\n\n");
    Ok(())
  }
}

// ---------------------------------------------------------------------------------------------
// checked fragments
// ---------------------------------------------------------------------------------------------

/// Whether a member of `__Schema.types` is one of the five built-in scalars as the build injects
/// it, so leaving it to the build loses nothing: no `specifiedByURL`, and a `description` that is
/// either not in the response at all — the query did not ask — or the built-in's own text.
fn is_the_built_in_scalar(ty: &IntrospectedType<'_>, name: &str) -> bool {
  let Some(at) = builtin::BUILT_IN_SCALARS
    .iter()
    .position(|scalar| *scalar == name)
  else {
    return false;
  };
  ty.specified_by_url.is_none()
    && says_only(
      ty.description.as_ref(),
      builtin::BUILT_IN_SCALAR_DESCRIPTIONS[at],
    )
}

/// Whether a description a response wrote says nothing the built-in's would not: absent, because
/// the query did not ask, or the built-in's own text. A `null` one says there is none.
fn says_only(description: Option<&Option<Text<'_>>>, built_in: &str) -> bool {
  match description {
    None => true,
    Some(Some(text)) => text.as_str() == built_in,
    Some(None) => false,
  }
}

/// One of the five specified directives as every build injects it, in the terms introspection
/// reports it in.
struct SpecifiedDirective {
  name: &'static str,
  description: &'static str,
  locations: DirectiveLocations,
  repeatable: bool,
  argument: Option<SpecifiedArgument>,
}

/// The one argument a specified directive declares, if it declares one.
struct SpecifiedArgument {
  name: &'static str,
  description: &'static str,
  /// The type, spelled as SDL.
  ty: &'static str,
  /// The default's canonical text.
  default: Option<&'static str>,
}

/// The five specified directives as every build injects them, in
/// [`builtin::BUILT_IN_DIRECTIVES`]' order: what a response's entry for one has to be, exactly,
/// for the door to leave it to the build. `the_specified_directives_are_the_injected_ones` holds
/// this to what the build injects.
static SPECIFIED_DIRECTIVES: [SpecifiedDirective; 5] = {
  const EXECUTABLE: DirectiveLocations = DirectiveLocations::from_bits(0)
    .with(DirectiveLocation::Field)
    .with(DirectiveLocation::FragmentSpread)
    .with(DirectiveLocation::InlineFragment);
  [
    SpecifiedDirective {
      name: "skip",
      description: "Directs the executor to skip this field or fragment when the `if` argument is true.",
      locations: EXECUTABLE,
      repeatable: false,
      argument: Some(SpecifiedArgument {
        name: "if",
        description: "Skipped when true.",
        ty: "Boolean!",
        default: None,
      }),
    },
    SpecifiedDirective {
      name: "include",
      description: "Directs the executor to include this field or fragment only when the `if` argument is true.",
      locations: EXECUTABLE,
      repeatable: false,
      argument: Some(SpecifiedArgument {
        name: "if",
        description: "Included when true.",
        ty: "Boolean!",
        default: None,
      }),
    },
    SpecifiedDirective {
      name: "deprecated",
      description: "Marks an element of a GraphQL schema as no longer supported.",
      locations: DirectiveLocations::from_bits(0)
        .with(DirectiveLocation::FieldDefinition)
        .with(DirectiveLocation::ArgumentDefinition)
        .with(DirectiveLocation::InputFieldDefinition)
        .with(DirectiveLocation::EnumValue),
      repeatable: false,
      argument: Some(SpecifiedArgument {
        name: "reason",
        description: "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax, as specified by [CommonMark](https://commonmark.org/).",
        ty: "String",
        default: Some("\"No longer supported\""),
      }),
    },
    SpecifiedDirective {
      name: "specifiedBy",
      description: "Exposes a URL that specifies the behavior of this scalar.",
      locations: DirectiveLocations::from_bits(0).with(DirectiveLocation::Scalar),
      repeatable: false,
      argument: Some(SpecifiedArgument {
        name: "url",
        description: "The URL that specifies the behavior of this scalar.",
        ty: "String!",
        default: None,
      }),
    },
    SpecifiedDirective {
      name: "oneOf",
      description: "Indicates exactly one field must be supplied and this field must not be `null`.",
      locations: DirectiveLocations::from_bits(0).with(DirectiveLocation::InputObject),
      repeatable: false,
      argument: None,
    },
  ]
};

/// Whether a response's directive is one of the five specified directives exactly as the build
/// injects it — the same description or none asked for, the same locations, the same argument with
/// the same description, type and default, and the same repeatability — so leaving it to the
/// build loses nothing.
fn is_the_specified_directive(directive: &IntrospectedDirective<'_>) -> bool {
  let name = directive.name.as_str();
  let Some(specified) = SPECIFIED_DIRECTIVES
    .iter()
    .find(|specified| specified.name == name)
  else {
    return false;
  };
  let locations: DirectiveLocations = directive.locations.iter().copied().collect();
  says_only(directive.description.as_ref(), specified.description)
    && locations == specified.locations
    && directive.is_repeatable == specified.repeatable
    && match (directive.args.as_slice(), &specified.argument) {
      ([], None) => true,
      ([argument], Some(expected)) => is_the_specified_argument(argument, expected, name),
      _ => false,
    }
}

/// Whether a response's argument is a specified directive's own, exactly: name, description,
/// type, default and no deprecation.
fn is_the_specified_argument(
  argument: &IntrospectedInputValue<'_>,
  expected: &SpecifiedArgument,
  owner: &str,
) -> bool {
  argument.name.as_str() == expected.name
    && says_only(argument.description.as_ref(), expected.description)
    && argument.deprecated.is_none()
    && type_reference(&argument.ty, owner, expected.name).is_ok_and(|ty| ty == expected.ty)
    && match (&argument.default_value, expected.default) {
      (None, None) => true,
      (Some(default), Some(expected)) => {
        canonical_default(default.as_str()).is_some_and(|default| default == expected)
      }
      _ => false,
    }
}

/// Writes prose as the string literal graphql-js's `printString` writes for it: in quotes, every
/// quote, backslash and control inside escaped. See the module header for why that is the check.
fn quote(text: &str, out: &mut String) {
  out.push('"');
  escape(text, out);
  out.push('"');
}

/// Reads a `__TypeKind` that must name a type.
///
/// The reader already refused a kind that is not one of the eight, so what is left is the question
/// only a use site can answer: a wrapper kind is the shape of a *reference*, so it names no type
/// and cannot be a member of `__Schema.types` nor the base a reference bottoms out at.
fn named_kind(kind: IntrospectedKind, subject: &str, owner: Option<&str>) -> Rendered<TypeKind> {
  match kind {
    IntrospectedKind::Named(named) => Ok(named),
    IntrospectedKind::List | IntrospectedKind::NonNull => Err(attach(
      ResponseError::new(ResponseErrorKind::NotANamedType, subject),
      owner,
    )),
  }
}

/// Reads a type reference that must be a bare named type — a union member or an implemented
/// interface, neither of which may be wrapped.
fn base_name<'a>(reference: &TypeRef<'a>, owner: &str, slot: &str) -> Rendered<&'a str> {
  named_kind(reference.kind, slot, Some(owner))?;
  match reference.name {
    Some(name) => Ok(name.as_str()),
    None => Err(ResponseError::new(ResponseErrorKind::UnnamedType, slot).owned_by(owner)),
  }
}

/// Flattens a `__Type` reference chain into its GraphQL spelling.
///
/// The recursion is bounded before it starts: the reader refuses JSON nested more than 128 deep,
/// so the chain this walks cannot be longer than that however hostile the response. The
/// *representation's* limit is much lower — [`MAX_WRAPPERS`](super::super::MAX_WRAPPERS) is 15 —
/// and is left to the build, which already reports `TypeReferenceTooDeep` for the SDL that says
/// the same thing.
fn type_reference(reference: &TypeRef<'_>, owner: &str, subject: &str) -> Rendered<String> {
  let mut out = String::new();
  write_type_reference(&mut out, reference, owner, subject, false)?;
  Ok(out)
}

fn write_type_reference(
  out: &mut String,
  reference: &TypeRef<'_>,
  owner: &str,
  subject: &str,
  in_non_null: bool,
) -> Rendered {
  match reference.kind {
    IntrospectedKind::List => {
      out.push('[');
      write_type_reference(out, item(reference, owner, subject)?, owner, subject, false)?;
      out.push(']');
    }
    IntrospectedKind::NonNull => {
      // `Int!!` is not a type any grammar can spell — `NonNullType` wraps a `NamedType` or a
      // `ListType`, never another `NonNullType` — so a doubled wrapper is refused here rather than
      // rendered and handed to a parser that would report it as a syntax error with no idea which
      // field it came from.
      if in_non_null {
        return Err(ResponseError::new(ResponseErrorKind::DoubleNonNull, subject).owned_by(owner));
      }
      write_type_reference(out, item(reference, owner, subject)?, owner, subject, true)?;
      out.push('!');
    }
    IntrospectedKind::Named(_) => {
      let name = match reference.name {
        Some(name) => name.as_str(),
        None => {
          return Err(ResponseError::new(ResponseErrorKind::UnnamedType, subject).owned_by(owner));
        }
      };
      out.push_str(name);
    }
  }
  Ok(())
}

fn item<'a, 'r>(
  reference: &'r TypeRef<'a>,
  owner: &str,
  subject: &str,
) -> Rendered<&'r TypeRef<'a>> {
  reference
    .of_type
    .as_deref()
    .ok_or_else(|| ResponseError::new(ResponseErrorKind::MissingItemType, subject).owned_by(owner))
}

/// Refuses a root operation type with no name.
fn named_root<'a>(name: Option<Name<'a>>, slot: &str) -> Rendered<&'a str> {
  match name {
    Some(name) => Ok(name.as_str()),
    None => Err(ResponseError::new(ResponseErrorKind::UnnamedType, slot).owned_by("schema")),
  }
}

fn attach(error: ResponseError, owner: Option<&str>) -> ResponseError {
  match owner {
    Some(owner) => error.owned_by(owner),
    None => error,
  }
}

// ---------------------------------------------------------------------------------------------
// the default value, and why it gets a parser to itself
// ---------------------------------------------------------------------------------------------

/// Refuses an `__InputValue.defaultValue` that is not exactly one GraphQL const value.
///
/// # This check is what makes the field safe to embed
///
/// `defaultValue` is the only text in a response that the renderer copies through verbatim: it is
/// a *value*, not a name, so no grammar of names can admit it, and
/// [`Schema`](super::super::Schema) keeps only whether it is present and whether it is `null`.
/// Copying it through unchecked would let a response close the definition it sits in and open
/// another — `1} type Evil {…} input Z {q: Int` in one field's default rewrites the whole schema,
/// and every downstream check would run happily on the rewritten one.
///
/// So the value is handed to the parser inside a throwaway document of its own, and the result is
/// checked structurally rather than merely for success. Success alone is not enough: `1 g: Int`
/// parses, and would add a field. **One definition, holding one input value, with a default** is
/// the shape that admits nothing but a complete const value — anything the value did not consume
/// becomes a second field, a second definition, or a syntax error, and all three fail this.
fn check_default_value(default: &str, owner: &str, subject: &str) -> Rendered {
  read_default_value(default, |_| ()).ok_or_else(|| {
    ResponseError::new(ResponseErrorKind::MalformedDefaultValue, default)
      .owned_by(path(&[owner, subject]))
  })
}

/// Parses a server-printed default inside a throwaway document of its own and hands `read` the
/// one const value in it, or answers `None` for text that is anything else. See
/// [`check_default_value`] for why the shape is what admits nothing but a complete value.
fn read_default_value<R>(
  default: &str,
  read: impl FnOnce(&ConstInputValue<&str>) -> R,
) -> Option<R> {
  let probe = std::format!("input P{{f:T={default}}}");
  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(&probe)
  .ok()?;

  let definitions: &[_] = document.definitions();
  let [TypeSystemDefinitionOrExtension::Definition(described)] = definitions else {
    return None;
  };
  let TypeSystemDefinition::Type(TypeDefinition::InputObject(input)) = described.node() else {
    return None;
  };
  let values: &[_] = input.fields_definition()?.input_value_definitions();
  match values {
    [only] => Some(read(only.default_value()?.value())),
    _ => None,
  }
}

/// A server-printed default printed again the way the build prints every default, so two spellings
/// of one value compare equal — or `None` when it is not one const value.
fn canonical_default(default: &str) -> Option<String> {
  read_default_value(default, |value| {
    let mut out = String::new();
    canonical(value, &mut out).ok().map(|()| out)
  })
  .flatten()
}

#[cfg(test)]
mod tests;

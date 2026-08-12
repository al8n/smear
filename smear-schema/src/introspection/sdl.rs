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
//! It emits **nothing it has not checked**, and most of the checking happened before it ran.
//! [`decode`](super::decode) admits a name only if it spells one, and resolves type kinds and
//! directive locations to their enums, so what reaches this file is already drawn from the closed
//! vocabularies. What is left here is everything a *use site* decides — that a union member may
//! not be a wrapper, that an enum value may not be `true` — and the default value, which is the
//! one fragment no vocabulary covers and which goes through the parser itself. The only bytes that
//! reach the SDL parser unexamined are the renderer's own punctuation.
//!
//! [`RedefinedBuiltInType`]: super::super::SchemaErrorKind::RedefinedBuiltInType
//! [`TypeDef::is_built_in`]: super::super::TypeDef::is_built_in

use std::string::String;

use tokora::{Parse as _, Parser};

use smear_parser::graphql::{
  GraphQL,
  ast::{
    TypeDefinition, TypeSystemDefinition, TypeSystemDefinitionOrExtension, TypeSystemDocument,
  },
  error::GraphqlErrors,
  syntactic::{GraphqlLexer, type_system_document},
};

use super::{
  super::{
    builtin,
    // The SDL door's own owner-path helper, so `Query.hero.first` reads the same whichever door
    // refused it.
    error::owner_path as path,
    repr::TypeKind,
  },
  decode,
  error::{ResponseError, ResponseErrorKind},
  model::{
    IntrospectedDirective, IntrospectedEnumValue, IntrospectedField, IntrospectedInputValue,
    IntrospectedKind, IntrospectedSchema, IntrospectedType, Name, TypeRef,
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
    // Likewise the five built-in scalars — but only when the response agrees they are scalars. A
    // response that calls `Int` an enum is describing a schema in which `Int` is an enum, and
    // dropping it would build a different schema than the one described.
    if matches!(kind, TypeKind::Scalar) && builtin::BUILT_IN_SCALARS.contains(&name) {
      return Ok(());
    }

    match kind {
      TypeKind::Scalar => {
        self.out.push_str("scalar ");
        self.out.push_str(name);
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
      self.out.push_str("  ");
      self.out.push_str(name);
      self.arguments(&field.args, &path(&[owner, name]))?;
      self.out.push_str(": ");
      let rendered = type_reference(&field.ty, owner, name)?;
      self.out.push_str(&rendered);
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

  /// `name: Type` with an optional `= default`, shared by arguments and input-object fields.
  fn input_value(&mut self, value: &IntrospectedInputValue<'_>, owner: &str) -> Rendered {
    let name = value.name.as_str();
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
      self.out.push_str("  ");
      self.out.push_str(name);
      self.out.push('\n');
    }
    self.out.push_str("}\n\n");
    Ok(())
  }

  fn directive(&mut self, directive: &IntrospectedDirective<'_>) -> Rendered {
    let name = directive.name.as_str();
    // As with the built-in scalars: injected by the build, so re-emitting one would flip
    // `DirectiveDef::is_built_in` for a definition the two doors are supposed to agree on.
    if builtin::BUILT_IN_DIRECTIVES.contains(&name) {
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
  let probe = std::format!("input P{{f:T={default}}}");
  let refuse = || {
    ResponseError::new(ResponseErrorKind::MalformedDefaultValue, default)
      .owned_by(path(&[owner, subject]))
  };

  let document = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(&probe)
  .map_err(|_| refuse())?;

  let definitions: &[_] = document.definitions();
  let [TypeSystemDefinitionOrExtension::Definition(described)] = definitions else {
    return Err(refuse());
  };
  let TypeSystemDefinition::Type(TypeDefinition::InputObject(input)) = described.node() else {
    return Err(refuse());
  };
  let values: &[_] = match input.fields_definition() {
    Some(fields) => fields.input_value_definitions(),
    None => return Err(refuse()),
  };
  match values {
    [only] if only.default_value().is_some() => Ok(()),
    _ => Err(refuse()),
  }
}

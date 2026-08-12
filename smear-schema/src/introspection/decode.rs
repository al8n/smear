//! Reading a draft §4 response **shape-first**: one function per meta-schema type, dispatching on
//! the member it expects.
//!
//! # Why there is no tree between the response and the model
//!
//! The previous reader allocated the same bytes three times — into a JSON value tree, into records
//! whose every field was owned, and finally into the schema's interner. The first two exist only
//! because a general reader has to answer "what is this?" for every node before anything can ask
//! it a question. **The shape is closed**: 8 types and 28 members, none of them open, so at every
//! point this file already knows which member of which type it expects and dispatches on that.
//! Nothing is built in order to be inspected, and the model borrows the response outright.
//!
//! # The closed vocabularies are resolved here, not compared later
//!
//! `kind` and `locations` are typed `String` in a response and are `__TypeKind` and
//! `__DirectiveLocation` — closed sets. They are matched as bytes and produce their enums, so
//! [`UnknownTypeKind`] and [`UnknownDirectiveLocation`] are refusals *at the literal that spells
//! them*. `InvalidName` moved with them, because [`Name`] is validated at construction.
//!
//! One consequence is worth stating rather than discovering. The renderer used to check a `__Type`
//! in a fixed order — name present, name well spelled, then kind — so a type with two defects
//! reported whichever the renderer looked at first. Reading is now the thing that refuses, and it
//! reads in **document order**, so such a type reports whichever defect the *response* wrote
//! first. Every refusal that a single-defect response produces is unchanged, kind, subject and
//! owner alike; only the arbitration between two defects in one object moved, and it moved from an
//! order nobody chose to the one the reader actually walks.
//!
//! # The owner is an offset until something goes wrong
//!
//! A refusal names what it refused and what owns it — `User.pet` for a field, `User.pet.first` for
//! its argument — and an owner is a *path*, so building one eagerly would allocate a `String` per
//! field of a document that is about to be accepted. Worse, a member's owner is its parent's
//! `name`, which JSON does not promise to have written first. [`Owner`] carries byte offsets
//! instead, and reads the names back out of the response only when a refusal actually needs them.
//! Reading is fail-fast, so that path is walked at most once.
//!
//! [`UnknownTypeKind`]: ResponseErrorKind::UnknownTypeKind
//! [`UnknownDirectiveLocation`]: ResponseErrorKind::UnknownDirectiveLocation

use std::{boxed::Box, string::String, vec::Vec};

use super::{
  super::{error::owner_path, repr::DirectiveLocation},
  error::{ResponseError, ResponseErrorKind},
  json::{Scanned, Scanner},
  model::{
    IntrospectedDirective, IntrospectedEnumValue, IntrospectedField, IntrospectedInputValue,
    IntrospectedKind, IntrospectedSchema, IntrospectedType, Name, NamedTypeRef, TypeRef,
  },
};

/// Finds the `__Schema` in a response and reads it.
///
/// Three envelopes are accepted because all three are what people have in hand: the GraphQL
/// response a server returns (`{"data":{"__schema":…}}`), the same with the transport layer peeled
/// off (`{"__schema":…}`), and the `__Schema` object alone, which is what a fixture file usually
/// holds.
pub(super) fn read(response: &str) -> Scanned<IntrospectedSchema<'_>> {
  let Some(at) = locate(response)? else {
    return Err(ResponseError::new(
      ResponseErrorKind::MissingSchema,
      "expected `data.__schema`, `__schema`, or a `__Schema` object",
    ));
  };
  schema(&mut Scanner::at(response, at))
}

// ---------------------------------------------------------------------------------------------
// the envelope
// ---------------------------------------------------------------------------------------------

/// Returns where the `__Schema` object begins, or `None` when the response carries none.
///
/// # By key, and not by trying each envelope in turn
///
/// The three shapes are told apart by **which keys the root object has**, decided after the whole
/// root has been walked. A response that has a `data` and a malformed `__schema` under it
/// therefore reports the malformation: `data` is present, so `data.__schema` is the reading, and
/// there is no second attempt that could quietly succeed against a different one. Trying each
/// envelope until one parses would turn every malformed response into a search for an
/// interpretation that happens to work, which is precisely the failure this ordering exists to
/// prevent.
///
/// # The walk is also what proves the response is JSON
///
/// Members the door does not read are skipped, not ignored: skipping validates. So a syntax error
/// anywhere in the document — in `errors`, in `extensions`, after the closing brace — is
/// [`MalformedJson`](ResponseErrorKind::MalformedJson) and is reported before any question about
/// the *shape* is asked, which is the order the previous reader answered in and the order the two
/// error kinds' documentation describes.
fn locate(response: &str) -> Scanned<Option<usize>> {
  let mut scanner = Scanner::new(response);
  let root = {
    if !scanner.at_object() {
      // Valid JSON that is not an object has no keys, so it is none of the three envelopes. The
      // document is still proved to be JSON first: "not JSON" and "JSON carrying no `__schema`"
      // are different refusals with different audiences.
      scanner.skip_value()?;
      scanner.finish()?;
      return Ok(None);
    }
    scanner.position()
  };

  let mut under_data = None;
  let mut has_data = false;
  let mut at_root = None;
  let mut has_types = false;

  let mut members = scanner.enter_object("the response")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"data" => {
        has_data = true;
        under_data = locate_under_data(&mut scanner)?;
      }
      b"__schema" => {
        scanner.skip_ws();
        at_root = Some(scanner.position());
        scanner.skip_value()?;
      }
      b"types" => {
        has_types = true;
        scanner.skip_value()?;
      }
      _ => scanner.skip_value()?,
    }
  }
  scanner.finish()?;

  // A `data` shadows a root-level `__schema` whether or not it carries one of its own: a GraphQL
  // response puts the payload under `data`, and a root-level `__schema` beside it is not a second
  // place to look.
  Ok(match if has_data { under_data } else { at_root } {
    Some(at) => Some(at),
    // No `data`, no `__schema`: the object is either the `__Schema` itself or nothing this door
    // can use. `types` is the discriminator — it is the one member of `__Schema` no other envelope
    // has at its root.
    None if has_types => Some(root),
    None => None,
  })
}

/// Records where `data.__schema` begins, walking the rest of `data` regardless.
fn locate_under_data(scanner: &mut Scanner<'_>) -> Scanned<Option<usize>> {
  if !scanner.at_object() {
    scanner.skip_value()?;
    return Ok(None);
  }
  let mut found = None;
  let mut members = scanner.enter_object("`data`")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    if key.as_bytes() == b"__schema" {
      scanner.skip_ws();
      found = Some(scanner.position());
    }
    scanner.skip_value()?;
  }
  Ok(found)
}

// ---------------------------------------------------------------------------------------------
// who owns a refusal
// ---------------------------------------------------------------------------------------------

/// The artifact a refusal names as the owner of its subject, held as offsets until one happens.
#[derive(Debug, Clone, Copy)]
enum Owner {
  /// The artifact stands alone: a member of `__Schema.types`, or a `__Directive`.
  Unowned,
  /// The `schema` block, which owns the three root operation slots.
  Schema,
  /// The object beginning at this offset, named by its own `name` member.
  Object(usize),
  /// The object at `.1` qualified by the object at `.0` — `User.pet`, a field's argument list.
  Member(usize, usize),
}

impl Owner {
  /// Reads the owner's name back out of the response.
  ///
  /// `None` when there is nothing to name, including the case where the owning object's own `name`
  /// is absent, null or unreadable — a refusal with no owner is the same refusal, and inventing a
  /// path would be worse than omitting one.
  fn resolve(self, response: &str) -> Option<String> {
    match self {
      Self::Unowned => None,
      Self::Schema => Some(String::from("schema")),
      Self::Object(at) => peek_name(response, at).map(String::from),
      Self::Member(outer, inner) => Some(owner_path(&[
        peek_name(response, outer)?,
        peek_name(response, inner)?,
      ])),
    }
  }
}

/// Reads one object's own `name` member, without descending into anything else.
///
/// Cold: this runs on the error path only, over an object the reader has already walked once, and
/// reading stops at the first refusal so it runs at most once per response.
///
/// The walk runs to the closing brace rather than stopping at the first `name`, so that a member
/// written twice resolves to the same one here as it does in the model.
fn peek_name(response: &str, at: usize) -> Option<&str> {
  let mut scanner = Scanner::at(response, at);
  let mut members = scanner.enter_object("an owner").ok()?;
  let mut name = None;
  while let Some(key) = scanner.next_key(&mut members).ok()? {
    scanner.skip_ws();
    let value = scanner.position();
    // Skipped on the shared cursor whatever it turns out to be, so the walk stays on the rails for
    // a `name` that is null or of the wrong type, and read again from the offset just recorded.
    scanner.skip_value().ok()?;
    if key.as_bytes() == b"name" {
      name = Scanner::at(response, value)
        .string("an owner")
        .ok()
        .map(|(literal, _)| literal);
    }
  }
  name
}

fn refuse(kind: ResponseErrorKind, subject: &str, owner: Owner, response: &str) -> ResponseError {
  let error = ResponseError::new(kind, subject);
  match owner.resolve(response) {
    Some(owner) => error.owned_by(owner),
    None => error,
  }
}

// ---------------------------------------------------------------------------------------------
// members, and the three ways draft §4 spells "may be absent"
// ---------------------------------------------------------------------------------------------
//
// Every member is read into an `Option` slot, so "absent" and "present and null" stay
// distinguishable: a nullable member's slot is `Option<Option<_>>` and the outer layer is the one
// that answers whether the response wrote the member at all. Absent-and-required is the
// [`Scanner::missing`] refusal, absent-and-nullable is `None`, and absent-and-defaulted is the
// empty list or `false`.
//
// A member written **twice** overwrites, so the last one wins. That is not a choice this file
// makes so much as one it keeps: the previous reader built a JSON value tree first, and a tree's
// object is a map, so the second `"name"` replaced the first long before the shape was consulted.

/// Reads a member draft §4 declares nullable.
fn nullable<'a, T>(
  scanner: &mut Scanner<'a>,
  read: impl FnOnce(&mut Scanner<'a>) -> Scanned<T>,
) -> Scanned<Option<T>> {
  if scanner.null()? {
    return Ok(None);
  }
  read(scanner).map(Some)
}

fn list<'a, T>(
  scanner: &mut Scanner<'a>,
  what: &str,
  mut element: impl FnMut(&mut Scanner<'a>) -> Scanned<T>,
) -> Scanned<Vec<T>> {
  let mut out = Vec::new();
  let mut elements = scanner.enter_array(what)?;
  while scanner.next_element(&mut elements)? {
    out.push(element(scanner)?);
  }
  Ok(out)
}

fn nullable_list<'a, T>(
  scanner: &mut Scanner<'a>,
  what: &str,
  element: impl FnMut(&mut Scanner<'a>) -> Scanned<T>,
) -> Scanned<Option<Vec<T>>> {
  nullable(scanner, |scanner| list(scanner, what, element))
}

// ---------------------------------------------------------------------------------------------
// the closed vocabularies
// ---------------------------------------------------------------------------------------------

fn graphql_name<'a>(scanner: &mut Scanner<'a>, what: &str, owner: Owner) -> Scanned<Name<'a>> {
  let (literal, _) = scanner.string(what)?;
  Name::new(literal).ok_or_else(|| {
    refuse(
      ResponseErrorKind::InvalidName,
      literal,
      owner,
      scanner.response(),
    )
  })
}

fn type_kind(scanner: &mut Scanner<'_>, what: &str, owner: Owner) -> Scanned<IntrospectedKind> {
  let (literal, _) = scanner.string(what)?;
  IntrospectedKind::from_name(literal).ok_or_else(|| {
    refuse(
      ResponseErrorKind::UnknownTypeKind,
      literal,
      owner,
      scanner.response(),
    )
  })
}

fn location(scanner: &mut Scanner<'_>, owner: Owner) -> Scanned<DirectiveLocation> {
  let (literal, _) = scanner.string("__Directive.locations")?;
  DirectiveLocation::from_name(literal).ok_or_else(|| {
    refuse(
      ResponseErrorKind::UnknownDirectiveLocation,
      literal,
      owner,
      scanner.response(),
    )
  })
}

// ---------------------------------------------------------------------------------------------
// the eight types
// ---------------------------------------------------------------------------------------------

fn schema<'a>(scanner: &mut Scanner<'a>) -> Scanned<IntrospectedSchema<'a>> {
  let mut types = None;
  let mut query_type = None;
  let mut mutation_type = None;
  let mut subscription_type = None;
  let mut directives = None;

  let mut members = scanner.enter_object("__Schema")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"types" => types = Some(list(scanner, "__Schema.types", ty)?),
      b"queryType" => query_type = Some(root_slot(scanner)?),
      b"mutationType" => mutation_type = Some(nullable(scanner, root_slot)?),
      b"subscriptionType" => subscription_type = Some(nullable(scanner, root_slot)?),
      b"directives" => directives = Some(list(scanner, "__Schema.directives", directive)?),
      _ => scanner.skip_value()?,
    }
  }

  Ok(IntrospectedSchema {
    types: types.ok_or_else(|| scanner.missing("types"))?,
    query_type: query_type.ok_or_else(|| scanner.missing("queryType"))?,
    mutation_type: mutation_type.flatten(),
    subscription_type: subscription_type.flatten(),
    directives: directives.ok_or_else(|| scanner.missing("directives"))?,
  })
}

/// A root operation slot: a `__Type` read for its name alone, owned by the `schema` block.
fn root_slot<'a>(scanner: &mut Scanner<'a>) -> Scanned<NamedTypeRef<'a>> {
  let mut name = None;
  let mut members = scanner.enter_object("__Type")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"name" => {
        name = Some(nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", Owner::Schema)
        })?)
      }
      _ => scanner.skip_value()?,
    }
  }
  Ok(NamedTypeRef {
    name: name.flatten(),
  })
}

fn ty<'a>(scanner: &mut Scanner<'a>) -> Scanned<IntrospectedType<'a>> {
  scanner.skip_ws();
  let at = scanner.position();
  let owner = Owner::Object(at);

  let mut kind = None;
  let mut name = None;
  let mut fields = None;
  let mut input_fields = None;
  let mut interfaces = None;
  let mut enum_values = None;
  let mut possible_types = None;
  let mut is_one_of = None;

  let mut members = scanner.enter_object("__Type")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      // A member of `__Schema.types` stands alone, so its own kind and its own name are refused
      // with no owner — exactly as the renderer refused them.
      b"kind" => kind = Some(type_kind(scanner, "__Type.kind", Owner::Unowned)?),
      b"name" => {
        name = Some(nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", Owner::Unowned)
        })?)
      }
      b"fields" => {
        fields = Some(nullable_list(scanner, "__Type.fields", |scanner| {
          field(scanner, at)
        })?)
      }
      b"inputFields" => {
        input_fields = Some(nullable_list(scanner, "__Type.inputFields", |scanner| {
          input_value(scanner, owner)
        })?)
      }
      b"interfaces" => {
        interfaces = Some(nullable_list(scanner, "__Type.interfaces", |scanner| {
          type_ref(scanner, owner)
        })?)
      }
      b"enumValues" => {
        enum_values = Some(nullable_list(scanner, "__Type.enumValues", |scanner| {
          enum_value(scanner, owner)
        })?)
      }
      b"possibleTypes" => {
        possible_types = Some(nullable_list(scanner, "__Type.possibleTypes", |scanner| {
          type_ref(scanner, owner)
        })?)
      }
      b"isOneOf" => {
        is_one_of = Some(nullable(scanner, |scanner| {
          scanner.boolean("__Type.isOneOf")
        })?)
      }
      _ => scanner.skip_value()?,
    }
  }

  Ok(IntrospectedType {
    kind: kind.ok_or_else(|| scanner.missing("kind"))?,
    name: name.flatten(),
    fields: fields.flatten(),
    input_fields: input_fields.flatten(),
    interfaces: interfaces.flatten(),
    enum_values: enum_values.flatten(),
    possible_types: possible_types.flatten(),
    is_one_of: is_one_of.flatten(),
  })
}

/// A `__Field` of the `__Type` beginning at `owner_at`.
///
/// Its own name and its result type are owned by that type; its **arguments** are owned by the
/// two-segment path — `User.pet` — because that is what tells an argument of a field apart from a
/// field of a type when both are called `first`.
fn field<'a>(scanner: &mut Scanner<'a>, owner_at: usize) -> Scanned<IntrospectedField<'a>> {
  scanner.skip_ws();
  let at = scanner.position();
  let owner = Owner::Object(owner_at);

  let mut name = None;
  let mut args = None;
  let mut ty = None;

  let mut members = scanner.enter_object("__Field")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"name" => name = Some(graphql_name(scanner, "__Field.name", owner)?),
      b"args" => {
        args = Some(list(scanner, "__Field.args", |scanner| {
          input_value(scanner, Owner::Member(owner_at, at))
        })?)
      }
      b"type" => ty = Some(type_ref(scanner, owner)?),
      _ => scanner.skip_value()?,
    }
  }

  Ok(IntrospectedField {
    name: name.ok_or_else(|| scanner.missing("name"))?,
    args: args.unwrap_or_default(),
    ty: ty.ok_or_else(|| scanner.missing("type"))?,
  })
}

fn input_value<'a>(scanner: &mut Scanner<'a>, owner: Owner) -> Scanned<IntrospectedInputValue<'a>> {
  let mut name = None;
  let mut ty = None;
  let mut default_value = None;

  let mut members = scanner.enter_object("__InputValue")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"name" => name = Some(graphql_name(scanner, "__InputValue.name", owner)?),
      b"type" => ty = Some(type_ref(scanner, owner)?),
      // The one prose member in a draft §4 response, and so the one that can allocate.
      b"defaultValue" => {
        default_value = Some(nullable(scanner, |scanner| {
          scanner.text("__InputValue.defaultValue")
        })?)
      }
      _ => scanner.skip_value()?,
    }
  }

  Ok(IntrospectedInputValue {
    name: name.ok_or_else(|| scanner.missing("name"))?,
    ty: ty.ok_or_else(|| scanner.missing("type"))?,
    default_value: default_value.flatten(),
  })
}

fn enum_value<'a>(scanner: &mut Scanner<'a>, owner: Owner) -> Scanned<IntrospectedEnumValue<'a>> {
  let mut name = None;
  let mut members = scanner.enter_object("__EnumValue")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"name" => name = Some(graphql_name(scanner, "__EnumValue.name", owner)?),
      _ => scanner.skip_value()?,
    }
  }
  Ok(IntrospectedEnumValue {
    name: name.ok_or_else(|| scanner.missing("name"))?,
  })
}

fn directive<'a>(scanner: &mut Scanner<'a>) -> Scanned<IntrospectedDirective<'a>> {
  scanner.skip_ws();
  let owner = Owner::Object(scanner.position());

  let mut name = None;
  let mut locations = None;
  let mut args = None;
  let mut is_repeatable = None;

  let mut members = scanner.enter_object("__Directive")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"name" => name = Some(graphql_name(scanner, "__Directive.name", Owner::Unowned)?),
      b"locations" => {
        locations = Some(list(scanner, "__Directive.locations", |scanner| {
          location(scanner, owner)
        })?)
      }
      b"args" => {
        args = Some(list(scanner, "__Directive.args", |scanner| {
          input_value(scanner, owner)
        })?)
      }
      b"isRepeatable" => is_repeatable = Some(scanner.boolean("__Directive.isRepeatable")?),
      _ => scanner.skip_value()?,
    }
  }

  Ok(IntrospectedDirective {
    name: name.ok_or_else(|| scanner.missing("name"))?,
    locations: locations.ok_or_else(|| scanner.missing("locations"))?,
    args: args.unwrap_or_default(),
    is_repeatable: is_repeatable.unwrap_or(false),
  })
}

fn type_ref<'a>(scanner: &mut Scanner<'a>, owner: Owner) -> Scanned<TypeRef<'a>> {
  let mut kind = None;
  let mut name = None;
  let mut of_type = None;

  let mut members = scanner.enter_object("__Type")?;
  while let Some(key) = scanner.next_key(&mut members)? {
    match key.as_bytes() {
      b"kind" => kind = Some(type_kind(scanner, "__Type.kind", owner)?),
      b"name" => {
        name = Some(nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", owner)
        })?)
      }
      b"ofType" => {
        of_type = Some(nullable(scanner, |scanner| {
          type_ref(scanner, owner).map(Box::new)
        })?)
      }
      _ => scanner.skip_value()?,
    }
  }

  Ok(TypeRef {
    kind: kind.ok_or_else(|| scanner.missing("kind"))?,
    name: name.flatten(),
    of_type: of_type.flatten(),
  })
}

#[cfg(test)]
mod tests;

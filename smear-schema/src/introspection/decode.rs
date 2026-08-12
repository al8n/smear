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
//! `__DirectiveLocation` — closed sets. They are resolved to their enums at the literal, so
//! [`UnknownTypeKind`] and [`UnknownDirectiveLocation`] are refusals *at the literal that spells
//! them*. `InvalidName` moved with them, because [`Name`] is validated at construction.
//!
//! **A member key and a closed vocabulary are both dispatches, and both read the literal's decoded
//! value; a `Name` is the one string read raw.** Which of the [five kinds of string](super::json)
//! an escape may change is tabulated there, because the distinction belongs where the literals are
//! read rather than where they are used.
//!
//! One consequence is worth stating rather than discovering. The renderer used to check a `__Type`
//! in a fixed order — name present, name well spelled, then kind — so a type with two defects
//! reported whichever the renderer looked at first. Reading is now the thing that refuses, and it
//! reads in **document order**, so such a type reports whichever defect the *response* wrote
//! first. Every refusal that a single-defect response produces is unchanged, kind, subject and
//! owner alike; only the arbitration between two defects in one object moved, and it moved from an
//! order nobody chose to the one the reader actually walks.
//!
//! # A refusal derives nothing, because a refusal is not necessarily reported
//!
//! A member written twice takes a failure away again, as [`Slot`] describes, so the reader **builds
//! refusals and discards them**. Whatever building one costs is therefore paid once per failing
//! *occurrence*, and a response repeating one bad member k times pays k times over for an answer it
//! throws k−1 copies of away. When that cost is proportional to the response, the product is
//! quadratic in what a network peer chose to send — which is a denial of service and not a slow
//! path.
//!
//! Two derivations cost exactly that much, and both are staged rather than performed:
//!
//! - **The owner.** A refusal names what it refused and what owns it — `User.pet` for a field,
//!   `User.pet.first` for its argument — and an owner is a *path*, read back out of the owning
//!   object. Worse, a member's owner is its parent's `name`, which JSON does not promise to have
//!   written first, so it cannot simply be remembered on the way past. [`Owner`] carries byte
//!   offsets instead.
//! - **The position.** Every refusal the scanner raises ends `at line L column C`, and the two are
//!   counted by walking every byte before the cursor. That one is [`json`](super::json)'s to stage.
//!
//! [`read`] — the door, and nowhere else — pays for both, on the one refusal it returns. `Pending`
//! in [`error`](super::error) is where the two meet, so that a third expensive derivation added
//! later inherits the rule instead of re-learning it, and
//! `a_discarded_refusal_costs_a_constant_number_of_bytes` is what says the rule still holds.
//!
//! # A deferred failure says where the reader may carry on
//!
//! Deferring costs something on the other side of the same mechanism. A member's failure waits for
//! its object's closing brace, so by the time anything knows the member failed the reader has
//! already walked that object to its end — and the enclosing member, which has to go on reading in
//! case a later occurrence replaces the failure, would rewind and walk the whole of it again.
//! Nested, that is once per ancestor, and an `ofType` chain can put 128 of them between a defect
//! and the document.
//!
//! So a refusal [`Refusals::check`] produces carries the mark the reader stands at, and [`member`]
//! resumes from it. Which of the two paths a response takes is chosen by whoever sent it, so the
//! path the cheap reading belongs on is the one an attacker selects.
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
///
/// **This is also where a refusal stops being staged**, and it is the only place: a refusal carries
/// its [`Owner`] and the offset of its line and column until it is the refusal the door returns, so
/// each of the two walks over the response runs exactly once per response however many refusals
/// were built and discarded on the way. See [`Slot`] for why any are discarded, and `Pending` in
/// [`error`](super::error) for the invariant that makes it one mechanism rather than two.
pub(super) fn read(response: &str) -> Scanned<IntrospectedSchema<'_>> {
  locate_and_read(response).map_err(|refusal| refusal.resolve(response))
}

fn locate_and_read(response: &str) -> Scanned<IntrospectedSchema<'_>> {
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
/// **By the key, and not by its spelling.** `data`, `__schema` and `types` are
/// [dispatch keys](super::json): [`Scanner::next_key`] hands them back decoded, so `"data"`
/// selects this arm exactly as `"data"` does. Matching the raw literal instead would leave an
/// escaped `data` unrecognised, and the reading would then fall through to whatever else the root
/// happened to admit — the very outcome the paragraph above rules out, reached by the spelling
/// rather than by the order.
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
    match key.as_str() {
      "data" => {
        has_data = true;
        under_data = locate_under_data(&mut scanner)?;
      }
      "__schema" => {
        scanner.skip_ws();
        at_root = Some(scanner.position());
        scanner.skip_value()?;
      }
      "types" => {
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
    if key.as_str() == "__schema" {
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

/// The artifact a refusal names as the owner of its subject, held as offsets until the door
/// returns the refusal.
///
/// Not "until one happens": a refusal is *built* wherever a member could not be read, and a
/// member written twice can take one away again — see [`Slot`] — so building one has to be cheap
/// enough that a response repeating a bad member ten thousand times costs ten thousand cheap
/// refusals rather than ten thousand walks of the object that owns them. [`read`] resolves the one
/// that survives.
///
/// One of two things a refusal stages rather than derives; `Pending` in [`error`](super::error)
/// holds both and states the invariant they share.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(super) enum Owner {
  /// The artifact stands alone: a member of `__Schema.types`, or a `__Directive`.
  #[default]
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
  pub(super) fn resolve(self, response: &str) -> Option<String> {
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
    if key.as_str() == "name" {
      name = Scanner::at(response, value)
        .string("an owner")
        .ok()
        .map(|(literal, _)| literal);
    }
  }
  name
}

/// Builds a refusal, naming its owner by the offsets it will be read back from.
fn refuse(kind: ResponseErrorKind, subject: &str, owner: Owner) -> ResponseError {
  ResponseError::new(kind, subject).owned_at(owner)
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

/// One recognised member, as its **last** occurrence left it: where the value began, and what
/// reading it produced.
///
/// # Why a member's failure waits for the closing brace
///
/// Last-wins has to cover *whether the member could be read at all*, and not only which value ends
/// up in the slot. A tree-building reader collapsed `{"name":"bad-name","name":"ok"}` into one
/// member before the shape was ever consulted, so a first occurrence nobody kept could not refuse
/// the response. A reader that validates each occurrence where it meets it would let exactly that
/// first spelling decide, and last-wins would hold for the value and not for the verdict.
///
/// So a refusal is held in the slot the occurrence wrote, and a later occurrence replaces it like
/// any other value. Reading is still fail-fast at the *document* — [`Refusals::check`] reports
/// before the object is built, nothing accumulates across objects — it is fail-fast at the member
/// that survived rather than at the first one written.
type Slot<T> = Option<(usize, Scanned<T>)>;

/// Reads one occurrence of a recognised member into its slot, replacing whatever an earlier
/// occurrence left there.
fn member<'a, T>(
  scanner: &mut Scanner<'a>,
  slot: &mut Slot<T>,
  read: impl FnOnce(&mut Scanner<'a>) -> Scanned<T>,
) -> Scanned {
  scanner.skip_ws();
  let at = scanner.mark();
  match read(scanner) {
    Ok(value) => *slot = Some((at.position(), Ok(value))),
    Err(refusal) => {
      // Whatever the refusal did to the cursor, it has to end up at the end of this member's value:
      // the object's remaining members still have to be read, because one of them may be this
      // member written again.
      //
      // A refusal [`Refusals::check`] held until its object closed already names a value walked to
      // its end, and says where that end is. Taking the mark is the difference between walking a
      // refused subtree once and walking it once per enclosing member — which along an `ofType`
      // chain is up to the 128 the nesting bound allows, so a large skipped string in the deepest
      // object would otherwise be scanned 128 times for a response that is refused either way.
      //
      // Otherwise the cursor stopped wherever the refusal happened, which may be several containers
      // deep, so put it back and walk the value as an uninterpreted one instead.
      //
      // The walk cannot fail for a reason of its own. `locate` proved the whole document is JSON
      // before any of this ran, and it walked these bytes at a nesting depth at least as great as
      // this pass sees, so neither the grammar nor the bound can refuse them a second time.
      match refusal.resume() {
        Some(mark) if mark.resumes(&at) => scanner.rewind(mark),
        _ => {
          scanner.rewind(at);
          scanner.skip_value()?;
        }
      }
      *slot = Some((at.position(), Err(refusal)));
    }
  }
  Ok(())
}

/// The refusal an object's surviving members produce, once the object has closed.
#[derive(Debug, Default)]
struct Refusals {
  first: Option<(usize, ResponseError)>,
}

impl Refusals {
  /// Takes a slot's value, keeping its refusal if it has one.
  fn take<T>(&mut self, slot: Slot<T>) -> Option<T> {
    match slot {
      None => None,
      Some((_, Ok(value))) => Some(value),
      Some((at, Err(refusal))) => {
        // Document order, which is offset order: members are read left to right, so the smallest
        // offset among the surviving failures is the defect the response wrote first — the same
        // one a reader that stopped at the first failure would have named, whenever no duplicate
        // took that failure away.
        if !matches!(&self.first, Some((first, _)) if *first <= at) {
          self.first = Some((at, refusal));
        }
        None
      }
    }
  }

  /// Reports that refusal, if the object had one.
  ///
  /// Stamped with where the reader stands, which is immediately past the object's closing brace: a
  /// refusal held this long is by construction a refusal about a value the reader already walked
  /// whole, so [`member`] resumes from the stamp rather than walking the value again. Stamped on
  /// every call rather than only the first, because each enclosing object closes later than the one
  /// inside it and the mark has to name the value *its own* caller is stepping over.
  fn check(self, scanner: &Scanner<'_>) -> Scanned {
    match self.first {
      Some((_, refusal)) => Err(refusal.resume_at(scanner.mark())),
      None => Ok(()),
    }
  }
}

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

/// A `Name`, matched against the literal's **raw** bytes.
///
/// The one kind of string the door reads raw, and it is a decision priced at [`Name`] rather than
/// a shortcut taken here: the type has no owning variant, so an escaped spelling is refused as
/// [`InvalidName`](ResponseErrorKind::InvalidName) with the literal as the subject. See
/// [the five kinds of string](super::json) for the other four, all of which decode.
fn graphql_name<'a>(scanner: &mut Scanner<'a>, what: &str, owner: Owner) -> Scanned<Name<'a>> {
  let (literal, _) = scanner.string(what)?;
  Name::new(literal).ok_or_else(|| refuse(ResponseErrorKind::InvalidName, literal, owner))
}

/// A `__TypeKind`, resolved from the literal's **decoded** value.
///
/// A closed vocabulary is a dispatch: the literal names one of eight values, and `"OBJECT"`
/// names `OBJECT`. Decoding costs nothing on the path a real response takes — a spelling with no
/// backslash in it is [`Text::Borrowed`](super::json::Text::Borrowed), the response's own bytes —
/// and the text is dropped as soon as the enum is out of it.
fn type_kind(scanner: &mut Scanner<'_>, what: &str, owner: Owner) -> Scanned<IntrospectedKind> {
  let literal = scanner.text(what)?;
  IntrospectedKind::from_name(literal.as_str())
    .ok_or_else(|| refuse(ResponseErrorKind::UnknownTypeKind, literal.as_str(), owner))
}

/// A `__DirectiveLocation`, resolved from the literal's **decoded** value, as [`type_kind`] is.
fn location(scanner: &mut Scanner<'_>, owner: Owner) -> Scanned<DirectiveLocation> {
  let literal = scanner.text("__Directive.locations")?;
  DirectiveLocation::from_name(literal.as_str()).ok_or_else(|| {
    refuse(
      ResponseErrorKind::UnknownDirectiveLocation,
      literal.as_str(),
      owner,
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
    match key.as_str() {
      "types" => member(scanner, &mut types, |scanner| {
        list(scanner, "__Schema.types", ty)
      })?,
      "queryType" => member(scanner, &mut query_type, root_slot)?,
      "mutationType" => member(scanner, &mut mutation_type, |scanner| {
        nullable(scanner, root_slot)
      })?,
      "subscriptionType" => member(scanner, &mut subscription_type, |scanner| {
        nullable(scanner, root_slot)
      })?,
      "directives" => member(scanner, &mut directives, |scanner| {
        list(scanner, "__Schema.directives", directive)
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let types = refusals.take(types);
  let query_type = refusals.take(query_type);
  let mutation_type = refusals.take(mutation_type);
  let subscription_type = refusals.take(subscription_type);
  let directives = refusals.take(directives);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "name" => member(scanner, &mut name, |scanner| {
        nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", Owner::Schema)
        })
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let name = refusals.take(name);
  refusals.check(scanner)?;

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
    match key.as_str() {
      // A member of `__Schema.types` stands alone, so its own kind and its own name are refused
      // with no owner — exactly as the renderer refused them.
      "kind" => member(scanner, &mut kind, |scanner| {
        type_kind(scanner, "__Type.kind", Owner::Unowned)
      })?,
      "name" => member(scanner, &mut name, |scanner| {
        nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", Owner::Unowned)
        })
      })?,
      "fields" => member(scanner, &mut fields, |scanner| {
        nullable_list(scanner, "__Type.fields", |scanner| field(scanner, at))
      })?,
      "inputFields" => member(scanner, &mut input_fields, |scanner| {
        nullable_list(scanner, "__Type.inputFields", |scanner| {
          input_value(scanner, owner)
        })
      })?,
      "interfaces" => member(scanner, &mut interfaces, |scanner| {
        nullable_list(scanner, "__Type.interfaces", |scanner| {
          type_ref(scanner, owner)
        })
      })?,
      "enumValues" => member(scanner, &mut enum_values, |scanner| {
        nullable_list(scanner, "__Type.enumValues", |scanner| {
          enum_value(scanner, owner)
        })
      })?,
      "possibleTypes" => member(scanner, &mut possible_types, |scanner| {
        nullable_list(scanner, "__Type.possibleTypes", |scanner| {
          type_ref(scanner, owner)
        })
      })?,
      "isOneOf" => member(scanner, &mut is_one_of, |scanner| {
        nullable(scanner, |scanner| scanner.boolean("__Type.isOneOf"))
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let kind = refusals.take(kind);
  let name = refusals.take(name);
  let fields = refusals.take(fields);
  let input_fields = refusals.take(input_fields);
  let interfaces = refusals.take(interfaces);
  let enum_values = refusals.take(enum_values);
  let possible_types = refusals.take(possible_types);
  let is_one_of = refusals.take(is_one_of);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "name" => member(scanner, &mut name, |scanner| {
        graphql_name(scanner, "__Field.name", owner)
      })?,
      "args" => member(scanner, &mut args, |scanner| {
        list(scanner, "__Field.args", |scanner| {
          input_value(scanner, Owner::Member(owner_at, at))
        })
      })?,
      "type" => member(scanner, &mut ty, |scanner| type_ref(scanner, owner))?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let name = refusals.take(name);
  let args = refusals.take(args);
  let ty = refusals.take(ty);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "name" => member(scanner, &mut name, |scanner| {
        graphql_name(scanner, "__InputValue.name", owner)
      })?,
      "type" => member(scanner, &mut ty, |scanner| type_ref(scanner, owner))?,
      // The one prose member in a draft §4 response, and so the one whose text is kept.
      "defaultValue" => member(scanner, &mut default_value, |scanner| {
        nullable(scanner, |scanner| scanner.text("__InputValue.defaultValue"))
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let name = refusals.take(name);
  let ty = refusals.take(ty);
  let default_value = refusals.take(default_value);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "name" => member(scanner, &mut name, |scanner| {
        graphql_name(scanner, "__EnumValue.name", owner)
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let name = refusals.take(name);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "name" => member(scanner, &mut name, |scanner| {
        graphql_name(scanner, "__Directive.name", Owner::Unowned)
      })?,
      "locations" => member(scanner, &mut locations, |scanner| {
        list(scanner, "__Directive.locations", |scanner| {
          location(scanner, owner)
        })
      })?,
      "args" => member(scanner, &mut args, |scanner| {
        list(scanner, "__Directive.args", |scanner| {
          input_value(scanner, owner)
        })
      })?,
      "isRepeatable" => member(scanner, &mut is_repeatable, |scanner| {
        scanner.boolean("__Directive.isRepeatable")
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let name = refusals.take(name);
  let locations = refusals.take(locations);
  let args = refusals.take(args);
  let is_repeatable = refusals.take(is_repeatable);
  refusals.check(scanner)?;

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
    match key.as_str() {
      "kind" => member(scanner, &mut kind, |scanner| {
        type_kind(scanner, "__Type.kind", owner)
      })?,
      "name" => member(scanner, &mut name, |scanner| {
        nullable(scanner, |scanner| {
          graphql_name(scanner, "__Type.name", owner)
        })
      })?,
      "ofType" => member(scanner, &mut of_type, |scanner| {
        nullable(scanner, |scanner| type_ref(scanner, owner).map(Box::new))
      })?,
      _ => scanner.skip_value()?,
    }
  }

  let mut refusals = Refusals::default();
  let kind = refusals.take(kind);
  let name = refusals.take(name);
  let of_type = refusals.take(of_type);
  refusals.check(scanner)?;

  Ok(TypeRef {
    kind: kind.ok_or_else(|| scanner.missing("kind"))?,
    name: name.flatten(),
    of_type: of_type.flatten(),
  })
}

#[cfg(test)]
mod tests;

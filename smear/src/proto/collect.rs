//! Draft §6.3 `CollectFields`, and the response-key interner it groups into.
//!
//! # Grouping, and why the order survives
//!
//! `CollectFields` produces an *ordered* map from response key to the list of selections that
//! landed under it, and both halves of that matter: the key order is the field order of the
//! response object (draft §7.1.1 requires it to follow the query), and the list order is what
//! `MergeSelectionSets` concatenates.
//!
//! Neither is stored as a map. Selections are appended to a flat vector as `(group, field)`, with
//! the group discovered by scanning the groups collected so far — a linear scan, over a set whose
//! size is the number of *distinct response keys in one selection set*, which is small and
//! bounded by the document. A stable sort by group then makes every group a contiguous range,
//! preserving document order inside each. One vector, one sort, no allocation per key and no hash.
//!
//! # `visitedFragments` is per collection, not per operation
//!
//! Draft §6.3 threads `visitedFragments` through a single `CollectFields` call so a fragment
//! reached twice through different spreads is expanded once. It is deliberately *not* carried
//! across sibling object values: two elements of a list each collect their own fields, and a
//! fragment must expand in both.

use tokora::{SimpleSpan, span::AsSpan};

use crate::{
  parser::graphql::ast::{
    Directive, Directives, ExecutableDefinition, ExecutableDocument, Field, InputValue, Selection,
    SelectionSet,
  },
  validator::schema::{Schema, TypeId},
};

use super::{
  Values,
  error::{ConditionFault, Raw},
};

/// A `@skip`/`@include` condition that could not be read, and where in the document to point.
///
/// Collection stops at the first one, which is also what the reference implementation does — it
/// throws out of `collectFields`, so a selection set with two unreadable conditions produces one
/// error and not two.
pub(super) struct Fault {
  pub(super) raw: Raw,
  /// The `if` argument's value, or the directive itself when there is no `if` argument to point at.
  pub(super) location: SimpleSpan,
}

/// One response key's group of selections.
#[derive(Debug, Clone, Copy)]
pub(super) struct Group {
  /// The interned response key.
  pub(super) key: u32,
  /// Where the group's selections start in the flat vector.
  pub(super) start: u32,
  /// How many selections it holds. Always at least one.
  pub(super) len: u32,
}

/// The response keys and type names a response refers to, held once as bytes.
///
/// A response key repeats on every element of a list, so storing the bytes per slot would make a
/// thousand-element list a thousand copies of the same handful of names. Slots carry a `u32` into
/// this table instead. It also keeps the source type `S` out of the response types entirely: a
/// [`Node`](super::Node) is generic only over the driver's value.
#[derive(Debug, Default)]
pub(super) struct Interner {
  bytes: std::vec::Vec<u8>,
  spans: std::vec::Vec<(u32, u32)>,
}

impl Interner {
  /// Returns the id for `bytes`, adding it if it is not already there.
  pub(super) fn intern(&mut self, bytes: &[u8]) -> u32 {
    for (index, &(start, len)) in self.spans.iter().enumerate() {
      if &self.bytes[start as usize..(start + len) as usize] == bytes {
        return index as u32;
      }
    }
    let start = self.bytes.len() as u32;
    self.bytes.extend_from_slice(bytes);
    self.spans.push((start, bytes.len() as u32));
    (self.spans.len() - 1) as u32
  }

  #[inline]
  pub(super) fn bytes(&self) -> &[u8] {
    &self.bytes
  }

  #[inline]
  pub(super) fn spans(&self) -> &[(u32, u32)] {
    &self.spans
  }

  pub(super) fn clear(&mut self) {
    self.bytes.clear();
    self.spans.clear();
  }
}

/// Draft §6.3 `CollectFields`, over the concatenation of several selection sets.
///
/// Several rather than one because draft §6.4's `MergeSelectionSets` hands the sub-selections of
/// every field that shared a response key to the next round of collection, and running
/// `CollectFields` once over the concatenation is what that step means.
///
/// The [`Fault`] is a `@skip`/`@include` condition that could not be read. Nothing was collected
/// when one is returned, so the caller raises it at the object whose selection set this was.
#[allow(clippy::too_many_arguments)]
pub(super) fn collect_fields<'a, S, V>(
  schema: &Schema,
  document: &'a ExecutableDocument<S>,
  object_type: TypeId,
  sets: &[&'a SelectionSet<S>],
  ctx: &mut V,
  interner: &mut Interner,
  visited: &mut std::vec::Vec<u32>,
  fields: &mut std::vec::Vec<(u32, &'a Field<S>)>,
  groups: &mut std::vec::Vec<Group>,
) -> Result<(), Fault>
where
  S: AsRef<[u8]>,
  V: Values,
{
  fields.clear();
  groups.clear();
  visited.clear();
  for set in sets {
    walk(
      schema,
      document,
      object_type,
      set,
      ctx,
      interner,
      visited,
      fields,
      groups,
    )?;
  }
  // Stable, so document order inside a group survives; by group, so every group is contiguous.
  fields.sort_by_key(|&(group, _)| group);
  let mut cursor = 0usize;
  for (index, group) in groups.iter_mut().enumerate() {
    let start = cursor;
    while cursor < fields.len() && fields[cursor].0 as usize == index {
      cursor += 1;
    }
    group.start = start as u32;
    group.len = (cursor - start) as u32;
  }
  Ok(())
}

#[allow(clippy::too_many_arguments)]
fn walk<'a, S, V>(
  schema: &Schema,
  document: &'a ExecutableDocument<S>,
  object_type: TypeId,
  set: &'a SelectionSet<S>,
  ctx: &mut V,
  interner: &mut Interner,
  visited: &mut std::vec::Vec<u32>,
  fields: &mut std::vec::Vec<(u32, &'a Field<S>)>,
  groups: &mut std::vec::Vec<Group>,
) -> Result<(), Fault>
where
  S: AsRef<[u8]>,
  V: Values,
{
  for selection in set.selections() {
    match selection {
      Selection::Field(field) => {
        if !included(field.directives(), ctx, interner)? {
          continue;
        }
        let key = match field.alias() {
          Some(alias) => alias.name().source().as_ref(),
          None => field.name().source().as_ref(),
        };
        let key = interner.intern(key);
        let group = match groups.iter().position(|g| g.key == key) {
          Some(index) => index as u32,
          None => {
            groups.push(Group {
              key,
              start: 0,
              len: 0,
            });
            (groups.len() - 1) as u32
          }
        };
        fields.push((group, field));
      }
      Selection::FragmentSpread(spread) => {
        if !included(spread.directives(), ctx, interner)? {
          continue;
        }
        let name = spread.name().source().as_ref();
        let Some(index) = fragment_index(document, name) else {
          // Draft 5.5.2.1 makes an undefined spread a validation failure, so this is unreachable
          // for a validated document. Skipping is the only behaviour that cannot invent a field.
          continue;
        };
        if visited.contains(&index) {
          continue;
        }
        visited.push(index);
        let Some(ExecutableDefinition::Fragment(fragment)) = document
          .definitions()
          .get(index as usize)
          .map(|described| described.node())
        else {
          continue;
        };
        let condition = fragment.type_condition().name().source().as_ref();
        if !applies(schema, condition, object_type) {
          continue;
        }
        walk(
          schema,
          document,
          object_type,
          fragment.selection_set(),
          ctx,
          interner,
          visited,
          fields,
          groups,
        )?;
      }
      Selection::InlineFragment(inline) => {
        if !included(inline.directives(), ctx, interner)? {
          continue;
        }
        if let Some(condition) = inline.type_condition()
          && !applies(schema, condition.name().source().as_ref(), object_type)
        {
          continue;
        }
        walk(
          schema,
          document,
          object_type,
          inline.selection_set(),
          ctx,
          interner,
          visited,
          fields,
          groups,
        )?;
      }
    }
  }
  Ok(())
}

/// Draft §6.3's `DoesFragmentTypeApply`, all three arms at once.
///
/// [`Schema::is_possible_object`] is one bitset test and already answers the object, interface and
/// union cases the specification spells out separately — the same word the validator's draft
/// 5.5.2.3 reads.
fn applies(schema: &Schema, condition: &[u8], object_type: TypeId) -> bool {
  let Some(sym) = schema.sym(condition) else {
    return false;
  };
  let Some(id) = schema.type_of_sym(sym) else {
    return false;
  };
  schema.is_possible_object(id, object_type)
}

/// Draft §6.3 steps 3.a and 3.b: `@skip`, then `@include`.
///
/// Two passes rather than one over the directive list, because the step order is the
/// specification's and not the document's. A selection carrying both is removed if `@skip` says
/// so whatever `@include` says — and once step 3.a has removed it, step 3.b never runs, so
/// `{ f @include(if: $unreadable) @skip(if: true) }` produces no error. Reading them in document
/// order would raise one, and the reference implementation does not.
fn included<S, V>(
  directives: Option<&Directives<S>>,
  ctx: &mut V,
  interner: &mut Interner,
) -> Result<bool, Fault>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let Some(directives) = directives else {
    return Ok(true);
  };
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"skip" && condition_is_true(directive, ctx, interner)?
    {
      return Ok(false);
    }
  }
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"include"
      && !condition_is_true(directive, ctx, interner)?
    {
      return Ok(false);
    }
  }
  Ok(true)
}

/// Whether the directive's `if` argument is `true`, or why it could not be read as a boolean.
///
/// # Why a condition that cannot be read is an error and not a `false`
///
/// Because the two directives consume the answer with opposite sign. `@skip` removes the selection
/// when the condition is `true` and `@include` removes it when it is not, so a boolean default
/// that closes the disclosure for one opens it for the other: `false` keeps an `@skip`ped
/// selection and `true` keeps an `@include`d one. No third boolean exists. A guard whose condition
/// could not be evaluated therefore cannot be answered with a boolean at all, and the only outcome
/// safe under both senses is to raise and let draft §6.4.4 null the position.
///
/// # What the reference implementation does, measured
///
/// `graphql-js` 16.11.0 reads the condition through `getDirectiveValues`, which is full
/// `CoerceArgumentValues` over the directive's arguments, and raises on all three of the failures
/// draft §6.4.1 names. Run against it, `query ($flag: Boolean = true) { secret @skip(if: $flag) }`
/// with a runtime `flag: null` — a document its own validator accepts — answers
/// `{"errors":[{"message":"Argument \"if\" of non-null type \"Boolean!\" must not be null.",
/// "locations":[…]}],"data":null}`: the error carries no `path`, because at the root selection set
/// there is no field to attribute it to, and `data` is present and null, which is draft §7.1.1's
/// shape for an error raised *during* execution rather than before it. A genuine request error —
/// §6.1 `CoerceVariableValues` failing — omits `data` entirely there, so the two are distinct and
/// this is the field-shaped one. Several levels down the same condition produces the same message
/// with the enclosing object's path, and nulls that object.
///
/// This function reproduces that, including the messages word for word, with one deliberate
/// divergence: a value that is neither null nor a boolean.
/// [`ConditionFault::NotABoolean`] raises where `graphql-js` treats the directive as inert and
/// **returns the guarded selection**. That input needs an invalid document — draft 5.8.5 forbids a
/// non-`Boolean` variable at the `Boolean!` location — or a driver whose §6.1 did not coerce, so
/// no conforming request can tell the two apart; and of the two answers only this one is safe
/// under both senses.
fn condition_is_true<S, V>(
  directive: &Directive<S>,
  ctx: &mut V,
  interner: &mut Interner,
) -> Result<bool, Fault>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let argument = directive.arguments().and_then(|arguments| {
    arguments
      .arguments()
      .iter()
      .find(|argument| argument.name().source().as_ref() == b"if")
  });
  let Some(argument) = argument else {
    return Err(Fault {
      raw: Raw::DirectiveCondition {
        fault: ConditionFault::Missing,
      },
      location: *directive.span(),
    });
  };
  // Every remaining failure is about the value, so it is what the error points at — the same node
  // `graphql-js` reports, which for `if: $flag` is the variable and not the directive.
  let location = *argument.value().as_span();
  let unreadable = |fault| Fault {
    raw: Raw::DirectiveCondition { fault },
    location,
  };
  match argument.value() {
    InputValue::Boolean(literal) => Ok(literal.value()),
    InputValue::Variable(spelled) => {
      // Read once. Interning the name costs a scan of the name table, so it happens only on the
      // branch that needs it for a message.
      let spelling = spelled.name().source().as_ref();
      match core::str::from_utf8(spelling)
        .ok()
        .and_then(|name| ctx.variable(name))
      {
        None => Err(unreadable(ConditionFault::VariableMissing {
          variable: interner.intern(spelling),
        })),
        Some(value) if ctx.is_null(&value) => Err(unreadable(ConditionFault::Null)),
        Some(value) => ctx
          .as_bool(&value)
          .ok_or_else(|| unreadable(ConditionFault::NotABoolean)),
      }
    }
    InputValue::Null(_) => Err(unreadable(ConditionFault::Null)),
    _ => Err(unreadable(ConditionFault::NotABoolean)),
  }
}

fn fragment_index<S>(document: &ExecutableDocument<S>, name: &[u8]) -> Option<u32>
where
  S: AsRef<[u8]>,
{
  document
    .definitions()
    .iter()
    .position(|described| match described.node() {
      ExecutableDefinition::Fragment(fragment) => fragment.name().source().as_ref() == name,
      ExecutableDefinition::Operation(_) => false,
    })
    .map(|index| index as u32)
}

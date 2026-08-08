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
pub(super) struct Fault<'a> {
  pub(super) raw: Raw,
  /// The `if` argument's value, or the directive itself when there is no `if` argument to point at.
  pub(super) location: SimpleSpan,
  /// A name the message wants, still as the document's own bytes.
  ///
  /// Deliberately not an interner id. A fault means this collection is about to be *undone* —
  /// every response key it interned goes back, because a key belonging to a field that never became
  /// a position must not spend a later sibling's budget. An id minted before that restore would
  /// point into bytes the restore removes, which is the truncation defect the checked interner
  /// exists to refuse. So the caller interns this *after* restoring, when the arena has room again
  /// and the id it gets back is one that will still be there.
  pub(super) name: Option<&'a [u8]>,
}

/// What is left of [`Limits::max_selection_visits`](super::Limits::max_selection_visits).
///
/// # It is per operation, and that is the whole point
///
/// A per-*call* budget would bound one selection set and nothing else, and `collect_fields` runs
/// once per object position — so the total would be `positions × budget`, with positions coming
/// from the driver's list lengths. That is the product shape that has already cost this module two
/// rounds. Counting down across the whole operation means a driver **cannot** amplify collection
/// work at all: the query alone decides how much walking there is to do.
///
/// It bounds [`walk`]'s explicit stack too, since a frame is only ever pushed by a selection that
/// was charged — which is why deleting the recursion needed no depth knob of its own.
///
/// What it does *not* bound is the cost of *one* visit: `fragment_index` scans the definitions and
/// [`Interner::intern`] scans the names, both linear. Those are al8n/smear#141, and they are a
/// query quantity multiplying a query quantity — which is the property that matters here, because
/// with this budget in place no driver answer multiplies them.
pub(super) struct Visits {
  left: u32,
  limit: u32,
}

impl Visits {
  #[inline]
  pub(super) const fn new(limit: u32) -> Self {
    Self { left: limit, limit }
  }

  /// Charges one examined selection, or refuses once the operation has spent its budget.
  #[inline]
  fn spend(&mut self, location: SimpleSpan) -> Result<(), Fault<'static>> {
    match self.left.checked_sub(1) {
      Some(left) => {
        self.left = left;
        Ok(())
      }
      None => Err(Fault {
        raw: Raw::CollectionBudget { limit: self.limit },
        location,
        name: None,
      }),
    }
  }
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
///
/// # Why this one refuses instead of growing
///
/// Every other table here holds something the document or the schema produced, and both are
/// already in memory before the executor exists. This one also holds **driver** text:
/// [`Executor::handle_field_error`](super::Executor::handle_field_error) interns the message it is
/// given, once per failed position, and neither the message's length nor the number of failures is
/// anything the query bounds.
///
/// Left alone that is not merely unbounded memory, it is **silent corruption**. Offsets into the
/// arena are `u32`; an arena past four gigabytes does not fail to allocate, it *truncates*, and
/// every name interned afterwards reads back somebody else's bytes — wrong response keys, wrong
/// `__typename`, in a response that still looks well formed. A refusal is a contract a caller can
/// act on; that is not. So the narrowing is checked rather than argued, and the ceiling is what
/// keeps the check from ever being the thing that fires.
#[derive(Debug)]
pub(super) struct Interner {
  bytes: std::vec::Vec<u8>,
  spans: std::vec::Vec<(u32, u32)>,
  /// [`Limits::max_interned_bytes`](super::Limits::max_interned_bytes).
  cap: u32,
}

impl Interner {
  #[inline]
  pub(super) const fn new(cap: u32) -> Self {
    Self {
      bytes: std::vec::Vec::new(),
      spans: std::vec::Vec::new(),
      cap,
    }
  }

  /// The ceiling this table refuses at, for the message that reports the refusal.
  #[inline]
  pub(super) const fn cap(&self) -> u32 {
    self.cap
  }

  /// Returns the id for `bytes`, adding it if it is not already there, or `None` when it will not
  /// fit.
  ///
  /// `None` is a storage refusal and never a lookup failure: a name already present is always
  /// returned, whatever the ceiling says, so a full arena degrades what it *records* and never
  /// what it can still *read*.
  pub(super) fn intern(&mut self, bytes: &[u8]) -> Option<u32> {
    for (index, &(start, len)) in self.spans.iter().enumerate() {
      if &self.bytes[start as usize..(start + len) as usize] == bytes {
        return Some(index as u32);
      }
    }
    // Checked, not reasoned about. The ceiling below makes each of these unreachable, and they
    // stay because "unreachable given the ceiling" is exactly the kind of claim that stops being
    // true when somebody sets a different ceiling.
    let start = u32::try_from(self.bytes.len()).ok()?;
    let len = u32::try_from(bytes.len()).ok()?;
    let end = start.checked_add(len)?;
    if end > self.cap {
      return None;
    }
    let id = u32::try_from(self.spans.len()).ok()?;
    self.bytes.extend_from_slice(bytes);
    self.spans.push((start, len));
    Some(id)
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

  pub(super) fn set_cap(&mut self, cap: u32) {
    self.cap = cap;
  }

  /// Where the arena stands, so a failed collection or expansion can put it back.
  #[inline]
  pub(super) fn mark(&self) -> (usize, usize) {
    (self.bytes.len(), self.spans.len())
  }

  /// Undoes every name interned since `mark`.
  ///
  /// Sound only because the ids handed out in between die with the structures being undone. The
  /// one id that would have escaped — a variable's spelling inside a collection fault's message —
  /// is minted after this runs and not before, which is why [`Fault::name`](super::collect::Fault)
  /// carries bytes.
  #[inline]
  pub(super) fn restore(&mut self, (bytes, spans): (usize, usize)) {
    self.bytes.truncate(bytes);
    self.spans.truncate(spans);
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
  stack: &mut std::vec::Vec<(&'a SelectionSet<S>, usize)>,
  visits: &mut Visits,
) -> Result<(), Fault<'a>>
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
      stack,
      visits,
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

/// Draft §6.3's descent, over an explicit stack rather than the call stack.
///
/// # Why this is not recursive
///
/// A named fragment chain is **flat in the document**: `fragment F0 … ...F1`, each definition at
/// nesting depth one, so no parser depth limit sees it and none can. A recursive walk spends one
/// native frame per link, and measured on this tree a chain of 1,500 fragments — well under
/// 200 KB of perfectly valid text — overflowed the stack and took the process down with
/// `SIGABRT`. That is not a catchable panic: a server cannot turn it into a `400`, and one request
/// kills every other in flight.
///
/// So the frame is gone rather than counted. A depth *ceiling* on a recursive walk would still be
/// a ceiling whose right value depends on the deployment's stack size, its build profile and its
/// frame layout; an explicit stack makes the question disappear, because depth is heap the visit
/// budget already bounds.
///
/// Inline fragments recurse in the document rather than through definitions, and are **not** the
/// reachable case: the parser aborts on its own at around sixty levels (al8n/smear#61), so no
/// document deep enough to trouble this walk survives to reach it. The flat chain is the one that
/// gets here.
///
/// # The order is the recursion's, exactly
///
/// Draft §6.3 fixes response-key order to document order, and `MergeSelectionSets` concatenates in
/// document order within a key — so this has to be pre-order depth-first, entering a fragment
/// where its spread sits and resuming at the *next sibling* afterwards. That is why the stack
/// holds `(set, index)` and not just `set`: the index is where the recursion would have resumed.
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
  stack: &mut std::vec::Vec<(&'a SelectionSet<S>, usize)>,
  visits: &mut Visits,
) -> Result<(), Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  stack.clear();
  stack.push((set, 0));

  while let Some(&mut (set, ref mut cursor)) = stack.last_mut() {
    let selections = set.selections();
    let Some(selection) = selections.get(*cursor) else {
      stack.pop();
      continue;
    };
    *cursor += 1;

    // Charged here, before the arms, so that *every* selection examined costs the same whether or
    // not it survives. Charging what is appended instead — which is what the metadata ceiling
    // does — leaves a document made of fragments that collect nothing walking for free, and that
    // document is as cheap to write as one that collects everything.
    visits.spend(*set.as_span())?;

    match selection {
      Selection::Field(field) => {
        if !included(field.directives(), ctx)? {
          continue;
        }
        let key = match field.alias() {
          Some(alias) => alias.name().source().as_ref(),
          None => field.name().source().as_ref(),
        };
        let Some(key) = interner.intern(key) else {
          return Err(Fault {
            raw: Raw::NameStorage {
              limit: interner.cap(),
            },
            location: *field.name().as_span(),
            name: None,
          });
        };
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
        if !included(spread.directives(), ctx)? {
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
        stack.push((fragment.selection_set(), 0));
      }
      Selection::InlineFragment(inline) => {
        if !included(inline.directives(), ctx)? {
          continue;
        }
        if let Some(condition) = inline.type_condition()
          && !applies(schema, condition.name().source().as_ref(), object_type)
        {
          continue;
        }
        stack.push((inline.selection_set(), 0));
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
fn included<'a, S, V>(directives: Option<&'a Directives<S>>, ctx: &mut V) -> Result<bool, Fault<'a>>
where
  S: AsRef<[u8]>,
  V: Values,
{
  let Some(directives) = directives else {
    return Ok(true);
  };
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"skip" && condition_is_true(directive, ctx)? {
      return Ok(false);
    }
  }
  for directive in directives.directives() {
    if directive.name().source().as_ref() == b"include" && !condition_is_true(directive, ctx)? {
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
fn condition_is_true<'a, S, V>(directive: &'a Directive<S>, ctx: &mut V) -> Result<bool, Fault<'a>>
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
      name: None,
    });
  };
  // Every remaining failure is about the value, so it is what the error points at — the same node
  // `graphql-js` reports, which for `if: $flag` is the variable and not the directive.
  let location = *argument.value().as_span();
  let unreadable = |fault| Fault {
    raw: Raw::DirectiveCondition { fault },
    location,
    name: None,
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
        // The variable was not supplied, and that is the finding whether or not its spelling can
        // be quoted — so an arena with no room shortens the message and keeps the diagnosis. The
        // spelling travels as bytes because the caller has an arena to restore before it can mint
        // an id that survives.
        None => Err(Fault {
          raw: Raw::DirectiveCondition {
            fault: ConditionFault::VariableMissing { variable: None },
          },
          location,
          name: Some(spelling),
        }),
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

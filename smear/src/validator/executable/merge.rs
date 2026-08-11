//! Draft 5.3.2 `FieldsInSetCanMerge`, and the resource bound it is not safe without.
//!
//! # Why this rule has an engine and the other twenty-eight do not
//!
//! Every other draft §5 rule is a predicate over one node. This one is a predicate over every
//! *pair* of selections that a response name collects, at every level of the response shape, after
//! fragments have been expanded — and the expansion of a nested, reused fragment graph is where
//! both graphql-js and apollo-compiler have had denial-of-service incidents. So it gets a working
//! set, a memo, and a [`Budget`](crate::validator::Budget), from the first line rather than after
//! the first incident.
//!
//! # The algorithm
//!
//! The formulation is the one XING described and apollo-compiler adopted: two independent passes
//! over *merged field sets*, each memoised on the set's contents.
//!
//! - **`SameResponseShape`** groups a merged set by response name and, within each group, checks
//!   every member against the first for a compatible return type. Then it merges the group's
//!   subselection sets and repeats. This is the pass that refuses `x: name` beside `x: barkVolume`
//!   even where the two can never be encountered together.
//! - **The common-parent pass** does the same grouping, then splits each group by parent type —
//!   one part per concrete object parent, each carrying every abstract-parent selection as well,
//!   because a future schema change can make any abstract type overlap any other. Within a part,
//!   the members must name the same field with the same arguments.
//!
//! Both are written as **explicit stacks**. Recursion depth here is chosen by whoever wrote the
//! document, and the rest of this validator already refuses to recurse on that; a rule whose whole
//! purpose is to survive hostile input would be a strange place to start.
//!
//! # What makes it cheap enough to run on a request path
//!
//! One pass over the document builds a table of every selection set, every field occurrence, and
//! every spread, all reduced to integers — response names and field names interned into the
//! caller's [`Scratch`](crate::validator::Scratch), return types already resolved to
//! the schema's own packed form. After that the engine sorts, groups and compares `u32`s, and goes back to the
//! syntax tree only to compare argument literals and to quote a name in a diagnostic.
//!
//! Two properties make the table shareable rather than per-expansion:
//!
//! 1. **A selection set's scope is syntactic.** Which type a set's selections are written against
//!    is fixed by where the set appears, never by how a fragment was reached, so it can be
//!    computed once.
//! 2. **An expansion enters each named fragment at most once.** That is the specification's own
//!    "including visiting fragments" read as the set it is, and it is also what makes a cyclic
//!    fragment graph terminate here — though draft 5.5.2.2 has already refused one by the time
//!    this runs, which is deliberate and is part of the resource story rather than an accident of
//!    ordering.
//!
//! # The budget
//!
//! Absolute, never proportional to the input. Depth bounds the recursion; work bounds expanded
//! rows, comparisons, partition rows and tree-resolution steps as one running total for the call.
//! Reaching either one emits its own rule and abandons the engine — the document is refused, and
//! [`Invalid::budget_tripped`](super::Invalid::budget_tripped) says why. It never passes the
//! document unvalidated and it never panics.

use core::ops::ControlFlow;

use crate::parser::graphql::ast::{Argument, Field, InputValue, Selection, SelectionSet};

use super::{
  Diagnostic, Rule, Validator,
  nodes::{
    ValueLike, child_selection_set, fragment, name_bytes, response_name, root_selection_set,
  },
};
use crate::validator::{
  diagnostic::{Context, MergeConflict},
  schema::{Range32, RootOperation, TypeId},
  scratch::{MergeField, MergeFrame, MergeKid, MergeMemo, MergeSet, NONE, get_bit, hash_u32},
};

/// One of the two independent traversals of a merged field set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pass {
  /// Draft 5.3.2's `SameResponseShape`.
  Shape,
  /// Draft 5.3.2's step 2b: same field, same arguments, for selections that can overlap.
  Parents,
}

impl Pass {
  /// The bit this pass claims in a memo entry's flags.
  #[inline]
  const fn bit(self) -> u8 {
    match self {
      Self::Shape => 1,
      Self::Parents => 2,
    }
  }
}

/// The smallest probe table the memo builds.
const MIN_MEMO_SLOTS: usize = 64;

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: super::Sink<S>,
{
  // -- entry ------------------------------------------------------------------------------------

  /// Draft 5.3.2 over every selection set the document defines.
  ///
  /// Runs after draft 5.5.2.2 has established the fragment graph — the expansion below trusts
  /// nothing about it, but an unbounded rule that runs *before* the cheap structural refusals is
  /// a denial-of-service posture, not just an ordering preference.
  pub(super) fn check_field_merging(&mut self) -> ControlFlow<()> {
    let wanted = self.on(Rule::FieldSelectionMerging)
      || self.on(Rule::MergeDepthBudget)
      || self.on(Rule::MergeWorkBudget);
    if !wanted {
      return ControlFlow::Continue(());
    }

    self.build_merge_index()?;

    // Every operation, then every fragment no operation reached. The second half is this
    // validator's standing position — a definition's own structure is checked whether or not
    // anybody spread it — and the specification agrees: the rule is over *any* selection set
    // defined in the document, not only the reachable ones.
    for index in 0..self.scratch.operations.len() {
      if self.tripped {
        return ControlFlow::Continue(());
      }
      let row = self.scratch.operations[index];
      let root = self.scratch.merge_roots[row.definition as usize];
      self.blame = row.span;
      self.merge_from(root)?;
    }
    for ordinal in 0..self.scratch.fragments.len() {
      if self.tripped {
        return ControlFlow::Continue(());
      }
      if get_bit(&self.scratch.reachable, ordinal as u32) {
        continue;
      }
      let row = self.scratch.fragments[ordinal];
      let root = self.scratch.merge_roots[row.definition as usize];
      self.blame = row.span;
      self.merge_from(root)?;
    }
    ControlFlow::Continue(())
  }

  /// Runs both passes over one root selection set.
  fn merge_from(&mut self, root: u32) -> ControlFlow<()> {
    if root == NONE {
      return ControlFlow::Continue(());
    }
    self.scratch.merge_inputs.clear();
    self.scratch.merge_inputs.push(root);
    let Some(rows) = self.expand(0, 1)? else {
      return ControlFlow::Continue(());
    };
    self.scratch.merge_inputs.clear();

    // Claim before running, and carry the *canonical* range from the first pass into the second.
    // A freshly expanded range that duplicates a known one is released, so the range this call
    // produced may no longer be the one holding the rows — reusing it for the second pass would
    // read whatever the next expansion put there.
    let (rows, done) = self.claim(rows, Pass::Shape);
    if !done {
      self.run_pass(rows, Pass::Shape)?;
    }
    if self.tripped {
      return ControlFlow::Continue(());
    }
    let (rows, done) = self.claim(rows, Pass::Parents);
    if !done {
      self.run_pass(rows, Pass::Parents)?;
    }
    ControlFlow::Continue(())
  }

  // -- the budget --------------------------------------------------------------------------------

  /// Charges `units` of work, returning whether the engine may continue.
  #[inline]
  fn charge(&mut self, units: u32) -> bool {
    self.work = self.work.saturating_add(units);
    self.work <= self.budget.merge_work()
  }

  /// Refuses the document for exceeding a budget, and abandons the engine.
  ///
  /// Idempotent. The engine gives up at the first trip, but the caller's sink may have said to
  /// keep going, and every loop that unwinds afterwards would otherwise report the same refusal
  /// again — one budget diagnostic per remaining unit of work is noise, not information.
  fn trip(&mut self, rule: Rule, limit: u32) -> ControlFlow<()> {
    if self.tripped {
      return ControlFlow::Continue(());
    }
    self.tripped = true;
    if !self.on(rule) {
      // The caller switched this bound's own diagnostic off. The engine still stops — one that
      // kept going would be the denial of service — but with nothing to emit there is nothing to
      // make the document invalid either, so a document the bound refuses is reported on what was
      // examined before it. Switching a bound's rule off is switching off the *refusal*, not the
      // resource protection.
      return ControlFlow::Continue(());
    }
    self.budget_tripped = true;
    let diagnostic = Diagnostic::new(rule, self.blame).context(Context::Count(limit));
    self.emit(diagnostic)
  }

  // -- the index build ----------------------------------------------------------------------------

  /// Records every selection set, field occurrence and spread in the document, as integers.
  fn build_merge_index(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let total = document.definitions().len();

    self.scratch.merge_roots.clear();
    self.scratch.merge_roots.resize(total, NONE);
    self.scratch.merge_seen.clear();
    self
      .scratch
      .merge_seen
      .resize(self.scratch.fragments.len(), 0);

    // Every definition's own selection set exists before anything is filled, so that a spread can
    // name its target's root even when the target is defined later in the document.
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      let scope = self
        .schema
        .root(RootOperation::ALL[row.root as usize])
        .map_or(NONE, |id| id.get());
      let set = self.new_merge_set(row.definition, NONE, NONE, scope);
      self.scratch.merge_roots[row.definition as usize] = set;
    }
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      let Some(body) = fragment(document, row.definition) else {
        continue;
      };
      let scope = self
        .composite_of(body.type_condition().name())
        .map_or(NONE, |id| id.get());
      let set = self.new_merge_set(row.definition, NONE, NONE, scope);
      self.scratch.merge_roots[row.definition as usize] = set;
    }

    // Breadth-first, and read from the front rather than popped from the back, so that field rows
    // are numbered in document order. Nothing about the algorithm depends on that; the diagnostics
    // do, because the earliest selection in a group is the one every later one is blamed against.
    let mut head = 0usize;
    while head < self.scratch.merge_todo.len() {
      if self.tripped {
        return ControlFlow::Continue(());
      }
      let set = self.scratch.merge_todo[head];
      head += 1;
      if !self.charge(1) {
        return self.trip(Rule::MergeWorkBudget, self.budget.merge_work());
      }
      let Some(node) = self.resolve_merge_set(set)? else {
        continue;
      };
      self.fill_merge_set(set, node)?;
    }
    ControlFlow::Continue(())
  }

  /// Appends a selection set row and queues it to be filled.
  fn new_merge_set(&mut self, definition: u32, parent: u32, index: u32, scope: u32) -> u32 {
    let id = self.scratch.merge_sets.len() as u32;
    self.scratch.merge_sets.push(MergeSet {
      definition,
      parent,
      index,
      scope,
      fields: Range32::EMPTY,
      kids: Range32::EMPTY,
    });
    self.scratch.merge_todo.push(id);
    id
  }

  /// Locates a selection set by descending from its definition along the parent chain.
  ///
  /// `Ok(None)` is a set whose node has gone missing — unreachable for a tree this walk built
  /// itself, and handled rather than asserted because a caller may assemble an AST by hand.
  fn resolve_merge_set(&mut self, set: u32) -> ControlFlow<(), Option<&'d SelectionSet<S>>> {
    let document = self.document;
    self.scratch.merge_path.clear();
    let mut cursor = set;
    loop {
      let row = self.scratch.merge_sets[cursor as usize];
      if row.parent == NONE {
        break;
      }
      self.scratch.merge_path.push(row.index);
      cursor = row.parent;
    }
    let steps = self.scratch.merge_path.len() as u32;
    if !self.charge(steps.saturating_add(1)) {
      self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
      return ControlFlow::Continue(None);
    }
    let definition = self.scratch.merge_sets[cursor as usize].definition;
    let Some(mut node) = root_selection_set(document, definition) else {
      return ControlFlow::Continue(None);
    };
    for step in self.scratch.merge_path.iter().rev() {
      let Some(selection) = node.selections().get(*step as usize) else {
        return ControlFlow::Continue(None);
      };
      let Some(child) = child_selection_set(selection) else {
        return ControlFlow::Continue(None);
      };
      node = child;
    }
    ControlFlow::Continue(Some(node))
  }

  /// Records one selection set's own fields and the sub-sets an expansion must follow.
  fn fill_merge_set(&mut self, set: u32, node: &'d SelectionSet<S>) -> ControlFlow<()> {
    let schema = self.schema;
    let row = self.scratch.merge_sets[set as usize];
    let parent = (row.scope != NONE).then(|| TypeId::new(row.scope));

    if !self.charge(node.selections().len() as u32) {
      return self.trip(Rule::MergeWorkBudget, self.budget.merge_work());
    }

    let fields_start = self.scratch.merge_fields.len() as u32;
    let kids_start = self.scratch.merge_kids.len() as u32;

    for (index, selection) in node.selections().iter().enumerate() {
      let index = index as u32;
      match selection {
        Selection::Field(field) => {
          let name = field.name();
          let name_id = self.scratch.names.intern(name_bytes(name));
          let response = match field.alias() {
            Some(alias) => self.scratch.names.intern(name_bytes(alias.name())),
            None => name_id,
          };
          let ty = parent
            .and_then(|id| {
              schema
                .sym(name_bytes(name))
                .and_then(|sym| schema.field(id, sym))
            })
            .map(|definition| definition.ty());
          let child = match field.selection_set() {
            Some(inner) if !inner.selections().is_empty() => {
              let scope = ty.map_or(NONE, |ty| {
                let base = ty.base_id();
                if schema.type_def(base).kind().is_composite() {
                  base.get()
                } else {
                  NONE
                }
              });
              self.new_merge_set(row.definition, set, index, scope)
            }
            _ => NONE,
          };
          let args = field
            .arguments()
            .map_or(0, |arguments| arguments.arguments().len() as u32);
          self.scratch.merge_fields.push(MergeField {
            set,
            index,
            response,
            name: name_id,
            parent: row.scope,
            child,
            ty,
            args,
            span: *field.span(),
          });
        }
        Selection::InlineFragment(inline) => {
          // A condition the schema does not know leaves the level with no scope at all, which is
          // what drops its fields out of the common-parent partition below: nothing is known about
          // what they could be encountered with. 5.5.1.2 has already said so.
          let scope = match inline.type_condition() {
            Some(condition) => self
              .composite_of(condition.name())
              .map_or(NONE, |id| id.get()),
            None => row.scope,
          };
          let child = self.new_merge_set(row.definition, set, index, scope);
          self.scratch.merge_kids.push(MergeKid {
            set: child,
            fragment: NONE,
          });
        }
        Selection::FragmentSpread(spread) => {
          let Some(ordinal) = self.find_fragment(name_bytes(spread.name())) else {
            continue;
          };
          let target = self.scratch.fragments[ordinal as usize].definition;
          let root = self.scratch.merge_roots[target as usize];
          if root == NONE {
            continue;
          }
          self.scratch.merge_kids.push(MergeKid {
            set: root,
            fragment: ordinal,
          });
        }
      }
    }

    let fields = Range32::new(fields_start, self.scratch.merge_fields.len() as u32);
    let kids = Range32::new(kids_start, self.scratch.merge_kids.len() as u32);
    let entry = &mut self.scratch.merge_sets[set as usize];
    entry.fields = fields;
    entry.kids = kids;
    ControlFlow::Continue(())
  }

  // -- expansion ----------------------------------------------------------------------------------

  /// Expands `merge_inputs[start..end]` into a fresh, sorted row range.
  ///
  /// `Ok(None)` means the budget stopped the expansion; the engine is already refusing by then.
  fn expand(&mut self, start: u32, end: u32) -> ControlFlow<(), Option<Range32>> {
    self.generation = self.generation.wrapping_add(1);
    let generation = self.generation;
    self.scratch.merge_queue.clear();
    for slot in (start..end).rev() {
      self
        .scratch
        .merge_queue
        .push(self.scratch.merge_inputs[slot as usize]);
    }

    let base = self.scratch.merge_rows.len() as u32;
    while let Some(set) = self.scratch.merge_queue.pop() {
      let row = self.scratch.merge_sets[set as usize];
      if !self.charge(row.fields.len().saturating_add(row.kids.len()).max(1)) {
        self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
        return ControlFlow::Continue(None);
      }
      for field in row.fields.start()..row.fields.end() {
        self.scratch.merge_rows.push(field);
      }
      // Reversed so the queue pops them in source order; the sort below makes it cosmetic, but a
      // deterministic diagnostic order is worth the two characters.
      for slot in (row.kids.start()..row.kids.end()).rev() {
        let kid = self.scratch.merge_kids[slot as usize];
        if kid.fragment != NONE {
          if self.scratch.merge_seen[kid.fragment as usize] == generation {
            continue;
          }
          self.scratch.merge_seen[kid.fragment as usize] = generation;
        }
        self.scratch.merge_queue.push(kid.set);
      }
    }

    // Grouping is by response name, so the canonical order is by response name — which also makes
    // two expansions that reached the same occurrences by different routes compare equal in the
    // memo below, whatever order they arrived in. Within a group the order is the source's, so
    // that the selection every other one is compared against is the one written first.
    {
      let scratch = &mut *self.scratch;
      let fields = &scratch.merge_fields;
      scratch.merge_rows[base as usize..].sort_unstable_by_key(|row| {
        let field = &fields[*row as usize];
        (field.response, field.span.start(), *row)
      });
    }
    ControlFlow::Continue(Some(Range32::new(
      base,
      self.scratch.merge_rows.len() as u32,
    )))
  }

  // -- the memo -------------------------------------------------------------------------------------

  /// Claims a merged set for `pass`.
  ///
  /// Returns the **canonical** row range for this exact set of field occurrences, together with
  /// whether it has already been through this pass. A freshly expanded range that turns out to
  /// duplicate a known one is released rather than kept, so the arena holds one copy of each — and
  /// the caller must use the range that comes back, not the one it handed in.
  fn claim(&mut self, rows: Range32, pass: Pass) -> (Range32, bool) {
    let hash = {
      let mut state = 0xcbf2_9ce4_8422_2325u64 ^ u64::from(rows.len());
      for row in rows.slice(&self.scratch.merge_rows) {
        state = hash_u32(state, *row);
      }
      state
    };

    if (self.scratch.merge_memo.len() + 1) * 4 >= self.scratch.merge_slots.len() * 3 {
      self.grow_memo();
    }
    let mask = self.scratch.merge_slots.len() - 1;
    let mut slot = (hash as usize) & mask;
    loop {
      let entry = self.scratch.merge_slots[slot];
      if entry == NONE {
        self.scratch.merge_slots[slot] = self.scratch.merge_memo.len() as u32;
        self.scratch.merge_memo.push(MergeMemo {
          hash,
          rows,
          flags: pass.bit(),
        });
        return (rows, false);
      }
      let known = self.scratch.merge_memo[entry as usize];
      if known.hash == hash
        && known.rows.len() == rows.len()
        && known.rows.slice(&self.scratch.merge_rows) == rows.slice(&self.scratch.merge_rows)
      {
        if rows.end() as usize == self.scratch.merge_rows.len()
          && known.rows.start() != rows.start()
        {
          self.scratch.merge_rows.truncate(rows.start() as usize);
        }
        if known.flags & pass.bit() != 0 {
          return (known.rows, true);
        }
        self.scratch.merge_memo[entry as usize].flags |= pass.bit();
        return (known.rows, false);
      }
      slot = (slot + 1) & mask;
    }
  }

  /// Doubles the memo's probe table and reinserts every entry.
  fn grow_memo(&mut self) {
    let next = (self.scratch.merge_slots.len() * 2).max(MIN_MEMO_SLOTS);
    self.scratch.merge_slots.clear();
    self.scratch.merge_slots.resize(next, NONE);
    let mask = next - 1;
    for index in 0..self.scratch.merge_memo.len() as u32 {
      let hash = self.scratch.merge_memo[index as usize].hash;
      let mut slot = (hash as usize) & mask;
      while self.scratch.merge_slots[slot] != NONE {
        slot = (slot + 1) & mask;
      }
      self.scratch.merge_slots[slot] = index;
    }
  }

  // -- the two passes ---------------------------------------------------------------------------------

  /// Walks one pass over a merged set and everything it merges into, iteratively.
  /// `root` must already have been claimed for `pass`.
  fn run_pass(&mut self, root: Range32, pass: Pass) -> ControlFlow<()> {
    self.scratch.merge_stack.clear();
    self.scratch.merge_parts.clear();
    self.scratch.merge_bounds.clear();
    self.push_merge_frame(root);

    while let Some(frame) = self.scratch.merge_stack.last().copied() {
      if self.tripped {
        return ControlFlow::Continue(());
      }

      // A part of the group this frame is on: compare its members, then merge their subselections
      // and go one level deeper.
      if (frame.part as usize) < self.scratch.merge_bounds.len() {
        if let Some(top) = self.scratch.merge_stack.last_mut() {
          top.part += 1;
        }
        let part = self.scratch.merge_bounds[frame.part as usize];
        self.compare_part(part, pass)?;
        if self.tripped {
          return ControlFlow::Continue(());
        }
        self.descend(part, pass)?;
        continue;
      }

      // The group is finished: release its partition and take the next one, or leave the level.
      self.scratch.merge_parts.truncate(frame.parts as usize);
      self.scratch.merge_bounds.truncate(frame.bounds as usize);
      if frame.cursor >= frame.rows.end() {
        self.scratch.merge_stack.pop();
        continue;
      }
      self.open_group(pass)?;
    }
    ControlFlow::Continue(())
  }

  /// Starts a level.
  fn push_merge_frame(&mut self, rows: Range32) {
    let parts = self.scratch.merge_parts.len() as u32;
    let bounds = self.scratch.merge_bounds.len() as u32;
    self.scratch.merge_stack.push(MergeFrame {
      rows,
      cursor: rows.start(),
      parts,
      bounds,
      part: bounds,
    });
  }

  /// Takes the next response-name group on the current level and partitions it for `pass`.
  fn open_group(&mut self, pass: Pass) -> ControlFlow<()> {
    let Some(frame) = self.scratch.merge_stack.last().copied() else {
      return ControlFlow::Continue(());
    };
    let start = frame.cursor;
    let key = self.response_at(start);
    let mut end = start + 1;
    while end < frame.rows.end() && self.response_at(end) == key {
      end += 1;
    }
    if !self.charge(end - start) {
      return self.trip(Rule::MergeWorkBudget, self.budget.merge_work());
    }

    let parts = self.scratch.merge_parts.len() as u32;
    let bounds = self.scratch.merge_bounds.len() as u32;
    match pass {
      Pass::Shape => {
        for slot in start..end {
          let row = self.scratch.merge_rows[slot as usize];
          self.scratch.merge_parts.push(row);
        }
        self
          .scratch
          .merge_bounds
          .push(Range32::new(parts, self.scratch.merge_parts.len() as u32));
      }
      Pass::Parents => self.partition_by_parents(start, end)?,
    }

    if let Some(top) = self.scratch.merge_stack.last_mut() {
      top.cursor = end;
      top.parts = parts;
      top.bounds = bounds;
      top.part = bounds;
    }
    ControlFlow::Continue(())
  }

  /// Splits a response-name group into the sets of selections that could be encountered together.
  ///
  /// One part per concrete object parent, each carrying every abstract-parent selection too: a
  /// schema change can give any abstract type any implementor, so two selections under abstract
  /// parents — or one abstract and one concrete — must already agree. Selections whose parent type
  /// did not resolve contribute to nothing; some other rule has already refused them.
  fn partition_by_parents(&mut self, start: u32, end: u32) -> ControlFlow<()> {
    let schema = self.schema;
    let scratch_base = self.scratch.merge_parts.len() as u32;

    // The staging area: concrete-parent rows first, then abstract-parent rows.
    let mut abstracts = 0u32;
    for slot in start..end {
      let row = self.scratch.merge_rows[slot as usize];
      let parent = self.scratch.merge_fields[row as usize].parent;
      if parent == NONE {
        continue;
      }
      if schema.type_def(TypeId::new(parent)).kind().is_composite()
        && !schema.type_def(TypeId::new(parent)).kind().is_abstract()
      {
        self.scratch.merge_parts.push(row);
      }
    }
    let concrete_end = self.scratch.merge_parts.len() as u32;
    for slot in start..end {
      let row = self.scratch.merge_rows[slot as usize];
      let parent = self.scratch.merge_fields[row as usize].parent;
      if parent == NONE {
        continue;
      }
      if schema.type_def(TypeId::new(parent)).kind().is_abstract() {
        self.scratch.merge_parts.push(row);
        abstracts += 1;
      }
    }
    let staged_end = self.scratch.merge_parts.len() as u32;

    {
      let scratch = &mut *self.scratch;
      let fields = &scratch.merge_fields;
      scratch.merge_parts[scratch_base as usize..concrete_end as usize]
        .sort_unstable_by_key(|row| (fields[*row as usize].parent, *row));
    }

    if concrete_end == scratch_base {
      // No concrete parent anywhere in the group: one part, every abstract selection in it.
      let out = self.scratch.merge_parts.len() as u32;
      for slot in concrete_end..staged_end {
        let row = self.scratch.merge_parts[slot as usize];
        self.scratch.merge_parts.push(row);
      }
      self.order_part(out);
      self
        .scratch
        .merge_bounds
        .push(Range32::new(out, self.scratch.merge_parts.len() as u32));
      return ControlFlow::Continue(());
    }

    let mut run = scratch_base;
    while run < concrete_end {
      let parent = self.parent_of_part(run);
      let mut run_end = run + 1;
      while run_end < concrete_end && self.parent_of_part(run_end) == parent {
        run_end += 1;
      }
      if !self.charge((run_end - run).saturating_add(abstracts)) {
        return self.trip(Rule::MergeWorkBudget, self.budget.merge_work());
      }
      let out = self.scratch.merge_parts.len() as u32;
      for slot in run..run_end {
        let row = self.scratch.merge_parts[slot as usize];
        self.scratch.merge_parts.push(row);
      }
      for slot in concrete_end..staged_end {
        let row = self.scratch.merge_parts[slot as usize];
        self.scratch.merge_parts.push(row);
      }
      self.order_part(out);
      self
        .scratch
        .merge_bounds
        .push(Range32::new(out, self.scratch.merge_parts.len() as u32));
      run = run_end;
    }
    ControlFlow::Continue(())
  }

  /// Puts a part into source order, so its first member is the selection written first.
  fn order_part(&mut self, from: u32) {
    let scratch = &mut *self.scratch;
    let fields = &scratch.merge_fields;
    scratch.merge_parts[from as usize..].sort_unstable_by_key(|row| {
      let field = &fields[*row as usize];
      (field.span.start(), *row)
    });
  }

  /// Checks every member of a part against the first.
  ///
  /// Both checks are transitive over a part — "the same shape as" and "the same field with the
  /// same arguments as" are equivalence relations — so one member is enough to compare against,
  /// and the part costs a linear scan rather than every pair. That is the whole of the XING
  /// formulation's advantage over the specification's literal "each pair of distinct members".
  fn compare_part(&mut self, part: Range32, pass: Pass) -> ControlFlow<()> {
    let Some(first) = part.slice(&self.scratch.merge_parts).first().copied() else {
      return ControlFlow::Continue(());
    };
    if !self.charge(part.len()) {
      return self.trip(Rule::MergeWorkBudget, self.budget.merge_work());
    }
    for slot in part.start() + 1..part.end() {
      let other = self.scratch.merge_parts[slot as usize];
      match pass {
        Pass::Shape => {
          if !self.same_response_shape(first, other) {
            self.report_merge(first, other, MergeConflict::Shapes)?;
          }
        }
        Pass::Parents => {
          let (a, b) = (
            self.scratch.merge_fields[first as usize],
            self.scratch.merge_fields[other as usize],
          );
          if a.name != b.name {
            self.report_merge(first, other, MergeConflict::Fields)?;
          } else if !self.same_arguments(first, other)? {
            self.report_merge(first, other, MergeConflict::Arguments)?;
          }
        }
      }
      if self.tripped {
        return ControlFlow::Continue(());
      }
    }
    ControlFlow::Continue(())
  }

  /// Merges the subselection sets of a part's members and goes one level deeper.
  fn descend(&mut self, part: Range32, pass: Pass) -> ControlFlow<()> {
    let base = self.scratch.merge_inputs.len() as u32;
    for slot in part.start()..part.end() {
      let row = self.scratch.merge_parts[slot as usize];
      let child = self.scratch.merge_fields[row as usize].child;
      if child != NONE {
        self.scratch.merge_inputs.push(child);
      }
    }
    let end = self.scratch.merge_inputs.len() as u32;
    if base == end {
      return ControlFlow::Continue(());
    }
    // Merging one set with itself adds nothing but rows, so the duplicates go before the work is
    // charged for them.
    let kept = {
      let inputs = &mut self.scratch.merge_inputs[base as usize..];
      inputs.sort_unstable();
      let mut write = 0usize;
      for read in 0..inputs.len() {
        if read == 0 || inputs[read] != inputs[write - 1] {
          inputs[write] = inputs[read];
          write += 1;
        }
      }
      write as u32
    };
    self.scratch.merge_inputs.truncate((base + kept) as usize);

    if self.scratch.merge_stack.len() as u32 >= self.budget.merge_depth() {
      self.scratch.merge_inputs.truncate(base as usize);
      return self.trip(Rule::MergeDepthBudget, self.budget.merge_depth());
    }

    let expanded = self.expand(base, base + kept)?;
    self.scratch.merge_inputs.truncate(base as usize);
    let Some(rows) = expanded else {
      return ControlFlow::Continue(());
    };
    let (rows, done) = self.claim(rows, pass);
    if !done {
      self.push_merge_frame(rows);
    }
    ControlFlow::Continue(())
  }

  // -- the two checks -----------------------------------------------------------------------------

  /// Draft 5.3.2's `SameResponseShape`, steps 3 to 6, on two packed type references.
  ///
  /// Steps 3 and 4 unwrap non-null and list in lockstep and refuse any asymmetry, which on a
  /// packed reference is one integer comparison: the wrapper word *is* the unwrapping sequence.
  /// What is left is two named types — the same one, two leaves that must therefore be equal, or
  /// two composites, which merge and are checked one level down.
  fn same_response_shape(&self, a: u32, b: u32) -> bool {
    let left = self.scratch.merge_fields[a as usize].ty;
    let right = self.scratch.merge_fields[b as usize].ty;
    let (Some(left), Some(right)) = (left, right) else {
      // One of the two is not a field this schema defines. Draft 5.3.1 has already said so and
      // there is nothing here to compare against.
      return true;
    };
    if left.wrappers() != right.wrappers() {
      return false;
    }
    if left.base_id() == right.base_id() {
      return true;
    }
    let left = self.schema.type_def(left.base_id()).kind();
    let right = self.schema.type_def(right.base_id()).kind();
    if left.is_leaf() || right.is_leaf() {
      return false;
    }
    left.is_composite() && right.is_composite()
  }

  /// Whether two field selections were written with identical sets of arguments.
  fn same_arguments(&mut self, a: u32, b: u32) -> ControlFlow<(), bool> {
    let left = self.scratch.merge_fields[a as usize];
    let right = self.scratch.merge_fields[b as usize];
    if left.args == 0 && right.args == 0 {
      return ControlFlow::Continue(true);
    }
    if left.args != right.args {
      return ControlFlow::Continue(false);
    }
    let Some(left) = self.resolve_merge_field(a)? else {
      return ControlFlow::Continue(true);
    };
    let Some(right) = self.resolve_merge_field(b)? else {
      return ControlFlow::Continue(true);
    };
    let left = left
      .arguments()
      .map_or(&[][..], |arguments| arguments.arguments());
    let right = right
      .arguments()
      .map_or(&[][..], |arguments| arguments.arguments());
    // Matching by name is a scan, so each scan is charged for its own length rather than the pair
    // being charged once: a field written with a thousand arguments is a quadratic comparison, and
    // an uncharged quadratic inside a budgeted engine is the hole the budget exists to close.
    for argument in left {
      if !self.charge(right.len() as u32) {
        self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
        return ControlFlow::Continue(true);
      }
      let Some(other) = by_name(right, argument) else {
        return ControlFlow::Continue(false);
      };
      if !self.same_value(argument.value(), other.value())? {
        return ControlFlow::Continue(false);
      }
    }
    // The counts already agree, but a set with a repeated argument name — draft 5.4.2's business,
    // not this rule's — could still hide a name the other side does not have.
    for argument in right {
      if !self.charge(left.len() as u32) {
        self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
        return ControlFlow::Continue(true);
      }
      if by_name(left, argument).is_none() {
        return ControlFlow::Continue(false);
      }
    }
    ControlFlow::Continue(true)
  }

  /// Whether two value literals are identical.
  ///
  /// Iterative, over a stack of child-index pairs in the caller's working set: value nesting is
  /// chosen by whoever wrote the document, exactly like selection nesting, and is walked the same
  /// way. Object literals are compared as maps — field order does not matter — and everything else
  /// by source spelling.
  fn same_value(
    &mut self,
    left: &'d InputValue<S>,
    right: &'d InputValue<S>,
  ) -> ControlFlow<(), bool> {
    self.scratch.merge_compare.clear();
    self.scratch.merge_compare.push((NONE, NONE, 0));

    while let Some(&(_, _, cursor)) = self.scratch.merge_compare.last() {
      let depth = self.scratch.merge_compare.len() as u32;
      // A value literal's nesting is bounded by the work counter rather than by `merge_depth`,
      // which means the *response shape*'s nesting and nothing else. Charging the depth makes the
      // total quadratic in it, so a hostile literal exhausts the budget long before it exhausts
      // anything else.
      if !self.charge(depth) {
        self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
        return ControlFlow::Continue(true);
      }
      let (Some(a), Some(b)) = (
        descend_value(left, &self.scratch.merge_compare, true),
        descend_value(right, &self.scratch.merge_compare, false),
      ) else {
        // Only reachable if the two trees stopped agreeing about their own shape, which the
        // shallow check below has already refused.
        return ControlFlow::Continue(false);
      };
      if cursor == 0 && !shallow_equal(a, b) {
        return ControlFlow::Continue(false);
      }

      let children = match (a.as_list(), a.as_object()) {
        (Some(values), _) => values.len() as u32,
        (_, Some(fields)) => fields.len() as u32,
        _ => 0,
      };
      if cursor >= children {
        self.scratch.merge_compare.pop();
        continue;
      }
      if let Some(top) = self.scratch.merge_compare.last_mut() {
        top.2 += 1;
      }

      let mate = match (a.as_object(), b.as_object()) {
        (Some(fields), Some(others)) => {
          // Charged for the scan, not for the field: pairing an object literal's fields by name is
          // quadratic in its width, and the budget has to see that.
          if !self.charge(others.len() as u32) {
            self.trip(Rule::MergeWorkBudget, self.budget.merge_work())?;
            return ControlFlow::Continue(true);
          }
          let name = name_bytes(fields[cursor as usize].name());
          match others
            .iter()
            .position(|other| name_bytes(other.name()) == name)
          {
            Some(index) => index as u32,
            None => return ControlFlow::Continue(false),
          }
        }
        // A list pairs positionally; `shallow_equal` has already established equal lengths.
        _ => cursor,
      };
      self.scratch.merge_compare.push((cursor, mate, 0));
    }
    ControlFlow::Continue(true)
  }

  // -- reporting ------------------------------------------------------------------------------------

  /// Reports one pair of selections that cannot merge, blaming the later of the two.
  fn report_merge(&mut self, first: u32, other: u32, conflict: MergeConflict) -> ControlFlow<()> {
    if !self.on(Rule::FieldSelectionMerging) {
      return ControlFlow::Continue(());
    }
    let a = self.scratch.merge_fields[first as usize];
    let b = self.scratch.merge_fields[other as usize];
    let subject = self
      .resolve_merge_field(other)?
      .map(|field| response_name(field).source().clone());
    let mut diagnostic = Diagnostic::new(Rule::FieldSelectionMerging, b.span)
      .related(a.span)
      .context(Context::Merge(conflict));
    if let Some(subject) = subject {
      diagnostic = diagnostic.subject(subject);
    }
    self.emit(diagnostic)
  }

  // -- helpers --------------------------------------------------------------------------------------

  /// The response name of the row at a slot of the current merged set.
  #[inline]
  fn response_at(&self, slot: u32) -> u32 {
    let row = self.scratch.merge_rows[slot as usize];
    self.scratch.merge_fields[row as usize].response
  }

  /// The parent type of the row at a slot of the partition staging area.
  #[inline]
  fn parent_of_part(&self, slot: u32) -> u32 {
    let row = self.scratch.merge_parts[slot as usize];
    self.scratch.merge_fields[row as usize].parent
  }

  /// Locates a field occurrence's syntax node.
  fn resolve_merge_field(&mut self, row: u32) -> ControlFlow<(), Option<&'d Field<S>>> {
    let field = self.scratch.merge_fields[row as usize];
    let Some(node) = self.resolve_merge_set(field.set)? else {
      return ControlFlow::Continue(None);
    };
    ControlFlow::Continue(match node.selections().get(field.index as usize) {
      Some(Selection::Field(field)) => Some(field),
      _ => None,
    })
  }
}

// ---------------------------------------------------------------------------------------------
// free helpers
// ---------------------------------------------------------------------------------------------

/// Finds the argument of the same name.
fn by_name<'a, S>(arguments: &'a [Argument<S>], wanted: &Argument<S>) -> Option<&'a Argument<S>>
where
  S: AsRef<[u8]>,
{
  let name = name_bytes(wanted.name());
  arguments
    .iter()
    .find(|argument| name_bytes(argument.name()) == name)
}

/// Follows a comparison stack's child-index chain into one of the two value trees.
fn descend_value<'a, S>(
  root: &'a InputValue<S>,
  frames: &[(u32, u32, u32)],
  left: bool,
) -> Option<&'a InputValue<S>> {
  let mut value = root;
  for frame in &frames[1..] {
    let index = (if left { frame.0 } else { frame.1 }) as usize;
    value = match value {
      InputValue::List(list) => list.values().get(index)?,
      InputValue::Object(object) => object.fields().get(index)?.value(),
      _ => return None,
    };
  }
  Some(value)
}

/// Whether two values agree at their own level, ignoring their children.
fn shallow_equal<S>(left: &InputValue<S>, right: &InputValue<S>) -> bool
where
  S: AsRef<[u8]>,
{
  match (left, right) {
    (InputValue::Null(_), InputValue::Null(_)) => true,
    (InputValue::Boolean(a), InputValue::Boolean(b)) => a.value() == b.value(),
    (InputValue::Int(a), InputValue::Int(b)) => a.source().as_ref() == b.source().as_ref(),
    (InputValue::Float(a), InputValue::Float(b)) => a.source().as_ref() == b.source().as_ref(),
    (InputValue::String(a), InputValue::String(b)) => a.source().as_ref() == b.source().as_ref(),
    (InputValue::Enum(a), InputValue::Enum(b)) => a.source().as_ref() == b.source().as_ref(),
    (InputValue::Variable(a), InputValue::Variable(b)) => {
      name_bytes(a.name()) == name_bytes(b.name())
    }
    (InputValue::List(a), InputValue::List(b)) => a.values().len() == b.values().len(),
    // Equal widths, and the caller pairs the fields by name. A literal with a *repeated* field
    // name could hide a name the other side lacks; that is draft 5.6.3's business, it has already
    // refused the document, and apollo-compiler makes the same assumption here.
    (InputValue::Object(a), InputValue::Object(b)) => a.fields().len() == b.fields().len(),
    _ => false,
  }
}

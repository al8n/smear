//! The selection walk: draft 5.3.1, 5.3.3 and 5.5.2.3, and the traversal every other per-node
//! rule rides.
//!
//! # One walk per operation, and one check per definition
//!
//! A fragment reached by three operations is walked three times, because the variable rules are
//! properties of an operation and the same fragment may be valid under one operation's variables
//! and invalid under another's. Everything else is a property of the *definition*, so those rules
//! are gated on [`Frame::CHECK`], which is set the first time a definition is entered and cleared
//! for every later visit. A fragment with a misspelled field reports it once, not once per caller.

use core::ops::ControlFlow;

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  ExecutableDocument, Field, FragmentSpread, InlineFragment, Selection, SelectionSet,
};

use super::{
  Rule, TypeId, Validator,
  nodes::{child_selection_set, fragment, name_bytes, root_selection_set},
};
use crate::{
  diagnostic::Context,
  schema::DirectiveLocation,
  scratch::{Frame, NONE, get_bit, push_frame},
};

/// Locates a selection set by descending from a definition along the frame chain.
///
/// The innermost frame with no parent index names the definition to start from — which is how a
/// fragment body entered through a spread rejoins the same stack without the frames below it
/// needing to know.
pub(super) fn resolve<'d, S>(
  document: &'d ExecutableDocument<S>,
  suffix: &[Frame],
) -> Option<&'d SelectionSet<S>> {
  let (root, rest) = suffix.split_first()?;
  let mut set = root_selection_set(document, root.definition)?;
  for frame in rest {
    let selection = set.selections().get(frame.child as usize)?;
    set = child_selection_set(selection)?;
  }
  Some(set)
}

/// The part of a selection stack a [`resolve`] descends: the suffix beginning at the nearest
/// definition root.
///
/// The stack itself is longer, and that is the whole of al8n/smear#198's sixteenth round. This used
/// to be found by scanning backwards for the root — `O(suffix)` on its own, so a resolution walked
/// the suffix twice and the obvious number to charge was the length of the *stack*. It is read off
/// the top frame now, in `O(1)`, and the scan is gone rather than repriced.
///
/// Its two callers charge `suffix_of(..).len()` and then resolve `suffix_of(..)`, which is
/// [`walk_value`](Validator::walk_value)'s shape one tree up: that walk has always charged
/// `depth - base` and resolved `values[base..depth]`, so its quantity and its slice are the same
/// expression and cannot drift apart. These two had a quantity in one place and a slice computed in
/// another, and they disagreed.
pub(super) fn suffix_of(frames: &[Frame]) -> &[Frame] {
  match frames.last() {
    Some(top) => frames
      .get(top.definition_root() as usize..)
      .unwrap_or_default(),
    None => &[],
  }
}

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: super::Sink<S>,
{
  /// Walks a definition's selections, following fragment spreads when an operation scope is in
  /// effect.
  pub(super) fn walk_selections(&mut self, root: Frame) -> ControlFlow<()> {
    let document = self.document;
    self.scratch.frames.clear();
    push_frame(&mut self.scratch.frames, root);
    let mut current = root_selection_set(document, root.definition);
    // The definition's own selection set, as the span a refusal in this walk points at. The same
    // choice `walk_value` makes for the same reason: the descent charged below is a property of
    // the walk rather than of any one selection in it.
    let blame = current.map_or(SimpleSpan::const_new(0, 0), |set| *set.span());

    while let Some(frame) = self.scratch.frames.last().copied() {
      // One unit for the iteration. Examining the next sibling is `O(1)`; what costs the depth is
      // `resolve`, and `resolve` runs only on the two arms that pop.
      //
      // The depth used to be charged here, at the top, on **every** iteration — so a level of `W`
      // siblings was billed `W · D` for `W` `O(1)` examinations and one `O(D)` resolution. That is
      // the mirror of the defect it was written to fix: the charge was moved in front of the work
      // and not onto the branch that performs it. al8n/smear#198.
      self.spend(1, blame)?;
      let Some(set) = current else {
        self.scratch.frames.pop();
        current = self.resolve_frames(blame)?;
        continue;
      };
      let Some(selection) = set.selections().get(frame.cursor as usize) else {
        self.scratch.frames.pop();
        current = self.resolve_frames(blame)?;
        continue;
      };
      if let Some(top) = self.scratch.frames.last_mut() {
        top.cursor += 1;
      }
      match selection {
        Selection::Field(field) => {
          let scope = self.check_field(field, frame)?;
          if let Some(child) = field.selection_set() {
            push_frame(
              &mut self.scratch.frames,
              Frame::child(frame.definition, frame.cursor, scope, frame.flags),
            );
            current = Some(child);
          }
        }
        Selection::InlineFragment(inline) => {
          let scope = self.check_inline_fragment(inline, frame)?;
          push_frame(
            &mut self.scratch.frames,
            Frame::child(frame.definition, frame.cursor, scope, frame.flags),
          );
          current = Some(inline.selection_set());
        }
        Selection::FragmentSpread(spread) => {
          if let Some(entered) = self.check_fragment_spread(spread, frame)? {
            push_frame(&mut self.scratch.frames, entered.0);
            current = Some(entered.1);
          }
        }
      }
    }
    ControlFlow::Continue(())
  }

  /// [`resolve`] over the selection stack, charged for the descent it makes.
  ///
  /// Called only after a pop, which is why the charge lives here rather than at the top of the loop
  /// that sometimes calls it — al8n/smear#198's eleventh round moved it onto the arm that resolves.
  ///
  /// # The quantity is the suffix, not the stack
  ///
  /// The arm was right and the number was not. `resolve` starts at the **nearest definition root**,
  /// so what it walks is `len − root`, and a stack is not one definition deep: an operation that
  /// reaches a fragment at depth `D` gives every level of that fragment body a resolution of its
  /// own, each `O(1)` and each billed `D`. `Θ(D · W)` charged for `Θ(W)` performed, and at
  /// `D = 128` about thirty-two thousand such levels exhaust the default ceiling and answer
  /// [`Refusal::Budget`](crate::Refusal::Budget) for a document with nothing wrong with it — under
  /// any rule set, with no merge engine involved.
  ///
  /// A depth is a population like any other, and the question the list, group and bitset audits ask
  /// of a population is whether the quantity is the part traversed or the whole. This one was the
  /// whole.
  pub(super) fn resolve_frames(
    &mut self,
    blame: SimpleSpan,
  ) -> ControlFlow<(), Option<&'d SelectionSet<S>>> {
    // Charged for the slice that is then resolved, not for a number computed beside it: both lines
    // ask `suffix_of` for the same answer, so there is no second place for the quantity to be
    // wrong. `O(1)`, and in front of the descent — the alternative was one unit per step while
    // scanning back for the root, and a per-step charge is sound only when the step it prices has
    // not been read yet. There is no ordering to get right if there is no scan.
    self.spend(suffix_of(&self.scratch.frames).len() as u32, blame)?;
    ControlFlow::Continue(resolve(self.document, suffix_of(&self.scratch.frames)))
  }

  /// Draft 5.3.1 and 5.3.3, plus the field's arguments and directives.
  ///
  /// Returns the scope its subselections are written against, or [`NONE`] when there is none to
  /// know — an unknown field, or a leaf, whose inner selections are then left alone rather than
  /// reported one by one against a type that cannot have them.
  fn check_field(&mut self, field: &'d Field<S>, frame: Frame) -> ControlFlow<(), u32> {
    let check = frame.flags & Frame::CHECK != 0;
    let name = field.name();

    // Resolved — and therefore charged — only when something reads the answer, which is
    // [`resolves_positions`](Validator::resolves_positions) one tree up from the value walk it was
    // written for. Two readers: draft 5.3.1 and 5.3.3, which are definition-local and ask with
    // `check`; and draft 5.8.5, which reaches an argument's *expected type* through this
    // definition and is a property of an operation.
    //
    // Neither exists on a **repeated** descent, and a repeated descent is the expensive one: a
    // fragment spread by `O` operations with `collects_usages` on walks its body `O` times with
    // `check` false, and the charge here is the spelling's own length. One 32 KiB field name over
    // 1,024 operations is 4,195,328 units off about sixty kilobytes of syntax — past the default
    // ceiling, for a name nothing was going to hash. al8n/smear#198's nineteenth round.
    //
    // The scope such a level answers with is [`NONE`], which is not a shortcut: it is what an
    // unresolved position *is*, and it is the same answer `check_directives`' descent-only arm
    // gives for an expected type. The children are still traversed.
    let definition = match frame.type_id() {
      Some(parent) if self.resolves_positions(check) => {
        self.spend_name(name)?;
        let found = self
          .schema
          .sym(name_bytes(name))
          .and_then(|sym| self.schema.field(parent, sym))
          .copied();
        if found.is_none() && check && self.on(Rule::FieldSelections) {
          let context = Context::Type(self.schema.type_def(parent).name());
          self.report_name(Rule::FieldSelections, name, context)?;
        }
        found
      }
      _ => None,
    };

    let mut scope = NONE;
    if let Some(definition) = definition {
      let base = definition.ty().base_id();
      let composite = self.schema.type_def(base).kind().is_composite();
      let has_subselection = field.selection_set().is_some();
      if composite {
        scope = base.get();
      }
      if composite != has_subselection && check && self.on(Rule::LeafFieldSelections) {
        let context = Context::Expected(definition.ty());
        self.report_name(Rule::LeafFieldSelections, name, context)?;
      }
    }

    let arguments = field
      .arguments()
      .map_or(&[][..], |arguments| arguments.arguments());
    self.check_arguments(
      arguments,
      definition.map(|definition| definition.args()),
      definition.map(|definition| definition.name()),
      check,
      *name.as_span(),
    )?;

    if let Some(directives) = field.directives() {
      self.check_directives(directives.directives(), DirectiveLocation::Field, check)?;
    }

    ControlFlow::Continue(scope)
  }

  /// Draft 5.5.1.2, 5.5.1.3 and 5.5.2.3 at an inline fragment, plus its directives.
  fn check_inline_fragment(
    &mut self,
    inline: &'d InlineFragment<S>,
    frame: Frame,
  ) -> ControlFlow<(), u32> {
    let check = frame.flags & Frame::CHECK != 0;
    let mut scope = frame.ty;

    if let Some(condition) = inline.type_condition() {
      let name = condition.name();
      // The same gate `check_field` takes, and for the same two readers: a condition narrows the
      // scope, so a level whose scope nobody reads has nothing to resolve. `check` true is what
      // 5.5.1.2, 5.5.1.3 and 5.5.2.3 need and it implies this, so no report is lost — and the
      // level that skips answers [`NONE`], which is what an unresolved position is. Its directives
      // below are still walked, because a usage inside one is exactly what put this walk here.
      if self.resolves_positions(check) {
        self.spend_name(name)?;
        let resolved = if check {
          self.check_type_condition(name)?
        } else {
          self.composite_of(name)
        };
        scope = match resolved {
          Some(id) => {
            // No `if check` here: `check_spread_possible` reads the same flag off the same frame
            // through `reaches_spread_target`, and a second copy of the condition is the thing
            // that predicate exists to prevent.
            self.check_spread_possible(name, id, frame)?;
            id.get()
          }
          None => NONE,
        };
      } else {
        scope = NONE;
      }
    }

    if let Some(directives) = inline.directives() {
      self.check_directives(
        directives.directives(),
        DirectiveLocation::InlineFragment,
        check,
      )?;
    }

    ControlFlow::Continue(scope)
  }

  /// Draft 5.5.2.3 at a named spread, plus its directives; enters the fragment body when an
  /// operation scope is in effect and this walk has not entered it yet.
  #[allow(clippy::type_complexity)]
  fn check_fragment_spread(
    &mut self,
    spread: &'d FragmentSpread<S>,
    frame: Frame,
  ) -> ControlFlow<(), Option<(Frame, &'d SelectionSet<S>)>> {
    let check = frame.flags & Frame::CHECK != 0;
    let document = self.document;
    let name = spread.name();
    // Nothing here reads the spelling when no fragment is declared: `find_fragment` returns on an
    // empty index without a comparison, and the arms below it never run.
    if !self.scratch.fragments.is_empty() {
      self.spend_name(name)?;
    }

    if let Some(directives) = spread.directives() {
      self.check_directives(
        directives.directives(),
        DirectiveLocation::FragmentSpread,
        check,
      )?;
    }

    // 5.5.2.1 was reported while the graph was collected; here a miss simply has nothing to enter.
    let Some(ordinal) = self.find_fragment(name_bytes(name)) else {
      return ControlFlow::Continue(None);
    };
    let row = self.scratch.fragments[ordinal as usize];
    let Some(body) = fragment(document, row.definition) else {
      return ControlFlow::Continue(None);
    };
    // Every operation's expansion, not only the first — **when there is an operation-local reader
    // for the repeat**. `enter` is the definition-local half: the directive rules over the
    // definition's own directives, reported once however many operations reach it. The usages those
    // directives can carry are operation-local, so al8n/smear#198's eighth round made the walk
    // repeat, which fixed a verdict that depended on the order the operations were written in.
    //
    // It was not gated on that reader being present. With `collects_usages` false there is no
    // operation-local usage to collect, and a repeat then carries `flags = 0`: `check` is false at
    // every level of the body, and with `check` false every reader downstream reduces to
    // [`descends_for_usages`](Validator::descends_for_usages), which is `collects_usages` again. So
    // `O` operations sharing one `W`-field fragment spent `Θ(O · W)` units off `O(O + W)` of syntax
    // to reach conclusions no rule set could act on, and a valid document under
    // `RuleSet::only(FieldSelections)` could be answered [`Refusal::Budget`](crate::Refusal::Budget)
    // at the default ceiling. Round 6's discipline — *gate on the readers, not on the family* —
    // applied to a traversal rather than to a charge. al8n/smear#198's seventeenth round.
    //
    // **Why the gate is not wider than this.** The one thing a `flags = 0` walk still produces is
    // the `checked` bit of a *nested* fragment it reaches. It cannot produce a new one here: this
    // definition's bit is set only by the entry that returns its body with `Frame::CHECK`, so
    // everything reachable from it was reached by that walk. And it does not have to be right about
    // that, because [`walk_unreached_fragments`](Validator::walk_unreached_fragments) enumerates
    // **every** fragment afterwards and enters the ones no walk checked — a backstop over the whole
    // population, not an argument about reachability.
    let enter = !get_bit(&self.scratch.checked, row.definition);
    let enters_body = self.in_operation && (enter || self.collects_usages);

    // **Asked before anything is resolved.** `enters_body` decides whether an entry is on the
    // table; `Visited::visit` decides whether *this* one does anything, because the
    // specification's transitive inclusion is a set — a second expansion in the same walk could
    // only repeat what the first one said, and on a cyclic graph it would not terminate. The
    // condition's resolution used to sit between the two, so a duplicate spread paid a
    // `Schema::sym` hash over a document-chosen spelling for an entry that then did not happen:
    // work performed in front of the check that establishes it is unnecessary.
    //
    // `&&` short-circuits, and that is load-bearing: `visit` **marks**, so it must not be asked
    // where no entry was on the table.
    let entering = enters_body && !self.scratch.visited.visit(ordinal);

    // Two readers of the target's type, asked separately because they are reached differently: the
    // spread-site rule fires at **every** spread, while the entered level's scope is read only by
    // the rules that read a level's scope at all — 5.3.1 and 5.3.3 under `check`, and 5.8.5's
    // expected types. With neither, an entry propagates `NONE`, which is what an unresolved
    // position is and not a shortcut.
    let reads_target = self.reaches_spread_target(check);
    let needs_scope = entering && self.resolves_positions(enter);

    let scope = if reads_target || needs_scope {
      let condition = body.type_condition().name();
      // The spread charge above pays for the **spread's** name. This is the fragment's *type
      // condition*, a different spelling the document also chose, and `composite_of` resolves it
      // through `Schema::sym`, which hashes every byte. `O` spreads of one fragment therefore read
      // `O · L` bytes off `O + L` of syntax, and the charge for it used to be zero.
      self.spend_name(condition)?;
      let target = self.composite_of(condition);
      if reads_target && let Some(target) = target {
        self.check_spread_possible(name, target, frame)?;
      }
      target.map_or(NONE, |id| id.get())
    } else {
      NONE
    };

    if !entering {
      return ControlFlow::Continue(None);
    }

    self.begin_fragment(row.definition, enter)?;
    let flags = if enter { Frame::CHECK } else { 0 };
    ControlFlow::Continue(Some((
      Frame::root(row.definition, scope, flags),
      body.selection_set(),
    )))
  }

  /// Whether draft 5.5.2.3 will read a spread's **target type** at this site.
  ///
  /// A named predicate rather than a condition written twice, for the reason
  /// [`merges`](super::merges) is one: it has a **producer** as well as a consumer.
  /// `check_fragment_spread` asks it to decide whether resolving the target's condition — a
  /// `Schema::sym` hash over a document-chosen spelling — has anybody to answer for, and
  /// [`check_spread_possible`](Self::check_spread_possible) asks it again to decide whether to run.
  /// A hand-written copy on the producer is how a caller and a callee come to disagree about which
  /// rules reach a site.
  #[inline]
  fn reaches_spread_target(&self, check: bool) -> bool {
    check && self.on(Rule::FragmentSpreadIsPossible)
  }

  /// Draft 5.5.2.3, all four subsections, as one bitset intersection.
  fn check_spread_possible(
    &mut self,
    name: &smear_parser::graphql::ast::Name<S>,
    target: TypeId,
    frame: Frame,
  ) -> ControlFlow<()> {
    if !self.reaches_spread_target(frame.flags & Frame::CHECK != 0) {
      return ControlFlow::Continue(());
    }
    let Some(parent) = frame.type_id() else {
      return ControlFlow::Continue(());
    };
    // Draft 5.5.2.3 is `GetPossibleTypes(fragmentType) ∩ GetPossibleTypes(parentType) ≠ ∅`, and this
    // is the ecosystem's **self-spread exception** to it.
    //
    // The exception matters for exactly one shape: an interface with no implementors, spread on
    // itself. Its possible-object set is empty, so a literal reading of the rule refuses
    // `fragment F on Empty { ... F }`. graphql-js, apollo-compiler and the graphql-spec#1109
    // discussion all accept it, and diverging alone would only make a differential comparison noisy
    // without protecting anybody from anything.
    //
    // It is taken **before the charge** because it is taken before the scan. This was one
    // `target == parent || intersect(..)` below the charge, so an equal pair — which answers on the
    // first operand and never touches a bitset — was billed for the whole width of one. A charge
    // sized to the work's worst path rather than its taken one is a false refusal, which is the
    // same defect as a charge in the wrong dimension wearing different clothes. It was a charge
    // this branch added one round earlier, from its own count audit.
    if target == parent {
      return ControlFlow::Continue(());
    }

    // The intersection, walked here rather than through `Schema::possible_objects_intersect`, so
    // that each word can be paid for **immediately before it is read**.
    //
    // The bitset is the schema's and its width is not an input; the number of spreads that reach it
    // is — so it is charged, and it was charged at its full width. But the walk stops at the first
    // overlapping word, and the overlapping case is the common one: a spread that *can* apply
    // usually says so in the first word. Prepaying the width billed every legal spread for a scan
    // that ended immediately.
    //
    // A per-step charge is only sound when the step it prices has not yet been read, which is the
    // whole reason this is a loop here and not a `zip(..).any(..)` with a charge above it.
    let schema = self.schema;
    let (Some(target_words), Some(parent_words)) = (
      schema.possible_objects(target),
      schema.possible_objects(parent),
    ) else {
      // A type with no possible-object set intersects nothing, and no word is read to find out.
      let context = Context::Type(schema.type_def(parent).name());
      return self.report_name(Rule::FragmentSpreadIsPossible, name, context);
    };
    for (a, b) in target_words.iter().zip(parent_words.iter()) {
      self.spend(1, *name.as_span())?;
      if a & b != 0 {
        return ControlFlow::Continue(());
      }
    }
    let context = Context::Type(self.schema.type_def(parent).name());
    self.report_name(Rule::FragmentSpreadIsPossible, name, context)
  }

  /// Resolves a type condition without reporting: the reporting pass has already run over every
  /// fragment declaration, and an inline fragment inside a fragment body must not report twice
  /// because two operations reached it.
  pub(super) fn composite_of(
    &self,
    condition: &smear_parser::graphql::ast::Name<S>,
  ) -> Option<TypeId> {
    let id = self.type_of(condition)?;
    self.schema.type_def(id).kind().is_composite().then_some(id)
  }
}

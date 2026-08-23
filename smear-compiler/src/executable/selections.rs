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
  scratch::{Frame, NONE, get_bit},
};

/// Locates a selection set by descending from a definition along the frame chain.
///
/// The innermost frame with no parent index names the definition to start from — which is how a
/// fragment body entered through a spread rejoins the same stack without the frames below it
/// needing to know.
pub(super) fn resolve<'d, S>(
  document: &'d ExecutableDocument<S>,
  frames: &[Frame],
) -> Option<&'d SelectionSet<S>> {
  let start = frames.iter().rposition(Frame::is_definition_root)?;
  let mut set = root_selection_set(document, frames[start].definition)?;
  for frame in &frames[start + 1..] {
    let selection = set.selections().get(frame.child as usize)?;
    set = child_selection_set(selection)?;
  }
  Some(set)
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
    self.scratch.frames.push(root);
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
            self.scratch.frames.push(Frame::child(
              frame.definition,
              frame.cursor,
              scope,
              frame.flags,
            ));
            current = Some(child);
          }
        }
        Selection::InlineFragment(inline) => {
          let scope = self.check_inline_fragment(inline, frame)?;
          self.scratch.frames.push(Frame::child(
            frame.definition,
            frame.cursor,
            scope,
            frame.flags,
          ));
          current = Some(inline.selection_set());
        }
        Selection::FragmentSpread(spread) => {
          if let Some(entered) = self.check_fragment_spread(spread, frame)? {
            self.scratch.frames.push(entered.0);
            current = Some(entered.1);
          }
        }
      }
    }
    ControlFlow::Continue(())
  }

  /// [`resolve`] over the selection stack, charged for the descent it makes.
  ///
  /// It scans the stack for the definition root and then descends it again, so it costs the depth —
  /// and it is called only after a pop, which is why the charge lives here rather than at the top
  /// of the loop that sometimes calls it.
  pub(super) fn resolve_frames(
    &mut self,
    blame: SimpleSpan,
  ) -> ControlFlow<(), Option<&'d SelectionSet<S>>> {
    self.spend(self.scratch.frames.len() as u32, blame)?;
    ControlFlow::Continue(resolve(self.document, &self.scratch.frames))
  }

  /// Draft 5.3.1 and 5.3.3, plus the field's arguments and directives.
  ///
  /// Returns the scope its subselections are written against, or [`NONE`] when there is none to
  /// know — an unknown field, or a leaf, whose inner selections are then left alone rather than
  /// reported one by one against a type that cannot have them.
  fn check_field(&mut self, field: &'d Field<S>, frame: Frame) -> ControlFlow<(), u32> {
    let check = frame.flags & Frame::CHECK != 0;
    let name = field.name();
    self.spend_name(name)?;

    let definition = match frame.type_id() {
      Some(parent) => {
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
      None => None,
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
      self.spend_name(name)?;
      let resolved = if check {
        self.check_type_condition(name)?
      } else {
        self.composite_of(name)
      };
      scope = match resolved {
        Some(id) => {
          if check {
            self.check_spread_possible(name, id, frame)?;
          }
          id.get()
        }
        None => NONE,
      };
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
    let condition = body.type_condition().name();
    // The spread charge above pays for the **spread's** name. This is the fragment's *type
    // condition*, a different spelling the document also chose, and `composite_of` resolves it
    // through `Schema::sym`, which hashes every byte. `O` spreads of one fragment therefore read
    // `O · L` bytes off `O + L` of syntax, and the charge for it used to be zero.
    self.spend_name(condition)?;
    let target = self.composite_of(condition);

    if check && let Some(target) = target {
      self.check_spread_possible(name, target, frame)?;
    }

    if !self.in_operation {
      return ControlFlow::Continue(None);
    }
    if self.scratch.visited.visit(ordinal) {
      // Already expanded during this operation's walk. The specification's transitive inclusion
      // is a set, so a second expansion could only repeat what the first one said — and on a
      // cyclic graph it would not terminate.
      return ControlFlow::Continue(None);
    }

    // Every operation's expansion, not only the first. `enter` is the definition-local half — the
    // directive rules over the definition's own directives, deduplicated — and the usages those
    // directives can carry are operation-local, so they have to be collected again each time. See
    // `Validator::begin_fragment`.
    let enter = !get_bit(&self.scratch.checked, row.definition);
    self.begin_fragment(row.definition, enter)?;
    let scope = target.map_or(NONE, |id| id.get());
    let flags = if enter { Frame::CHECK } else { 0 };
    ControlFlow::Continue(Some((
      Frame::root(row.definition, scope, flags),
      body.selection_set(),
    )))
  }

  /// Draft 5.5.2.3, all four subsections, as one bitset intersection.
  fn check_spread_possible(
    &mut self,
    name: &smear_parser::graphql::ast::Name<S>,
    target: TypeId,
    frame: Frame,
  ) -> ControlFlow<()> {
    if !self.on(Rule::FragmentSpreadIsPossible) {
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

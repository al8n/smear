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

use tokora::span::AsSpan;

use smear_parser::graphql::ast::{
  ExecutableDocument, Field, FragmentSpread, InlineFragment, Selection, SelectionSet,
};

use super::{
  Rule, TypeId, Validator,
  nodes::{child_selection_set, fragment, name_bytes, root_selection_set},
};
use crate::{
  diagnostic::Context,
  schema::{DirectiveLocation, Schema},
  scratch::{Frame, NONE, get_bit, set_bit},
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

    while let Some(frame) = self.scratch.frames.last().copied() {
      let Some(set) = current else {
        self.scratch.frames.pop();
        current = resolve(document, &self.scratch.frames);
        continue;
      };
      let Some(selection) = set.selections().get(frame.cursor as usize) else {
        self.scratch.frames.pop();
        current = resolve(document, &self.scratch.frames);
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

  /// Draft 5.3.1 and 5.3.3, plus the field's arguments and directives.
  ///
  /// Returns the scope its subselections are written against, or [`NONE`] when there is none to
  /// know — an unknown field, or a leaf, whose inner selections are then left alone rather than
  /// reported one by one against a type that cannot have them.
  fn check_field(&mut self, field: &'d Field<S>, frame: Frame) -> ControlFlow<(), u32> {
    let check = frame.flags & Frame::CHECK != 0;
    let name = field.name();

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
    let target = self.composite_of(condition);

    if check && let Some(target) = target {
      self.check_spread_possible(name, target, frame)?;
    }

    if !self.in_operation {
      return ControlFlow::Continue(None);
    }
    if set_bit(&mut self.scratch.visited, ordinal) {
      // Already expanded during this operation's walk. The specification's transitive inclusion
      // is a set, so a second expansion could only repeat what the first one said — and on a
      // cyclic graph it would not terminate.
      return ControlFlow::Continue(None);
    }

    let enter = !get_bit(&self.scratch.checked, row.definition);
    if enter {
      self.begin_fragment(row.definition)?;
    }
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
    if possible(self.schema, target, parent) {
      return ControlFlow::Continue(());
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

/// `GetPossibleTypes(fragmentType) ∩ GetPossibleTypes(parentType) ≠ ∅`, with the ecosystem's
/// self-spread exception.
///
/// The exception matters for exactly one shape: an interface with no implementors, spread on
/// itself. Its possible-object set is empty, so a literal reading of the rule refuses
/// `fragment F on Empty { ... F }`. graphql-js, apollo-compiler and the graphql-spec#1109
/// discussion all accept it, and diverging alone would only make a differential comparison noisy
/// without protecting anybody from anything.
fn possible(schema: &Schema, target: TypeId, parent: TypeId) -> bool {
  target == parent || schema.possible_objects_intersect(target, parent)
}

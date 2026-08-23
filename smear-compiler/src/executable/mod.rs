//! Executable-document validation: the entry point, and the passes that do not ride the selection
//! walk.
//!
//! # The shape of a run
//!
//! 1. **Prep** records the operations and fragments, sorts a fragment index, and collects every
//!    fragment spread as an edge of the fragment graph. Draft 5.5.1.1 and 5.5.2.1 fall out of it.
//! 2. **Operation rules** (5.2.1.1, 5.2.2.1, 5.2.3.1) read the operation list.
//! 3. **Fragment declaration rules** (5.5.1.2, 5.5.1.3) read the fragment table.
//! 4. **The fragment graph** answers 5.5.2.2 and 5.5.1.4 — with integers, over an explicit stack.
//!    Cycles are established here, before anything expands a fragment, so no later rule can meet
//!    an unestablished graph. Unlike apollo-compiler, unused fragments are included: a rule that
//!    trusts the graph must be able to trust all of it.
//! 5. **Subscriptions** get 5.2.4.1's own root-field collection.
//! 6. **One selection walk per operation**, following spreads with a visited bitset, carries every
//!    remaining rule. Rules that are properties of a *definition* rather than of an operation fire
//!    the first time that definition is reached and are suppressed afterwards, so a fragment three
//!    operations share reports its bad field once. Rules that are properties of an *operation* —
//!    the variable rules — run every time, which is what the specification asks for: the same
//!    fragment may be valid under one operation's variables and invalid under another's.
//! 7. **A final pass over fragments no operation reached**, so their structure is validated too.
//! 8. **Draft 5.3.2's merge engine**, last of all, over every selection set the document defines.
//!    It is the only rule with a working set and the only one an attacker can make expensive, so
//!    every cheap structural refusal — undefined spreads, fragment cycles — is already established
//!    before it expands anything, and what it may spend is the caller's [`Budget`].
//!
//! # Nothing recurses on document shape
//!
//! Selection sets and value literals are both walked with explicit stacks living in the caller's
//! [`Scratch`]. The frames are coordinates rather than references — a definition index plus the
//! chain of child indices that reaches a level — which is what lets a stack that knows nothing
//! about the document's source slice type replace recursion over an attacker-chosen shape. Draft
//! 5.3.2's two merge recursions and its value comparison are written the same way, for the same
//! reason and with more at stake.

mod merge;
mod nodes;
mod selections;
mod values;

use core::ops::ControlFlow;

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::{
  DescribedVariableDefinition, ExecutableDefinition, ExecutableDocument, Name, OperationDefinition,
  OperationType, Selection, Type,
};

use super::{
  Budget, Diagnostic, Rule, RuleSet, Scratch, Sink,
  diagnostic::Context,
  schema::{
    DirectiveLocation, MAX_WRAPPERS, PackedType, RootOperation, Schema, Sym, TypeId, is_reserved,
  },
  scratch::{
    Edge, FragmentRow, Frame, GraphFrame, NONE, OperationRow, Work, clear_bit, get_bit, reset_bits,
    set_bit,
  },
};

use nodes::{child_selection_set, definition, fragment, name_bytes, operation, root_selection_set};
use values::ValueLocation;

/// The verdict of a failed validation.
///
/// Returned when the document was refused: because at least one diagnostic was emitted, or because
/// a [`Budget`] abandoned a rule partway through. What the diagnostics *were* is the sink's
/// business — this is only the count, whether a budget refused, and whether the sink asked to stop
/// before the document had been fully examined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Invalid {
  emitted: u32,
  stopped: bool,
  budget: bool,
}

impl Invalid {
  /// Returns how many diagnostics were produced.
  ///
  /// This counts what the validator emitted, not what the sink kept: [`Ignore`](super::Ignore)
  /// discards everything and the count is still right.
  ///
  /// **Zero is a possible count on a verdict that is still `Err`**, and it means exactly one
  /// thing: a budget refused the document with the bound's own rule outside the
  /// [`RuleSet`](super::RuleSet), so there was nothing to emit. [`Invalid::budget_tripped`] is
  /// true whenever that happens, and a caller that reports "no findings" without reading it would
  /// be describing a check the engine abandoned.
  #[inline]
  pub const fn emitted(&self) -> u32 {
    self.emitted
  }

  /// Returns whether the **sink** stopped validation before the document was fully examined.
  ///
  /// True for [`First`](super::First) on any invalid document *that produced a diagnostic*. The
  /// qualification is not pedantry: there is exactly one verdict that is `Err` with nothing
  /// emitted — a resource bound refusing with its own rule outside the
  /// [`RuleSet`](super::RuleSet) — and no diagnostic ever reaches the sink there, so the sink
  /// never asks for anything to stop and this reads `false` on a document that was very much not
  /// fully examined. Read as "was the whole document looked at", it says the opposite of the truth
  /// on the one case where it matters most.
  ///
  /// So the two flags answer two questions and neither answers the other's: this one says **who**
  /// stopped the walk, and [`Invalid::budget_tripped`] says whether a bound refused. A caller who
  /// wants "is anything about this document still unknown" reads both.
  ///
  /// When it is true, the absence of a diagnostic says nothing: the rest of the document was never
  /// looked at. al8n/smear#196.
  #[inline]
  pub const fn stopped(&self) -> bool {
    self.stopped
  }

  /// Returns whether a [`Budget`] refused the document.
  ///
  /// True when draft 5.3.2's merge engine reached
  /// [`Budget::merge_depth`](super::Budget::merge_depth) or
  /// [`Budget::merge_work`](super::Budget::merge_work). With the bound's own rule in the
  /// [`RuleSet`](super::RuleSet) the diagnostic is
  /// [`Rule::MergeDepthBudget`](super::Rule::MergeDepthBudget) or
  /// [`Rule::MergeWorkBudget`](super::Rule::MergeWorkBudget) and says which; **without it there is
  /// no diagnostic and this flag is the whole of the report**, on a verdict whose
  /// [`Invalid::emitted`] is zero. Filtering a bound's rule out switches off its diagnostic, not
  /// the refusal: an engine that stopped and then answered `Ok` would be reporting a clean result
  /// for a check it never finished. al8n/smear#196.
  ///
  /// When it is true the document is **invalid**, not "unvalidated": the engine refuses rather
  /// than passing what it could not finish examining. What it does *not* mean is that the rest of
  /// the document is clean — the merge engine stopped, so anything it had not reached is unknown,
  /// exactly as [`Invalid::stopped`] means for the sink.
  #[inline]
  pub const fn budget_tripped(&self) -> bool {
    self.budget
  }
}

impl core::fmt::Display for Invalid {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    if self.emitted == 0 {
      // The budget refused with its own rule filtered out. "0 validation errors" would read as the
      // opposite of what happened.
      return f.write_str("resource budget exceeded before the document was fully examined");
    }
    let plural = if self.emitted == 1 { "" } else { "s" };
    write!(f, "{} validation error{plural}", self.emitted)?;
    if self.stopped {
      f.write_str(" (validation stopped early)")?;
    }
    if self.budget {
      f.write_str(" (resource budget exceeded)")?;
    }
    Ok(())
  }
}

impl core::error::Error for Invalid {}

/// Validates an executable document against a schema, checking every draft §5 rule.
///
/// `scratch` is the caller's reusable working set and `sink` the caller's diagnostic storage; the
/// validator owns neither, which is what lets the steady state allocate nothing. `budget` bounds
/// draft 5.3.2's merge engine.
///
/// Returns `Err(Invalid)` when at least one rule fired **or** a [`Budget`] bound refused the
/// document. Those are not the same thing and [`Invalid::budget_tripped`] is what separates them:
/// a refusal with the bound's own rule filtered out has an [`Invalid::emitted`] of zero.
///
/// # Example
///
/// ```
/// use smear_compiler::{Budget, First, Schema, Scratch, validate_executable};
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::{ExecutableDocument, TypeSystemDocument},
///     error::GraphqlErrors,
///     syntactic::{GraphqlLexer, executable_document, type_system_document},
///   },
///   lexer::tokora::{Parse as _, Parser},
/// };
///
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str("type Query { hero: Character } interface Character { name: String! }")
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let parse = |src| {
///   Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     executable_document,
///   )
///   .parse_str(src)
///   .expect("the query parses")
/// };
///
/// let mut scratch = Scratch::new();
/// let budget = Budget::default();
///
/// let good = parse("{ hero { name } }");
/// let mut sink = First::new();
/// assert!(validate_executable(&schema, &good, &mut scratch, &budget, &mut sink).is_ok());
///
/// let bad = parse("{ hero { nickname } }");
/// let mut sink = First::new();
/// let invalid = validate_executable(&schema, &bad, &mut scratch, &budget, &mut sink)
///   .expect_err("`nickname` is not on `Character`");
/// assert_eq!(invalid.emitted(), 1);
/// assert_eq!(
///   sink.get().expect("a diagnostic").display(&schema).to_string(),
///   "5.3.1 Field Selections: `nickname` on type `Character` (9..17)"
/// );
/// ```
pub fn validate_executable<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  validate_executable_with(schema, document, scratch, budget, RuleSet::ALL, sink)
}

/// Validates an executable document against a subset of the rules.
///
/// A draft §5 rule outside `rules` is not evaluated, not merely filtered: a consumer that wants
/// only the fragment rules does not pay for value coercion. With [`RuleSet::ALL`] this is exactly
/// [`validate_executable`].
///
/// **`rules` does not reach the resource bounds.** Narrowing removes a bound's diagnostic, never
/// the bound: `budget` is enforced whatever this set contains, so a caller who asks for
/// [`Rule::FieldSelectionMerging`](super::Rule::FieldSelectionMerging) alone can still be handed
/// `Err` with [`Invalid::emitted`] zero and [`Invalid::budget_tripped`] set. The knob is what
/// switches a bound off — see [`Budget`]. al8n/smear#196.
pub fn validate_executable_with<S, K>(
  schema: &Schema,
  document: &ExecutableDocument<S>,
  scratch: &mut Scratch,
  budget: &Budget,
  rules: RuleSet,
  sink: &mut K,
) -> Result<(), Invalid>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  scratch.reset();
  let mut validator = Validator {
    schema,
    document,
    scratch,
    sink,
    rules,
    budget: *budget,
    scalars: Scalars::resolve(schema),
    variables: &[],
    in_operation: false,
    emitted: 0,
    stopped: false,
    work: Work::new(budget.merge_work()),
    generation: 0,
    tripped: false,
    blame: SimpleSpan::const_new(0, 0),
  };
  let _ = validator.run();
  let (emitted, stopped, budget) = (validator.emitted, validator.stopped, validator.tripped);
  // A budget refusal is a refusal whether or not it had a rule to emit. `Ok` here would be a clean
  // verdict on a rule the engine abandoned partway through. al8n/smear#196.
  if emitted == 0 && !budget {
    Ok(())
  } else {
    Err(Invalid {
      emitted,
      stopped,
      budget,
    })
  }
}

/// The five built-in scalar names, resolved against this schema once per run.
///
/// Coercion is decided by *name*, not by the built-in flag: a document may spell `scalar String`
/// out — a printed schema does — and it is still the specification's `String`. Everything else is
/// a custom scalar, and a custom scalar accepts any literal because only the service knows how to
/// read it.
#[derive(Debug, Clone, Copy)]
struct Scalars {
  int: Option<Sym>,
  float: Option<Sym>,
  string: Option<Sym>,
  boolean: Option<Sym>,
  id: Option<Sym>,
}

impl Scalars {
  fn resolve(schema: &Schema) -> Self {
    Self {
      int: schema.sym(b"Int"),
      float: schema.sym(b"Float"),
      string: schema.sym(b"String"),
      boolean: schema.sym(b"Boolean"),
      id: schema.sym(b"ID"),
    }
  }
}

/// One validation run.
struct Validator<'a, 'd, S, K> {
  schema: &'a Schema,
  document: &'d ExecutableDocument<S>,
  scratch: &'a mut Scratch,
  sink: &'a mut K,
  rules: RuleSet,
  budget: Budget,
  scalars: Scalars,
  /// The variable definitions of the operation being walked; empty outside one.
  variables: &'d [DescribedVariableDefinition<S>],
  /// Whether a variable scope is in effect. Distinguishes "an operation with no variables" from
  /// "a fragment nothing reached", which is the difference between draft 5.8.3 firing and not.
  in_operation: bool,
  emitted: u32,
  stopped: bool,
  /// How much of the [`Budget`]'s work knob draft 5.3.2's engine has spent, and the ceiling it is
  /// spending against.
  work: Work,
  /// Distinguishes one fragment expansion from the next without clearing a bitset per expansion.
  generation: u32,
  /// Whether a budget stopped the merge engine, which is both what abandons the walk and what the
  /// verdict reports. Set even when the bound's own rule is switched off: the bound holds either
  /// way, and only the *diagnostic* is optional. It used to be two fields — one for the walk and
  /// one for the verdict, the second set only when something was emitted — which is how a refused
  /// document with its rule filtered out came back `Ok`. al8n/smear#196.
  tripped: bool,
  /// The definition to blame for a budget diagnostic — the one being merged when the bound hit.
  blame: SimpleSpan,
}

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: Sink<S>,
{
  // -- reporting ------------------------------------------------------------------------------

  /// Returns whether a rule is enabled. Checked before building a diagnostic, so a disabled rule
  /// costs nothing.
  #[inline]
  fn on(&self, rule: Rule) -> bool {
    self.rules.contains(rule)
  }

  fn emit(&mut self, diagnostic: Diagnostic<S>) -> ControlFlow<()> {
    self.emitted = self.emitted.saturating_add(1);
    match self.sink.diagnostic(diagnostic) {
      ControlFlow::Continue(()) => ControlFlow::Continue(()),
      ControlFlow::Break(()) => {
        self.stopped = true;
        ControlFlow::Break(())
      }
    }
  }

  /// Emits a diagnostic naming a source spelling, at that spelling's own span.
  fn report_name(&mut self, rule: Rule, name: &Name<S>, context: Context) -> ControlFlow<()> {
    let diagnostic = Diagnostic::new(rule, *name.as_span())
      .subject(name.source().clone())
      .context(context);
    self.emit(diagnostic)
  }

  // -- schema helpers -------------------------------------------------------------------------

  /// Resolves a source name to a schema type.
  fn type_of(&self, name: &Name<S>) -> Option<TypeId> {
    self.schema.type_of_sym(self.schema.sym(name_bytes(name))?)
  }

  /// Flattens an AST type reference into the schema's packed form.
  ///
  /// `None` when the base type is not in the schema or the reference nests deeper than the
  /// representation admits. The walk is iterative for the same reason every other walk here is.
  fn pack_type(&self, ty: &Type<Name<S>>) -> Option<PackedType> {
    let mut list_required = [false; MAX_WRAPPERS as usize];
    let mut depth = 0usize;
    let mut cursor = ty;
    let named = loop {
      match cursor {
        Type::Name(named) => break named,
        Type::List(list) => {
          if depth >= list_required.len() {
            return None;
          }
          list_required[depth] = list.required();
          depth += 1;
          cursor = list.ty();
        }
      }
    };
    let sym = self.schema.sym(name_bytes(named.name()))?;
    let id = self.schema.type_of_sym(sym)?;
    let mut packed = PackedType::named(sym, id);
    if named.required() {
      packed = packed.push_non_null()?;
    }
    for required in list_required[..depth].iter().rev() {
      packed = packed.push_list()?;
      if *required {
        packed = packed.push_non_null()?;
      }
    }
    Some(packed)
  }

  // -- the run --------------------------------------------------------------------------------

  fn run(&mut self) -> ControlFlow<()> {
    self.prep()?;
    self.check_operations()?;
    self.check_fragment_declarations()?;
    self.check_fragment_cycles()?;
    self.check_fragments_used()?;
    self.check_subscriptions()?;
    self.walk_operations()?;
    self.walk_unreached_fragments()?;
    // Last, and deliberately so. Draft 5.3.2 is the only rule with a working set and the only one
    // an attacker can make expensive, so every cheap structural refusal — undefined spreads,
    // fragment cycles — is already established before it expands anything. Reordering this would
    // change the threat model, not just the diagnostic order.
    self.check_field_merging()
  }

  // -- 1. prep --------------------------------------------------------------------------------

  fn prep(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for (index, described) in document.definitions().iter().enumerate() {
      let index = index as u32;
      match described.node() {
        ExecutableDefinition::Operation(operation) => {
          let (root, named, span) = match operation {
            OperationDefinition::Named(named) => (
              root_operation(named.operation_type()),
              named.name().is_some(),
              match named.name() {
                Some(name) => *name.as_span(),
                None => *named.operation_type().span(),
              },
            ),
            // Query shorthand: an anonymous query, blamed at its own braces.
            OperationDefinition::Shorthand(set) => (RootOperation::Query, false, *set.span()),
          };
          self.scratch.operations.push(OperationRow {
            definition: index,
            root: root.index() as u8,
            named,
            span,
            edges: Default::default(),
          });
        }
        ExecutableDefinition::Fragment(fragment) => {
          self.scratch.fragments.push(FragmentRow {
            definition: index,
            span: *fragment.name().as_span(),
            group: Default::default(),
            edges: Default::default(),
          });
        }
      }
    }

    self.index_fragments()?;
    self.collect_edges()
  }

  /// Sorts the fragment ordinals by name, records each name's group, and reports draft 5.5.1.1.
  fn index_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let count = self.scratch.fragments.len() as u32;
    {
      let scratch = &mut *self.scratch;
      scratch.order.extend(0..count);
      let rows = &scratch.fragments;
      // Ties break on the ordinal, so the order is total and the first definition of a name is
      // always the group's first element — which is the one spreads resolve to.
      scratch.order.sort_unstable_by(|a, b| {
        let left = fragment_name(document, rows, *a);
        let right = fragment_name(document, rows, *b);
        left.cmp(right).then(a.cmp(b))
      });
    }

    let mut start = 0usize;
    while start < self.scratch.order.len() {
      let first = self.scratch.order[start];
      let name = fragment_name(document, &self.scratch.fragments, first);
      let mut end = start + 1;
      while end < self.scratch.order.len()
        && fragment_name(document, &self.scratch.fragments, self.scratch.order[end]) == name
      {
        end += 1;
      }
      let group = range32(start as u32, end as u32);
      for slot in start..end {
        let ordinal = self.scratch.order[slot] as usize;
        self.scratch.fragments[ordinal].group = group;
      }
      if end - start > 1 && self.on(Rule::FragmentNameUniqueness) {
        let related = self.scratch.fragments[first as usize].span;
        for slot in start + 1..end {
          let ordinal = self.scratch.order[slot];
          let row = self.scratch.fragments[ordinal as usize];
          let Some(fragment) = fragment(document, row.definition) else {
            continue;
          };
          let diagnostic = Diagnostic::new(Rule::FragmentNameUniqueness, row.span)
            .subject(fragment.name().source().clone())
            .related(related);
          self.emit(diagnostic)?;
        }
      }
      start = end;
    }
    ControlFlow::Continue(())
  }

  /// Returns the ordinal a fragment name resolves to — the first definition of that name.
  fn find_fragment(&self, name: &[u8]) -> Option<u32> {
    let document = self.document;
    let rows = &self.scratch.fragments;
    let order = &self.scratch.order;
    let slot = order
      .binary_search_by(|ordinal| fragment_name(document, rows, *ordinal).cmp(name))
      .ok()?;
    // Ties are ordered by ordinal, so walking back to the group's start finds the first
    // definition rather than whichever one the search landed on.
    let ordinal = order[slot];
    let group = rows[ordinal as usize].group;
    order.get(group.start() as usize).copied()
  }

  /// Walks every definition once, recording its fragment spreads as graph edges and reporting
  /// draft 5.5.2.1 for the ones that name nothing.
  fn collect_edges(&mut self) -> ControlFlow<()> {
    let document = self.document;
    let total = document.definitions().len() as u32;
    let mut operations = 0usize;
    let mut fragments = 0usize;
    for index in 0..total {
      let start = self.scratch.edges.len() as u32;
      self.collect_definition_edges(index)?;
      let range = range32(start, self.scratch.edges.len() as u32);
      match definition(document, index) {
        Some(ExecutableDefinition::Operation(_)) => {
          self.scratch.operations[operations].edges = range;
          operations += 1;
        }
        Some(ExecutableDefinition::Fragment(_)) => {
          self.scratch.fragments[fragments].edges = range;
          fragments += 1;
        }
        None => {}
      }
    }
    ControlFlow::Continue(())
  }

  fn collect_definition_edges(&mut self, index: u32) -> ControlFlow<()> {
    let document = self.document;
    self.scratch.frames.clear();
    self.scratch.frames.push(Frame::root(index, NONE, 0));
    let mut current = root_selection_set(document, index);
    while let Some(frame) = self.scratch.frames.last().copied() {
      let Some(set) = current else {
        self.scratch.frames.pop();
        current = selections::resolve(document, &self.scratch.frames);
        continue;
      };
      let selections = set.selections();
      let Some(selection) = selections.get(frame.cursor as usize) else {
        self.scratch.frames.pop();
        current = selections::resolve(document, &self.scratch.frames);
        continue;
      };
      if let Some(top) = self.scratch.frames.last_mut() {
        top.cursor += 1;
      }
      match selection {
        Selection::FragmentSpread(spread) => {
          let name = spread.name();
          let to = self.find_fragment(name_bytes(name));
          if to.is_none() && self.on(Rule::FragmentSpreadTargetDefined) {
            let diagnostic = Diagnostic::new(Rule::FragmentSpreadTargetDefined, *name.as_span())
              .subject(name.source().clone());
            self.emit(diagnostic)?;
          }
          self.scratch.edges.push(Edge {
            to: to.unwrap_or(NONE),
            span: *spread.span(),
          });
        }
        _ => {
          if let Some(child) = child_selection_set(selection) {
            self
              .scratch
              .frames
              .push(Frame::child(index, frame.cursor, NONE, 0));
            current = Some(child);
          }
        }
      }
    }
    ControlFlow::Continue(())
  }

  // -- 2. operation rules ---------------------------------------------------------------------

  fn check_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;

    // 5.2.1.1 — the schema must provide the root operation type the operation needs.
    if self.on(Rule::OperationTypeExistence) {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        let root = RootOperation::ALL[row.root as usize];
        if self.schema.root(root).is_none() {
          let diagnostic =
            Diagnostic::new(Rule::OperationTypeExistence, row.span).context(Context::Root(root));
          let diagnostic = match operation_name(document, row.definition) {
            Some(name) => diagnostic.subject(name.source().clone()),
            None => diagnostic,
          };
          self.emit(diagnostic)?;
        }
      }
    }

    // 5.2.2.1 — named operations must not collide. Sorted index scan, no map.
    if self.on(Rule::OperationNameUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..self.scratch.operations.len() {
        if self.scratch.operations[index].named {
          self.scratch.keys.push(index as u32);
        }
      }
      {
        let scratch = &mut *self.scratch;
        let rows = &scratch.operations;
        // Ties break on the document index, so the first definition of a name is always the
        // group's first element and every later one is blamed against it.
        scratch.keys[base..].sort_unstable_by(|a, b| {
          let left = operation_name_bytes(document, rows, *a);
          let right = operation_name_bytes(document, rows, *b);
          left.cmp(right).then(a.cmp(b))
        });
      }
      let mut start = base;
      while start < self.scratch.keys.len() {
        let first = self.scratch.keys[start];
        let name = operation_name_bytes(document, &self.scratch.operations, first);
        let mut end = start + 1;
        while end < self.scratch.keys.len()
          && operation_name_bytes(document, &self.scratch.operations, self.scratch.keys[end])
            == name
        {
          end += 1;
        }
        let related = self.scratch.operations[first as usize].span;
        for slot in start + 1..end {
          let row = self.scratch.operations[self.scratch.keys[slot] as usize];
          if let Some(name) = operation_name(document, row.definition) {
            let diagnostic = Diagnostic::new(Rule::OperationNameUniqueness, row.span)
              .subject(name.source().clone())
              .related(related);
            self.emit(diagnostic)?;
          }
        }
        start = end;
      }
      self.scratch.keys.truncate(base);
    }

    // 5.2.3.1 — an anonymous operation must be the document's only one.
    if self.on(Rule::LoneAnonymousOperation) && self.scratch.operations.len() > 1 {
      for index in 0..self.scratch.operations.len() {
        let row = self.scratch.operations[index];
        if !row.named {
          self.emit(
            Diagnostic::new(Rule::LoneAnonymousOperation, row.span)
              .context(Context::Count(self.scratch.operations.len() as u32)),
          )?;
        }
      }
    }

    ControlFlow::Continue(())
  }

  // -- 3. fragment declaration rules ------------------------------------------------------------

  fn check_fragment_declarations(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      let Some(fragment) = fragment(document, row.definition) else {
        continue;
      };
      let condition = fragment.type_condition().name();
      self.check_type_condition(condition)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.2 and 5.5.1.3, shared by fragment definitions and inline fragments.
  ///
  /// Returns the condition's type when it resolved to a composite one, which is also the scope its
  /// selection set is written against.
  fn check_type_condition(&mut self, condition: &Name<S>) -> ControlFlow<(), Option<TypeId>> {
    let Some(id) = self.type_of(condition) else {
      if self.on(Rule::FragmentSpreadTypeExistence) {
        self.report_name(Rule::FragmentSpreadTypeExistence, condition, Context::None)?;
      }
      return ControlFlow::Continue(None);
    };
    if !self.schema.type_def(id).kind().is_composite() {
      if self.on(Rule::FragmentsOnCompositeTypes) {
        let context = Context::Type(self.schema.type_def(id).name());
        self.report_name(Rule::FragmentsOnCompositeTypes, condition, context)?;
      }
      return ControlFlow::Continue(None);
    }
    ControlFlow::Continue(Some(id))
  }

  // -- 4. the fragment graph --------------------------------------------------------------------

  /// Draft 5.5.2.2, as an iterative depth-first search over integers.
  fn check_fragment_cycles(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::FragmentSpreadsMustNotFormCycles) {
      return ControlFlow::Continue(());
    }
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.on_path, count);
    reset_bits(&mut self.scratch.done, count);
    for start in 0..count as u32 {
      if get_bit(&self.scratch.done, start) {
        continue;
      }
      self.scratch.graph.clear();
      set_bit(&mut self.scratch.on_path, start);
      self.scratch.graph.push(GraphFrame {
        fragment: start,
        edge: self.scratch.fragments[start as usize].edges.start(),
      });
      while let Some(top) = self.scratch.graph.last().copied() {
        let row = self.scratch.fragments[top.fragment as usize];
        if top.edge >= row.edges.end() {
          clear_bit(&mut self.scratch.on_path, top.fragment);
          set_bit(&mut self.scratch.done, top.fragment);
          self.scratch.graph.pop();
          continue;
        }
        if let Some(frame) = self.scratch.graph.last_mut() {
          frame.edge += 1;
        }
        let edge = self.scratch.edges[top.edge as usize];
        if edge.to == NONE {
          continue;
        }
        if get_bit(&self.scratch.on_path, edge.to) {
          let subject = self.scratch.fragments[edge.to as usize];
          let Some(target) = fragment(self.document, subject.definition) else {
            continue;
          };
          let diagnostic = Diagnostic::new(Rule::FragmentSpreadsMustNotFormCycles, edge.span)
            .subject(target.name().source().clone())
            .related(subject.span);
          self.emit(diagnostic)?;
          continue;
        }
        if get_bit(&self.scratch.done, edge.to) {
          continue;
        }
        set_bit(&mut self.scratch.on_path, edge.to);
        self.scratch.graph.push(GraphFrame {
          fragment: edge.to,
          edge: self.scratch.fragments[edge.to as usize].edges.start(),
        });
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.5.1.4, as reachability from the operations over the same graph.
  ///
  /// Reachability propagates across every definition sharing a name, so one duplicated fragment
  /// name reports 5.5.1.1 and nothing else — the copy is not also "unused".
  fn check_fragments_used(&mut self) -> ControlFlow<()> {
    let count = self.scratch.fragments.len();
    reset_bits(&mut self.scratch.reachable, count);
    self.scratch.graph.clear();
    for index in 0..self.scratch.operations.len() {
      let edges = self.scratch.operations[index].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to);
      }
    }
    while let Some(frame) = self.scratch.graph.pop() {
      let edges = self.scratch.fragments[frame.fragment as usize].edges;
      for slot in edges.start()..edges.end() {
        let to = self.scratch.edges[slot as usize].to;
        self.mark_reachable(to);
      }
    }

    if !self.on(Rule::FragmentsMustBeUsed) {
      return ControlFlow::Continue(());
    }
    for ordinal in 0..count as u32 {
      if get_bit(&self.scratch.reachable, ordinal) {
        continue;
      }
      let row = self.scratch.fragments[ordinal as usize];
      let Some(target) = fragment(self.document, row.definition) else {
        continue;
      };
      let diagnostic = Diagnostic::new(Rule::FragmentsMustBeUsed, row.span)
        .subject(target.name().source().clone());
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }

  /// Marks a fragment and every same-named definition reachable, queueing the newly marked.
  fn mark_reachable(&mut self, ordinal: u32) {
    if ordinal == NONE {
      return;
    }
    let group = self.scratch.fragments[ordinal as usize].group;
    for slot in group.start()..group.end() {
      let member = self.scratch.order[slot as usize];
      if !set_bit(&mut self.scratch.reachable, member) {
        self.scratch.graph.push(GraphFrame {
          fragment: member,
          edge: 0,
        });
      }
    }
  }

  // -- 5. subscriptions ---------------------------------------------------------------------------

  fn check_subscriptions(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::SingleRootField) {
      return ControlFlow::Continue(());
    }
    let Some(root) = self.schema.root(RootOperation::Subscription) else {
      return ControlFlow::Continue(());
    };
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      if RootOperation::ALL[row.root as usize] != RootOperation::Subscription {
        continue;
      }
      self.check_subscription_roots(row, root)?;
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.2.4.1's `CollectSubscriptionFields`, as an explicit walk.
  ///
  /// It only has to answer "is the collected map a set of one, and is that one an introspection
  /// field", so it never builds the map: two distinct response names are enough to fail, and the
  /// first response name seen is the only one it needs to keep.
  fn check_subscription_roots(&mut self, row: OperationRow, root: TypeId) -> ControlFlow<()> {
    let document = self.document;
    reset_bits(&mut self.scratch.visited, self.scratch.fragments.len());
    self.scratch.roots.clear();
    self
      .scratch
      .roots
      .push(Frame::root(row.definition, NONE, 0));
    let mut current = root_selection_set(document, row.definition);

    // The rule only asks whether the collected map is a set of one, so the first response name
    // seen is all that has to be kept: any later name that differs makes at least two entries.
    // The *field* name is kept alongside it, because "must not be an introspection field" is a
    // question about the field and not about the key an alias would give it.
    let mut first_response: Option<&'d Name<S>> = None;
    let mut first_field: Option<&'d Name<S>> = None;
    let mut multiple = false;

    while let Some(frame) = self.scratch.roots.last().copied() {
      let Some(set) = current else {
        self.scratch.roots.pop();
        current = selections::resolve(document, &self.scratch.roots);
        continue;
      };
      let Some(selection) = set.selections().get(frame.cursor as usize) else {
        self.scratch.roots.pop();
        current = selections::resolve(document, &self.scratch.roots);
        continue;
      };
      if let Some(top) = self.scratch.roots.last_mut() {
        top.cursor += 1;
      }

      // "{selection} must not provide the `@skip`/`@include` directive" — the whole reason this
      // collection exists is that it has no runtime variables to evaluate them with.
      if let Some(directive) = self.conditional_directive(selection) {
        let diagnostic = Diagnostic::new(Rule::SingleRootField, *directive.as_span())
          .subject(directive.source().clone());
        self.emit(diagnostic)?;
      }

      match selection {
        Selection::Field(field) => {
          let response = nodes::response_name(field);
          match first_response {
            None => {
              first_response = Some(response);
              first_field = Some(field.name());
            }
            Some(seen) if name_bytes(seen) != name_bytes(response) => multiple = true,
            Some(_) => {}
          }
        }
        Selection::InlineFragment(inline) => {
          let applies = match inline.type_condition() {
            Some(condition) => self.condition_applies(condition.name(), root),
            None => true,
          };
          if applies {
            self
              .scratch
              .roots
              .push(Frame::child(frame.definition, frame.cursor, NONE, 0));
            current = Some(inline.selection_set());
          }
        }
        Selection::FragmentSpread(spread) => {
          let Some(ordinal) = self.find_fragment(name_bytes(spread.name())) else {
            continue;
          };
          if set_bit(&mut self.scratch.visited, ordinal) {
            continue;
          }
          let target = self.scratch.fragments[ordinal as usize];
          let Some(body) = fragment(document, target.definition) else {
            continue;
          };
          if !self.condition_applies(body.type_condition().name(), root) {
            continue;
          }
          self
            .scratch
            .roots
            .push(Frame::root(target.definition, NONE, 0));
          current = Some(body.selection_set());
        }
      }
    }

    if multiple {
      // At least two entries. The exact count would cost a second walk and change nothing about
      // the verdict, so the context reports the bound it established.
      self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(2)))?;
    } else {
      match first_field {
        None => {
          self.emit(Diagnostic::new(Rule::SingleRootField, row.span).context(Context::Count(0)))?;
        }
        Some(field) if is_reserved(name_bytes(field)) => {
          self.report_name(Rule::SingleRootField, field, Context::None)?;
        }
        Some(_) => {}
      }
    }
    ControlFlow::Continue(())
  }

  /// Returns the `@skip` or `@include` a selection carries, if it carries one.
  fn conditional_directive(&self, selection: &'d Selection<S>) -> Option<&'d Name<S>> {
    let directives = match selection {
      Selection::Field(field) => field.directives(),
      Selection::FragmentSpread(spread) => spread.directives(),
      Selection::InlineFragment(inline) => inline.directives(),
    }?;
    directives.directives().iter().find_map(|directive| {
      let name = directive.name();
      matches!(name_bytes(name), b"skip" | b"include").then_some(name)
    })
  }

  /// `DoesFragmentTypeApply` against a known object type.
  fn condition_applies(&self, condition: &Name<S>, object: TypeId) -> bool {
    match self.type_of(condition) {
      // An undefined or non-composite condition is 5.5.1.2/5.5.1.3's business; here it simply
      // cannot contribute a root field.
      Some(id) => self.schema.is_possible_object(id, object),
      None => false,
    }
  }

  // -- 6. the per-operation walks ------------------------------------------------------------

  fn walk_operations(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for index in 0..self.scratch.operations.len() {
      let row = self.scratch.operations[index];
      let Some(operation) = operation(document, row.definition) else {
        continue;
      };

      self.variables = match operation {
        OperationDefinition::Named(named) => match named.variable_definitions() {
          Some(definitions) => definitions.variable_definitions(),
          None => &[],
        },
        OperationDefinition::Shorthand(_) => &[],
      };
      self.in_operation = true;

      self.check_variable_definitions()?;

      let root = RootOperation::ALL[row.root as usize];
      if let OperationDefinition::Named(named) = operation
        && let Some(directives) = named.directives()
      {
        self.check_directives(directives.directives(), directive_location(root), true)?;
      }

      let scope = self.schema.root(root).map_or(NONE, |id| id.get());
      reset_bits(&mut self.scratch.visited, self.scratch.fragments.len());
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;

      self.check_variables_used()?;

      self.variables = &[];
      self.in_operation = false;
    }
    ControlFlow::Continue(())
  }

  // -- 7. fragments nothing reached -------------------------------------------------------------

  fn walk_unreached_fragments(&mut self) -> ControlFlow<()> {
    let document = self.document;
    for ordinal in 0..self.scratch.fragments.len() {
      let row = self.scratch.fragments[ordinal];
      if get_bit(&self.scratch.checked, row.definition) {
        continue;
      }
      let Some(body) = fragment(document, row.definition) else {
        continue;
      };
      // The condition was already reported on, if it needed reporting, by the declaration pass.
      let scope = self
        .composite_of(body.type_condition().name())
        .map_or(NONE, |id| id.get());
      self.begin_fragment(row.definition)?;
      self.walk_selections(Frame::root(row.definition, scope, Frame::CHECK))?;
    }
    ControlFlow::Continue(())
  }

  /// Records that a fragment's definition-local rules are running now, and checks the directives
  /// on the definition itself.
  pub(super) fn begin_fragment(&mut self, index: u32) -> ControlFlow<()> {
    grow_bits(&mut self.scratch.checked, index as usize + 1);
    set_bit(&mut self.scratch.checked, index);
    let Some(body) = fragment(self.document, index) else {
      return ControlFlow::Continue(());
    };
    match body.directives() {
      Some(directives) => self.check_directives(
        directives.directives(),
        DirectiveLocation::FragmentDefinition,
        true,
      ),
      None => ControlFlow::Continue(()),
    }
  }

  // -- variables ------------------------------------------------------------------------------

  /// Draft 5.8.1 and 5.8.2, plus the value and directive rules over a variable definition's own
  /// default value and directives — the one place in an executable document where constant values
  /// appear.
  fn check_variable_definitions(&mut self) -> ControlFlow<()> {
    let definitions = self.variables;
    reset_bits(&mut self.scratch.used, definitions.len());

    // 5.8.1 — one type per variable name, per operation.
    if self.on(Rule::VariableUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..definitions.len() {
        self.scratch.keys.push(index as u32);
      }
      self.scratch.keys[base..].sort_unstable_by(|a, b| {
        let left = name_bytes(definitions[*a as usize].node().variable().name());
        let right = name_bytes(definitions[*b as usize].node().variable().name());
        left.cmp(right).then(a.cmp(b))
      });
      let mut slot = base + 1;
      while slot < self.scratch.keys.len() {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        let first = definitions[earlier as usize].node().variable();
        let repeat = definitions[later as usize].node().variable();
        if name_bytes(first.name()) == name_bytes(repeat.name()) {
          let diagnostic = Diagnostic::new(Rule::VariableUniqueness, *repeat.span())
            .subject(repeat.name().source().clone())
            .related(*first.span());
          self.emit(diagnostic)?;
        }
        slot += 1;
      }
      self.scratch.keys.truncate(base);
    }

    for described in definitions {
      let definition = described.node();
      let variable = definition.variable();
      let declared = self.pack_type(definition.ty());

      // 5.8.2 — objects, interfaces and unions cannot be variable types, and neither can a name
      // the schema does not have.
      let is_input =
        declared.is_some_and(|packed| self.schema.type_def(packed.base_id()).kind().is_input());
      if !is_input && self.on(Rule::VariablesAreInputTypes) {
        let context = match declared {
          Some(packed) => Context::Expected(packed),
          None => Context::None,
        };
        self.report_name(Rule::VariablesAreInputTypes, variable.name(), context)?;
      }

      if let Some(default) = definition.default_value() {
        self.walk_value(default.value(), declared, ValueLocation::PLAIN, true)?;
      }

      if let Some(directives) = definition.directives() {
        self.check_directives(
          directives.directives(),
          DirectiveLocation::VariableDefinition,
          true,
        )?;
      }
    }
    ControlFlow::Continue(())
  }

  /// Draft 5.8.4, read off the marks the usages left.
  fn check_variables_used(&mut self) -> ControlFlow<()> {
    if !self.on(Rule::AllVariablesUsed) {
      return ControlFlow::Continue(());
    }
    for index in 0..self.variables.len() {
      if get_bit(&self.scratch.used, index as u32) {
        continue;
      }
      let variable = self.variables[index].node().variable();
      let diagnostic = Diagnostic::new(Rule::AllVariablesUsed, *variable.span())
        .subject(variable.name().source().clone());
      self.emit(diagnostic)?;
    }
    ControlFlow::Continue(())
  }
}

// ---------------------------------------------------------------------------------------------
// free helpers
// ---------------------------------------------------------------------------------------------

/// Builds a table range.
fn range32(start: u32, end: u32) -> super::schema::Range32 {
  super::schema::Range32::new(start, end)
}

/// Grows a bitset to hold at least `bits` bits, preserving what is already set.
fn grow_bits(words: &mut std::vec::Vec<u64>, bits: usize) {
  let needed = bits.div_ceil(64);
  if words.len() < needed {
    words.resize(needed, 0);
  }
}

/// The root operation slot an operation keyword needs.
fn root_operation(keyword: &OperationType) -> RootOperation {
  match keyword {
    OperationType::Query(_) => RootOperation::Query,
    OperationType::Mutation(_) => RootOperation::Mutation,
    OperationType::Subscription(_) => RootOperation::Subscription,
  }
}

/// The directive location an operation of this kind is.
fn directive_location(root: RootOperation) -> DirectiveLocation {
  match root {
    RootOperation::Query => DirectiveLocation::Query,
    RootOperation::Mutation => DirectiveLocation::Mutation,
    RootOperation::Subscription => DirectiveLocation::Subscription,
  }
}

/// Returns an operation's name node, when it has one.
fn operation_name<S>(document: &ExecutableDocument<S>, index: u32) -> Option<&Name<S>> {
  match operation(document, index)? {
    OperationDefinition::Named(named) => named.name(),
    OperationDefinition::Shorthand(_) => None,
  }
}

/// Returns an operation's name bytes, or the empty slice when it is anonymous.
fn operation_name_bytes<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[OperationRow],
  index: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(index as usize)
    .and_then(|row| operation_name(document, row.definition))
    .map_or(&[][..], name_bytes)
}

/// Returns a fragment's name bytes.
fn fragment_name<'d, S>(
  document: &'d ExecutableDocument<S>,
  rows: &[FragmentRow],
  ordinal: u32,
) -> &'d [u8]
where
  S: AsRef<[u8]>,
{
  rows
    .get(ordinal as usize)
    .and_then(|row| fragment(document, row.definition))
    .map_or(&[][..], |fragment| name_bytes(fragment.name()))
}

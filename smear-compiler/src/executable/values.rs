//! Arguments, directives, value literals and variable usages — draft 5.4, 5.6, 5.7 and the parts
//! of 5.8 that fire at a usage site.
//!
//! Every rule here is written once and instantiated twice, over the constant and non-constant
//! value families ([`ValueLike`]). The value walk is iterative for the same reason the selection
//! walk is: a literal's nesting is chosen by whoever wrote the document.

use core::{cmp::Ordering, ops::ControlFlow};

use tokora::{SimpleSpan, span::AsSpan};

use smear_parser::graphql::ast::VariableValue;

use super::{
  Diagnostic, Ledger, Meter, Rule, TypeId, Validator,
  nodes::{ArgumentLike, DirectiveLike, ObjectFieldLike, ValueLike, name_bytes},
  probe_partition, sort_metered, units,
};
use crate::{
  diagnostic::Context,
  schema::{DirectiveLocation, PackedType, Range32, Sym, TypeKind},
  scratch::{
    BYTES_PER_UNIT, Compared, NONE, ValueFrame, ValueLevel, count_units, probe_cmp, scan_eq,
    set_bit,
  },
};

/// The position a value sits in, as the two bits draft 5.8.5 asks about.
///
/// `IsVariableUsageAllowed` needs to know whether the *location* — the `Argument` or `ObjectField`
/// the value was written for — declares a default, and whether the enclosing object literal is a
/// OneOf input object. Neither is derivable from the value or from its expected type, so both are
/// carried down the walk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ValueLocation(u8);

impl ValueLocation {
  /// A position with neither property: a list entry, or an argument with no schema default.
  pub(crate) const PLAIN: Self = Self(0);
  /// The argument or input field this value was written for declares a default value.
  pub(crate) const HAS_DEFAULT: u8 = 1 << 0;
  /// The value is a field of a OneOf input object literal, so its position is non-null whatever
  /// the field's declared nullability says.
  pub(crate) const ONE_OF: u8 = 1 << 1;

  #[inline]
  pub(crate) const fn from_bits(bits: u8) -> Self {
    Self(bits)
  }

  #[inline]
  pub(crate) const fn with_default(self, present: bool) -> Self {
    if present {
      Self(self.0 | Self::HAS_DEFAULT)
    } else {
      Self(self.0 & !Self::HAS_DEFAULT)
    }
  }

  #[inline]
  pub(crate) const fn has_default(self) -> bool {
    self.0 & Self::HAS_DEFAULT != 0
  }

  #[inline]
  pub(crate) const fn is_one_of(self) -> bool {
    self.0 & Self::ONE_OF != 0
  }
}

impl<'d, S, K> Validator<'_, 'd, S, K>
where
  S: AsRef<[u8]> + Clone,
  K: super::Sink<S>,
{
  // -- directives -----------------------------------------------------------------------------

  /// Draft 5.7.1, 5.7.2 and 5.7.3 over one directive list, plus 5.4 and 5.6 over its arguments.
  pub(super) fn check_directives<D>(
    &mut self,
    directives: &'d [D],
    location: DirectiveLocation,
    check: bool,
  ) -> ControlFlow<()>
  where
    D: DirectiveLike<S>,
  {
    // Asked before anything is charged or read, over **every** rule this list can reach: 5.7's
    // own three, the argument rules over each directive's arguments, the value rules under those,
    // and the variable usages inside them. The prepayment below was unconditional, so an empty
    // rule set — or one asking only for 5.3.1 — could be handed a budget refusal for a long
    // directive spelling that no enabled rule was ever going to look at.
    // Per consumer, not per family. 5.7.1 and 5.7.2 read a directive's *definition* at any length;
    // 5.7.3 compares the spellings a request wrote against each other and reads no definition at
    // all — and compares nothing on a list of fewer than two, which is the `n <= 1` shape one
    // dimension over from the loop inventory. `check_arguments` below makes the same distinction
    // for itself, so a rule set holding only 5.4.2 still gets 5.4.2 through the descent-only arm.
    let local = check
      && (self.reads_argument_positions
        || self.on(Rule::DirectivesAreDefined)
        || self.on(Rule::DirectivesAreInValidLocations)
        || (self.on(Rule::DirectivesAreUniquePerLocation) && directives.len() > 1));
    if !self.reaches_directives(check, D::HAS_VARIABLES) {
      return ControlFlow::Continue(());
    }

    // Descent without resolution. Reached when the only reason to be here is to find variable
    // leaves for draft 5.8.3 or 5.8.4, which read nothing above a leaf — so no directive name is
    // charged, hashed or looked up on the way past it. See `Validator::resolves_positions`.
    //
    // **One unit per directive, in front of the walk past them.** The seventh round removed the
    // *name* charge here, and that was right: nothing on this path reads a spelling, so pricing one
    // is a refusal for bytes nobody looks at. What went with it was the charge on the **iteration**,
    // which was never part of that finding. A directive with no arguments costs `O(1)` here and the
    // list length is the document's, so `O` operations over one fragment's `D` bare directives ran
    // `Θ(O · D)` of it while the ledger saw only the surrounding constant — and the repeat is
    // *correct*, because `collects_usages` is exactly what puts this walk on this path, so the work
    // is real and nothing priced it. al8n/smear#198's eighteenth round.
    //
    // Not an over-charge, and round 17's lesson does not reach it: `Θ(O · D)` of work charged
    // `Θ(O · D)` units is honest. An empty list charges nothing, which is the `n = 0` half.
    if !self.resolves_positions(local) {
      if let Some(first) = directives.first() {
        self.spend(
          count_units(directives.len()),
          *first.directive_name().as_span(),
        )?;
      }
      for directive in directives {
        self.check_arguments(
          directive.directive_arguments(),
          None,
          None,
          check,
          *directive.directive_name().as_span(),
        )?;
      }
      return ControlFlow::Continue(());
    }

    // Prepaid, and the position matters more than the amount. `check_directive_uniqueness` below
    // **sorts** these names — `O(N log N)` comparisons over bytes the client chose — and the charge
    // used to sit in the loop *after* it, so every one of those comparisons happened before a
    // refusal was possible. A charge behind the work it prices is not a bound on that work.
    self.spend_names(directives.iter().map(D::directive_name))?;

    // 5.7.3 — only definitions the schema knows can be known non-repeatable, so an undefined
    // directive written twice is 5.7.1's business twice over and not also this rule's.
    if check && self.on(Rule::DirectivesAreUniquePerLocation) {
      self.check_directive_uniqueness(directives)?;
    }

    for directive in directives {
      let name = directive.directive_name();
      let definition = self
        .schema
        .sym(name_bytes(name))
        .and_then(|sym| self.schema.directive(sym))
        .copied();

      let Some(definition) = definition else {
        if check && self.on(Rule::DirectivesAreDefined) {
          self.report_name(Rule::DirectivesAreDefined, name, Context::None)?;
        }
        // The arguments still hold variable usages, and draft 5.8.4 must not call a variable
        // unused because the directive carrying it was misspelled.
        self.check_arguments(
          directive.directive_arguments(),
          None,
          None,
          check,
          *name.as_span(),
        )?;
        continue;
      };

      if check
        && self.on(Rule::DirectivesAreInValidLocations)
        && !definition.locations().contains(location)
      {
        self.report_name(
          Rule::DirectivesAreInValidLocations,
          name,
          Context::Location(location),
        )?;
      }

      self.check_arguments(
        directive.directive_arguments(),
        Some(definition.args()),
        Some(definition.name()),
        check,
        *name.as_span(),
      )?;
    }
    ControlFlow::Continue(())
  }

  fn check_directive_uniqueness<D>(&mut self, directives: &'d [D]) -> ControlFlow<()>
  where
    D: DirectiveLike<S>,
  {
    let base = self.scratch.keys.len();
    for (index, directive) in directives.iter().enumerate() {
      let sym = self.schema.sym(name_bytes(directive.directive_name()));
      // A repeatable directive is allowed as many times as it likes, so it never enters the scan.
      let repeatable = sym
        .and_then(|sym| self.schema.directive(sym))
        .is_none_or(|definition| definition.is_repeatable());
      if !repeatable {
        self.scratch.keys.push(index as u32);
      }
    }
    // Uncharged, for 5.4.2's reason: `check_directives`' `spend_names` has already paid one whole
    // pass over every one of these spellings, for the `Schema::sym` loop that hashes every one of
    // them — the loop directly above this one is that same hash, per directive, with no early exit
    // before it.
    sort_keys(&mut self.scratch.keys[base..], |a, b| {
      name_bytes(directives[a as usize].directive_name())
        .cmp(name_bytes(directives[b as usize].directive_name()))
    });
    let mut slot = base + 1;
    while slot < self.scratch.keys.len() {
      let previous = self.scratch.keys[slot - 1];
      let current = self.scratch.keys[slot];
      let earlier = previous.min(current);
      let later = previous.max(current);
      if name_bytes(directives[earlier as usize].directive_name())
        == name_bytes(directives[later as usize].directive_name())
      {
        let repeat = &directives[later as usize];
        let subject = self.subject_v(repeat.directive_name())?;
        let diagnostic = Diagnostic::new(
          Rule::DirectivesAreUniquePerLocation,
          repeat.directive_span(),
        )
        .subject(subject)
        .related(directives[earlier as usize].directive_span());
        self.emit(diagnostic)?;
      }
      slot += 1;
    }
    self.scratch.keys.truncate(base);
    ControlFlow::Continue(())
  }

  // -- arguments ------------------------------------------------------------------------------

  /// Draft 5.4.1, 5.4.2 and 5.4.3 over one argument set, and the value rules over each value.
  ///
  /// `definitions` is `None` when the field or directive itself did not resolve: the argument
  /// names cannot be checked against anything, but their values are still walked so that a
  /// variable used inside one is not later reported unused.
  pub(super) fn check_arguments<A>(
    &mut self,
    arguments: &'d [A],
    definitions: Option<Range32>,
    owner: Option<Sym>,
    check: bool,
    blame: SimpleSpan,
  ) -> ControlFlow<()>
  where
    A: ArgumentLike<S>,
  {
    // [`Validator::reaches_directives`] one level in, and for the same reason: the prepayment
    // below is unconditional, so without this an argument list would charge for its spellings
    // whether or not any rule that reads an argument, a value or a variable usage is enabled.
    // The same split one level in: 5.4.1 and 5.4.3 read an argument's declared type, 5.4.2 reads
    // only the spellings beside it — and reads nothing at all when there are fewer than two of
    // them, which is the case `only(ArgumentUniqueness)` used to charge every name for.
    let local = check
      && (self.reads_argument_positions
        || (self.on(Rule::ArgumentUniqueness) && arguments.len() > 1));
    if !self.reaches_arguments(check, A::HAS_VARIABLES) {
      return ControlFlow::Continue(());
    }

    // Descent without resolution, for the reason `check_directives` states — including the charge on
    // the iteration, which is this list's own length and priced here for the same reason and by the
    // same unit. `None` as the expected type is exactly right rather than a shortcut: it is what
    // the position resolves to when nothing resolves it, and `check_variable_usage` already answers
    // 5.8.3 and 5.8.4 without one.
    if !self.resolves_positions(local) {
      if !arguments.is_empty() {
        self.spend(count_units(arguments.len()), blame)?;
      }
      for argument in arguments {
        self.walk_value(argument.argument_value(), None, ValueLocation::PLAIN, check)?;
      }
      return ControlFlow::Continue(());
    }

    // Prepaid ahead of the sort below, for the reason `check_directives` states, and for the
    // per-argument `Schema::sym` in the loop that follows it. 5.4.3's presence half no longer needs
    // it: that scan charges each spelling as it resolves it, which is what let the sizing fold —
    // an `O(arguments)` walk taken to compute a charge — stop existing.
    self.spend_names(arguments.iter().map(A::argument_name))?;

    // 5.4.2 — one argument set, one value per name.
    if check && self.on(Rule::ArgumentUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..arguments.len() {
        self.scratch.keys.push(index as u32);
      }
      // Uncharged, and it is the one sort here that may be: `spend_names` above has already paid
      // one whole pass over every one of these spellings, for the `Schema::sym` loop below that
      // hashes every one of them end to end. The sort reads no name further than that pass did,
      // and its `N log N` factor is the bounded constant this module's header names.
      sort_keys(&mut self.scratch.keys[base..], |a, b| {
        name_bytes(arguments[a as usize].argument_name())
          .cmp(name_bytes(arguments[b as usize].argument_name()))
      });
      let mut slot = base + 1;
      while slot < self.scratch.keys.len() {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        if name_bytes(arguments[earlier as usize].argument_name())
          == name_bytes(arguments[later as usize].argument_name())
        {
          let repeat = &arguments[later as usize];
          let subject = self.subject_v(repeat.argument_name())?;
          let diagnostic = Diagnostic::new(Rule::ArgumentUniqueness, repeat.argument_span())
            .subject(subject)
            .related(arguments[earlier as usize].argument_span());
          self.emit(diagnostic)?;
        }
        slot += 1;
      }
      self.scratch.keys.truncate(base);
    }

    for argument in arguments {
      let name = argument.argument_name();
      let sym = self.schema.sym(name_bytes(name));
      let definition = match (definitions, sym) {
        (Some(group), Some(sym)) => self.schema.input(group, sym).copied(),
        _ => None,
      };

      if definitions.is_some() && definition.is_none() {
        if check && self.on(Rule::ArgumentNames) {
          let context = match owner {
            Some(owner) => Context::Type(owner),
            None => Context::None,
          };
          self.report_name(Rule::ArgumentNames, name, context)?;
        }
        // Unknown argument: no expected type, but the value may still name variables.
        self.walk_value(argument.argument_value(), None, ValueLocation::PLAIN, check)?;
        continue;
      }

      // 5.4.3's explicit-`null` half. Reported here rather than left to 5.6.1 so that one
      // mistake produces one diagnostic, and it is the rule that names the obligation.
      if let Some(definition) = definition
        && definition.is_required()
        && argument.argument_value().is_null()
      {
        if check && self.on(Rule::RequiredArguments) {
          let context = match owner {
            Some(owner) => Context::Member {
              owner,
              name: definition.name(),
            },
            None => Context::Expected(definition.ty()),
          };
          self.report_name(Rule::RequiredArguments, name, context)?;
        }
        continue;
      }

      let location = ValueLocation::PLAIN
        .with_default(definition.is_some_and(|d| d.default_kind().is_present()));
      self.walk_value(
        argument.argument_value(),
        definition.map(|definition| definition.ty()),
        location,
        check,
      )?;
    }

    // 5.4.3's presence half.
    //
    // A cross product, and the only one in this file whose factors are not both the document's:
    // every *declared* required argument rescans every *written* one, so a field position costs
    // `declared · written` name resolutions and a document repeating that position pays it again
    // each time. `declared` is the schema's and bounded by it; `written` and the position count
    // are the client's. It is charged per scan, before the scan, for the third factor's sake.
    //
    // "Bounded by it" is a claim about **two** groups, because this function is reached from
    // `check_arguments`'s field caller and from `check_directives` — and for a round it held for
    // only one of them: `smear_schema::MAX_FIELD_ARGUMENTS` was enforced at a single site, the
    // field path, so a directive definition's declared list was unbounded here and the charge
    // below was the only thing standing in front of it. A charge is not a bound, it is a bill,
    // and this one bills the request for a width the deployment wrote.
    // `smear_schema::MAX_DIRECTIVE_ARGUMENTS` closes the other group at the same place and for
    // the same reason. al8n/smear#198.
    if let Some(group) = definitions
      && check
      && self.on(Rule::RequiredArguments)
    {
      // One per declared entry the scan **examines**, before it examines any.
      //
      // The group is the schema's and its size is not an input; how many times a request can reach
      // it is. This runs once per position that selects the field, so `positions × group` is a
      // product with one caller-controlled factor — and the per-required charge below sees none of
      // it: an optional entry `continue`s before spending, and with no written arguments the
      // required ones spend zero. "Not the caller's population" was true about the size and silent
      // about the count. al8n/smear#198.
      let declared = self.schema.inputs(group).len() as u32;
      self.spend(declared, blame)?;
      for index in 0..self.schema.inputs(group).len() {
        let definition = self.schema.inputs(group)[index];
        if !definition.is_required() {
          continue;
        }
        // Per written argument, before resolving it, because `any` stops at the first match — a
        // required argument that *is* supplied, which is the ordinary case, usually stops on the
        // first or second. Charging the whole list per declared entry billed the common case for
        // the worst one. Found by the taken-branch audit.
        let schema = self.schema;
        let mut supplied = false;
        for argument in arguments {
          let spelling = name_bytes(argument.argument_name());
          self.spend(units(spelling.len()), blame)?;
          if schema.sym(spelling) == Some(definition.name()) {
            supplied = true;
            break;
          }
        }
        if !supplied {
          let context = match owner {
            Some(owner) => Context::Member {
              owner,
              name: definition.name(),
            },
            None => Context::Expected(definition.ty()),
          };
          let diagnostic = Diagnostic::new(Rule::RequiredArguments, blame).context(context);
          self.emit(diagnostic)?;
        }
      }
    }

    ControlFlow::Continue(())
  }

  // -- values ---------------------------------------------------------------------------------

  /// Walks one value literal against the type expected in its position.
  ///
  /// `expected` is `None` when the position did not resolve — an argument of an unknown field, for
  /// instance. The walk then does nothing but collect variable usages, which is what keeps draft
  /// 5.8.4 from reporting a variable unused because of a mistake somewhere else.
  pub(super) fn walk_value<V>(
    &mut self,
    root: &'d V,
    expected: Option<PackedType>,
    location: ValueLocation,
    check: bool,
  ) -> ControlFlow<()>
  where
    V: ValueLike<S>,
  {
    // Two demands, asked separately, because they are answered at different rates.
    //
    // Draft 5.6's literal rules are a property of a **definition** and fire under `Frame::CHECK`,
    // which a definition carries exactly once however many operations reach it. The variable rules
    // are a property of an **operation** and must be collected on every visit. One boolean for
    // both meant that with only 5.6.1 enabled, the second and later operations to reach a shared
    // fragment still descended and charged its literals to produce nothing at all — `O(operations
    // × literal size)` off `O(operations + literal size)` of input, and a `validation_work`
    // exhausted on work that could not have had an effect.
    if !self.walks_values(check, V::HAS_VARIABLES) {
      return ControlFlow::Continue(());
    }
    let base = self.scratch.values.len();
    if let Some(frame) = self.visit_value(root, expected, location, check)? {
      self.scratch.values.push(frame);
    }

    while self.scratch.values.len() > base {
      let depth = self.scratch.values.len();
      // Charged in front of `resolve`, which descends the frame chain **from the root** and
      // therefore costs the current depth — on every iteration, including the ones that only pop.
      // One unit per iteration priced `O(nodes)` for `O(nodes · depth)` of work, so a literal
      // nested `D` deep did `O(D²)` on `O(D)` of units. This charge subsumes the per-literal unit
      // it replaces, since the depth is never less than one.
      self.spend((depth - base) as u32, root.value_span())?;
      let frame = self.scratch.values[depth - 1];
      let Some(level) = resolve(root, &self.scratch.values[base..depth]) else {
        self.scratch.values.pop();
        continue;
      };
      let count = match frame.level {
        ValueLevel::List => level.as_list().map_or(0, <[V]>::len),
        ValueLevel::Object => level.as_object().map_or(0, <[V::Field]>::len),
      };
      if frame.cursor as usize >= count {
        self.scratch.values.pop();
        continue;
      }
      self.scratch.values[depth - 1].cursor += 1;
      let index = frame.cursor;

      let next = match frame.level {
        ValueLevel::List => {
          let Some(entry) = level.as_list().and_then(|list| list.get(index as usize)) else {
            continue;
          };
          // A list entry is not an argument or an input field, so it has no location default.
          self.visit_value(entry, frame.expected, ValueLocation::PLAIN, check)?
        }
        ValueLevel::Object => {
          let Some(field) = level
            .as_object()
            .and_then(|fields| fields.get(index as usize))
          else {
            continue;
          };
          let name = field.field_name();
          let definition = if frame.object == NONE {
            // An unknown position resolves nothing, so there is nothing here to charge for. The
            // charge sat above this branch and paid for a lookup only the other one makes.
            None
          } else {
            self.spend_name(name)?;
            let object = TypeId::new(frame.object);
            self
              .schema
              .sym(name_bytes(name))
              .and_then(|sym| self.schema.input_field(object, sym))
              .copied()
          };
          if definition.is_none() && frame.object != NONE {
            // 5.6.2 — the input object type is known and does not define this field.
            if check && self.on(Rule::InputObjectFieldNames) {
              let context = Context::Type(self.schema.type_def(TypeId::new(frame.object)).name());
              self.report_name(Rule::InputObjectFieldNames, name, context)?;
            }
            self.visit_value(field.field_value(), None, ValueLocation::PLAIN, check)?
          } else {
            // 5.6.4's explicit-`null` half, for the same reason 5.4.3 keeps its own.
            if let Some(definition) = definition
              && definition.is_required()
              && field.field_value().is_null()
            {
              if check && self.on(Rule::InputObjectRequiredFields) {
                let context = Context::Member {
                  owner: self.schema.type_def(TypeId::new(frame.object)).name(),
                  name: definition.name(),
                };
                self.report_name(Rule::InputObjectRequiredFields, name, context)?;
              }
              continue;
            }
            let one_of = if frame.flags & ValueLocation::ONE_OF != 0 {
              ValueLocation::ONE_OF
            } else {
              0
            };
            let location = ValueLocation::from_bits(one_of)
              .with_default(definition.is_some_and(|d| d.default_kind().is_present()));
            self.visit_value(
              field.field_value(),
              definition.map(|definition| definition.ty()),
              location,
              check,
            )?
          }
        }
      };

      if let Some(mut next) = next {
        next.child = index;
        self.scratch.values.push(next);
      }
    }

    self.scratch.values.truncate(base);
    ControlFlow::Continue(())
  }

  /// The level to descend into for a container literal sitting in a position **no rule can type**.
  ///
  /// A custom scalar accepts every literal, so `withJson(payload: [{ a: 1, a: 1 }])` is a legal
  /// thing to write and the object inside it is still an object the document wrote. Draft 5.6.3 is
  /// over *every* input-object value in the document and draft 5.8 is over every variable in it;
  /// neither asks what position the value sits in. The scalar and enum arms answered
  /// `Continue(None)` — no frame, no descent — so nothing under a custom scalar was ever visited.
  ///
  /// Gated on the two readers that exist for such a descent, because there is nothing else down
  /// there to find: with 5.6.3 off and no usage rule, walking into a literal the schema has no
  /// opinion about produces nothing. al8n/smear#198's twenty-second round.
  fn untyped_descent<V>(&self, value: &'d V) -> Option<ValueFrame>
  where
    V: ValueLike<S>,
  {
    if !self.on(Rule::InputObjectFieldUniqueness) && !self.descends_for_usages(V::HAS_VARIABLES) {
      return None;
    }
    if value.as_list().is_some() {
      return Some(level_frame(ValueLevel::List, None, NONE, 0));
    }
    if value.as_object().is_some() {
      return Some(level_frame(ValueLevel::Object, None, NONE, 0));
    }
    None
  }

  /// Checks one value in place, and returns the level to descend into when it is a container.
  fn visit_value<V>(
    &mut self,
    value: &'d V,
    expected: Option<PackedType>,
    location: ValueLocation,
    check: bool,
  ) -> ControlFlow<(), Option<ValueFrame>>
  where
    V: ValueLike<S>,
  {
    if let Some(variable) = value.as_variable() {
      // `Range32` made an unbuilt index safe to search; it did not make searching it worth doing.
      // With no usage rule enabled there is nothing for 5.8.3, 5.8.4 or 5.8.5 to conclude, and
      // `check_variable_usage` would charge the spelling and search an empty range to conclude it.
      if self.collects_usages {
        self.check_variable_usage(variable, expected, location)?;
      }
      return ControlFlow::Continue(None);
    }

    // **Draft 5.6.3, above every dispatch.** It is a syntactic rule — the same field name written
    // twice in one literal — so its subject is the literal, not the position the literal sits in,
    // and asking it here is the only placement that reaches every object a request can write.
    //
    // The twenty-first round put it at two of `visit_value`'s exits instead: the unknown-position
    // arm and the `InputObject` arm. The boundary that justified stopping there was *"an object
    // literal in a position that resolves to a scalar or enum is not descended, because 5.6.1 has
    // already said the literal cannot be there"* — and that sentence is false exactly where it
    // matters. **A custom scalar accepts every literal**, so 5.6.1 says nothing about
    // `withJson(payload: { a: 1, a: 2 })`, nothing descends it, and `RuleSet::ALL` answered `Ok`
    // for a document draft §5.6.3 refuses. A recorded boundary that was never tested.
    // al8n/smear#198's twenty-second round.
    //
    // Above the dispatch also means 5.6.3 stops being a reader of a resolved position at all, which
    // is why it leaves [`reads_value_positions`](super::reads_value_positions). It still belongs to
    // [`checks_values`](super::checks_values), because that predicate answers *is this walk worth
    // making* and this rule needs the walk.
    if check && let Some(fields) = value.as_object() {
      self.check_object_field_uniqueness(fields)?;
    }

    let Some(expected) = expected else {
      // Unknown position: descend to find variable usages. 5.6.3 was already asked above.
      if value.as_list().is_some() {
        return ControlFlow::Continue(Some(level_frame(ValueLevel::List, None, NONE, 0)));
      }
      if value.as_object().is_some() {
        return ControlFlow::Continue(Some(level_frame(ValueLevel::Object, None, NONE, 0)));
      }
      return ControlFlow::Continue(None);
    };

    if value.is_null() {
      if expected.is_non_null() && check && self.on(Rule::ValuesOfCorrectType) {
        self.report_value(value, Context::Expected(expected))?;
      }
      return ControlFlow::Continue(None);
    }

    // Strip the outer non-null, then apply the specification's singleton-to-list coercion: a
    // non-list value in a list position is the one-element list containing it, at any depth.
    let mut expected = expected.nullable();
    while expected.is_list() && value.as_list().is_none() {
      let Some(item) = expected.list_item() else {
        break;
      };
      expected = item.nullable();
    }

    if expected.is_list() {
      let item = expected.list_item();
      return ControlFlow::Continue(Some(level_frame(ValueLevel::List, item, NONE, 0)));
    }

    let base = expected.base_id();
    let definition = *self.schema.type_def(base);
    // Draft 5.6.1 is the only rule the three leaf arms below serve, so it is asked **first** —
    // before the literal is read and before the literal is charged.
    //
    // It used to be asked last, in a `!scalar_accepts(..) && check && self.on(..)` whose
    // short-circuit runs the coercion before the guard, and in an enum arm that hashed the
    // spelling into a `member` binding the guard then discarded. With the rule off that is a
    // client-chosen name hashed and a client-chosen digit string parsed for a verdict nobody
    // wanted — and once the read is charged, it is a refusal nobody asked for either. `RuleSet`'s
    // own contract says a consumer that wants only the fragment rules does not pay for value
    // coercion; this is where that sentence was false. al8n/smear#198.
    let coerces = check && self.on(Rule::ValuesOfCorrectType);
    match definition.kind() {
      TypeKind::InputObject => {
        let Some(fields) = value.as_object() else {
          if check && self.on(Rule::ValuesOfCorrectType) {
            self.report_value(value, Context::Expected(expected))?;
          }
          return ControlFlow::Continue(None);
        };
        if check {
          self.check_input_object(value, fields, base, expected)?;
        }
        let flags = if definition.is_one_of() {
          ValueLocation::ONE_OF
        } else {
          0
        };
        ControlFlow::Continue(Some(level_frame(
          ValueLevel::Object,
          None,
          base.get(),
          flags,
        )))
      }
      TypeKind::Enum => {
        if coerces {
          // Charged on the arm that reads, not above the match. A literal in an enum position is
          // not necessarily an enum literal — `{ f(e: 12345…) }` is a legal thing to write and a
          // rejected thing to write — and `as_enum` answers `None` for it without looking at a
          // byte, so the spelling that used to be billed there was never read.
          let member = match value.as_enum() {
            Some(literal) => {
              let spelling = literal.source().as_ref();
              self.spend(units(spelling.len()), value.value_span())?;
              self
                .schema
                .sym(spelling)
                .is_some_and(|sym| self.schema.has_enum_value(base, sym))
            }
            None => false,
          };
          if !member {
            self.report_value(value, Context::Expected(expected))?;
          }
        }
        ControlFlow::Continue(self.untyped_descent(value))
      }
      TypeKind::Scalar => {
        if coerces && !self.scalar_accepts(value, definition.name(), value.value_span())? {
          self.report_value(value, Context::Expected(expected))?;
        }
        ControlFlow::Continue(self.untyped_descent(value))
      }
      // An object, interface or union in an input position. `Schema::build` refuses an argument
      // or input field declared that way, so this is unreachable from a built schema; refusing
      // rather than accepting keeps it that way if the schema ever gains another door.
      TypeKind::Object | TypeKind::Interface | TypeKind::Union => {
        if coerces {
          self.report_value(value, Context::Expected(expected))?;
        }
        ControlFlow::Continue(self.untyped_descent(value))
      }
    }
  }

  /// Draft 5.6.3, over an object literal's own field spellings.
  ///
  /// # It needs no resolved position, and it used to be behind one
  ///
  /// 5.6.3 compares the names a request **wrote** against each other. It never asks the schema what
  /// they mean, so an object literal at a position nothing resolved still has the property the rule
  /// is about — and the walk reached exactly such literals through `visit_value`'s unknown-position
  /// arm, descended them for variable usages, and never asked. A rule silently not firing, which is
  /// the direction a budget test cannot see at all.
  ///
  /// It is the twin of a carve-out made one round earlier and not enumerated:
  /// [`reads_argument_positions`](super::reads_argument_positions) excludes
  /// [`Rule::ArgumentUniqueness`] on precisely this reasoning, and
  /// [`Rule::InputObjectFieldUniqueness`] is the same sentence about object fields.
  /// al8n/smear#198's twenty-first round.
  ///
  /// The charge admits the **sort**, and nothing else any more: 5.6.4's scan charges each spelling
  /// as it resolves it. A sort of one field compares nothing — the duplicate scan that reads its
  /// output starts at `base + 1` — so the charge is paired with a length, which is the same
  /// `n <= 1` companion every compare-what-was-written rule carries.
  ///
  /// **One unit per field and not one spelling per field.** A whole spelling per name stood here,
  /// and the only thing that reads these names is `[u8]::cmp` inside the sort and `[u8]::eq`
  /// inside the scan — both of which stop at the first byte that disagrees. Unlike 5.4.2's and
  /// 5.7.3's prepayments, which stand in front of a `Schema::sym` loop that hashes every name end
  /// to end, this one had no such reader below it: `walk_value` resolves an object field only when
  /// the position resolved to a type, and this rule fires either way. So the depth is taken inside
  /// the comparisons, in `Meter`. al8n/smear#198's twenty-third round.
  fn check_object_field_uniqueness<F>(&mut self, fields: &'d [F]) -> ControlFlow<()>
  where
    F: ObjectFieldLike<S>,
  {
    if !self.on(Rule::InputObjectFieldUniqueness) || fields.len() < 2 {
      return ControlFlow::Continue(());
    }
    // **One slot appended behind its own field's accepted unit, and not `fields.len()` of them in
    // front of the first.** A `resize(fields.len(), 0)` stood here, and it is the defect this whole
    // branch exists to remove, reappearing in the machinery that removes it: the walk reaches an
    // object literal through `visit_value`'s unknown-position arm before the value walk has charged
    // it, so a literal writing `F` fields grew and kept `4F` bytes of meter with the ledger holding
    // no room for the first field's unit. Measured under a ceiling that reaches the literal and
    // cannot pay that unit: 4,224 bytes at 1,000 fields, 16,224 at 4,000, 64,224 at 16,000 and
    // 256,224 at 64,000. `clear` first — `O(1)`, no drop glue, no allocation, and it is what leaves
    // the table empty on a refusal rather than the previous literal's width — then one push per
    // charge. `index_fragments` is the shape; three sites drifted from it.
    // al8n/smear#198's twenty-fourth round.
    self.scratch.paid.clear();
    let mut deepest = 0u32;
    for field in fields {
      let name = field.field_name();
      self.spend(1, *name.as_span())?;
      self.scratch.paid.push(1);
      deepest = deepest.max(units(name_bytes(name).len()));
    }

    let base = self.scratch.keys.len();
    for index in 0..fields.len() {
      self.scratch.keys.push(index as u32);
    }
    let refused = {
      let scratch = &mut *self.scratch;
      let (ledger, refused) = sort_metered(
        &mut scratch.keys[base..],
        &mut scratch.paid,
        self.left,
        deepest,
        |index| name_bytes(fields[index as usize].field_name()),
      );
      self.left = ledger;
      refused
    };
    if refused {
      return self.refuse(*fields[0].field_name().as_span());
    }
    let mut slot = base + 1;
    while slot < self.scratch.keys.len() {
      let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
      let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
      let scratch = &mut *self.scratch;
      let mut meter = Meter::new(&mut scratch.paid, self.left, deepest);
      let compared = meter.eq(
        earlier as usize,
        name_bytes(fields[earlier as usize].field_name()),
        later as usize,
        name_bytes(fields[later as usize].field_name()),
      );
      let (ledger, _) = meter.finish();
      self.left = ledger;
      if compared == Compared::Refused {
        return self.refuse(*fields[later as usize].field_name().as_span());
      }
      if compared == Compared::Equal {
        let repeat = &fields[later as usize];
        let subject = self.subject_v(repeat.field_name())?;
        let diagnostic = Diagnostic::new(Rule::InputObjectFieldUniqueness, repeat.field_span())
          .subject(subject)
          .related(fields[earlier as usize].field_span());
        self.emit(diagnostic)?;
      }
      slot += 1;
    }
    self.scratch.keys.truncate(base);
    ControlFlow::Continue(())
  }

  /// Draft 5.6.3, 5.6.4 and the OneOf literal rules, at an object level.
  fn check_input_object<V>(
    &mut self,
    value: &'d V,
    fields: &'d [V::Field],
    object: TypeId,
    expected: PackedType,
  ) -> ControlFlow<()>
  where
    V: ValueLike<S>,
  {
    let definition = *self.schema.type_def(object);
    if definition.is_one_of() {
      // 5.6.1's OneOf half: exactly one field, and it is not `null`.
      if self.on(Rule::ValuesOfCorrectType) {
        if fields.len() != 1 {
          self.report_value(value, Context::Expected(expected))?;
        } else if fields[0].field_value().is_null() {
          // `check_input_object`'s prepayment is gated on 5.6.3 and 5.6.4, and this is 5.6.1's
          // report — so under `only(ValuesOfCorrectType)` the spelling reaches a clone before the
          // descent one level down charges it. The helper is what pays for it.
          let subject = self.subject_v(fields[0].field_name())?;
          let diagnostic = Diagnostic::new(Rule::ValuesOfCorrectType, fields[0].field_span())
            .subject(subject)
            .context(Context::Type(definition.name()));
          self.emit(diagnostic)?;
        }
      }
      // A OneOf input object's fields are all nullable and default-free by construction, so the
      // required-field rule below has nothing to say about one.
      return ControlFlow::Continue(());
    }

    // 5.6.4 — every required field is supplied. The same cross product 5.4.3's presence half is,
    // and charged the same way and for the same reason.
    if self.on(Rule::InputObjectRequiredFields) {
      // The same scan over the schema's population, reached once per literal a request writes.
      // See 5.4.3's presence half for the argument.
      let count = self.schema.input_fields_of(object).len();
      self.spend(count as u32, value.value_span())?;
      for index in 0..count {
        let field_definition = self.schema.input_fields_of(object)[index];
        if !field_definition.is_required() {
          continue;
        }
        // The same short circuit one rule over. See 5.4.3's presence half.
        let schema = self.schema;
        let mut supplied = false;
        for field in fields {
          let spelling = name_bytes(field.field_name());
          self.spend(units(spelling.len()), value.value_span())?;
          if schema.sym(spelling) == Some(field_definition.name()) {
            supplied = true;
            break;
          }
        }
        if !supplied {
          let diagnostic = Diagnostic::new(Rule::InputObjectRequiredFields, value.value_span())
            .context(Context::Member {
              owner: definition.name(),
              name: field_definition.name(),
            });
          self.emit(diagnostic)?;
        }
      }
    }
    ControlFlow::Continue(())
  }

  /// Whether a built-in scalar accepts this literal. Custom scalars accept everything: only the
  /// service knows how to read one, so a validator that guessed would reject valid documents.
  ///
  /// # The other copy
  ///
  /// `validator::schema::literal` answers the same question for `Schema::build`, over the
  /// builder's owned reduction rather than the syntactic AST and against scalar *names* rather
  /// than the interned `Sym`s compared here — which is why the two exist separately, this being
  /// the per-request path. They must never disagree, and nothing in the type system makes that so:
  /// `the_two_coercion_tables_agree` in `tests/validator_rules.rs` asserts it literal by literal,
  /// because an audit that forced the *other* copy's `ID` range arm open found every gate in the
  /// repository still green. An arm added here needs the matching arm there.
  fn scalar_accepts<V>(&mut self, value: &V, name: Sym, blame: SimpleSpan) -> ControlFlow<(), bool>
  where
    V: ValueLike<S>,
  {
    let name = Some(name);
    if name == self.scalars.int {
      // `fits_i32` reads the digits, so this arm pays for them. The `None` arm has already
      // decided on the variant.
      return match value.as_int() {
        Some(int) => match self.spelling(int.source().as_ref(), blame)? {
          Some(digits) => ControlFlow::Continue(fits_i32(digits)),
          None => ControlFlow::Continue(false),
        },
        None => ControlFlow::Continue(false),
      };
    }
    if name == self.scalars.float {
      // The coercion rules let an Int literal stand for a Float — and that arm reads **nothing**:
      // being an Int is the whole of the answer. Only `is_finite` reads a spelling.
      return match (value.as_float(), value.as_int()) {
        (Some(float), _) => match self.spelling(float.source().as_ref(), blame)? {
          Some(digits) => ControlFlow::Continue(is_finite(digits)),
          None => ControlFlow::Continue(false),
        },
        (None, Some(_)) => ControlFlow::Continue(true),
        (None, None) => ControlFlow::Continue(false),
      };
    }
    if name == self.scalars.string {
      return ControlFlow::Continue(value.as_string().is_some());
    }
    if name == self.scalars.boolean {
      return ControlFlow::Continue(value.as_boolean().is_some());
    }
    if name == self.scalars.id {
      // ID accepts both spellings an identifier is written with, and the `||` short-circuits: a
      // string answers without the integer arm being reached, so the digits are read — and paid
      // for — only when they are the thing being tested.
      if value.as_string().is_some() {
        return ControlFlow::Continue(true);
      }
      return match value.as_int() {
        Some(int) => match self.spelling(int.source().as_ref(), blame)? {
          Some(digits) => ControlFlow::Continue(fits_id(digits)),
          None => ControlFlow::Continue(false),
        },
        None => ControlFlow::Continue(false),
      };
    }
    // A custom scalar accepts everything **without inspecting anything**: only the service knows
    // how to read one. There is no spelling read on this path and there is no charge for one, which
    // is the whole of al8n/smear#198's tenth-round shape — the branch a document actually takes is
    // the branch that decides what it owes.
    ControlFlow::Continue(true)
  }

  /// A numeric literal's spelling as text, charging a unit in front of each run of bytes it reads.
  ///
  /// `None` is "no `i32` and no `f64` can be named by this", which is what a byte outside ASCII
  /// establishes: [`str::parse`] accepts digits, a sign, a point, an exponent marker and the three
  /// non-finite spellings, and every one of those is ASCII. So scanning for that byte decides the
  /// same question [`core::str::from_utf8`] was being asked here — and, unlike `from_utf8`, it can
  /// be stopped and charged a run at a time.
  ///
  /// That is the whole repair. `units(spelling.len())` stood in front of `from_utf8`, which
  /// returns at the **first** byte outside the encoding: a spelling whose first byte is not one
  /// costs a load and was charged for its whole length. An all-ASCII spelling — every `Int` and
  /// `Float` token any lexer in this workspace produces, and the only kind that can be accepted —
  /// reads to the end and pays exactly `units(len)`, which is what it paid before.
  /// al8n/smear#198's twenty-third round.
  fn spelling<'a>(
    &mut self,
    spelling: &'a [u8],
    blame: SimpleSpan,
  ) -> ControlFlow<(), Option<&'a str>> {
    let mut ledger = self.left;
    let mut refused = false;
    // The opening unit covers the emptiness test and the first `BYTES_PER_UNIT - 1` bytes, so the
    // running total after `k` bytes is exactly `units(k)` — `scan_eq`'s schedule, one operation
    // over.
    //
    // `charge` is declared inside this block, not the function, so its mutable borrow of `ledger`
    // and `refused` ends where the block does — structurally, at the closing brace — rather than
    // through a `drop(charge)` call. The closure has no `Drop` impl, so `drop()` was never a
    // destructor here; it was only ever a way to say "end this borrow now", which the block says
    // on its own.
    let readable = {
      let mut charge = || match ledger.take(1) {
        Some(left) => {
          ledger = left;
          true
        }
        None => {
          ledger = Ledger::Left(0);
          refused = true;
          false
        }
      };
      if charge() {
        let mut budgeted = BYTES_PER_UNIT - 1;
        let mut read = 0usize;
        loop {
          if read >= spelling.len() {
            break true;
          }
          if read == budgeted {
            if !charge() {
              break false;
            }
            budgeted += BYTES_PER_UNIT;
          }
          let end = budgeted.min(spelling.len());
          if !spelling[read..end].is_ascii() {
            break false;
          }
          read = end;
        }
      } else {
        false
      }
    };
    self.left = ledger;
    if refused {
      self.refuse(blame)?;
    }
    // ASCII, so the conversion cannot fail; written as the total form for the reason this crate's
    // other unreachable conversions are.
    ControlFlow::Continue(
      readable
        .then(|| core::str::from_utf8(spelling).ok())
        .flatten(),
    )
  }

  fn report_value<V>(&mut self, value: &V, context: Context) -> ControlFlow<()>
  where
    V: ValueLike<S>,
  {
    let diagnostic =
      Diagnostic::new(Rule::ValuesOfCorrectType, value.value_span()).context(context);
    self.emit(diagnostic)
  }

  // -- variable usages ------------------------------------------------------------------------

  /// Draft 5.8.3 and 5.8.5 at one usage, and the mark 5.8.4 reads afterwards.
  fn check_variable_usage(
    &mut self,
    variable: &'d VariableValue<S>,
    expected: Option<PackedType>,
    location: ValueLocation,
  ) -> ControlFlow<()> {
    if !self.in_operation {
      // A fragment no operation reaches has no variable scope, and the specification scopes both
      // rules to an operation. apollo-compiler skips such fragments entirely; this one still
      // validates their structure, but there is nothing to check a variable against.
      return ControlFlow::Continue(());
    }
    let name = variable.name();
    let bytes = name_bytes(name);

    // The operation's variable-name index, built once by `check_variable_definitions`: ordinals
    // sorted by name, ties broken on the ordinal. This used to be a scan over *every* definition
    // at *every* usage — `U · V` name comparisons, quadratic in an operation's own size and
    // outside any ledger.
    //
    // Two partition points rather than a `binary_search`, because what the rules need is the run's
    // **bounds** and not a member of it: a `binary_search` over duplicates lands wherever it
    // likes. Both are `probe_partition` rather than the standard library's, so that a probe with
    // nothing left to spend stops rather than finishing its `log V` comparisons; see that
    // function. The run is contiguous and ordered by ordinal, so `lo` is the lowest-numbered
    // definition of the name — the one the scan's `first.get_or_insert` picked, and the one the
    // type check below reads — and `lo..hi` is every definition of it, which is what gets marked.
    // Every one of them, not only the first: a duplicated variable is 5.8.1's business, and
    // calling the copy "never used" as well would report one mistake twice.
    let variables = self.variables;
    let base = self.variable_index.start() as usize;
    let end = self.variable_index.end() as usize;
    // **Charged for the bytes the searches read, not for the spelling they are over.** A whole
    // pass over this usage's name stood here, gated on the index being non-empty, and the searches
    // below are `<` and `==` — both of which stop at the first byte that disagrees. A usage whose
    // name differs from every declaration at byte zero reads `log V` bytes and was charged for its
    // whole length, so a long enough spelling refused an operation over a search that never left
    // its first byte.
    //
    // The probe's own depth is the high-water: a comparison that reads `L` bytes of a stored name
    // needed this spelling to agree for `L`, so paying for this side once, in front of each run
    // some comparison is about to read, prices both. With no declarations `partition_point` runs
    // zero comparisons and nothing is charged, which is the gate that used to stand here. The
    // report's own copy is charged by `Validator::subject`, wherever it happens.
    // al8n/smear#198's twenty-third round.
    // The opening unit, taken once. A non-empty index invokes its predicate at least once, and
    // unit one is exactly what that comparison's length test and first run of bytes cost.
    if end > base {
      self.spend(1, *name.as_span())?;
    }
    let mut paid = 1u32;
    let mut ledger = self.left;
    let mut refused = false;
    // What a full pass over this spelling costs. A spelling under `BYTES_PER_UNIT` is that one
    // unit, so the searches below can take no further one and run the plain comparison — decided
    // out here rather than inside the predicate, for `sort_metered`'s reason.
    let whole = units(bytes.len());
    /// One unit for this spelling, if the run it pays for has not been paid for already.
    ///
    /// A macro rather than a `let`-bound closure, because the predicates below also *read* `paid`
    /// to take the settled path and a closure holding it mutably would outlive that read.
    macro_rules! charge {
      () => {
        |unit: u32| {
          if unit <= paid {
            return true;
          }
          paid = unit;
          match ledger.take(1) {
            Some(left) => {
              ledger = left;
              true
            }
            None => {
              ledger = Ledger::Left(0);
              refused = true;
              false
            }
          }
        }
      };
    }
    // **The searches abandon themselves at the first charge they cannot pay.** `partition_point`
    // stood here, and it runs its whole `log V` comparisons however the ledger answers — the
    // reading of `scan_cmp` that `probe_partition`'s header corrects. Nothing consumes a refused
    // probe's answer, so there is no ordering to keep total and no reason to finish.
    // al8n/smear#198's twenty-fourth round.
    let (lo, equal) = {
      let index = &self.scratch.keys[base..end];
      let named =
        |slot: usize| name_bytes(variables[index[slot] as usize].node().variable().name());
      // The arm is chosen out here rather than inside the predicate, for `sort_metered`'s reason:
      // with nothing left to charge these are the plain searches — `partition_point` itself, since
      // a search with nothing to charge has nothing to abandon — and a predicate holding the
      // ledger by `&mut` is not one the search can prove loop-invariant.
      if whole <= 1 {
        let lo = index.partition_point(|slot| {
          name_bytes(variables[*slot as usize].node().variable().name()) < bytes
        });
        // One comparison decides existence, which is what 5.8.3 asks and what 5.8.5 needs the
        // ordinal for. The run's *end* is a different question with exactly one reader.
        let equal = index.get(lo).is_some_and(|slot| {
          name_bytes(variables[*slot as usize].node().variable().name()) == bytes
        });
        (lo, equal)
      } else {
        let lo = probe_partition(index.len(), |slot| {
          probe_cmp(named(slot), bytes, charge!()).map(Ordering::is_lt)
        });
        match lo {
          Some(lo) if lo < index.len() => {
            let equal = scan_eq(named(lo), bytes, charge!()) == Compared::Equal;
            (lo, equal)
          }
          Some(lo) => (lo, false),
          // Refused part-way through the search, so `refused` is set and the check below returns.
          None => (0, false),
        }
      }
    };
    self.left = ledger;
    if refused {
      return self.refuse(*name.as_span());
    }
    let found = lo < end - base && equal;

    // **Only draft 5.8.4 reads the `used` bitset**, so only 5.8.4 pays for filling it. The run was
    // walked, and charged, whenever any usage rule was on — and with `V` duplicate declarations
    // against `U` usages that is `O(U · V)` of marking that 5.8.3 and 5.8.5 never look at. A gate
    // named after a rule *family* is not a gate on the family's readers. al8n/smear#198.
    if found && self.marks_usage {
      // The run's end, charged the same way and against the same high-water: the spelling's
      // deepest paid prefix is already `lo`'s, so this search adds units only where it reads
      // further than any comparison above it did — and where nothing is left to charge it is the
      // plain search, for `sort_metered`'s reason.
      //
      // **That is what the sentence above says, and a `let mut paid = 1` stood here contradicting
      // it.** The high-water is what keeps the metered form inside the prepayment it replaced —
      // one pass over this usage's spelling, taken once in front of the whole function — and a
      // second search restarting from unit one charges the spelling twice. A one-declaration,
      // one-usage operation with a four-kilobyte variable name cost 1,015 units against 5.8.3's
      // 513, so a caller whose `validation_work` sat between them had a valid document refused for
      // switching draft 5.8.4 on. `paid`, `ledger` and the refusal are the ones the two searches
      // above already carry. al8n/smear#198's twenty-fourth round.
      let hi = {
        let index = &self.scratch.keys[base..end];
        let named =
          |slot: usize| name_bytes(variables[index[slot] as usize].node().variable().name());
        if whole <= 1 {
          Some(index.partition_point(|slot| {
            name_bytes(variables[*slot as usize].node().variable().name()) <= bytes
          }))
        } else {
          probe_partition(index.len(), |slot| {
            probe_cmp(named(slot), bytes, charge!()).map(Ordering::is_le)
          })
        }
      };
      self.left = ledger;
      let Some(hi) = hi else {
        return self.refuse(*name.as_span());
      };
      self.spend((hi - lo) as u32, *name.as_span())?;
      let scratch = &mut *self.scratch;
      for slot in lo..hi {
        let ordinal = scratch.keys[base + slot];
        set_bit(&mut scratch.used, ordinal);
      }
    }
    let first = found.then(|| self.scratch.keys[base + lo] as usize);

    let Some(index) = first else {
      if self.on(Rule::AllVariableUsesDefined) {
        return self.report_name(Rule::AllVariableUsesDefined, name, Context::None);
      }
      return ControlFlow::Continue(());
    };

    if !self.on(Rule::AllVariableUsagesAreAllowed) {
      return ControlFlow::Continue(());
    }
    let Some(expected) = expected else {
      return ControlFlow::Continue(());
    };
    let definition = self.variables[index].node();
    // `pack_type` hashes the declared type's base name, once per **usage**.
    self.spend_type(definition.ty(), *name.as_span())?;
    let definition = self.variables[index].node();
    let Some(declared) = self.pack_type(definition.ty()) else {
      // The declared type is not in the schema; 5.8.2 has already said so.
      return ControlFlow::Continue(());
    };

    let non_null_position = expected.is_non_null() || location.is_one_of();
    let allowed = if non_null_position && !declared.is_non_null() {
      let variable_default = definition
        .default_value()
        .is_some_and(|default| !default.value().is_null());
      if !variable_default && !location.has_default() {
        false
      } else {
        are_types_compatible(declared, expected.nullable())
      }
    } else {
      are_types_compatible(declared, expected)
    };

    if !allowed {
      return self.report_name(
        Rule::AllVariableUsagesAreAllowed,
        name,
        Context::Usage {
          variable: declared,
          expected,
        },
      );
    }
    ControlFlow::Continue(())
  }
}

// ---------------------------------------------------------------------------------------------
// free helpers
// ---------------------------------------------------------------------------------------------

/// Locates a nested value by descending the frame chain from the root value.
///
/// This is what buys the explicit stack: a frame stores an index, not a pointer, so the stack can
/// live in a [`Scratch`](super::Scratch) that is not generic over the document's source type.
fn resolve<'v, S, V>(root: &'v V, frames: &[ValueFrame]) -> Option<&'v V>
where
  V: ValueLike<S>,
  V::Field: 'v,
{
  let mut value = root;
  for depth in 1..frames.len() {
    let index = frames[depth].child as usize;
    value = match frames[depth - 1].level {
      ValueLevel::List => value.as_list()?.get(index)?,
      ValueLevel::Object => value.as_object()?.get(index)?.field_value(),
    };
  }
  Some(value)
}

/// Builds a level frame; the caller fills in the child index when it pushes.
fn level_frame(
  level: ValueLevel,
  expected: Option<PackedType>,
  object: u32,
  flags: u8,
) -> ValueFrame {
  ValueFrame {
    child: NONE,
    cursor: 0,
    expected,
    level,
    object,
    flags,
  }
}

/// Sorts a duplicate-scan segment by name, breaking ties on the source index so the order is
/// total and the earlier occurrence always sorts first.
///
/// `compare` rather than an accessor, because the two callers price their comparisons differently:
/// 5.4.2's names are read again by a `Schema::sym` loop that hashes every one of them end to end
/// and are prepaid for that pass, while 5.6.3's have no such reader and are charged for the depth
/// each comparison reaches. The tie-break is the sort's own and belongs to neither.
fn sort_keys(keys: &mut [u32], mut compare: impl FnMut(u32, u32) -> core::cmp::Ordering) {
  keys.sort_unstable_by(|a, b| compare(*a, *b).then(a.cmp(b)));
}

/// Draft 5.8.5's `AreTypesCompatible`, as an integer walk over two packed references.
fn are_types_compatible(mut variable: PackedType, mut location: PackedType) -> bool {
  loop {
    if location.is_non_null() {
      if !variable.is_non_null() {
        return false;
      }
      let (Some(next_location), Some(next_variable)) =
        (location.strip_outer(), variable.strip_outer())
      else {
        return false;
      };
      location = next_location;
      variable = next_variable;
      continue;
    }
    if variable.is_non_null() {
      variable = variable.nullable();
      continue;
    }
    if location.is_list() {
      let (Some(item_location), Some(item_variable)) = (location.list_item(), variable.list_item())
      else {
        return false;
      };
      location = item_location;
      variable = item_variable;
      continue;
    }
    if variable.is_list() {
      return false;
    }
    return variable.base_id() == location.base_id();
  }
}

/// Whether an `Int` literal's spelling fits GraphQL's 32-bit signed range.
///
/// Ten characters of digits is the first length that can overflow, so the common case is a length
/// check and nothing else.
fn fits_i32(spelling: &str) -> bool {
  let digits = spelling.strip_prefix('-').unwrap_or(spelling);
  if digits.len() <= 9 {
    return !digits.is_empty();
  }
  spelling.parse::<i32>().is_ok()
}

/// Whether an `Int` literal may stand for an `ID`.
///
/// The specification's `ID` input coercion accepts integer values, and the range that matters is
/// the one an `Int` literal is allowed to carry in the first place.
fn fits_id(spelling: &str) -> bool {
  fits_i32(spelling)
}

/// Whether a `Float` literal's spelling names a finite double.
fn is_finite(spelling: &str) -> bool {
  spelling.parse::<f64>().is_ok_and(f64::is_finite)
}

#[cfg(test)]
mod tests {
  use super::{are_types_compatible, fits_i32, is_finite};
  use smear_schema::{PackedType, Sym, TypeId};

  fn named(id: u32) -> PackedType {
    PackedType::named(Sym::new(id), TypeId::new(id))
  }

  #[test]
  fn compatibility_follows_the_specification_examples() {
    let boolean = named(1);
    let int = named(2);
    let boolean_nn = boolean.push_non_null().unwrap();
    let list = boolean.push_list().unwrap();
    let list_nn = list.push_non_null().unwrap();
    let list_of_nn = boolean_nn.push_list().unwrap();

    assert!(are_types_compatible(boolean, boolean));
    assert!(!are_types_compatible(int, boolean));
    assert!(!are_types_compatible(list, boolean));
    assert!(!are_types_compatible(boolean, boolean_nn));
    assert!(are_types_compatible(boolean_nn, boolean));
    // `[T]!` into `[T]` is fine; `[T]` into `[T]!` and `[T]` into `[T!]` are not.
    assert!(are_types_compatible(list_nn, list));
    assert!(!are_types_compatible(list, list_nn));
    assert!(!are_types_compatible(list, list_of_nn));
    assert!(are_types_compatible(list_of_nn, list));
  }

  #[test]
  fn int_range_uses_the_retained_spelling() {
    assert!(fits_i32("0"));
    assert!(fits_i32("-1"));
    assert!(fits_i32("999999999"));
    assert!(fits_i32("2147483647"));
    assert!(fits_i32("-2147483648"));
    assert!(!fits_i32("2147483648"));
    assert!(!fits_i32("-2147483649"));
    assert!(!fits_i32("99999999999999999999"));
  }

  #[test]
  fn float_literals_must_be_finite() {
    assert!(is_finite("1.0"));
    assert!(is_finite("-1.5e3"));
    assert!(!is_finite("1e400"));
  }
}

//! Arguments, directives, value literals and variable usages — draft 5.4, 5.6, 5.7 and the parts
//! of 5.8 that fire at a usage site.
//!
//! Every rule here is written once and instantiated twice, over the constant and non-constant
//! value families ([`ValueLike`]). The value walk is iterative for the same reason the selection
//! walk is: a literal's nesting is chosen by whoever wrote the document.

use core::ops::ControlFlow;

use tokora::{SimpleSpan, span::AsSpan};

use crate::parser::graphql::ast::VariableValue;

use super::{
  Diagnostic, Rule, TypeId, Validator,
  nodes::{ArgumentLike, DirectiveLike, ObjectFieldLike, ValueLike, name_bytes},
};
use crate::validator::{
  diagnostic::Context,
  schema::{DirectiveLocation, PackedType, Range32, Sym, TypeKind},
  scratch::{NONE, ValueFrame, ValueLevel, set_bit},
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
    sort_keys(&mut self.scratch.keys[base..], |index| {
      name_bytes(directives[index as usize].directive_name())
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
        let diagnostic = Diagnostic::new(
          Rule::DirectivesAreUniquePerLocation,
          repeat.directive_span(),
        )
        .subject(repeat.directive_name().source().clone())
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
    // 5.4.2 — one argument set, one value per name.
    if check && self.on(Rule::ArgumentUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..arguments.len() {
        self.scratch.keys.push(index as u32);
      }
      sort_keys(&mut self.scratch.keys[base..], |index| {
        name_bytes(arguments[index as usize].argument_name())
      });
      let mut slot = base + 1;
      while slot < self.scratch.keys.len() {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        if name_bytes(arguments[earlier as usize].argument_name())
          == name_bytes(arguments[later as usize].argument_name())
        {
          let repeat = &arguments[later as usize];
          let diagnostic = Diagnostic::new(Rule::ArgumentUniqueness, repeat.argument_span())
            .subject(repeat.argument_name().source().clone())
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
    if let Some(group) = definitions
      && check
      && self.on(Rule::RequiredArguments)
    {
      let schema = self.schema;
      for definition in schema.inputs(group) {
        if !definition.is_required() {
          continue;
        }
        let supplied = arguments.iter().any(|argument| {
          schema.sym(name_bytes(argument.argument_name())) == Some(definition.name())
        });
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
    let base = self.scratch.values.len();
    if let Some(frame) = self.visit_value(root, expected, location, check)? {
      self.scratch.values.push(frame);
    }

    while self.scratch.values.len() > base {
      let depth = self.scratch.values.len();
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
            None
          } else {
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
      self.check_variable_usage(variable, expected, location)?;
      return ControlFlow::Continue(None);
    }

    let Some(expected) = expected else {
      // Unknown position: descend only to find variable usages.
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
        let member = value
          .as_enum()
          .and_then(|literal| self.schema.sym(literal.source().as_ref()))
          .is_some_and(|sym| self.schema.has_enum_value(base, sym));
        if !member && check && self.on(Rule::ValuesOfCorrectType) {
          self.report_value(value, Context::Expected(expected))?;
        }
        ControlFlow::Continue(None)
      }
      TypeKind::Scalar => {
        if !self.scalar_accepts(value, definition.name())
          && check
          && self.on(Rule::ValuesOfCorrectType)
        {
          self.report_value(value, Context::Expected(expected))?;
        }
        ControlFlow::Continue(None)
      }
      // An object, interface or union in an input position. `Schema::build` refuses an argument
      // or input field declared that way, so this is unreachable from a built schema; refusing
      // rather than accepting keeps it that way if the schema ever gains another door.
      TypeKind::Object | TypeKind::Interface | TypeKind::Union => {
        if check && self.on(Rule::ValuesOfCorrectType) {
          self.report_value(value, Context::Expected(expected))?;
        }
        ControlFlow::Continue(None)
      }
    }
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
    // 5.6.3 — one value per field name.
    if self.on(Rule::InputObjectFieldUniqueness) {
      let base = self.scratch.keys.len();
      for index in 0..fields.len() {
        self.scratch.keys.push(index as u32);
      }
      sort_keys(&mut self.scratch.keys[base..], |index| {
        name_bytes(fields[index as usize].field_name())
      });
      let mut slot = base + 1;
      while slot < self.scratch.keys.len() {
        let earlier = self.scratch.keys[slot - 1].min(self.scratch.keys[slot]);
        let later = self.scratch.keys[slot - 1].max(self.scratch.keys[slot]);
        if name_bytes(fields[earlier as usize].field_name())
          == name_bytes(fields[later as usize].field_name())
        {
          let repeat = &fields[later as usize];
          let diagnostic = Diagnostic::new(Rule::InputObjectFieldUniqueness, repeat.field_span())
            .subject(repeat.field_name().source().clone())
            .related(fields[earlier as usize].field_span());
          self.emit(diagnostic)?;
        }
        slot += 1;
      }
      self.scratch.keys.truncate(base);
    }

    let definition = *self.schema.type_def(object);
    if definition.is_one_of() {
      // 5.6.1's OneOf half: exactly one field, and it is not `null`.
      if self.on(Rule::ValuesOfCorrectType) {
        if fields.len() != 1 {
          self.report_value(value, Context::Expected(expected))?;
        } else if fields[0].field_value().is_null() {
          let diagnostic = Diagnostic::new(Rule::ValuesOfCorrectType, fields[0].field_span())
            .subject(fields[0].field_name().source().clone())
            .context(Context::Type(definition.name()));
          self.emit(diagnostic)?;
        }
      }
      // A OneOf input object's fields are all nullable and default-free by construction, so the
      // required-field rule below has nothing to say about one.
      return ControlFlow::Continue(());
    }

    // 5.6.4 — every required field is supplied.
    if self.on(Rule::InputObjectRequiredFields) {
      let schema = self.schema;
      for field_definition in schema.input_fields_of(object) {
        if !field_definition.is_required() {
          continue;
        }
        let supplied = fields
          .iter()
          .any(|field| schema.sym(name_bytes(field.field_name())) == Some(field_definition.name()));
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
  fn scalar_accepts<V>(&self, value: &V, name: Sym) -> bool
  where
    V: ValueLike<S>,
  {
    let name = Some(name);
    if name == self.scalars.int {
      return value
        .as_int()
        .is_some_and(|int| fits_i32(int.source().as_ref()));
    }
    if name == self.scalars.float {
      // The coercion rules let an Int literal stand for a Float.
      return match (value.as_float(), value.as_int()) {
        (Some(float), _) => is_finite(float.source().as_ref()),
        (None, Some(_)) => true,
        (None, None) => false,
      };
    }
    if name == self.scalars.string {
      return value.as_string().is_some();
    }
    if name == self.scalars.boolean {
      return value.as_boolean().is_some();
    }
    if name == self.scalars.id {
      // ID accepts both spellings an identifier is written with.
      return value.as_string().is_some()
        || value
          .as_int()
          .is_some_and(|int| fits_id(int.source().as_ref()));
    }
    true
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
    // Every definition of the name is marked used, not only the first. A duplicated variable is
    // 5.8.1's business; calling the copy "never used" as well would report one mistake twice.
    let variables = self.variables;
    let mut first = None;
    for (index, definition) in variables.iter().enumerate() {
      if name_bytes(definition.node().variable().name()) == bytes {
        first.get_or_insert(index);
        set_bit(&mut self.scratch.used, index as u32);
      }
    }
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
fn sort_keys<'a>(keys: &mut [u32], name: impl Fn(u32) -> &'a [u8]) {
  keys.sort_unstable_by(|a, b| name(*a).cmp(name(*b)).then(a.cmp(b)));
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
fn fits_i32(spelling: &[u8]) -> bool {
  let digits = spelling.strip_prefix(b"-").unwrap_or(spelling);
  if digits.len() <= 9 {
    return !digits.is_empty();
  }
  core::str::from_utf8(spelling).is_ok_and(|text| text.parse::<i32>().is_ok())
}

/// Whether an `Int` literal may stand for an `ID`.
///
/// The specification's `ID` input coercion accepts integer values, and the range that matters is
/// the one an `Int` literal is allowed to carry in the first place.
fn fits_id(spelling: &[u8]) -> bool {
  fits_i32(spelling)
}

/// Whether a `Float` literal's spelling names a finite double.
fn is_finite(spelling: &[u8]) -> bool {
  core::str::from_utf8(spelling).is_ok_and(|text| text.parse::<f64>().is_ok_and(f64::is_finite))
}

#[cfg(test)]
mod tests {
  use super::{are_types_compatible, fits_i32, is_finite};
  use crate::validator::schema::{PackedType, Sym, TypeId};

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
    assert!(fits_i32(b"0"));
    assert!(fits_i32(b"-1"));
    assert!(fits_i32(b"999999999"));
    assert!(fits_i32(b"2147483647"));
    assert!(fits_i32(b"-2147483648"));
    assert!(!fits_i32(b"2147483648"));
    assert!(!fits_i32(b"-2147483649"));
    assert!(!fits_i32(b"99999999999999999999"));
  }

  #[test]
  fn float_literals_must_be_finite() {
    assert!(is_finite(b"1.0"));
    assert!(is_finite(b"-1.5e3"));
    assert!(!is_finite(b"1e400"));
  }
}

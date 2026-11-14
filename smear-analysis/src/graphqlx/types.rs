//! The type services: what deriving a type needs, and the budgets the phases after lowering spend
//! on types, owned by one value for the whole analysis.
//!
//! The instantiation phase types every occurrence with them, fills what a use leaves out and judges
//! every written instantiation's bounds; each phase after it borrows the same value. So a type any
//! of them derives is completed as an occurrence's type is, its depth checked against
//! `Limits::max_generic_depth` and its instantiation counted toward `Limits::max_instantiations`,
//! a count kept for the whole analysis; a type's non-null form is made once; and what a budget has
//! spent stays spent. The built-in library's types are neither checked nor counted.
//!
//! Each budget counts one thing: `Limits::max_substitutions` the filling of the type arguments a
//! use leaves out, and `Limits::max_bound_steps` the judging of bounds. The rebuild, which puts an
//! instantiation's arguments in place of its definition's type parameters in a type that names
//! them, charges the budget its caller passes, so each phase pays for its rebuilds from its own.

use std::{collections::BTreeMap, vec::Vec};

use crate::graphqlx::{
  id::{DefId, TyId},
  limits::Limits,
  model::{ModelBuilder, Shape},
};

/// A budget of steps: the most it may spend, and what it has spent.
pub(crate) struct Counter {
  max: u32,
  spent: u32,
}

impl Counter {
  /// A budget of `max` steps, none of them spent.
  pub(crate) const fn new(max: u32) -> Self {
    Self { max, spent: 0 }
  }

  /// Spends `steps`, all of them or none: `false`, with nothing spent, when the budget cannot pay
  /// them.
  pub(crate) fn charge(&mut self, steps: u32) -> bool {
    match self.spent.checked_add(steps) {
      Some(spent) if spent <= self.max => {
        self.spent = spent;
        true
      }
      _ => false,
    }
  }

  /// Spends `steps` that a routine counted out of what the budget had left, which are never more
  /// than that.
  pub(crate) fn take(&mut self, steps: u32) {
    debug_assert!(
      steps <= self.left(),
      "a routine takes no more steps than the budget has left"
    );
    self.spent = self.spent.saturating_add(steps);
  }

  /// How many steps the budget has left.
  pub(crate) const fn left(&self) -> u32 {
    self.max.saturating_sub(self.spent)
  }

  /// The most the budget may spend.
  pub(crate) const fn max(&self) -> u32 {
    self.max
  }
}

/// The limit a type passed, which its report names.
#[derive(Clone, Copy)]
pub(crate) enum Limit {
  /// `Limits::max_generic_depth`.
  Depth,
  /// `Limits::max_instantiations`.
  Instantiations,
  /// `Limits::max_substitutions`.
  Substitutions,
}

/// What stopped a rebuild, or the judging that asked for one.
#[derive(Clone, Copy)]
pub(crate) enum Stop {
  /// The budget it charges.
  Steps,
  /// A type it would derive passes this limit.
  Built(Limit),
}

/// The type services, and the budgets they spend: one for the whole analysis, borrowed by each
/// phase after lowering in turn.
pub(crate) struct Types {
  /// The analysis's limits.
  limits: Limits,
  /// `Limits::max_substitutions`: the filling of the type arguments a use leaves out.
  pub(crate) substitutions: Counter,
  /// `Limits::max_bound_steps`: the judging of bounds.
  pub(crate) bound_steps: Counter,
  /// The routines that derive a type, and what they keep.
  pub(crate) derived: Derived,
}

impl Types {
  /// The services of an analysis run within `limits`, with nothing spent and nothing derived.
  pub(crate) fn new(limits: Limits) -> Self {
    Self {
      limits,
      substitutions: Counter::new(limits.max_substitutions()),
      bound_steps: Counter::new(limits.max_bound_steps()),
      derived: Derived {
        max_depth: limits.max_generic_depth(),
        max_instantiations: limits.max_instantiations(),
        counted: 0,
        non_null: BTreeMap::new(),
        unresolved: None,
        slots: Vec::new(),
        rebuilt: BTreeMap::new(),
        frames: Vec::new(),
      },
    }
  }

  /// The analysis's limits.
  pub(crate) const fn limits(&self) -> &Limits {
    &self.limits
  }
}

/// The routines that derive a type, and what they keep from one type to the next.
pub(crate) struct Derived {
  /// `Limits::max_generic_depth`.
  max_depth: u32,
  /// `Limits::max_instantiations`.
  max_instantiations: u32,
  /// The instantiations of generic definitions interned outside the built-in library.
  counted: u32,
  /// Each type's non-null form, once made: every non-null occurrence, non-null parameter read from
  /// a slot, collection key and rebuilt non-null node asks through it, so a type's arguments are
  /// read once for it.
  non_null: BTreeMap<TyId, TyId>,
  /// The one unresolved type, once interned.
  unresolved: Option<TyId>,
  /// The slots of the type being completed, which `instantiation` reads: one buffer for every
  /// type.
  pub(crate) slots: Vec<TyId>,
  /// While one type is rebuilt: each open node of it rebuilt, with what it became. Keyed by the
  /// node, which the rebuild looks up for each child it meets; cleared for each type.
  rebuilt: BTreeMap<TyId, TyId>,
  /// While one type is rebuilt: the nodes being rebuilt, each with its next child to look at.
  frames: Vec<(TyId, u32)>,
}

impl Derived {
  /// The nullable named type of `def` with the slots `slots` holds, one level deeper than its
  /// deepest slot: one interned already, or one more instantiation, counted, when the limits admit
  /// it. When `library`, for the built-in library, it is neither checked nor counted.
  pub(crate) fn instantiation(
    &mut self,
    builder: &mut ModelBuilder,
    library: bool,
    def: DefId,
  ) -> Result<TyId, Limit> {
    let mut depth: u32 = 0;
    for slot in &self.slots {
      depth = depth.max(builder.ty_depth(*slot).saturating_add(1));
    }
    if !library && depth > self.max_depth {
      return Err(Limit::Depth);
    }
    if let Some(found) = builder.find_named(def, &self.slots, false) {
      return Ok(found);
    }
    if !library {
      if self.counted >= self.max_instantiations {
        return Err(Limit::Instantiations);
      }
      self.counted = self.counted.saturating_add(1);
    }
    Ok(builder.intern_named(def, &self.slots, false))
  }

  /// `ty`'s non-null form, made the first time it is asked for and remembered.
  pub(crate) fn non_null_of(&mut self, builder: &mut ModelBuilder, ty: TyId) -> TyId {
    if let Some(&made) = self.non_null.get(&ty) {
      return made;
    }
    let made = builder.intern_nullness(ty, true);
    self.non_null.insert(ty, made);
    made
  }

  /// The one unresolved type, interned the first time it is needed.
  pub(crate) fn unresolved(&mut self, builder: &mut ModelBuilder) -> TyId {
    if let Some(ty) = self.unresolved {
      return ty;
    }
    let ty = builder.intern_unresolved();
    self.unresolved = Some(ty);
    ty
  }

  /// The one unresolved type, when it is interned already: what holds it is unresolved too.
  pub(crate) const fn interned_unresolved(&self) -> Option<TyId> {
    self.unresolved
  }

  /// `open`, a type that names type parameters of `def`, with the arguments of `ty`, an
  /// instantiation of `def`, in place of them, each as it is: interned, a node's arguments or
  /// element before it, each distinct node of `open` once, completed as an occurrence's type is.
  /// `steps` pays one step for the root, and one for each argument or element of a node rebuilt,
  /// as it is looked at; `library` says that the type is the built-in library's.
  ///
  /// `Err` when `steps`, or a limit on what it derives, stopped it; `Ok(None)` for a type parameter
  /// of another definition, and for a node that cannot be rebuilt.
  pub(crate) fn rebuild(
    &mut self,
    builder: &mut ModelBuilder,
    steps: &mut Counter,
    library: bool,
    open: TyId,
    ty: TyId,
    def: DefId,
  ) -> Result<Option<TyId>, Stop> {
    self.rebuilt.clear();
    self.frames.clear();
    if !steps.charge(1) {
      return Err(Stop::Steps);
    }
    self.frames.push((open, 0));
    while let Some(&(node, next)) = self.frames.last() {
      let child = match builder.ty_shape(node) {
        Some(Shape::Named(_)) => builder.ty_args(node).get(next as usize).copied(),
        Some(Shape::List(element)) => (next == 0).then_some(element),
        Some(Shape::Param { .. } | Shape::Unresolved) | None => None,
      };
      if let Some(child) = child {
        if let Some(frame) = self.frames.last_mut() {
          frame.1 = next.saturating_add(1);
        }
        // Looked at, whether it is closed, rebuilt already or still to do.
        if !steps.charge(1) {
          return Err(Stop::Steps);
        }
        if !builder.ty_closed(child) && !self.rebuilt.contains_key(&child) {
          self.frames.push((child, 0));
        }
        continue;
      }
      self.frames.pop();
      let Some(made) = self.rebuilt_node(builder, library, node, ty, def)? else {
        return Ok(None);
      };
      if self.frames.is_empty() {
        return Ok(Some(made));
      }
      self.rebuilt.insert(node, made);
    }
    Ok(None)
  }

  /// What `node`, an open node of a type that names `def`'s type parameters, whose arguments or
  /// element `rebuilt` holds already, becomes with the arguments of `ty`, an instantiation of
  /// `def`, in place of them.
  fn rebuilt_node(
    &mut self,
    builder: &mut ModelBuilder,
    library: bool,
    node: TyId,
    ty: TyId,
    def: DefId,
  ) -> Result<Option<TyId>, Stop> {
    let non_null = builder.ty_non_null(node);
    match builder.ty_shape(node) {
      // The argument for the parameter, as it is, its `!` included; written `T!`, its non-null
      // form, which a collection's key, already non-null, is.
      Some(Shape::Param { def: owner, index }) => {
        debug_assert!(
          owner == def,
          "a type is rebuilt under the arguments of the definition whose parameters it names"
        );
        if owner != def {
          return Ok(None);
        }
        let Some(argument) = builder.ty_args(ty).get(index as usize).copied() else {
          return Ok(None);
        };
        Ok(Some(if non_null {
          self.non_null_of(builder, argument)
        } else {
          argument
        }))
      }
      Some(Shape::List(element)) => {
        let element = if builder.ty_closed(element) {
          element
        } else {
          match self.rebuilt.get(&element) {
            Some(&made) => made,
            None => return Ok(None),
          }
        };
        let depth = builder.ty_depth(element).saturating_add(1);
        if !library && depth > self.max_depth {
          return Err(Stop::Built(Limit::Depth));
        }
        Ok(Some(builder.intern_list(element, non_null)))
      }
      // Completed as an occurrence's type is from its slots, and recorded as no instantiation:
      // the program does not write it.
      Some(Shape::Named(named)) => {
        self.slots.clear();
        let args: &[TyId] = builder.ty_args(node);
        for &arg in args {
          let slot = if builder.ty_closed(arg) {
            arg
          } else {
            match self.rebuilt.get(&arg) {
              Some(&made) => made,
              None => return Ok(None),
            }
          };
          self.slots.push(slot);
        }
        let nullable = self
          .instantiation(builder, library, named)
          .map_err(Stop::Built)?;
        Ok(Some(if non_null {
          self.non_null_of(builder, nullable)
        } else {
          nullable
        }))
      }
      Some(Shape::Unresolved) | None => Ok(None),
    }
  }
}

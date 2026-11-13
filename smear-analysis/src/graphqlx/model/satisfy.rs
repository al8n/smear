//! What a type satisfies, read off what the model's rows hold, and nothing stored.
//!
//! A type satisfies an interface type by declaration, as an identity of types: a named type
//! satisfies what it is, each interface type its definition's `implements` entries write with the
//! type's own arguments in place of the definition's parameters, and `core`'s `MapKey`
//! intrinsically when it is an integer primitive, `bool`, `string`, `id`, an enum, or `NonZero` of
//! an integer primitive; a type parameter satisfies each of its bounds, and what each bound's
//! interface declares with the bound's arguments in place of the interface's parameters; a list and
//! the unresolved type satisfy nothing. Non-null is transparent on the type asked about, and the
//! interface type is compared as one type, the `!` of each of its arguments included.
//!
//! Nothing is closed: an interface an interface declares is followed only from a type parameter's
//! bound, one step. An entry that names its definition's parameters is compared with the interface
//! type node by node, under the type's arguments, and nothing is interned.
//!
//! One routine answers both public accessors and the instantiation phase, which asks it of the
//! rows written so far. Every row is read with `get`: an id that is not the model's, an occurrence
//! that has no type yet and a run out of its pool each answer `false` for the step that meets it,
//! so the routine panics on no model.

use std::{collections::BTreeMap, vec, vec::Vec};

use crate::graphqlx::id::{DefId, TyId, TypeExprId};

use super::{
  Model,
  def::{DefKind, Primitive, TypeBound},
  table::{DefRow, KIND_LIST, KIND_NAMED, KIND_PARAM, TyRow},
};

/// The built-in definitions satisfaction is keyed on, each by its `DefId` and never by a spelling.
/// `None` where a model holds none.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Keys {
  /// `core`'s `MapKey`.
  pub(crate) map_key: Option<DefId>,
  /// `core`'s `NonZero`.
  pub(crate) non_zero: Option<DefId>,
  /// `alloc`'s `string`.
  pub(crate) string: Option<DefId>,
  /// `alloc`'s `id`.
  pub(crate) id: Option<DefId>,
}

/// How many steps a question may still take.
///
/// A step is one `implements` entry or one bound looked at, one pair of nodes compared, or one
/// argument compared: the units the routine's work is counted in.
#[derive(Debug)]
pub(crate) struct Steps {
  /// What is left, when counted.
  left: u32,
  /// Whether the steps are counted: a reader's own question is not, and never runs out.
  counted: bool,
}

impl Steps {
  /// The steps of a reader's own question, which are not counted: it never runs out.
  pub(crate) const fn uncounted() -> Self {
    Self {
      left: 0,
      counted: false,
    }
  }

  /// The steps of a question a budget pays for, which has `left` steps left.
  pub(crate) const fn counted(left: u32) -> Self {
    Self {
      left,
      counted: true,
    }
  }

  /// How many steps are left: what was given less what was taken. A question that is not counted
  /// answers 0.
  pub(crate) const fn left(&self) -> u32 {
    self.left
  }

  /// Takes `steps`: `false`, taking nothing, when they are counted and fewer are left.
  fn take(&mut self, steps: u32) -> bool {
    if !self.counted {
      return true;
    }
    match self.left.checked_sub(steps) {
      Some(left) => {
        self.left = left;
        true
      }
      None => false,
    }
  }
}

/// The interface side of a question.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Wanted {
  /// This interface type, exactly: compared as one type, the `!` of each of its arguments
  /// included.
  Type(TyId),
  /// An instantiation of this interface, whichever.
  Definition(DefId),
}

impl Model {
  /// The built-in definitions satisfaction is keyed on, as the finished model holds them.
  pub(super) fn keys(&self) -> Keys {
    let [_, map_key, non_zero, string, _, id] = self.builtins.types;
    Keys {
      map_key,
      non_zero,
      string,
      id,
    }
  }

  /// Whether `ty` satisfies `wanted`, with `keys` the built-in definitions it is keyed on; `None`
  /// when `steps` ran out before it was known.
  ///
  /// It answers `false` at once when the interface type is no named type, or is non-null, which no
  /// bound and no `implements` entry writes, and when the interface asked about is no interface.
  /// Otherwise, by what `ty` is:
  ///
  /// - a named type, whatever its nullness, satisfies the interface when it is the interface:
  ///   its definition's, or, exactly, the interface type, its arguments compared when it is
  ///   non-null, one step each; when `MapKey` asks it and its definition satisfies `MapKey`
  ///   intrinsically; or when its definition declares the interface under its arguments;
  /// - a type parameter satisfies it through its definition's bounds on it, one step for each bound
  ///   looked at: when a bound's interface type is it, or names it when it is asked by definition,
  ///   or when the bound's interface declares it under the bound's arguments;
  /// - a list, and the unresolved type, satisfy nothing.
  pub(crate) fn judge(
    &self,
    keys: &Keys,
    ty: TyId,
    wanted: Wanted,
    steps: &mut Steps,
  ) -> Option<bool> {
    let (interface, exact) = match wanted {
      Wanted::Definition(def) => (def, None),
      Wanted::Type(goal) => match self.tys.get(goal.get() as usize) {
        Some(row) if row.shape == KIND_NAMED && !row.non_null => (row.def, Some(goal)),
        _ => return Some(false),
      },
    };
    if self.kind_of(interface) != Some(DefKind::Interface) {
      return Some(false);
    }
    let Some(&row) = self.tys.get(ty.get() as usize) else {
      return Some(false);
    };
    match row.shape {
      KIND_NAMED => self.named(keys, ty, row, interface, exact, steps),
      KIND_PARAM => self.bounded(row.def, row.index, interface, exact, steps),
      _ => Some(false),
    }
  }

  /// Whether `ty`, a named type whose row is `row`, satisfies `interface`, exactly `exact` when it
  /// is given: what it is, then what it is intrinsically, then what its definition declares.
  fn named(
    &self,
    keys: &Keys,
    ty: TyId,
    row: TyRow,
    interface: DefId,
    exact: Option<TyId>,
    steps: &mut Steps,
  ) -> Option<bool> {
    let args: &[TyId] = row.args.of(&self.ty_args);
    if row.def == interface {
      match exact {
        None => return Some(true),
        Some(goal) if goal == ty => return Some(true),
        // A non-null type is the interface type when its nullable form is: the two have one
        // definition, and the same arguments.
        Some(goal) if row.non_null => {
          let wanted: &[TyId] = match self.tys.get(goal.get() as usize) {
            Some(goal_row) => goal_row.args.of(&self.ty_args),
            None => &[],
          };
          if args.len() == wanted.len() {
            let mut same = true;
            for (arg, other) in args.iter().zip(wanted) {
              if !steps.take(1) {
                return None;
              }
              if arg != other {
                same = false;
                break;
              }
            }
            if same {
              return Some(true);
            }
          }
        }
        Some(_) => {}
      }
    }
    if keys.map_key == Some(interface) && self.is_key(keys, row.def, args) {
      return Some(true);
    }
    self.declares(row.def, args, interface, exact, steps)
  }

  /// Whether a named type of `def`, whose arguments are `args`, satisfies `core`'s `MapKey`
  /// intrinsically: an integer primitive, `bool`, `string`, `id`, an enum, or `NonZero` of an
  /// integer primitive, each told by its definition and never by a spelling. A `NonZero`'s argument
  /// is told by its definition alone: its `!` and its width are another rule's to judge.
  fn is_key(&self, keys: &Keys, def: DefId, args: &[TyId]) -> bool {
    let Some(row) = self.defs.get(def.get() as usize) else {
      return false;
    };
    if matches!(row.intrinsic, Some(Primitive::Int { .. } | Primitive::Bool))
      || keys.string == Some(def)
      || keys.id == Some(def)
      || row.kind == DefKind::Enum
    {
      return true;
    }
    if keys.non_zero != Some(def) {
      return false;
    }
    let [argument] = args else {
      return false;
    };
    match self.tys.get(argument.get() as usize) {
      Some(argument) if argument.shape == KIND_NAMED => matches!(
        self
          .defs
          .get(argument.def.get() as usize)
          .and_then(|row: &DefRow| row.intrinsic),
        Some(Primitive::Int { .. })
      ),
      _ => false,
    }
  }

  /// Whether `def`'s type parameter `index` satisfies `interface`, exactly `exact` when it is
  /// given, through `def`'s bounds on it: one step for each bound of `def` looked at.
  fn bounded(
    &self,
    def: DefId,
    index: u32,
    interface: DefId,
    exact: Option<TyId>,
    steps: &mut Steps,
  ) -> Option<bool> {
    let Some(row) = self.defs.get(def.get() as usize) else {
      return Some(false);
    };
    let bounds: &[TypeBound] = row.bounds.of(&self.bounds);
    for bound in bounds {
      if !steps.take(1) {
        return None;
      }
      if bound.parameter != index {
        continue;
      }
      // A right side that names no interface was refused where it is written.
      let Some(occurrence) = self.type_exprs.get(bound.interface.get() as usize) else {
        continue;
      };
      if occurrence.shape != KIND_NAMED || self.kind_of(occurrence.def) != Some(DefKind::Interface)
      {
        continue;
      }
      let bounding = occurrence.ty;
      let holds = match exact {
        None => occurrence.def == interface,
        Some(goal) => bounding == goal,
      };
      if holds {
        return Some(true);
      }
      // What the bound's interface declares, under the bound's own arguments: one step from the
      // bound, and no further.
      let env: &[TyId] = match self.tys.get(bounding.get() as usize) {
        Some(bounding) if bounding.shape == KIND_NAMED => bounding.args.of(&self.ty_args),
        _ => continue,
      };
      if self.declares(occurrence.def, env, interface, exact, steps)? {
        return Some(true);
      }
    }
    Some(false)
  }

  /// Whether `def` declares `interface`, exactly `exact` when it is given, under `env`, the
  /// arguments in place of `def`'s parameters: one step for each of its `implements` entries
  /// looked at, every contribution's, and the steps of matching an entry that names a parameter.
  fn declares(
    &self,
    def: DefId,
    env: &[TyId],
    interface: DefId,
    exact: Option<TyId>,
    steps: &mut Steps,
  ) -> Option<bool> {
    let Some(row) = self.defs.get(def.get() as usize) else {
      return Some(false);
    };
    let entries: &[TypeExprId] = row.implements.of(&self.implements);
    for entry in entries {
      if !steps.take(1) {
        return None;
      }
      let Some(occurrence) = self.type_exprs.get(entry.get() as usize) else {
        continue;
      };
      if occurrence.shape != KIND_NAMED || occurrence.def != interface {
        continue;
      }
      let Some(goal) = exact else {
        return Some(true);
      };
      // An entry a count left unresolved is no named type, and declares nothing.
      let declared = occurrence.ty;
      let Some(written) = self.tys.get(declared.get() as usize) else {
        continue;
      };
      if written.shape != KIND_NAMED {
        continue;
      }
      if written.closed {
        if declared == goal {
          return Some(true);
        }
        continue;
      }
      if self.matches(def, declared, goal, env, steps)? {
        return Some(true);
      }
    }
    Some(false)
  }

  /// Whether `declared`, a type that names parameters of `owner`, is `goal` with `env[i]` in place
  /// of each of `owner`'s parameters `i`: compared pair of nodes by pair, one step for each pair,
  /// interning nothing.
  ///
  /// An open node is never answered by comparing ids, since it is another type under `env` unless
  /// `env` gives each parameter itself. A closed node is its own target or nothing. An open node
  /// already paired is not descended again: met with the target it was paired with it is nothing
  /// more, and with another it is `false`. So a type that names a parameter twice at each level is
  /// compared in its distinct nodes, not walked as a tree.
  fn matches(
    &self,
    owner: DefId,
    declared: TyId,
    goal: TyId,
    env: &[TyId],
    steps: &mut Steps,
  ) -> Option<bool> {
    let mut pending: Vec<(TyId, TyId)> = vec![(declared, goal)];
    // Each open node of `declared` met, with the node of `goal` it was paired with.
    let mut paired: BTreeMap<TyId, TyId> = BTreeMap::new();
    while let Some((node, target)) = pending.pop() {
      if !steps.take(1) {
        return None;
      }
      let (Some(&row), Some(&aim)) = (
        self.tys.get(node.get() as usize),
        self.tys.get(target.get() as usize),
      ) else {
        return Some(false);
      };
      if row.closed {
        if node == target {
          continue;
        }
        return Some(false);
      }
      match paired.get(&node).copied() {
        Some(before) if before == target => continue,
        Some(_) => return Some(false),
        None => {
          paired.insert(node, target);
        }
      }
      match row.shape {
        KIND_PARAM => {
          if row.def != owner {
            return Some(false);
          }
          let Some(&argument) = env.get(row.index as usize) else {
            return Some(false);
          };
          let same = if row.non_null {
            self.made_non_null(argument, target, aim, steps)?
          } else {
            argument == target
          };
          if !same {
            return Some(false);
          }
        }
        KIND_LIST => {
          if aim.shape != KIND_LIST || aim.non_null != row.non_null {
            return Some(false);
          }
          pending.push((row.element, aim.element));
        }
        KIND_NAMED => {
          let args: &[TyId] = row.args.of(&self.ty_args);
          let targets: &[TyId] = aim.args.of(&self.ty_args);
          if aim.shape != KIND_NAMED
            || aim.def != row.def
            || aim.non_null != row.non_null
            || args.len() != targets.len()
          {
            return Some(false);
          }
          // Pushed last to first, so they are compared first to last.
          for pair in args.iter().copied().zip(targets.iter().copied()).rev() {
            pending.push(pair);
          }
        }
        _ => return Some(false),
      }
    }
    Some(true)
  }

  /// Whether `target`, whose row is `aim`, is `argument` made non-null: `argument` itself when it
  /// is non-null already, and otherwise a non-null type of its shape, definition, index, element
  /// and arguments, one step for each argument compared.
  fn made_non_null(
    &self,
    argument: TyId,
    target: TyId,
    aim: TyRow,
    steps: &mut Steps,
  ) -> Option<bool> {
    let Some(&row) = self.tys.get(argument.get() as usize) else {
      return Some(false);
    };
    if row.non_null {
      return Some(argument == target);
    }
    if !aim.non_null
      || aim.shape != row.shape
      || aim.def != row.def
      || aim.index != row.index
      || aim.element != row.element
    {
      return Some(false);
    }
    let args: &[TyId] = row.args.of(&self.ty_args);
    let targets: &[TyId] = aim.args.of(&self.ty_args);
    if args.len() != targets.len() {
      return Some(false);
    }
    for (arg, other) in args.iter().zip(targets) {
      if !steps.take(1) {
        return None;
      }
      if arg != other {
        return Some(false);
      }
    }
    Some(true)
  }

  /// The kind of the definition `def`, or `None` for an id that is not the model's.
  fn kind_of(&self, def: DefId) -> Option<DefKind> {
    self
      .defs
      .get(def.get() as usize)
      .map(|row: &DefRow| row.kind)
  }
}

//! Whether a type fits where it stands: what the rules that judge a type's arguments share.
//!
//! A position takes a class of type: a field's type is an output type, and an argument's, a
//! directive's argument's and an input field's an input type. [`Fits`] answers whether a type is of
//! a class, and, when it is not, where under it the type that is not lies:
//!
//! - a list fits when its element does;
//! - a collection, a scalar `core`'s `@set` or `@map` makes one, when each of its arguments does,
//!   in order, since a value of it holds values of them;
//! - a type parameter and the unresolved type fit every class: what a parameter takes is judged
//!   where an instantiation gives it a type, and what did not resolve was refused where it is
//!   written;
//! - any other type by its definition's kind: an input object is no output type, an object, an
//!   interface and a union are no input types, and a scalar that is no collection, an enum and a
//!   directive are of every class, whatever their arguments.
//!
//! What it costs: each pair of a type and a class asked about is judged once, with an explicit
//! stack, a type's children before it, and kept in an ordered map keyed by the type's id; reading
//! where a misfit lies follows one path from the type down, no deeper than the type, beside the
//! written occurrence that holds it. Nothing recurses.

use std::{collections::BTreeMap, vec::Vec};

use tokora::diagnostic::Location;

use crate::graphqlx::{DefId, DefKind, Model, TyId, TyKind, TypeExprId, TypeExprKind, id::dense};

/// A class of type a position takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Class {
  /// An output type, which a field's type is.
  Output,
  /// An input type, which an argument's, a directive's argument's and an input field's are.
  Input,
}

/// Where a type does not fit a class: nowhere, in itself, or under one of its children.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Under {
  /// It fits.
  Nowhere,
  /// It is of a kind the class does not take.
  Itself,
  /// A list's element does not fit.
  Element,
  /// A collection's argument at this index does not fit, the first of them that does not.
  Argument(u32),
}

/// A type that does not fit a class, as [`Fits::misfit`] reads it.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Misfit {
  /// The definition of the type that does not fit, under the lists and the collections' arguments
  /// that hold it: an object, an interface, a union or an input object.
  pub(crate) leaf: DefId,
  /// The deepest of the written occurrences handed in and nested in it that writes the way to it.
  pub(crate) at: TypeExprId,
  /// The collection, and the index of its type parameter, whose slot the way leaves the written
  /// occurrences by: a slot the occurrence does not write, which its default fills.
  pub(crate) filled: Option<(DefId, u32)>,
}

/// Whether types fit classes, each type and class judged once.
///
/// A rule keeps one for its walk and resets it in `begin`: what it holds is keyed by the model's
/// ids.
#[derive(Default)]
pub(crate) struct Fits {
  /// Each type and class judged, with where the type does not fit.
  judged: BTreeMap<(TyId, Class), Under>,
  /// The types still to judge, each with whether its children were pushed above it.
  stack: Vec<(TyId, bool)>,
}

impl Fits {
  /// Where `ty`, of `model`, does not fit `class`, `written` being the occurrence that writes it:
  /// the definition of the type under it that does not fit, and the deepest occurrence of
  /// `written`'s tree that writes the way to it. `None` when it fits.
  pub(crate) fn misfit(
    &mut self,
    model: &Model,
    ty: TyId,
    class: Class,
    written: TypeExprId,
  ) -> Option<Misfit> {
    let mut under = self.under(model, ty, class);
    let mut ty = ty;
    let mut at = written;
    // Whether `at` still writes the type the way has reached.
    let mut writes = true;
    let mut filled: Option<(DefId, u32)> = None;
    // Each step goes one level down the type, so the loop ends.
    loop {
      match (under, model.ty(ty).kind()) {
        (Under::Itself, TyKind::Named { def, .. }) => {
          return Some(Misfit {
            leaf: def,
            at,
            filled,
          });
        }
        (Under::Element, TyKind::List { element }) => {
          ty = element;
          if writes {
            match model.type_expr(at).kind() {
              TypeExprKind::List { element } => at = element,
              _ => writes = false,
            }
          }
        }
        (Under::Argument(index), TyKind::Named { def, args }) => {
          ty = *args.get(index as usize)?;
          if writes {
            let next = match model.type_expr(at).kind() {
              TypeExprKind::Named { args: written, .. } => written.get(index as usize).copied(),
              _ => None,
            };
            match next {
              Some(next) => at = next,
              None => {
                writes = false;
                filled = Some((def, index));
              }
            }
          }
        }
        _ => return None,
      }
      // Every type under a judged one is judged, so this reads a kept answer.
      under = self
        .judged
        .get(&(ty, class))
        .copied()
        .unwrap_or(Under::Nowhere);
    }
  }

  /// Where `ty`, of `model`, does not fit `class`, judging it and every type under it not judged
  /// yet, children before their parent.
  fn under(&mut self, model: &Model, ty: TyId, class: Class) -> Under {
    if let Some(under) = self.judged.get(&(ty, class)) {
      return *under;
    }
    self.stack.clear();
    self.stack.push((ty, false));
    while let Some((next, pushed)) = self.stack.pop() {
      if self.judged.contains_key(&(next, class)) {
        continue;
      }
      let under = match model.ty(next).kind() {
        TyKind::List { element } => {
          if !pushed {
            self.stack.push((next, true));
            self.stack.push((element, false));
            continue;
          }
          if self.kept(element, class) {
            Under::Nowhere
          } else {
            Under::Element
          }
        }
        TyKind::Named { def, args } if model.def(def).collection().is_some() => {
          if !pushed {
            self.stack.push((next, true));
            self
              .stack
              .extend(args.iter().rev().map(|arg: &TyId| (*arg, false)));
            continue;
          }
          match args.iter().position(|arg: &TyId| !self.kept(*arg, class)) {
            Some(index) => Under::Argument(dense(index)),
            None => Under::Nowhere,
          }
        }
        TyKind::Named { def, .. } => leaf(model.def(def).kind(), class),
        TyKind::Param { .. } | TyKind::Unresolved => Under::Nowhere,
      };
      self.judged.insert((next, class), under);
    }
    self
      .judged
      .get(&(ty, class))
      .copied()
      .unwrap_or(Under::Nowhere)
  }

  /// Whether `ty`, judged for `class` already, fits it.
  fn kept(&self, ty: TyId, class: Class) -> bool {
    self
      .judged
      .get(&(ty, class))
      .is_none_or(|under: &Under| *under == Under::Nowhere)
  }
}

/// Whether a type of a definition of `kind` that is no collection fits `class`.
fn leaf(kind: DefKind, class: Class) -> Under {
  let refused = match class {
    Class::Output => kind == DefKind::InputObject,
    Class::Input => matches!(kind, DefKind::Object | DefKind::Interface | DefKind::Union),
  };
  if refused {
    Under::Itself
  } else {
    Under::Nowhere
  }
}

/// How a message names a definition of `kind`, before its name: "the object type `Obj`".
pub(crate) fn named(kind: DefKind) -> &'static str {
  match kind {
    DefKind::InputObject => "the input object",
    DefKind::Object => "the object type",
    DefKind::Interface => "the interface",
    DefKind::Union => "the union",
    DefKind::Scalar => "the scalar",
    DefKind::Enum => "the enum",
    DefKind::Directive => "the directive",
  }
}

/// Where the default of the type parameter `index` of `def`, of `model`, is written, when it has
/// one: what fills a slot an occurrence leaves out.
pub(crate) fn default_of(model: &Model, def: DefId, index: u32) -> Option<Location> {
  let default = model
    .def(def)
    .type_params()
    .nth(index as usize)?
    .default()?;
  Some(model.type_expr(default).location())
}

//! The relation an implementing field's type is judged by: whether one type is a subtype of
//! another, `IsSubType` of the validator's design, generalised over type arguments as the owner
//! ruled.
//!
//! # The relation
//!
//! A pair `(sub, sup)`, both types written in the definition being checked or read under its
//! entries' type arguments, so that the only type parameters either names are that definition's:
//!
//! - one type is a subtype of itself;
//! - the unresolved type is a subtype of anything and has every subtype: the resolver reported it;
//! - a nullable type is no subtype of a non-null one; past that, each is read without its `!`;
//! - a list is a subtype of a list when its element is of the other's element, and no list is of
//!   what is no list, nor the reverse;
//! - a type parameter is a subtype of itself alone among the type parameters;
//! - two named types of one definition, when each argument is a subtype of the other's in order:
//!   covariant in every argument, through collections and generic scalars too, so `Page<User>` is
//!   a subtype of `Page<Node>`;
//! - a named type of an interface has as subtypes: a named type of a definition one of whose
//!   `implements` entries its own package writes is, under that type's arguments, a named type of
//!   the interface whose arguments are each a subtype of the interface's; and a type parameter one
//!   of whose definition's bounds, or one of whose bounds' interface's entries under the bound's
//!   arguments, is such a type;
//! - a named type of a union has as subtypes each type of the definition, or each type parameter,
//!   that one of its members, under its arguments, is;
//! - and nothing else holds.
//!
//! The interface step is one step, by declaration: what a definition's entries implement is not
//! followed. `smear-schema` reads the declared list as the closure, and the transitive check makes
//! the declared list the closure of every program it accepts, so on every such program this is
//! the recursion through the `implements` chain. A type parameter goes through its bounds and one
//! step of each bound's interface, as satisfaction reads it. A union admits a member of `sub`'s own
//! definition alone, so no union recurses into another, and an interface or a union is a subtype
//! of no union but itself, as in GraphQL.
//!
//! # The machine, and why it ends
//!
//! An explicit stack: no routine calls itself. A pair looked at is a step of
//! `Limits::max_implementation_steps`, one asked before included, and so is each entry, bound,
//! bound's entry and union member looked at to list its alternatives, whoever wrote it; a type read
//! under arguments is rebuilt as the phase rebuilds, remembered. A pair opened lists its
//! alternatives, each two types and a kind, read in place: the arguments of two named types, or
//! one pair. Each alternative holds when each of its pairs does, and a pair holds when one of its
//! alternatives does. A pair is marked open while its frame is on the stack, and one asked while
//! it is open fails: it cannot hold through itself. Each frame keeps the least stack index of an
//! open pair its subtree met; a frame that holds is remembered for the whole phase, and so is one
//! that fails having met no open pair but its own, while one whose failure rested on an open pair
//! below it is judged afresh the next time it is asked. A stop removes the open mark of every frame
//! it clears.
//!
//! Every step shrinks a pair but the interface and the union steps: the interface step replaces
//! the interface type by its arguments, and the union step is always followed by the step of two
//! named types of one definition, or by the step of two type parameters. Every pair and every
//! item looked at is a charged step, and a type a rebuild derives is depth-checked and counted, so
//! the budget ends any chain that grows, a cycle through a union member's type arguments included.

use crate::graphqlx::{
  id::{DefId, TyId, dense},
  model::{DefKind, Occurrence, Shape, TypeBound, Written},
  types::Stop,
};

use super::Implementing;

/// What a pair the relation looked at came to, or that its frame is open.
#[derive(Clone, Copy)]
pub(super) enum Answer {
  /// It holds.
  Holds,
  /// It does not hold, whatever pair is asked after it.
  Fails,
  /// Its frame is on the stack, at this index.
  Open(u32),
}

/// One way a pair may hold, read in place: every pair it lists holds.
#[derive(Clone, Copy)]
pub(super) enum Alternative {
  /// The arguments of two named types of one definition, pair by pair in order.
  Arguments(TyId, TyId),
  /// One pair.
  Pair(TyId, TyId),
}

/// A pair being answered: its alternatives, `alternatives[start..end]`, the one it is trying and
/// the next pair of that one to ask, and the least stack index of an open pair its subtree met,
/// its own when it met none.
#[derive(Clone, Copy)]
pub(super) struct Frame {
  sub: TyId,
  sup: TyId,
  start: u32,
  end: u32,
  alt: u32,
  next: u32,
  low: u32,
}

/// What asking a pair came to.
#[derive(Clone, Copy)]
enum Asked {
  /// It holds, or it does not.
  Known(bool),
  /// It is open, its frame on the stack at this index: it fails here.
  Open(u32),
  /// A frame was pushed for it.
  Opened,
}

impl Implementing<'_> {
  /// Whether `sub` is a subtype of `sup`, both naming the type parameters of the definition being
  /// checked or none: `Err` when a limit stopped the question.
  pub(super) fn subtype(&mut self, sub: TyId, sup: TyId) -> Result<bool, Stop> {
    self.frames.clear();
    self.alternatives.clear();
    let answered = match self.ask(sub, sup) {
      Ok(Asked::Known(holds)) => Ok(holds),
      // No frame is open when a question begins: no pair it asks first is open.
      Ok(Asked::Open(_)) => Ok(false),
      Ok(Asked::Opened) => self.run(),
      Err(stop) => Err(stop),
    };
    if answered.is_err() {
      self.unwind();
    }
    answered
  }

  /// Runs the frames on the stack until the first closes: its answer.
  fn run(&mut self) -> Result<bool, Stop> {
    loop {
      let Some(frame) = self.frames.last().copied() else {
        return Ok(false);
      };
      if frame.alt >= frame.end {
        // No alternative held.
        if let Some(holds) = self.close(false) {
          return Ok(holds);
        }
        continue;
      }
      let pair = self
        .alternatives
        .get(frame.alt as usize)
        .copied()
        .and_then(|alternative: Alternative| self.pair_of(alternative, frame.next));
      let Some((sub, sup)) = pair else {
        // Past the alternative's last pair: every one held.
        if let Some(holds) = self.close(true) {
          return Ok(holds);
        }
        continue;
      };
      if let Some(top) = self.frames.last_mut() {
        top.next = frame.next.saturating_add(1);
      }
      match self.ask(sub, sup)? {
        Asked::Known(true) | Asked::Opened => {}
        Asked::Known(false) => self.next_alternative(),
        Asked::Open(index) => {
          if let Some(top) = self.frames.last_mut() {
            top.low = top.low.min(index);
          }
          self.next_alternative();
        }
      }
    }
  }

  /// The pair at `next` of `alternative`, read in place: none past its last.
  fn pair_of(&self, alternative: Alternative, next: u32) -> Option<(TyId, TyId)> {
    match alternative {
      Alternative::Pair(sub, sup) => (next == 0).then_some((sub, sup)),
      Alternative::Arguments(sub, sup) => {
        let sub = self.builder.ty_args(sub).get(next as usize).copied()?;
        let sup = self.builder.ty_args(sup).get(next as usize).copied()?;
        Some((sub, sup))
      }
    }
  }

  /// The frame on top tries its next alternative.
  fn next_alternative(&mut self) {
    if let Some(top) = self.frames.last_mut() {
      top.alt = top.alt.saturating_add(1);
      top.next = 0;
    }
  }

  /// Closes the frame on top, which `holds` or not: its alternatives let go, its answer remembered
  /// when it rested on no open pair below it, and handed to the frame under it, the answer of the
  /// question when there is none.
  fn close(&mut self, holds: bool) -> Option<bool> {
    let index = dense(self.frames.len().saturating_sub(1));
    let frame = self.frames.pop()?;
    self.alternatives.truncate(frame.start as usize);
    let key = (frame.sub, frame.sup);
    if holds {
      self.pairs.insert(key, Answer::Holds);
    } else if frame.low >= index {
      self.pairs.insert(key, Answer::Fails);
    } else {
      self.pairs.remove(&key);
    }
    match self.frames.last_mut() {
      None => Some(holds),
      Some(parent) => {
        parent.low = parent.low.min(frame.low);
        if !holds {
          parent.alt = parent.alt.saturating_add(1);
          parent.next = 0;
        }
        None
      }
    }
  }

  /// Lets go of every frame a stop leaves, the open mark of each removed.
  fn unwind(&mut self) {
    while let Some(frame) = self.frames.pop() {
      self.pairs.remove(&(frame.sub, frame.sup));
    }
    self.alternatives.clear();
  }

  /// Looks at the pair `(sub, sup)`, a step: itself, a pair remembered, or opened, which decides
  /// it or pushes a frame of its alternatives.
  fn ask(&mut self, sub: TyId, sup: TyId) -> Result<Asked, Stop> {
    if !self.steps.charge(1) {
      return Err(Stop::Steps);
    }
    if sub == sup {
      return Ok(Asked::Known(true));
    }
    match self.pairs.get(&(sub, sup)).copied() {
      Some(Answer::Holds) => return Ok(Asked::Known(true)),
      Some(Answer::Fails) => return Ok(Asked::Known(false)),
      Some(Answer::Open(index)) => return Ok(Asked::Open(index)),
      None => {}
    }
    let start = dense(self.alternatives.len());
    let decided = match self.open(sub, sup) {
      Ok(decided) => decided,
      Err(stop) => {
        self.alternatives.truncate(start as usize);
        return Err(stop);
      }
    };
    let end = dense(self.alternatives.len());
    // Decided at once, or with no alternative to try: it rests on no pair, and is remembered.
    let holds = match decided {
      Some(holds) => holds,
      None if end > start => {
        let index = dense(self.frames.len());
        self.frames.push(Frame {
          sub,
          sup,
          start,
          end,
          alt: start,
          next: 0,
          low: index,
        });
        self.pairs.insert((sub, sup), Answer::Open(index));
        return Ok(Asked::Opened);
      }
      None => false,
    };
    self.alternatives.truncate(start as usize);
    let answer = if holds { Answer::Holds } else { Answer::Fails };
    self.pairs.insert((sub, sup), answer);
    Ok(Asked::Known(holds))
  }

  /// Opens the pair `(sub, sup)`: `Some` when it is decided at once, and otherwise `None`, its
  /// alternatives pushed.
  fn open(&mut self, sub: TyId, sup: TyId) -> Result<Option<bool>, Stop> {
    let (Some(below), Some(above)) = (self.builder.ty_shape(sub), self.builder.ty_shape(sup))
    else {
      return Ok(Some(true));
    };
    if matches!(below, Shape::Unresolved) || matches!(above, Shape::Unresolved) {
      return Ok(Some(true));
    }
    if self.builder.ty_non_null(sup) && !self.builder.ty_non_null(sub) {
      return Ok(Some(false));
    }
    match (below, above) {
      (Shape::List(element), Shape::List(wanted)) => {
        self.alternatives.push(Alternative::Pair(element, wanted));
        Ok(None)
      }
      (Shape::List(_), _) | (_, Shape::List(_)) => Ok(Some(false)),
      (
        Shape::Param { def, index },
        Shape::Param {
          def: wanted,
          index: at,
        },
      ) => Ok(Some(def == wanted && index == at)),
      (Shape::Named(holder), Shape::Named(target)) if holder == target => {
        self.alternatives.push(Alternative::Arguments(sub, sup));
        Ok(None)
      }
      (_, Shape::Named(target)) => match self.builder.def_kind(target) {
        Some(DefKind::Interface) => {
          match below {
            Shape::Named(holder) => self.entry_alternatives(holder, sub, sup, target)?,
            Shape::Param { def, index } => self.bound_alternatives(def, index, sup, target)?,
            Shape::List(_) | Shape::Unresolved => {}
          }
          Ok(None)
        }
        Some(DefKind::Union) => {
          self.member_alternatives(sub, below, sup, target)?;
          Ok(None)
        }
        Some(_) | None => Ok(Some(false)),
      },
      // The unresolved type was answered above.
      (_, Shape::Param { .. } | Shape::Unresolved) => Ok(Some(false)),
    }
  }

  /// Pushes an alternative for each `implements` entry of `holder` its own package writes that
  /// is, under the arguments of `ty`, a named type of `interface`: the pairs of its arguments and
  /// `sup`'s. Each entry looked at is a step, whoever wrote it.
  fn entry_alternatives(
    &mut self,
    holder: DefId,
    ty: TyId,
    sup: TyId,
    interface: DefId,
  ) -> Result<(), Stop> {
    let own = self.owner(holder);
    let count = self.builder.def_implements(holder).len();
    for at in 0..count {
      if !self.steps.charge(1) {
        return Err(Stop::Steps);
      }
      let Some(&entry) = self.builder.def_implements(holder).get(at) else {
        break;
      };
      let Some((location, written)) = self
        .builder
        .occurrence(entry)
        .map(|occurrence: Occurrence<'_>| (occurrence.location, occurrence.ty))
      else {
        continue;
      };
      if own.is_none() || self.sources.written_by(location) != own {
        continue;
      }
      let Some(written) = written else {
        continue;
      };
      let Some(declared) = self.rebuilt(written, ty, holder)? else {
        continue;
      };
      if matches!(self.builder.ty_shape(declared), Some(Shape::Named(named)) if named == interface)
      {
        self
          .alternatives
          .push(Alternative::Arguments(declared, sup));
      }
    }
    Ok(())
  }

  /// Pushes an alternative for each bound of `def` on its type parameter `index` whose interface
  /// type is a named type of `interface`, and for each entry of a bound's interface that is one
  /// under the bound's arguments. Each bound looked at is a step, and so is each entry.
  fn bound_alternatives(
    &mut self,
    def: DefId,
    index: u32,
    sup: TyId,
    interface: DefId,
  ) -> Result<(), Stop> {
    let count = self.builder.def_bounds(def).len();
    for at in 0..count {
      if !self.steps.charge(1) {
        return Err(Stop::Steps);
      }
      let Some(bound) = self.builder.def_bounds(def).get(at).copied() else {
        break;
      };
      if bound.parameter() != index {
        continue;
      }
      let Some((bounding, ty)) = self.bound_interface(bound) else {
        continue;
      };
      if bounding == interface {
        self.alternatives.push(Alternative::Arguments(ty, sup));
      }
      self.entry_alternatives(bounding, ty, sup, interface)?;
    }
    Ok(())
  }

  /// The interface `bound`'s right side names, and its type, when it names an interface and its
  /// type is a named type: a right side that names none was refused where it is written.
  fn bound_interface(&self, bound: TypeBound) -> Option<(DefId, TyId)> {
    let occurrence = self.builder.occurrence(bound.interface())?;
    let Written::Named(bounding) = occurrence.written else {
      return None;
    };
    let ty = occurrence.ty?;
    if self.builder.def_kind(bounding) != Some(DefKind::Interface) {
      return None;
    }
    match self.builder.ty_shape(ty) {
      Some(Shape::Named(_)) => Some((bounding, ty)),
      _ => None,
    }
  }

  /// Pushes the one pair `(sub, member)` for each member of `union` its own package writes that
  /// is, under the arguments of `sup`, a named type of `sub`'s definition, or `sub`'s type
  /// parameter, `below` being `sub`'s shape. Each member looked at is a step, whoever wrote it.
  fn member_alternatives(
    &mut self,
    sub: TyId,
    below: Shape,
    sup: TyId,
    union: DefId,
  ) -> Result<(), Stop> {
    let own = self.owner(union);
    let count = self.builder.def_union_members(union).len();
    for at in 0..count {
      if !self.steps.charge(1) {
        return Err(Stop::Steps);
      }
      let Some(&member) = self.builder.def_union_members(union).get(at) else {
        break;
      };
      let Some((location, written)) = self
        .builder
        .occurrence(member)
        .map(|occurrence: Occurrence<'_>| (occurrence.location, occurrence.ty))
      else {
        continue;
      };
      if own.is_none() || self.sources.written_by(location) != own {
        continue;
      }
      let Some(written) = written else {
        continue;
      };
      let Some(member) = self.rebuilt(written, sup, union)? else {
        continue;
      };
      let fits = match (below, self.builder.ty_shape(member)) {
        (Shape::Named(holder), Some(Shape::Named(named))) => holder == named,
        (
          Shape::Param { def, index },
          Some(Shape::Param {
            def: owner,
            index: slot,
          }),
        ) => def == owner && index == slot,
        _ => false,
      };
      if fits {
        self.alternatives.push(Alternative::Pair(sub, member));
      }
    }
    Ok(())
  }
}

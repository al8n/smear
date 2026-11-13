//! No interface implements itself.
//!
//! GraphQL refuses an interface whose declared interfaces hold itself, and one whose declared
//! interfaces hold an interface that declares it. Here every interface on a cycle of `implements`
//! entries, of any length, is refused: the interfaces of a cycle each implement themselves through
//! it. Each is reported once, at its name, under the code the same rule has in GraphQL:
//! `smear-schema`'s `self-implementing-interface`, with its label. An entry an extension writes is
//! an edge as the definition's own entries are.
//!
//! The graph is over definitions: an entry is an edge to the interface it names, whatever its
//! arguments, so `interface G<X> implements G<Box<X>>` implements itself. An entry that names no
//! interface is no edge.
//!
//! An interface is judged when a supplied package wrote it, a GraphQL built-in the root package
//! replaces among them.
//!
//! The rule reads the public model and nothing else.

use std::{format, vec::Vec};

use smear_semantics::scc;

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, TypeExprKind, Visitor, code::schema,
  diagnostic::Excerpt, id::dense,
};

use super::supplied;

/// Reports each interface on a cycle of `implements` entries, of those a supplied package wrote.
///
/// It finds the interfaces on a cycle in the `begin` hook, over every definition of the model,
/// and reports each one in the `enter_def` hook. It says nothing about an interface whose own
/// contribution, its first, has its `has_errors()` set, where its name is: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's. An error in
/// another interface of the cycle does not silence it.
///
/// It costs, once per walk, one look at each definition and two at each `implements` entry of
/// each interface, which the strongly connected components are found from with an explicit stack;
/// and one answer of who wrote each interface on a cycle. It keeps one flag per definition for the
/// walk. Its messages quote the interface's name as an excerpt, so each is bounded whatever the
/// name's length.
#[derive(Default)]
pub(super) struct SelfImplementingInterface {
  /// For each definition, by index, whether it is an interface on a cycle of `implements` entries.
  cyclic: Vec<bool>,
}

impl Visitor for SelfImplementingInterface {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let count = usize::try_from(model.defs().len()).unwrap_or(0);
    self.cyclic = scc::on_cycle(count, count, |node: usize, reaches: &mut Vec<u32>| {
      let definition = model.def(DefId::new(dense(node)));
      if definition.kind() != DefKind::Interface {
        return;
      }
      for entry in definition.implements() {
        if let TypeExprKind::Named { def: named, .. } = model.type_expr(*entry).kind()
          && model.def(named).kind() == DefKind::Interface
        {
          reaches.push(named.get());
        }
      }
    });
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let on_cycle = usize::try_from(def.get())
      .ok()
      .and_then(|index: usize| self.cyclic.get(index))
      .is_some_and(|cyclic: &bool| *cyclic);
    if !on_cycle || !supplied(cx, def) {
      return;
    }
    let model = cx.model();
    let definition = model.def(def);
    let refused = definition
      .contributions()
      .next()
      .is_none_or(|own: Contribution<'_>| own.has_errors());
    if refused {
      return;
    }
    cx.report(
      Diagnostic::builder(schema::SELF_IMPLEMENTING_INTERFACE, definition.location())
        .message(format!(
          "the interface `{}` implements itself, through the interfaces it implements",
          Excerpt(definition.name().as_bytes())
        ))
        .primary_label("interface implements itself")
        .build(),
    );
  }
}

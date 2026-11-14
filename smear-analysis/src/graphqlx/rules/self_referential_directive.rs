//! No directive refers to itself.
//!
//! GraphQL refuses a directive definition that refers to itself: through the directives applied to
//! it, an extension's among them, and to its arguments, and through the types its arguments take,
//! with what those types apply and take in turn. Here the graph is over definitions. A definition
//! reaches, in one step:
//!
//! - the directive of each application on it, every contribution's, that resolved;
//! - for a directive definition, the same of each application on its arguments, and for any other
//!   definition, on its input fields and its enum values: an object's and an interface's fields
//!   are no part of the graph;
//! - every definition a written type occurrence names, at any depth: the type of each of a
//!   directive definition's arguments and of each input field, the type arguments of each of those
//!   applications, and its own type parameters' defaults, which stand for an argument a use leaves
//!   out.
//!
//! A scalar that is no collection hides its type arguments, as it does from the kinds a position
//! takes: an occurrence of one names it and not its arguments, and its own type parameters'
//! defaults name nothing. A collection's arguments are followed.
//!
//! Each directive on a cycle of that graph is reported once, at its name, under the code the same
//! rule has in GraphQL: `smear-schema`'s `self-referential-directive`, with its label and its
//! help. A directive that applies one on a cycle, and is not reached back, is not on it. The
//! library alone has no cycle, so one always passes through what a supplied package wrote; a
//! directive of `core` on it is reported all the same, at its own name, in `core`'s source, as
//! GraphQL reports it in its built-in document.
//!
//! The rule reads the public model and nothing else.

use std::{format, vec::Vec};

use smear_semantics::scc;

use crate::graphqlx::{
  ApplicationId, Contribution, Cx, Def, DefId, DefKind, Diagnostic, Model, Resolution, TypeExprId,
  TypeExprKind, Visitor, code::schema, diagnostic::Excerpt, id::dense,
};

/// Reports each directive on a cycle of the graph the module's header describes.
///
/// It finds the definitions on a cycle and reports each directive among them in the `begin` hook,
/// over every definition of the model. It says nothing about a directive whose own contribution,
/// its first, has its `has_errors()` set, where its name is: a phase already refused something in
/// it, and a second diagnostic within it would repeat the phase's. An edge written inside a
/// refused contribution counts all the same.
///
/// It costs, once per walk, two looks at each definition, at each application on a definition and
/// on a directive definition's argument, an input field or an enum value, and at each type
/// occurrence those positions write, which the strongly connected components are found from with
/// explicit stacks. It keeps nothing between two walks: the kernel's vectors and one stack of type
/// occurrences live while the hook runs. Its messages quote the directive's name as an excerpt, so
/// each is bounded whatever the name's length.
#[derive(Default)]
pub(super) struct SelfReferentialDirective;

impl Visitor for SelfReferentialDirective {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    let model = cx.model();
    let count = usize::try_from(model.defs().len()).unwrap_or(0);
    let mut stack: Vec<TypeExprId> = Vec::new();
    let cyclic = scc::on_cycle(count, count, |node: usize, reaches: &mut Vec<u32>| {
      let definition = model.def(DefId::new(dense(node)));
      successors(model, definition, &mut stack, reaches);
    });
    for (index, on_cycle) in cyclic.iter().enumerate() {
      if !*on_cycle {
        continue;
      }
      let definition = model.def(DefId::new(dense(index)));
      if definition.kind() != DefKind::Directive {
        continue;
      }
      let refused = definition
        .contributions()
        .next()
        .is_none_or(|own: Contribution<'_>| own.has_errors());
      if refused {
        continue;
      }
      cx.report(
        Diagnostic::builder(schema::SELF_REFERENTIAL_DIRECTIVE, definition.location())
          .message(format!(
            "the directive `@{}` refers to itself, through the directives and types it names",
            Excerpt(definition.name().as_bytes())
          ))
          .primary_label("directive definition refers to itself")
          .help(
            "a directive may not appear on its own definition or an extension of it, nor on any \
             type or directive they name.",
          )
          .build(),
      );
    }
  }
}

/// Writes into `reaches` what `definition`, of `model`, reaches in one step, `stack` being a
/// scratch stack of type occurrences.
fn successors(
  model: &Model,
  definition: Def<'_>,
  stack: &mut Vec<TypeExprId>,
  reaches: &mut Vec<u32>,
) {
  applied(model, definition.applications(), stack, reaches);
  if definition.kind() == DefKind::Directive {
    for argument in definition.arguments() {
      let member = model.member(argument);
      applied(model, member.applications(), stack, reaches);
      if let Some(ty) = member.ty() {
        named(model, ty, stack, reaches);
      }
    }
  } else {
    for field in definition.input_fields() {
      let member = model.member(field);
      applied(model, member.applications(), stack, reaches);
      if let Some(ty) = member.ty() {
        named(model, ty, stack, reaches);
      }
    }
    for value in definition.enum_values() {
      applied(model, model.member(value).applications(), stack, reaches);
    }
  }
  if !opaque(definition) {
    for param in definition.type_params() {
      if let Some(default) = param.default() {
        named(model, default, stack, reaches);
      }
    }
  }
}

/// Writes into `reaches` the directive each of `applications`, of `model`, resolved to, and every
/// definition its type arguments name, `stack` being a scratch stack of type occurrences.
fn applied(
  model: &Model,
  applications: &[ApplicationId],
  stack: &mut Vec<TypeExprId>,
  reaches: &mut Vec<u32>,
) {
  for id in applications {
    let application = model.application(*id);
    if let Resolution::Resolved(directive) = application.directive() {
      reaches.push(directive.get());
    }
    for argument in application.type_args() {
      named(model, *argument, stack, reaches);
    }
  }
}

/// Writes into `reaches` every definition the type occurrence `root`, of `model`, names at any
/// depth: each named occurrence's definition, then its arguments unless that definition hides
/// them, and a list's element. A type parameter, and an occurrence that did not resolve, name
/// nothing. `stack` is a scratch stack, emptied first.
fn named(model: &Model, root: TypeExprId, stack: &mut Vec<TypeExprId>, reaches: &mut Vec<u32>) {
  stack.clear();
  stack.push(root);
  while let Some(id) = stack.pop() {
    match model.type_expr(id).kind() {
      TypeExprKind::Named { def, args } => {
        reaches.push(def.get());
        if !opaque(model.def(def)) {
          stack.extend_from_slice(args);
        }
      }
      TypeExprKind::List { element } => stack.push(element),
      TypeExprKind::Param { .. } | TypeExprKind::Unresolved => {}
    }
  }
}

/// Whether `definition` hides its type arguments from the graph: a scalar that is no collection,
/// whose arguments say nothing of what its values hold.
fn opaque(definition: Def<'_>) -> bool {
  definition.kind() == DefKind::Scalar && definition.collection().is_none()
}

//! The roots the applications give, on a model built by hand that holds only what they read.

use std::{vec, vec::Vec};

use tokora::diagnostic::Location;

use super::application_roots;
use crate::graphqlx::{
  id::TyId,
  model::{DefKind, ModelBuilder, ModuleKind, Origin, PackageKind, PathRoot},
  package::{Layout, Tier},
};

/// How many applications share the one complete type.
const APPLICATIONS: usize = 64;

/// How many arguments that complete type has, each a type of its own: the slots a directive's
/// defaults fill for its bare applications.
const SLOTS: usize = 64;

/// Applications that share a complete type push it once between them, whole, and none of its
/// arguments, which the closure then reads once, when it reaches the type. Sixty-four applications
/// share a complete type of sixty-four arguments, as the bare applications of one directive share
/// the one type its defaults fill, which instantiation interns once: the roots leave that one type
/// to walk, so the applications cost their count and the slots theirs, and not the product of the
/// two. An application whose type did not complete, as when its directive takes no count it
/// writes, still roots the type argument it writes. Refuses the roots read argument by argument,
/// which leave the sixty-four arguments to walk in place of the type, after reading the shared
/// type's slots once for each application: 4,096 reads, where these make one push for each.
#[test]
fn applications_that_share_a_complete_type_push_it_once() {
  let mut builder = ModelBuilder::default();
  let package = builder.push_package("app", PackageKind::Root, Tier::Std, Layout::Flat, 1);
  let module = builder.push_module(package, None, None, ModuleKind::Root);
  let at = Location::entire(0);
  let tag = builder.intern("tag");
  let directive = builder.push_def(module, DefKind::Directive, tag, at, Origin::User);
  let name = builder.intern("S");
  let scalar = builder.push_def(module, DefKind::Scalar, name, at, Origin::User);

  // The slots: `S`, `[S]`, `[[S]]` and on, each a type of its own.
  let mut slots: Vec<TyId> = vec![builder.intern_named(scalar, &[], false)];
  while slots.len() < SLOTS {
    let last = *slots.last().expect("one slot at least");
    slots.push(builder.intern_list(last, false));
  }
  let complete = builder.intern_named(directive, &slots, false);
  for _ in 0..APPLICATIONS {
    let application = builder.push_application(at, PathRoot::Relative, &[tag], at);
    builder.resolve_application(application, directive);
    builder.set_application_ty(application, complete);
  }

  // An application whose type did not complete, which writes the type argument `S!`.
  let written = builder.intern_named(scalar, &[], true);
  let argument = builder.push_type_expr(at, true);
  builder.resolve_named(argument, scalar);
  builder.set_type_expr_ty(argument, written);
  let refused = builder.push_application(at, PathRoot::Relative, &[tag], at);
  builder.resolve_application(refused, directive);
  builder.set_application_type_args(refused, &[argument]);

  let mut seen: Vec<bool> = vec![false; builder.ty_count() as usize];
  let mut types: Vec<TyId> = Vec::new();
  application_roots(&builder, &mut seen, &mut types);
  assert_eq!(types, [complete, written]);
  assert_eq!(seen.iter().filter(|pushed: &&bool| **pushed).count(), 2);
}

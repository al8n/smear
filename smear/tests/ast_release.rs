//! Releasing a deeply nested AST node, which used to abort the process.
//!
//! # What was wrong
//!
//! Two shapes in the syntactic AST owned themselves recursively, and the `Drop` glue the compiler
//! generated for each descended **one native frame per level** with nothing at the bottom to stop
//! it:
//!
//! * `graphql::ast::Type` — and its `Rc` and `Arc` twins, and `graphqlx::ast::Type` — through
//!   `List(Box<ListType<Self>>)`, a single owned child;
//! * `graphql::ast::SelectionSet` — and GraphQLx's — through an inline fragment's required nested
//!   set and a field's optional one, a container of children.
//!
//! Neither needs a parse. `Type: From<ListType<Self>>` and every selection carrier's `new` are
//! public, so **the chain is built with a loop and no recursion at all**, exactly as the fixtures
//! below build it, and the parser's own nesting ceiling never sees it. Nothing measured the depth
//! on the way in, so nothing could refuse on the way out: releasing a node is not fallible, there
//! is no diagnostic to return, and the process aborts. Measured at `9f584d6` on
//! `aarch64-apple-darwin`, unoptimised, on the main thread's 8 MiB: a `Type` chain released 73 437
//! levels and aborted at 74 843; a `SelectionSet` chain released 22 812 and aborted at 24 218. A
//! 2 MiB test thread reaches both at about a quarter of that.
//!
//! # How this file can hold that without killing the runner
//!
//! It cannot, and it does not try. An abort is not a test failure — `SIGABRT` takes the harness
//! with it and no `#[should_panic]` sees a stack overflow — so **there is no red side in this
//! file**. Each test here builds a fixture far past the measured boundary and releases it as its
//! last act; a regression takes the whole file down loudly rather than passing quietly. The red
//! side was established out of suite, one process per depth, reading the child's exit status.
//!
//! `json_writer.rs` carries the same arrangement for the value tree (al8n/smear#165), and this
//! file is that finding's other half: the same defect, in the two shapes `value/nesting.rs`'s
//! header recorded as unrepaired when the value tree was fixed.
//!
//! # What is pinned, and what is not
//!
//! `Drop` is one of four generated impls that descend per level. The derived `Debug`, `Clone` and
//! `PartialEq` still do, on these nodes and on the value enums alike — `value/nesting.rs`'s header
//! measures the value ones. What the repair removes is the only one of the four **that fires
//! without a call being made**: release happens to whoever holds the node, on scope exit, on
//! unwind, in a collection's teardown, and it can be neither caught nor refused. So the tests below
//! release, and none of them clones or formats.
//!
//! # Why the fixtures pick their own stack
//!
//! Because the boundary is a property of the stack, and libtest's is not this file's to know.
//! Measured at `9f584d6` on `aarch64-apple-darwin`, **release**: on an 8 MiB main thread the type
//! chain aborted at 266 558 levels and the selection chain at 141 589, so a 100 000-level fixture
//! run there would have passed *before* the repair and proved nothing. On the 256 KiB thread these
//! tests spawn, the same binary aborted at 8 602 and 2 157. Sizing the stack is what makes
//! [`DEPTH`] a fixed multiple of the boundary instead of a bet on the runner — and it keeps the
//! fixtures cheap, since a release that no longer recurses does not care how deep they are.
//!
//! # Why the dialect gate is per fixture and not on the file
//!
//! Two CI rows build this package with **one dialect at a time**, and in each of them the other
//! dialect's `ast` module does not exist. A `#![cfg(all(feature = "graphql", feature =
//! "graphqlx"))]` header would satisfy both of those rows — by compiling a test binary holding
//! **zero** tests, which exits 0 and pins nothing. So the file's own gate stays at `parser` and
//! each fixture carries the dialect it actually names: seven run under `graphql` alone, two under
//! `graphqlx` alone, and ten with both. The tenth is `a_deep_graphqlx_list_type_is_released`,
//! which is the one fixture that genuinely needs the pair — and it is not linked here, because a
//! single-dialect row is a build in which that item does not exist. Its own documentation says
//! why it needs both.

#![cfg(feature = "parser")]
#![allow(missing_docs)]

use std::vec::Vec;

#[cfg(feature = "graphql")]
use std::{rc::Rc, sync::Arc};

use smear::lexer::tokora::SimpleSpan;
#[cfg(feature = "graphql")]
use smear::parser::graphql::ast as g;
#[cfg(feature = "graphqlx")]
use smear::parser::graphqlx::ast as x;

/// Forty-six times the depth at which the shallower of the two aborted on [`STACK`], and twelve
/// times the deeper one's.
const DEPTH: usize = 100_000;

/// The stack each fixture is built and released on.
///
/// Small on purpose, and small enough to be *decisive*: at `9f584d6` this size aborted at 8 602
/// levels of type and 2 157 of selection in a release build, so [`DEPTH`] is an order of magnitude
/// past both. It is also what the fixtures cost, since a release that does not recurse needs no
/// more of it at 100 000 levels than at one.
const STACK: usize = 256 * 1024;

fn span() -> SimpleSpan {
  SimpleSpan::new(0, 0)
}

/// Builds and releases a fixture on a stack this file sizes rather than one libtest hands out.
///
/// A regression aborts the process, which no assertion can soften and no `#[should_panic]` sees, so
/// the thread is not there to contain the failure — nothing can. It is there so the failure is
/// *reachable*: see the module header for the two boundaries this size sits between.
fn on_a_small_stack(fixture: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(STACK)
    .spawn(fixture)
    .expect("a fixture thread")
    .join()
    .expect("the fixture thread returned");
}

/// Releases the fixture, which is the whole of each gate below.
fn release<T>(owner: T) {
  drop(owner);
}

#[cfg(feature = "graphql")]
#[test]
fn a_deep_box_type_is_released() {
  on_a_small_stack(|| {
    let mut ty = g::Type::Name(g::NamedType::new(
      span(),
      g::Name::new(span(), "Leaf"),
      false,
    ));
    for _ in 0..DEPTH {
      ty = g::ListType::new(span(), ty, false).into();
    }
    release(ty);
  });
}

#[cfg(feature = "graphql")]
#[test]
fn a_deep_rc_type_is_released() {
  on_a_small_stack(|| {
    let mut ty = g::RcType::Name(g::NamedType::new(
      span(),
      g::Name::new(span(), "Leaf"),
      false,
    ));
    for _ in 0..DEPTH {
      ty = g::ListType::new(span(), ty, false).into();
    }
    release(ty);
  });
}

/// The shared arm, which is the one case the release answers `None` to and stops.
///
/// The clone holds every level alive, so releasing the original unlinks nothing; releasing the
/// clone afterwards is the last owner and runs the loop. Both halves have to survive, and the
/// order is the point — a release that descended on a *shared* pointer would be wrong as well as
/// deep.
#[cfg(feature = "graphql")]
#[test]
fn a_deep_arc_type_is_released_through_its_last_owner() {
  on_a_small_stack(|| {
    let mut ty = g::ArcType::Name(g::NamedType::new(
      span(),
      g::Name::new(span(), "Leaf"),
      false,
    ));
    for _ in 0..DEPTH {
      ty = g::ListType::new(span(), ty, false).into();
    }
    let shared = ty.clone();
    release(ty);
    release(shared);
  });
}

/// GraphQLx nests four ways, and the two reachable from outside this workspace are here.
///
/// `ListType` is nameable as `graphql::ast::ListType` — the same carrier, re-exported by the other
/// dialect — so a consumer with both features on can build this arm. `SetType` and `MapType` are
/// re-exported by neither, so their arms cannot be built outside the crate at all and their
/// release is pinned in `smear-parser`'s own `graphqlx::ast` tests instead.
#[cfg(all(feature = "graphql", feature = "graphqlx"))]
#[test]
fn a_deep_graphqlx_list_type_is_released() {
  on_a_small_stack(|| {
    let mut ty = graphqlx_leaf();
    for _ in 0..DEPTH {
      ty = x::Type::List(x::Nest::new(g::ListType::new(span(), ty, false)));
    }
    release(ty);
  });
}

/// The fourth route, and the one no reading of the three pointer arms finds: a path's generic
/// arguments are types, so `A<A<A<…>>>` nests without a `Box` anywhere in the cycle. Every
/// constructor it needs is public.
#[cfg(feature = "graphqlx")]
#[test]
fn a_deep_graphqlx_generic_argument_type_is_released() {
  on_a_small_stack(|| {
    let mut ty = graphqlx_leaf();
    for _ in 0..DEPTH {
      ty = x::Type::Path(x::DefinitionTypePath::new(
        span(),
        x::Path::new(span(), Vec::from([x::Name::new(span(), "Wrap")]), false),
        Some(x::TypeGenerics::new(span(), Vec::from([ty]).into())),
        false,
      ));
    }
    release(ty);
  });
}

#[cfg(feature = "graphqlx")]
fn graphqlx_leaf() -> x::Type<&'static str> {
  x::Type::Path(x::DefinitionTypePath::new(
    span(),
    x::Path::new(span(), Vec::from([x::Name::new(span(), "Leaf")]), false),
    None,
    false,
  ))
}

/// A field's optional nested set, which is how a query nests.
#[cfg(feature = "graphql")]
#[test]
fn a_deep_field_selection_set_is_released() {
  on_a_small_stack(|| {
    let mut set = g::SelectionSet::new(
      span(),
      Vec::from([g::Selection::Field(g::Field::new(
        span(),
        None,
        g::Name::new(span(), "leaf"),
        None,
        None,
        None,
      ))])
      .into(),
    );
    for _ in 0..DEPTH {
      set = g::SelectionSet::new(
        span(),
        Vec::from([g::Selection::Field(g::Field::new(
          span(),
          None,
          g::Name::new(span(), "nest"),
          None,
          None,
          Some(set),
        ))])
        .into(),
      );
    }
    release(set);
  });
}

/// An inline fragment's **required** nested set, which is the arm smear issue #61 was raised on and
/// the one a `None` in the field arm would let a repair miss.
#[cfg(feature = "graphql")]
#[test]
fn a_deep_inline_fragment_selection_set_is_released() {
  on_a_small_stack(|| {
    let mut set = g::SelectionSet::new(
      span(),
      Vec::from([g::Selection::Field(g::Field::new(
        span(),
        None,
        g::Name::new(span(), "leaf"),
        None,
        None,
        None,
      ))])
      .into(),
    );
    for _ in 0..DEPTH {
      set = g::SelectionSet::new(
        span(),
        Vec::from([g::Selection::InlineFragment(g::InlineFragment::new(
          span(),
          None,
          None,
          set,
        ))])
        .into(),
      );
    }
    release(set);
  });
}

#[cfg(feature = "graphqlx")]
#[test]
fn a_deep_graphqlx_selection_set_is_released() {
  on_a_small_stack(|| {
    let mut set = x::SelectionSet::new(
      span(),
      Vec::from([x::Selection::Field(x::Field::new(
        span(),
        None,
        x::Name::new(span(), "leaf"),
        None,
        None,
        None,
      ))])
      .into(),
    );
    for _ in 0..DEPTH {
      set = x::SelectionSet::new(
        span(),
        Vec::from([x::Selection::InlineFragment(x::InlineFragment::new(
          span(),
          None,
          None,
          set,
        ))])
        .into(),
      );
    }
    release(set);
  });
}

/// A chain whose levels are *shared*, so nothing but the last owner unlinks anything.
///
/// `Rc` and `Arc` are the two pointers whose release can legitimately stop early, and stopping
/// early is correct: a level with another owner has nothing below it to unlink yet. The clone is
/// taken level by level as the chain grows, so every level really is shared, and both owners are
/// then released.
#[cfg(feature = "graphql")]
#[test]
fn a_deep_rc_type_shared_at_every_level_is_released() {
  on_a_small_stack(|| {
    let mut ty = g::RcType::Name(g::NamedType::new(
      span(),
      g::Name::new(span(), "Leaf"),
      false,
    ));
    let mut owners = Vec::with_capacity(DEPTH);
    for _ in 0..DEPTH {
      ty = g::ListType::new(span(), ty, false).into();
      owners.push(ty.clone());
    }
    release(ty);
    release(owners);
  });
}

/// Keeps the pointer types named in a test file that is about how they are released.
#[cfg(feature = "graphql")]
#[test]
fn the_three_pointer_families_are_distinct_types() {
  let boxed: g::Type<g::Name<&str>> = g::ListType::new(
    span(),
    g::Type::Name(g::NamedType::new(span(), g::Name::new(span(), "T"), false)),
    false,
  )
  .into();
  let counted: g::RcType<g::Name<&str>> = g::ListType::new(
    span(),
    g::RcType::Name(g::NamedType::new(span(), g::Name::new(span(), "T"), false)),
    false,
  )
  .into();
  let atomic: g::ArcType<g::Name<&str>> = g::ListType::new(
    span(),
    g::ArcType::Name(g::NamedType::new(span(), g::Name::new(span(), "T"), false)),
    false,
  )
  .into();
  assert!(boxed.is_list() && counted.is_list() && atomic.is_list());
  // The by-value unwraps `derive_more` generates are still generated, which is what putting the
  // release on the arm rather than on the enum bought: `E0509` never reaches these types.
  let _: Rc<()> = Rc::new(());
  let _: Arc<()> = Arc::new(());
  assert!(boxed.unwrap_list().ty().is_name());
  assert!(counted.unwrap_list().ty().is_name());
  assert!(atomic.unwrap_list().ty().is_name());
}

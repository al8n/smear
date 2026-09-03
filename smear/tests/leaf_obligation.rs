//! What a broken [`Leaf`] obligation costs, measured rather than asserted.
//!
//! [`Leaf`] is a marker a caller implements, so it records a claim and cannot check one. This file
//! is the other half of that sentence: a caller who declares a source representation a leaf when
//! it owns a node of one of this crate's trees gets `al8n/smear#176`'s abort back, unchanged and
//! at the same depth. The
//! bound moved *who is answerable*; it did not make the abort unreachable, and a repair that
//! claimed otherwise would be a promise relocated rather than kept.
//!
//! The same line runs under the guarantee itself: what the crate promises is that **its** release
//! adds O(1) native frames per node. A payload's own destructor costs what its holder shaped it
//! to cost — an honest `Leaf` may still be a deep linked list — and that cost is paid once per
//! node rather than multiplied by the tree's depth. `Leaf` bounds what a payload may *reach*,
//! never how its own `Drop` is shaped.
//!
//! # Why a child process, and why not `#[should_panic]`
//!
//! Because a stack overflow is not a panic. Rust's handler prints and calls `abort()`, so the
//! process takes `SIGABRT` and no unwinding happens: `#[should_panic]` never sees it, `catch_unwind`
//! never sees it, and a test that provoked it in-process would take the whole runner down. The
//! only place the outcome is observable is a *second* process's exit status, which is what these
//! two cells read. `smear/tests/ast_release.rs` records the same arrangement for the shapes the
//! grammar forms; this is the caller-payload half.
//!
//! # The control is not decoration
//!
//! A child that dies for any reason at all satisfies "the child died". A broken filter, a renamed
//! test, an unset environment variable and a missing binary all produce a non-zero status, and
//! every one of them would leave the lying cell green while never reaching the release. So the two
//! cells differ in exactly one thing — the source representation — and the honest one must exit
//! **0** through the same spawn, the same filter and the same depth. The lying one must die by a
//! **signal**, which a failed spawn or a panicking child does not do.
//!
//! # Why the payload picks its own stack
//!
//! The boundary is a property of the stack, and libtest's is not this file's to know. Measured on
//! `aarch64-apple-darwin`, unoptimised, on a chain threaded through the `Variable` arm and the
//! process's default main-thread stack: 20 000 released and 100 000 aborted. The 256 KiB thread
//! below puts [`DEPTH`] two orders of magnitude past the boundary on any host, so the cell states
//! the *shape* rather than betting on the runner's stack.

#![cfg(all(feature = "parser", feature = "graphql"))]
#![allow(missing_docs)]

use std::{boxed::Box, string::String, vec::Vec};

use smear::{
  lexer::tokora::SimpleSpan,
  parser::graphql::ast::{InputValue, Leaf, Name, VariableValue},
};

/// Two orders of magnitude past the measured boundary on [`STACK`], and the depth the issue
/// reported the abort at.
const DEPTH: usize = 100_000;

/// A quarter-megabyte, so the boundary is this file's and not the runner's.
const STACK: usize = 256 * 1024;

/// Selects the payload in the re-executed child. Absent in the parent.
const ROLE: &str = "SMEAR_LEAF_OBLIGATION_ROLE";

/// The child's exit code when the release it was spawned to die in *returned*.
const SURVIVED: i32 = 70;

/// The child's exit code when the fixture it built was not the depth this file claims.
///
/// It needs a code of its own because the obvious spelling — `assert_eq!` on the depth — is
/// **masked by the very abort this cell is looking for**: a failed assertion unwinds, unwinding
/// drops the chain, dropping the chain overflows the stack, and the parent reads a signal death
/// and calls it a pass. So the mismatch path forgets the chain and exits instead of panicking,
/// and the parent names this code separately.
const BAD_FIXTURE: i32 = 71;

/// A source representation that owns a value tree, with the one line that declares it a leaf.
///
/// **The declaration is false**, and this type exists to be the counterexample: `Leaf` says
/// dropping a value runs no destructor that reaches a node of any of this crate's trees, and
/// dropping one of these
/// reaches `DEPTH` of them.
struct Lying(Option<Box<InputValue<Lying>>>);

impl Leaf for Lying {}

/// The same shape over the `S` every shipped entry point uses. `&str` owns no node, so this
/// chain is one level deep however many times the loop runs.
type Honest = &'static str;

fn span() -> SimpleSpan {
  SimpleSpan::new(0, 1)
}

/// Builds the issue's chain with a loop — no recursion on the way in, which is the whole point:
/// nothing measured the depth, so nothing can refuse on the way out.
fn lying_chain(depth: usize) -> Lying {
  let mut cur = Lying(None);
  for _ in 0..depth {
    let name: Name<Lying> = Name::new(span(), cur);
    let value: InputValue<Lying> = InputValue::Variable(VariableValue::new(span(), name));
    cur = Lying(Some(Box::new(value)));
  }
  cur
}

/// Walks the chain iteratively and returns how deep it actually is.
///
/// Not decoration, and not there to silence `dead_code`: it is what proves the loop above built
/// `DEPTH` levels rather than one. A cell that aborts is indistinguishable from a cell that
/// aborted *for a different reason* unless the fixture's own size is established first, and this
/// walk establishes it through the same public accessors — [`VariableValue::name`] and
/// [`Name::source`] — the chain was assembled through.
fn depth_of(chain: &Lying) -> usize {
  let mut node = chain;
  let mut levels = 0;
  while let Some(inner) = node.0.as_deref() {
    let InputValue::Variable(variable) = inner else {
      break;
    };
    node = variable.name().source();
    levels += 1;
  }
  levels
}

/// The control's chain: the same three calls, the same count, an `S` that owns nothing. Held in a
/// `Vec` so the release is the same *number* of nodes rather than the same shape — a `&str` cannot
/// be made to nest, which is exactly why it is the control.
fn honest_chain(depth: usize) -> Vec<InputValue<Honest>> {
  (0..depth)
    .map(|_| {
      let name: Name<Honest> = Name::new(span(), "userId");
      InputValue::Variable(VariableValue::new(span(), name))
    })
    .collect()
}

fn on_a_small_stack(payload: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(STACK)
    .spawn(payload)
    .expect("spawn")
    .join()
    .expect("join");
}

/// Runs one cell's payload in a child process and returns what the child's exit looked like.
fn run_child(role: &str, test: &str) -> std::process::Output {
  std::process::Command::new(std::env::current_exe().expect("current_exe"))
    .args(["--exact", test, "--nocapture", "--test-threads=1"])
    .env(ROLE, role)
    .output()
    .expect("spawn the child test binary")
}

fn died_by_signal(output: &std::process::Output) -> bool {
  #[cfg(unix)]
  {
    use std::os::unix::process::ExitStatusExt as _;
    return output.status.signal().is_some();
  }
  #[cfg(not(unix))]
  {
    // No signals to read; the abort still cannot be mistaken for the child's own clean exit or
    // for the `SURVIVED` code it would have returned had the release come back.
    !output.status.success() && output.status.code() != Some(SURVIVED)
  }
}

/// THE RESIDUAL. A caller who took the obligation and lied gets the abort back.
#[test]
fn a_lying_leaf_impl_still_aborts_the_process() {
  if std::env::var_os(ROLE).is_some() {
    on_a_small_stack(|| {
      let chain = lying_chain(DEPTH);
      if depth_of(&chain) != DEPTH {
        core::mem::forget(chain);
        std::process::exit(BAD_FIXTURE);
      }
      drop(chain);
    });
    std::process::exit(SURVIVED);
  }

  let output = run_child("lying", "a_lying_leaf_impl_still_aborts_the_process");
  let code = output.status.code();
  assert!(
    code != Some(BAD_FIXTURE),
    "the child built a chain that was not {DEPTH} levels deep, so whatever it did next says \
     nothing about a release at that depth"
  );
  assert!(
    code != Some(SURVIVED),
    "the release returned: `Lying`'s chain no longer recurses, so this cell's claim — that a \
     broken obligation is still a broken obligation — has stopped being true and the residual \
     recorded on `Leaf` needs re-deriving"
  );
  assert!(
    died_by_signal(&output),
    "the child exited {code:?} rather than dying by a signal, so it did not reach the release. \
     stderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );
}

/// THE CONTROL. The same spawn, the same filter, the same depth, one thing different.
#[test]
fn the_same_cell_over_a_borrowed_source_exits_cleanly() {
  if std::env::var_os(ROLE).is_some() {
    on_a_small_stack(|| drop(honest_chain(DEPTH)));
    return;
  }

  let output = run_child(
    "honest",
    "the_same_cell_over_a_borrowed_source_exits_cleanly",
  );
  assert!(
    output.status.success(),
    "the control child exited {:?}, so a non-zero status in the cell above proves nothing about \
     the release. stderr:\n{}",
    output.status.code(),
    String::from_utf8_lossy(&output.stderr)
  );
}

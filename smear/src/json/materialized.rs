//! [`WriteJson`] for the materialised value tree, at every width it is read at.
//!
//! # Why the constant tree and not its sibling
//!
//! The materialised AST declares two enums — `InputValue`, which can hold a `Variable`, and
//! `ConstInputValue`, which cannot. **Only the constant one is implemented**, and the reason is
//! not tidiness: a response has no variables in it. Draft §6.1 coerces every variable before
//! execution begins, so a `$name` reaching a response value would mean a value that was never
//! resolved, and there is no JSON for it — writing the name would invent a string the schema
//! never promised, and writing `null` would answer a question nobody asked. An unimplemented trait
//! is a compile error at the site that tried; either alternative is a wrong response.
//!
//! # One body, every width
//!
//! This was a macro at two instantiations, because the tree was two enums at two widths. It is
//! **one `impl`** now: `ConstInputValue<S, I>` is one type, and the only thing the widths ever
//! disagreed about — how the integer reaches [`Json::int_leaf`], which takes an [`i64`] — is
//! `I: Into<i64>`, a bound rather than a macro argument. Both widths satisfy it, and no third
//! payload can reach this impl at all, because the parser's `MaterializedInt` is sealed to those
//! two.
//!
//! `Frame` was already outside the macro because it did not differ, and it stays a free
//! declaration instantiated at whatever element and field types the width picks.
//!
//! # Where the widths show
//!
//! [`Json::int_leaf`] renders an integer outside draft §3.5.1's 32-bit range as a JSON string. At
//! `I = i32` the tree cannot produce one — its parser refused the literal, as
//! `ErrorData::IntOverflow` — so for that width the branch is unreachable and every `Int` in the
//! response is a JSON number. At `i64` it can, and `2147483648` comes out as `"2147483648"`. That
//! is the permissive reading and the specification's, told apart in the response rather than only
//! in the parse. **Nothing here branches on the width to do it**: the body branches on the value,
//! which is why one impl serves both and why the `i32` claim is a fact about the parser rather
//! than about this file.
//!
//! # A value is walked on an explicit stack, and the response's depth does not bound it
//!
//! This walk used to recurse, one native frame per level of nesting, and the reason that was fatal
//! is not the reason `response::write_node` recursing was. There, the depth was
//! bounded by ceilings nobody had counted native frames against. **Here it is bounded by nothing at
//! all.** A `ConstInputValue` inside a leaf slot, or under an `extensions` key, is the driver's
//! own object: [`Response::depth`](graphql_proto::Response::depth) counts positions in the response
//! tree and a value is *one* position however deep it is, so a response with a single depth-zero
//! leaf can carry a value ten thousand lists deep. Nothing between the parser and the sink has ever
//! looked inside one. Every constructor the tree needs is public, so building one is safe code, and
//! §7.1.3's request-error result reaches this walk through `extensions` without executing anything
//! at all.
//!
//! Measured before the repair, on the harness's default test-thread stack: a value **3 867** lists
//! deep wrote and **3 906** aborted the process with `SIGABRT`. All three routes into this walk —
//! the trait method, an execution result's `extensions`, and a request-error result's — abort at a
//! depth of 100 000 after **3 001 bytes** of the response have already gone to the sink, which is
//! the same reading on each of them because it is the same walk.
//!
//! # Why the frames grow amortised here and exactly in the response walk
//!
//! `response::write_node` reserves its stack **once and exactly**, from
//! `Response::depth` — a number the executor *recorded* while building the tree, when the path was
//! already in hand. This walk cannot have that, and the reason is worth stating because it is what
//! rules out the obvious alternative:
//!
//! **Nothing has measured a value's depth, and measuring one costs the thing the measurement would
//! bound.** A `ConstInputValue` is an owned tree with no parent links, walked through `&`, so any
//! traversal of it has to remember the siblings it has not taken on the path down — there is
//! nowhere to keep them but a stack as deep as the tree. A walk to measure the depth is therefore
//! a walk with the very structure it was going to size, and "measure, then reserve exactly" buys
//! one allocation with a second full traversal and the same growth. It is strictly worse than
//! growing once.
//!
//! So the frames grow **amortised and fallibly**, through the one `push` below, which is
//! [`Path::collect_into`](graphql_proto::Path::collect_into)'s choice one crate down and for the
//! same underlying reason: an exact reservation needs a number you did not have to walk for. What
//! it costs is exactness: a container-valued leaf `d` levels deep pays about log₂ `d` allocating
//! events instead of one — measured, five at 64 levels and nine at 1 024 — and they are freed when
//! the walk ends. A **scalar** leaf pays none at all, because `Vec::new` does not allocate, and a
//! scalar is what a leaf is unless the schema declares a composite custom scalar.
//!
//! # What was NOT chosen, and why refusing a legal value is worse than allocating for one
//!
//! A fixed nesting ceiling — a stack array, no heap, refuse past *N* — is the other shape that
//! bounds the native stack, and it was rejected. There is no *N* any layer here can derive: draft
//! §7.1 does not bound a value's nesting, `graphql-proto`'s ceilings bound the **response** and a
//! driver's value is not part of it, and the schema says nothing about the shape of a custom
//! scalar's payload. A writer that refused a value at depth 129 would be refusing a legal response
//! on a rule it invented, and it would do it after part of the body had gone to the client. The
//! allocator refusing is a different thing: it is a fact about the machine at that moment, and
//! [`Error::Allocation`] is this module's word for it everywhere else.
//!
//! # This bounds the writer, and the release is bounded separately
//!
//! Stated plainly because the difference was easy to lose, and for a while only one half of it
//! held: what is repaired *here* is that **writing** a value no longer spends a native frame per
//! level. That left the value itself a hazard — `ConstInputValue`'s **derived** `Drop` glue
//! recursed, so *releasing* a deeply nested one aborted the process whether or not this module
//! ever touched it (measured then: 7 734 lists deep dropped, 7 773 aborted, with the writer never
//! called). It was a property of the AST type and not of the writer, and it got its own change:
//! `smear_parser::value::nesting`, a hand-written `Drop` on the value enums that moves their
//! children onto a heap worklist.
//!
//! So the two bounds are independent and both hold, and the gates say so rather than working
//! around one of them: `super::tests`'s `dismantle` now simply drops its argument, and
//! `tests/json_writer.rs`'s three deep fixtures release what they built instead of leaking it.

use core::{fmt, slice};

use std::vec::Vec;

use smear_parser::graphql::ast::materialized;

use super::{Error, Json, WriteJson};

/// One container a value walk has opened and not yet closed.
///
/// The whole content of a frame is a cursor over the slice the container already owns, plus which
/// bracket closes it and whether a `,` is due — the same shape, and for the same reason, as
/// `response::write_node`'s own frame: a stack of
/// [`Array`](super::Array)/[`Object`](super::Object) scoped writers is not expressible, because
/// each one borrows the [`Json`] the enclosing one has already borrowed.
///
/// Generic over the element and field types rather than declared inside the macro, because it is
/// the part of the walk the two widths agree about. Declaring it inside would also collide: the
/// macro expands twice into this one module.
enum Frame<'v, Element, Field> {
  /// A list, and the elements it has left to write.
  List {
    rest: slice::Iter<'v, Element>,
    /// Whether the next element is the first, and so is not preceded by a `,`.
    first: bool,
  },
  /// An object, and the fields it has left to write.
  Object {
    rest: slice::Iter<'v, Field>,
    /// Whether the next field is the first.
    first: bool,
  },
}

/// Opens one more level of nesting, asking the allocator for room rather than assuming it.
///
/// **The one door this walk grows through**, so that "the frames are the only thing a value walk
/// allocates, and a refusal is [`Error::Allocation`] rather than an abort" is a claim about one
/// function. `Vec::push` would call the allocation-error handler instead, which kills the process
/// holding a response that is already half written — the exact failure this walk was rewritten to
/// stop having.
///
/// `try_reserve` and not `try_reserve_exact`: see the module header for why an exact reservation is
/// not available here and why buying one with a measuring walk is worse than not having it.
fn push<'v, Element, Field>(
  stack: &mut Vec<Frame<'v, Element, Field>>,
  frame: Frame<'v, Element, Field>,
) -> Result<(), Error> {
  stack.try_reserve(1).map_err(|_| Error::Allocation)?;
  stack.push(frame);
  Ok(())
}

impl<S, I> WriteJson for materialized::ConstInputValue<S, I>
where
  S: AsRef<str>,
  // `Copy` because the leaf hands out a `&I` and [`Json::int_leaf`] wants the value; `Into<i64>`
  // because that is the widest integer JSON's number branch is written against, and it is the
  // whole of what the two widths disagree about. The parser's `MaterializedInt` is sealed to `i32`
  // and `i64`, so nothing else reaches this impl however permissive these two bounds read.
  I: Copy + Into<i64>,
{
  fn write_json<W: fmt::Write>(&self, out: &mut Json<W>) -> Result<(), Error> {
    // One frame per open container, on the heap, so that the native stack does not grow with a
    // depth nothing measured. Empty until a container is met: a scalar value allocates nothing.
    let mut stack: Vec<Frame<'_, Self, materialized::ConstObjectField<S, I>>> = Vec::new();
    let mut node = self;

    loop {
      match node {
        Self::Null(_) => out.null()?,
        Self::Boolean(value) => out.bool(value.value())?,
        // The leaf keeps its source slice, escapes and all — that is what makes
        // materialisation allocate nothing — so the value is cooked on the way out.
        Self::String(value) => out.graphql_string(value.source().as_ref())?,
        // Draft §6.4.3 serialises an enum value as its member name, which is a `String` in the
        // response.
        Self::Enum(value) => out.string(value.source().as_ref())?,
        Self::Int(value) => out.int_leaf((*value.source()).into())?,
        Self::Float(value) => out.double(*value.source())?,
        Self::List(list) => {
          out.punct('[')?;
          push(
            &mut stack,
            Frame::List {
              rest: list.values().iter(),
              first: true,
            },
          )?;
        }
        Self::Object(object) => {
          out.punct('{')?;
          push(
            &mut stack,
            Frame::Object {
              rest: object.fields().iter(),
              first: true,
            },
          )?;
        }
      }

      // Climb back out of every container that has nothing left, then take one step into the
      // next value. An empty stack is the whole value written.
      node = loop {
        let Some(top) = stack.last_mut() else {
          return Ok(());
        };
        let object = matches!(top, Frame::Object { .. });
        // Everything the frame is asked for is taken in one step, so the borrow of the stack
        // ends before the body below can pop it. The references this yields borrow the value,
        // not the stack, which is what makes that possible.
        let step = match top {
          Frame::List { rest, first } => rest
            .next()
            .map(|element| (core::mem::replace(first, false), None, element)),
          Frame::Object { rest, first } => rest.next().map(|field| {
            (
              core::mem::replace(first, false),
              Some(field.name()),
              field.value(),
            )
          }),
        };

        let Some((first, key, value)) = step else {
          stack.pop();
          out.punct(if object { '}' } else { ']' })?;
          continue;
        };

        if !first {
          out.punct(',')?;
        }
        if let Some(key) = key {
          out.string(key.source().as_ref())?;
          out.punct(':')?;
        }
        break value;
      };
    }
  }
}

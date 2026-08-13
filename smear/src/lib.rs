#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![deny(missing_docs)]

// THE ALIAS IS BACK, AND `json` IS WHY. It left when this crate lost its last module — every
// member carries its own, and an umbrella that forwards a `no_std` stack must not itself link
// `std` — and the note that replaced it said `json` did not bring it back "because it writes into
// a caller's `core::fmt::Write` and allocates nothing at all". That sentence was true of the
// module as first written and is no longer true of it: both of the writer's walks over
// attacker-shaped input now run on the heap rather than on the native stack or on a rescan, which
// is two `Vec`s and the reason for each is in `json::response`. The claim it replaces is in
// `json`'s own header, restated as what the two allocations cost rather than deleted.
//
// Gated on `proto`, which is the feature that compiles the module: `#[cfg]`d rather than
// unconditional so that a lexer-only or parser-only build — neither of which has anything to
// allocate — still links neither `alloc` nor `std` through this file. Every member spells the
// alias the same way and for the same reason, and the `#[allow]` is the workspace-wide exception
// `unused_extern_crates = "deny"` requires: the users are feature-gated, so a configuration that
// compiles none of them would otherwise fail on the item itself.
#[cfg(all(feature = "proto", not(feature = "std")))]
#[allow(unused_extern_crates)]
extern crate alloc as std;

#[cfg(all(feature = "proto", feature = "std"))]
#[allow(unused_extern_crates)]
extern crate std;

// Deliberately no outer doc comment, unlike the modules below. Rustdoc resolves the MERGED
// fragments of a module's documentation in the scope of whichever attribute came from outside, so
// an outer comment here would reinterpret every link in the module's own header as one rooted in
// `smear` — sixteen `unresolved link` errors under `RUSTDOCFLAGS="-D warnings"`, all of them for
// items that are right there in the module. `validator/mod.rs` pays for its outer comment by
// spelling every link crate-absolute; this module does not, because it is written to be lifted
// into `tokora` unchanged and a `crate::`-rooted link is the one thing in it that would not
// survive the move. The module's own header supplies the summary line the crate index shows.
//
// It lives in `smear-schema` now, not because it is about schemas but because that is the LOWEST
// crate that needs it: `schema::error` and `schema::introspection::error` implement `Diagnose`.
// The re-export is what keeps `smear::diagnostic` the ungated path it has always been, and it is
// why `smear-schema` is an unconditional dependency below rather than one behind `validator`.
pub use smear_schema::diagnostic;

/// Lexers for GraphQL and GraphQL-like DSLs.
///
/// Turns source text into zero-copy tokens over `&str`, `&[u8]`, `bytes::Bytes`,
/// `hipstr::{HipStr, HipByt}` and friends.
///
/// Each dialect exposes two token streams:
///
/// - `syntactic` — skips trivia (whitespace, commas, comments). For servers and
///   query execution, where speed matters and formatting does not.
/// - `lossless` — preserves every byte of trivia. For formatters, linters, IDEs and
///   syntax highlighters, where the source must round-trip exactly.
///
/// A dialect is a module only in a build that enables its feature, so each link below is gated
/// to the build that has something to link to. Ungated, they are `unresolved link` errors under
/// `RUSTDOCFLAGS="-D warnings"` in every single-dialect configuration — the failure the `docs`
/// CI job's dialect-alone legs exist to catch.
///
/// This module is the crate's irreducible base and carries no feature of its own: the parser
/// cannot exist without it, and a build with both switched off would be an empty crate. The
/// cross-reference below is gated for the same reason the dialect links are — `parser` is a
/// module only in a build that enables its feature, and an ungated link to it is an
/// `unresolved link` error under `RUSTDOCFLAGS="-D warnings"` in every lexer-alone build.
///
#[cfg_attr(
  feature = "parser",
  doc = "See [`parser`] for the layer built on top of it."
)]
/// Available dialects:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](lexer::graphql)")]
#[cfg_attr(feature = "graphqlx", doc = "- [`graphqlx`](lexer::graphqlx)")]
///
/// The layer is the `smear-lexer` crate; this is the name it has always had inside `smear` and
/// the path every consumer already writes.
#[doc(inline)]
pub use smear_lexer as lexer;

/// Declares a keyword token type for a dialect.
///
/// Re-exported so `smear::keyword!` keeps naming the macro it named before the lexer became its
/// own crate. `#[macro_export]` puts a macro at its defining crate's root, so the split would
/// otherwise have moved this one out from under every caller.
pub use smear_lexer::keyword;

/// Parsers for GraphQL and GraphQL-like DSLs.
///
/// Atomic parser combinators that build AST nodes from the token streams produced by [`lexer`],
/// composed with the `tokora` combinator library, plus the lossless CST tower behind the `rowan`
/// feature.
///
/// Available dialects, each link gated on its feature for the reason recorded on [`lexer`]:
///
#[cfg_attr(feature = "graphql", doc = "- [`graphql`](parser::graphql)")]
#[cfg_attr(
  feature = "graphqlx",
  doc = "- [`graphqlx`](parser::graphqlx) — adds imports, generics, where-clauses, map and set \
         types, and namespaced paths on top of GraphQL."
)]
///
/// The layer is the `smear-parser` crate; this is the name it has always had inside `smear` and
/// the path every consumer already writes.
#[cfg(feature = "parser")]
#[cfg_attr(docsrs, doc(cfg(feature = "parser")))]
#[doc(inline)]
pub use smear_parser as parser;

/// Declares a typed keyword atom.
///
/// Re-exported for the reason [`keyword!`](keyword) is: `#[macro_export]` puts a macro at its
/// defining crate's root, so `smear::typed_keyword_atom!` would otherwise have moved out from
/// under every caller when the parser became its own crate.
#[cfg(feature = "parser")]
#[cfg_attr(docsrs, doc(cfg(feature = "parser")))]
pub use smear_parser::typed_keyword_atom;

/// Declares a typed wrapper over a lossless CST node.
///
/// Re-exported for the reason [`keyword!`](keyword) is. Gated on `rowan` rather than on `parser`
/// because the module that defines it is: a `#[macro_export]` macro exists exactly when the file
/// holding it is compiled.
#[cfg(feature = "rowan")]
#[cfg_attr(docsrs, doc(cfg(feature = "rowan")))]
pub use smear_parser::ast_node;

/// GraphQL document validation, and the built-once schema every rule reads.
///
/// Draft §3 "Type Validation" runs inside [`Schema::build`](validator::Schema::build), so a
/// malformed SDL is refused once at startup rather than rediscovered per request.
///
#[cfg_attr(
  feature = "introspection",
  doc = "A schema can also be built from a server's draft §4 introspection response — \
         [`Schema::from_introspection`](validator::Schema::from_introspection), behind the \
         `introspection` feature. It is a second entrance to the same value: it renders the \
         response as SDL and hands it to the same builder, so draft §3 runs there too and the \
         two doors cannot drift apart."
)]
#[cfg_attr(
  not(feature = "introspection"),
  doc = "The `introspection` feature adds a second construction door, building the same schema \
         out of a server's draft §4 introspection response instead of out of SDL."
)]
///
/// Standard GraphQL only —
#[cfg_attr(
  feature = "graphqlx",
  doc = "[`graphqlx`](parser::graphqlx) has no validator, deliberately: its generics, \
         `where` constraints, map and set types and namespaced paths have no specification \
         semantics, so validating them would be language design rather than conformance."
)]
#[cfg_attr(
  not(feature = "graphqlx"),
  doc = "the GraphQLx dialect has no validator, deliberately: its extensions have no \
         specification semantics to validate against."
)]
///
/// The layer is the `smear-compiler` crate; `validator` is the name it has always had inside
/// `smear` and the path every consumer already writes.
#[cfg(feature = "validator")]
#[cfg_attr(docsrs, doc(cfg(feature = "validator")))]
#[doc(inline)]
pub use smear_compiler as validator;

/// Draft §6 execution, as a Sans-I/O state machine.
///
/// Runs a validated query against a built [`Schema`](validator::Schema) and produces a draft §7.1
/// response, performing no I/O and calling no resolver: it says which field it needs and waits to
/// be told the answer.
///
/// It defines no value type. The driver keeps values in its own representation and answers
/// structural questions about them through [`Values`](proto::Values) — the same choice [`lexer`]
/// makes about its source, made for the same reason: an owned enum would force an allocation per
/// leaf and would make a wasm or FFI handle second-class.
///
/// Requires `validator`, because execution is entered with a document the draft §5 rules have
/// already accepted.
///
/// The layer is the `graphql-proto` crate; this is the name it has always had inside `smear` and
/// the path every consumer already writes.
#[cfg(feature = "proto")]
#[cfg_attr(docsrs, doc(cfg(feature = "proto")))]
#[doc(inline)]
pub use graphql_proto as proto;

// THE ONE MODULE THIS CRATE OWNS, and it is here rather than one layer down for a stated reason.
//
// `graphql-proto` refuses to write a response out because writing one means writing the driver's
// `V`, and `Values` asks seven structural questions of which none is "write this". The
// materialised value layer is what closed that: with a concrete value tree there is something to
// write, and the writer belongs ABOVE both — over `proto` for the response and over `parser` for
// the values — which is this crate and no other. Neither layer grows a question it should not
// have.
//
// Gated on `proto` alone. The materialised implementations inside it need
// `materialized-numbers` too and carry their own `#[cfg]`, so the module and the number formatters
// are in a `proto` build whether or not the value trees are, and `cargo hack --each-feature`
// compiles it in the `proto` cell rather than only under `--all-features`. NO NEW FEATURE: a
// `json` feature would be a fourteenth cell in each of `ci/hack.sh`'s two passes and a pair
// `ci/feature_reachability.py` would have to be told about, to gate a module that is already
// exactly as reachable as the crate it writes.
#[cfg(feature = "proto")]
#[cfg_attr(docsrs, doc(cfg(feature = "proto")))]
pub mod json;

#[doc(hidden)]
pub mod __private {
  pub use tokora;
}

// ── THE UMBRELLA'S FEATURES ARE THE GATE, AND THIS IS WHAT MAKES THAT TRUE ───────────────────
//
// A feature of `smear` gates what it claims ONLY where this file writes the `#[cfg]`. `parser`,
// `validator` and `proto` do — they gate the `pub use` statements above, and a second dependency
// naming the member directly cannot conjure the module. Every other feature here merely FORWARDS
// to a member feature, and the `#[cfg]` that decides the item then lives inside the member, where
// cargo unifies it across the whole graph.
//
// Measured on this tree, not argued. A consumer with
//
//     smear        = { default-features = false, features = ["std", "parser"] }
//     smear-parser = { path = … }            # a second dependency, at its own defaults
//
// compiled `pub use smear::parser::graphqlx;` — a path it had never enabled — and the same source
// with the second dependency removed failed with `unresolved import`. Ten of ten forwarded pairs
// behaved that way; the four that smear `#[cfg]`s itself did not. `ci/downstream.sh` is that
// experiment, kept.
//
// A FACADE CANNOT CLOSE IT. `pub mod parser { #[cfg(feature = "graphqlx")] pub use … }` would fix
// the root path and reach neither the nested gates (`parser::graphql::lossless` is `rowan`'s,
// three levels down) nor the trait impls, which are not namespaced at all — `smear/bytes` gates
// `impl Source for Bytes`, and no re-export can hide an impl.
//
// So the equivalence is asserted instead, and a graph that violates it does not compile. Each
// member publishes its resolved features as constants and this block requires them to agree with
// smear's own. The wrong state is unrepresentable rather than merely discouraged, and the error
// names the feature and the two ways out.
//
// WHAT IT COSTS, stated because it is a real cost: a consumer who deliberately wants
// `smear-parser/graphqlx` while holding `smear/graphqlx` off can no longer have both in one graph.
// That configuration is exactly the one the pre-split crate made impossible, so refusing it is the
// contract being kept, not a new restriction — but it is now a hard error where before the split
// it was unreachable and after the split it was silent.
//
// WHAT THIS FENCE DOES NOT COVER, stated because a claim wider than what it holds is the defect it
// was built against. It covers MEMBER features — the 26 `cargo metadata` reports across the five
// crates smear re-exports. Several `smear` features ALSO forward to third-party ones: `bytes`
// forwards `tokora/bytes_1`, and `impl Source for bytes::Bytes` comes from tokora, not from
// `smear-lexer`. Measured on this tree: a consumer with `smear/bytes` off and `tokora/bytes_1`
// enabled directly gets that impl, and nothing here fires — tokora publishes no feature witnesses
// and this repository cannot give it any. That is NOT a regression from the split: `smear/bytes`
// forwarded `tokora/bytes_1` on `origin/feat/proto` too, so the same graph did the same thing
// before any crate moved. It is a pre-existing bound of the umbrella, now written down.
//
// Derived, not remembered: `ci/feature_reachability.py` reads every member feature out of
// `cargo metadata`, pairs each with its umbrella twin — the namesake, or an explicit entry in
// `ci/downstream_pairs.py`'s `EQ_TWIN`, and a pair with neither is a FINDING — and fails when one
// has no constant in its member or no assertion here. There is no way to leave a pair out; the
// revision that had one skipped `smear-schema/build`, which is impl-bearing, and was green over it.

const _: () = {
  assert!(
    smear_lexer::__features::BSTR == cfg!(feature = "bstr"),
    "`smear-lexer/bstr` and `smear/bstr` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/bstr` too, or stop enabling\n     `smear-lexer/bstr`."
  );
  assert!(
    smear_lexer::__features::BYTES == cfg!(feature = "bytes"),
    "`smear-lexer/bytes` and `smear/bytes` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/bytes` too, or stop enabling\n     `smear-lexer/bytes`."
  );
  assert!(
    smear_lexer::__features::GRAPHQL == cfg!(feature = "graphql"),
    "`smear-lexer/graphql` and `smear/graphql` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/graphql` too, or stop enabling\n     `smear-lexer/graphql`."
  );
  assert!(
    smear_lexer::__features::GRAPHQLX == cfg!(feature = "graphqlx"),
    "`smear-lexer/graphqlx` and `smear/graphqlx` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/graphqlx` too, or stop enabling\n     `smear-lexer/graphqlx`."
  );
  assert!(
    smear_lexer::__features::HIPSTR == cfg!(feature = "hipstr"),
    "`smear-lexer/hipstr` and `smear/hipstr` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/hipstr` too, or stop enabling\n     `smear-lexer/hipstr`."
  );
  assert!(
    smear_lexer::__features::SMALLVEC == cfg!(feature = "smallvec"),
    "`smear-lexer/smallvec` and `smear/smallvec` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/smallvec` too, or stop enabling\n     `smear-lexer/smallvec`."
  );
  assert!(
    smear_lexer::__features::SMOL_BYTES == cfg!(feature = "smol-bytes"),
    "`smear-lexer/smol-bytes` and `smear/smol-bytes` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/smol-bytes` too, or stop enabling\n     `smear-lexer/smol-bytes`."
  );
  assert!(
    smear_lexer::__features::STD == cfg!(feature = "std"),
    "`smear-lexer/std` and `smear/std` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/std` too, or stop enabling\n     `smear-lexer/std`."
  );
};

#[cfg(feature = "parser")]
const _: () = {
  assert!(
    smear_parser::__features::BSTR == cfg!(feature = "bstr"),
    "`smear-parser/bstr` and `smear/bstr` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/bstr` too, or stop enabling\n     `smear-parser/bstr`."
  );
  assert!(
    smear_parser::__features::BYTES == cfg!(feature = "bytes"),
    "`smear-parser/bytes` and `smear/bytes` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/bytes` too, or stop enabling\n     `smear-parser/bytes`."
  );
  assert!(
    smear_parser::__features::GRAPHQL == cfg!(feature = "graphql"),
    "`smear-parser/graphql` and `smear/graphql` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/graphql` too, or stop enabling\n     `smear-parser/graphql`."
  );
  assert!(
    smear_parser::__features::GRAPHQLX == cfg!(feature = "graphqlx"),
    "`smear-parser/graphqlx` and `smear/graphqlx` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/graphqlx` too, or stop enabling\n     `smear-parser/graphqlx`."
  );
  assert!(
    smear_parser::__features::HIPSTR == cfg!(feature = "hipstr"),
    "`smear-parser/hipstr` and `smear/hipstr` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/hipstr` too, or stop enabling\n     `smear-parser/hipstr`."
  );
  assert!(
    smear_parser::__features::LOSSLESS_COVERAGE == cfg!(feature = "lossless-coverage"),
    "`smear-parser/lossless-coverage` and `smear/lossless-coverage` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/lossless-coverage` too, or stop enabling\n     `smear-parser/lossless-coverage`."
  );
  assert!(
    smear_parser::__features::MATERIALIZED_NUMBERS == cfg!(feature = "materialized-numbers"),
    "`smear-parser/materialized-numbers` and `smear/materialized-numbers` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/materialized-numbers` too, or stop enabling\n     `smear-parser/materialized-numbers`."
  );
  assert!(
    smear_parser::__features::ROWAN == cfg!(feature = "rowan"),
    "`smear-parser/rowan` and `smear/rowan` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/rowan` too, or stop enabling\n     `smear-parser/rowan`."
  );
  assert!(
    smear_parser::__features::SMALLVEC == cfg!(feature = "smallvec"),
    "`smear-parser/smallvec` and `smear/smallvec` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/smallvec` too, or stop enabling\n     `smear-parser/smallvec`."
  );
  assert!(
    smear_parser::__features::SMOL_BYTES == cfg!(feature = "smol-bytes"),
    "`smear-parser/smol-bytes` and `smear/smol-bytes` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/smol-bytes` too, or stop enabling\n     `smear-parser/smol-bytes`."
  );
  assert!(
    smear_parser::__features::STD == cfg!(feature = "std"),
    "`smear-parser/std` and `smear/std` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/std` too, or stop enabling\n     `smear-parser/std`."
  );
  assert!(
    smear_parser::__features::TEST_SUPPORT == cfg!(feature = "test-support"),
    "`smear-parser/test-support` and `smear/test-support` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/test-support` too, or stop enabling\n     `smear-parser/test-support`."
  );
};

const _: () = {
  // `build` is the one pair whose twin is not its namesake: there is no `smear/build`, and
  // `smear/validator` is what turns it on. It was EXEMPTED from this fence in `0c1a841`, on a
  // reason measured over PATHS — "a consumer with `smear-schema/build` on and `smear/validator`
  // off cannot name `SchemaBuilder`", which is true and is not the property. `build` also gates
  // `impl Diagnose for SchemaError` (`smear-schema/src/error.rs:1001`), and an impl is not
  // namespaced — which is the argument this whole fence was built on, applied to every pair
  // except the one it was written in the same commit as. Measured on this tree before the fix: a
  // consumer with `smear/{std,parser,graphql}` and `smear-schema/build` compiled a function
  // demanding `smear::diagnostic::Diagnose` over `smear_schema::SchemaError`, with every const
  // assertion passing.
  assert!(
    smear_schema::__features::BUILD == cfg!(feature = "validator"),
    "`smear-schema/build` and `smear/validator` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` items gated by it — including `impl Diagnose for\n     SchemaError`, which is not namespaced and no re-export can hide — would then appear or\n     vanish behind a consumer who never asked. Enable `smear/validator` too, or stop enabling\n     `smear-schema/build`."
  );
  assert!(
    smear_schema::__features::INTROSPECTION == cfg!(feature = "introspection"),
    "`smear-schema/introspection` and `smear/introspection` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/introspection` too, or stop enabling\n     `smear-schema/introspection`."
  );
  assert!(
    smear_schema::__features::STD == cfg!(feature = "std"),
    "`smear-schema/std` and `smear/std` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/std` too, or stop enabling\n     `smear-schema/std`."
  );
};

#[cfg(feature = "validator")]
const _: () = {
  assert!(
    smear_compiler::__features::INTROSPECTION == cfg!(feature = "introspection"),
    "`smear-compiler/introspection` and `smear/introspection` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/introspection` too, or stop enabling\n     `smear-compiler/introspection`."
  );
  assert!(
    smear_compiler::__features::ROWAN == cfg!(feature = "rowan"),
    "`smear-compiler/rowan` and `smear/rowan` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/rowan` too, or stop enabling\n     `smear-compiler/rowan`."
  );
  assert!(
    smear_compiler::__features::STD == cfg!(feature = "std"),
    "`smear-compiler/std` and `smear/std` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/std` too, or stop enabling\n     `smear-compiler/std`."
  );
};

#[cfg(feature = "proto")]
const _: () = {
  assert!(
    graphql_proto::__features::STD == cfg!(feature = "std"),
    "`graphql-proto/std` and `smear/std` disagree. Something in this dependency graph enabled\n     one without the other, and `smear::…` paths gated by it would then appear or vanish\n     behind a consumer who never asked. Enable `smear/std` too, or stop enabling\n     `graphql-proto/std`."
  );
};

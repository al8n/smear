//! Draft §7.1.3's *request error result*: the second of §7.1's three result kinds this crate has.
//!
//! # Why this is a type and not a paragraph
//!
//! §7.1 defines a *response* as "either an *execution result*, a *response stream*, or a *request
//! error result*", and this crate had one of the three. [`Response`](crate::Response) is §7.1.1's
//! execution result; §7.1.2's response stream belongs to the subscription phase; and §7.1.3's
//! request error result was **recorded as unreachable-from-here and left to the driver** — a
//! decision `StartError`'s header carried, and one this module reverses. What changed is not the
//! reachability, which was already correct and already pinned by test: it is the reading of what
//! §7.1.3 needs.
//!
//! §7.1.3 makes four demands, and every one of them is structural:
//!
//! > A *request error result* must be a map. […] The *request error result* map must contain an
//! > entry with key {"errors"}. The value of this entry must be a non-empty list of *request
//! > error* raised during the *request*. […] The *request error result* map must not contain an
//! > entry with key {"data"}. […] The *request error result* map may also contain an entry with
//! > key `extensions`.
//!
//! [`RequestErrorResult`] is each of those made unrepresentable-otherwise, which is the same
//! argument [`Extensions`] makes about §7.1.7's one clause. It is a map by construction; its
//! `errors` list is non-empty by construction, because its first entry is a field and only the
//! entries after it are a `Vec`; it has **no `data` accessor at all**, so the one entry §7.1.3
//! forbids cannot be written; and `extensions` is an `Option` around the same budgeted container
//! the execution result carries.
//!
//! The alternative was a documentation note telling a driver to assemble the map itself, and that
//! is what the base shipped. A driver following it correctly produces the same bytes. A driver
//! that reaches for [`Response`](crate::Response) — the only response type the crate had —
//! produces a map with a `data` entry, because [`Response::data`](crate::Response::data) is
//! infallible, and §7.1.3 says that map must not have one.
//!
//! # Who raises a request error, and so what the list holds
//!
//! §7.1.3's own note names four ways a request error arises — "missing information, syntax errors,
//! validation failure, coercion failure" — and they are raised in two places. That is why the
//! list's first entry is one of two kinds, and why [`errors`](RequestErrorResult::errors) reads
//! every entry as a [`RequestErrorEntry`] that says which.
//!
//! **The executor's, and there is exactly one.** [`Executor::start`](crate::Executor::start)
//! returns on its first draft §6.1 `GetOperation` failure and accumulates nothing, so a result
//! [`new`](RequestErrorResult::new) builds from the [`StartError`] it returned holds that one
//! entry. That is not a simplification of a list — it is the whole of the executor's population.
//!
//! **The driver's, as many as the ceilings admit.** The other three are outside this crate by its
//! own division of labour: a syntax error is `smear-parser`'s, a validation failure is
//! `smear-compiler`'s draft §5, and a variable coercion failure is the driver's, per
//! [`Values::variable`](crate::Values::variable)'s contract that values arriving there are already
//! coerced. None of the three ever reaches an [`Executor`](crate::Executor), so none of them has a
//! `StartError` to build a result from. Each is a [`RequestError`] instead — the driver's own
//! entry, with §7.1.6's `message`, its `locations` and its per-error `extensions` — and
//! [`from_driver`](RequestErrorResult::from_driver) and [`push`](RequestErrorResult::push) list
//! them.
//!
//! **That half used to be the residue, stated as a boundary**, and the boundary named its own
//! price: a public request-error *entry* type, and two new ceilings besides, "because a
//! driver-grown list of driver-supplied messages is a count multiplied by a length and a budget on
//! one factor of a product bounds nothing". Meanwhile a driver holding one of the three assembled
//! its own JSON map, which is exactly what this type exists to prevent. The price is paid below,
//! and it came to **three** ceilings rather than two: an entry's `locations` are a second length
//! beside its message, so the product has three factors.
//!
//! The two kinds are not exclusive after the first entry. [`push`](RequestErrorResult::push)
//! appends a driver's entry to either kind of result, because §7.1.3's list is "request errors
//! raised during the request", whoever raised them — and refusing one there would have needed a
//! refusal that no [`Limits`] ceiling names, which is a wall rather than an error.
//!
//! # What each kind carries of §7.1.6
//!
//! §7.1.6's *Error Result Format* has exactly one `must`: "Every error must contain an entry with
//! the key {"message"} with a string description of the error". The other three entries do not
//! apply or do not bind, and the two kinds answer them differently:
//!
//! | §7.1.6 entry | strength | a [`StartError`] | a [`RequestError`] |
//! |---|---|---|---|
//! | `message` | must | its `Display`, rendered rather than stored, for the reason `error.rs` gives about the execution-result side | the driver's text, owned, within [`Limits::max_request_error_message_bytes`] |
//! | `path` | must, **if** the error "can be associated to a particular field in the GraphQL result" | never: a request error result has no result to hold a field | never, for the same reason |
//! | `locations` | should, **if** associable to "a particular point in the requested GraphQL document" | not carried — see below | the driver's spans, within [`Limits::max_request_error_locations`]; `smear::json` resolves them against the document exactly as it resolves a field error's |
//! | `extensions` | may | not carried: a §6.1 refusal is raised before the driver has answered anything, so there is no driver input to attach | the driver's map: the same budgeted [`Extensions`] a field error carries when the driver reports it through [`Executor::handle_field_error_with`](crate::Executor::handle_field_error_with) |
//!
//! A driver's `locations` are spans rather than lines and columns, for the reason
//! [`Error::locations`](crate::Error::locations) gives: turning one into the other needs the source
//! text, which this crate does not hold.
//!
//! For a [`StartError`], `locations` is the only judgement call, and the honest answer is "for all
//! but one of them, and not for that one". §7.1.6 conditions it on the error being associable to "a
//! particular point in the requested GraphQL document", and a §6.1 `GetOperation` failure is a
//! property of the **request** rather than of a point in the text: which operation to run is chosen
//! by the request's `operationName`, so [`AmbiguousOperation`](StartError::AmbiguousOperation) is
//! about a name that was *not* given, [`UnknownOperation`](StartError::UnknownOperation) about one
//! that names nothing, and [`NoOperation`](StartError::NoOperation) about a document with none — no
//! syntax element to point at in any of the three.
//! [`ResponseStreamOpen`](StartError::ResponseStreamOpen) is a property of the *executor* and not of
//! any document at all: it says a previous subscription's response stream has not been ended, so
//! there is no point in the requested text to associate it with either.
//! [`NoQueryRoot`](StartError::NoQueryRoot) is unreachable: `Schema::build` refuses a schema with
//! no query root (`MissingQueryRootOperationType`, observed) and it is the sole door —
//! `Schema::from_introspection` renders SDL and calls it — so no `Schema` an executor can be handed
//! raises it. `the_errors_entry_is_the_one_refusal_start_raised` records that as the one variant its
//! table cannot reach.
//!
//! **[`NoMutationRoot`](StartError::NoMutationRoot) is the sixth, it *is* associable, and this does
//! not carry its span.** The fault is the schema's, but the operation definition that asked for a
//! mutation root is in the requested document and has a span; `graphql-js` 16.11.0 attaches exactly
//! that node — `new GraphQLError(\`Schema is not configured to execute … operation.\`, { nodes:
//! operation })` in `executeOperation`, against three bare `GraphQLError(message)` constructions
//! for the `GetOperation` three. So this is an unmet "should" and is recorded as one rather than
//! argued away.
//!
//! **The reason recorded here for not closing it has since stopped being true, and the honest
//! replacement is a smaller one.** It said closing it means [`StartError`] carrying data, which
//! changes a `Copy`, data-free, `PartialEq` enum. The enum is no longer data-free:
//! [`SourceFieldArgumentBudget`](StartError::SourceFieldArgumentBudget) carries the ceiling that
//! refused, because a resource refusal a caller cannot act on is a wall rather than an error —
//! the argument the [`TooLarge`] section below makes, applied where §6.2.3.1 had been reporting a
//! budget as a bad argument. It is still `Copy`, `PartialEq` and `Eq`, and a `SimpleSpan` would be
//! all three too, so nothing about the *shape* of the type stands in the way any more.
//!
//! What still does is narrower and worth stating as the cost it is: adding a **new** variant to a
//! `#[non_exhaustive]` enum breaks no existing comparison, and changing `NoMutationRoot` from a
//! unit variant to one carrying a span breaks every `assert_eq!(err, StartError::NoMutationRoot)`
//! in `proto_execute.rs` and every equivalent line in a driver. That is a real cost for a "should",
//! and it is the one this section is declining to pay — not a property of the type, which is what
//! it used to claim.
//!
//! # An entry built from a diagnostic
//!
//! [`RequestError::from_diagnose`] turns any [`Diagnose`] into an entry. The seam is the one this
//! crate already reads through `smear-schema`, and the families that answer it —
//! `smear-parser`'s syntax `Error`, `smear-compiler`'s `DiagnosticDisplay`, `smear-schema`'s
//! `SchemaError` and the introspection door's `ResponseError` — need nothing new to reach a
//! request error result. Three decisions are taken there, and recorded here:
//!
//! - **The message is the diagnostic's rendering, as it renders.** Nothing is cut out of it or
//!   added to it. The position travels in `locations`, which is where §7.1.6 puts it and the form a
//!   client can use; a diagnostic whose sentence also spells its span says so twice, and that is
//!   the diagnostic's choice to change, not this door's to edit.
//! - **The locations are the primary span and then each label's**, in the diagnostic's own order.
//!   A [`Location::entire`](smear_schema::diagnostic::Location::entire) is skipped rather than
//!   written: it says the diagnostic has no point in the text to show, and §7.1.6 asks for a point.
//! - **No code is invented.** §7.1.6 gives an error no `code` entry. `extensions.code` is a
//!   convention a driver may adopt — the diagnostic's [`Code`](smear_schema::diagnostic::Code) is
//!   right there for [`set_extensions`](RequestError::set_extensions) — and not one this crate
//!   imposes on every driver's wire format.
//!
//! **A parse error is a diagnostic too.** `smear-parser`'s `Error`, in both dialects, implements
//! [`Diagnose`]: its code is one of the `smear::syntax` family, its message names no position, and
//! its primary location is where the document was refused. So a draft §2 syntax error reaches a
//! request error result through this same door, and a driver has no sentence of its own to write
//! for one — though [`RequestError::new`] stays open to a driver that wants to.
//!
//! # What it retains, and what bounds it
//!
//! Two resources. The map is the one [`Executor::set_extensions`](crate::Executor::set_extensions)
//! defends, so it gets the same row the executor's own lifetime table would give it, and the
//! driver's entries are this type's own:
//!
//! | member | bound | readers | dead at | released by | what would strand it |
//! |---|---|---|---|---|---|
//! | draft §7.1.7 `extensions` | this result's two extension ceilings, **and** its spine capacity | [`RequestErrorResult::extensions`] | drop | [`take_extensions`](RequestErrorResult::take_extensions); drop | a refusal that dropped the map instead of handing it back; and a map retained with the capacity a *different* [`Limits`] let it grow to |
//! | the driver's [`RequestError`]s | `max_request_errors × (max_request_error_message_bytes + max_request_error_locations · size_of::<SimpleSpan>() + max_extension_entries + max_extension_key_bytes)`, plus a list spine of at most `max_request_errors` | [`RequestErrorResult::errors`] | drop | drop; a refused one is never retained, and comes back in a [`RefusedEntry`] | a refusal that dropped the entry instead of handing it back, when its map may hold a handle; an entry retained with the message, the spans or the map spine a *different* [`Limits`] let it grow to; and a list spine that `Vec`'s doubling grew past the count |
//!
//! Three factors and not two for the map, because that is the lesson the execution-result side
//! paid for twice: entries and key bytes bound what a map *reports*, and a `Vec`'s capacity is
//! derived from neither — [`Extensions::remove`] refunds the budget and returns no slot. So
//! [`set_extensions`](RequestErrorResult::set_extensions) re-checks both reported quantities
//! **and** re-derives the spine, exactly as the executor's does. **A ceiling bounds a retained
//! buffer only if every growth of that buffer passed through it**, and the growth site is
//! [`Extensions::insert`], charged against whatever ceilings the *map* was created under.
//!
//! **The entries take the same rule one level up.** An entry is built under whatever [`Limits`]
//! the driver handed [`RequestError::new`], and that bounds what the entry *allocates*. The result
//! that lists it is a second retainer, so [`from_driver`](RequestErrorResult::from_driver) and
//! [`push`](RequestErrorResult::push) re-check every quantity the entry reports against the
//! result's own ceilings — its message bytes, its span count, and its map's entries and key bytes
//! — and re-derive that map's spine exactly as `set_extensions` does. The message and the spans
//! need no re-derivation: each is boxed at its exact length, so what it reports is what it holds.
//! The list's own spine grows by doubling clamped to the count ceiling, so `Vec`'s amortisation
//! never holds a slot the ceiling would refuse.
//!
//! Which is why this type carries ceilings at all rather than accepting any map or any entry — and
//! it is worth being exact about what they buy, because the executor's version of this sentence
//! does not transfer. There, the re-check defends a *third party*: the map is retained inside a
//! machine the driver keeps calling, under a budget the driver did not choose. Here there is no
//! third party. A [`RequestErrorResult`] is the caller's value and the caller drops it, so these
//! ceilings are not a claim about anyone else's memory.
//!
//! What they buy is that **one** budget governs everything this result retains — the one named
//! once at construction — instead of whatever budget each map or entry happened to be built under.
//! Pass [`Executor::limits`](crate::Executor::limits) and that is the refusing executor's, which is
//! the intended call and what the worked example shows; a driver with no executor yet passes the
//! [`Limits`] it runs its executors under. Pass something laxer and the caller has widened its own
//! footprint on purpose: the same freedom [`Extensions::new`] already gives, and the reason no
//! constructor here can take it away.
//!
//! # A refusal is repairable, and returns what it refused
//!
//! [`TooLarge`] carries a map back and names which [`Ceiling`] refused; [`RefusedEntry`] carries
//! an entry back and names which [`RequestErrorCeiling`] refused. Both halves are obligations
//! rather than courtesies. The value comes back because a map's value may be a wasm or FFI handle,
//! and an entry carries a map, and a refusal that closes a handle has done more than refuse — the
//! argument [`Full`](crate::Full) and [`SetExtensionsError`](crate::SetExtensionsError) both make.
//! The ceiling is named because the caller's remedy differs — drop entries, shorten keys, shorten
//! the message, point at fewer places, or stop listing — and a refusal a caller cannot act on is a
//! wall rather than an error.
//!
//! [`RequestError::new`] refuses with the ceiling alone, because it took nothing to hand back: the
//! message and the spans are borrowed, and nothing is allocated until both are known to fit — the
//! argument [`Extensions::insert`] makes for taking its key as `&str`.

use core::fmt;

use smear_schema::diagnostic::Diagnose;
use tokora::SimpleSpan;

use crate::{Ceiling, Extensions, Limits, StartError};

/// A map [`RequestErrorResult::set_extensions`] refused, carrying it back.
///
/// Separate from [`SetExtensionsError`](crate::SetExtensionsError), whose other two variants are
/// the executor's operation lifecycle. A [`RequestErrorResult`] is a finished value with no
/// lifecycle at all — there is no phase in which attaching a map to it is meaningless — so a
/// signature admitting `NoOperation` or `AlreadyDelivered` here would promise refusals that cannot
/// happen.
pub struct TooLarge<V> {
  ceiling: Ceiling,
  extensions: Extensions<V>,
}

impl<V> TooLarge<V> {
  /// Returns which ceiling refused, so the caller knows which way to shrink.
  #[inline]
  pub const fn ceiling(&self) -> Ceiling {
    self.ceiling
  }

  /// Returns the map that was refused, giving up ownership.
  #[inline]
  pub fn into_extensions(self) -> Extensions<V> {
    self.extensions
  }
}

impl<V> fmt::Debug for TooLarge<V> {
  /// Written out rather than derived, because a derive would bound `V: Debug`.
  ///
  /// [`Extensions`]'s own `Debug` requires nothing of `V` — it renders keys and the *presence* of
  /// a value — so the derive's bound would be strictly larger than the field needs, and it would
  /// reach every caller that wants to print a refusal. That is the same over-bound `Extensions`
  /// avoided so that [`Response`](crate::Response) could keep its `Debug` unbounded.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("TooLarge")
      .field("ceiling", &self.ceiling)
      .field("extensions", &self.extensions)
      .finish()
  }
}

impl<V> fmt::Display for TooLarge<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&self.ceiling, f)
  }
}

#[cfg(feature = "std")]
impl<V> std::error::Error for TooLarge<V> {}

/// Which of the ceilings a request error entry meets refused it.
///
/// Every variant is one [`Limits`] ceiling, and [`field`](RequestErrorCeiling::field) spells its
/// getter's name, so a driver reports the knob it wants raised without parsing a message — the
/// accessor [`Ceiling::field`] is, for the same reason.
///
/// # Which door answers which
///
/// | refused by | can answer |
/// |---|---|
/// | [`RequestError::new`], [`RequestError::from_diagnose`] | [`MessageBytes`](Self::MessageBytes), [`Locations`](Self::Locations) |
/// | [`RequestErrorResult::from_driver`] | those two, and [`Extensions`](Self::Extensions) |
/// | [`RequestErrorResult::push`] | all four |
///
/// One enum for the three doors rather than one per door, because the vocabulary is the result's:
/// an entry is built to be listed, and the knob a driver turns is the same whichever door said no.
/// What the table records is that an entry under construction is in no list yet, so it cannot meet
/// [`Errors`](Self::Errors), and carries no map yet, so it cannot meet
/// [`Extensions`](Self::Extensions).
///
/// # Not [`Ceiling`], and not a widening of it
///
/// [`Ceiling`] says exactly the two things an extension insert can be refused by, and
/// [`Resource`](crate::Resource) records why widening it would hand a [`Full`](crate::Full) a type
/// claiming more than it can ever hold. An entry's own map is refused by those same two, so they
/// arrive here wrapped, as [`Extensions`](Self::Extensions), and [`Ceiling`] is unchanged.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum RequestErrorCeiling {
  /// [`Limits::max_request_errors`] has no room for another entry.
  Errors,
  /// [`Limits::max_request_error_message_bytes`] has no room for the entry's message.
  MessageBytes,
  /// [`Limits::max_request_error_locations`] has no room for the entry's `locations`.
  Locations,
  /// One of the two extension ceilings has no room for the entry's own draft §7.1.6 `extensions`
  /// map, and this is which.
  Extensions(Ceiling),
}

impl RequestErrorCeiling {
  /// Returns the [`Limits`] ceiling that refused, by the name of its getter.
  #[inline]
  pub const fn field(self) -> &'static str {
    match self {
      Self::Errors => "max_request_errors",
      Self::MessageBytes => "max_request_error_message_bytes",
      Self::Locations => "max_request_error_locations",
      Self::Extensions(ceiling) => ceiling.field(),
    }
  }
}

impl fmt::Display for RequestErrorCeiling {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "`Limits::{}` has no room for the request error",
      self.field()
    )
  }
}

#[cfg(feature = "std")]
impl std::error::Error for RequestErrorCeiling {}

/// A request error the driver raised: one entry of a draft §7.1.3 result's `errors`, built where no
/// executor exists.
///
/// Draft §7.1.6's *Error Result Format*, entry by entry: the `message` it must carry, the
/// `locations` it should, and the `extensions` it may. There is no `path`, because §7.1.6 asks for
/// one only of an error associated with "a particular field in the GraphQL result" and a request
/// error result has no result. `request_error.rs`'s module header has who raises these and why
/// they are carried here rather than assembled by the driver.
///
/// # Built under a budget, and re-checked by the result that lists it
///
/// [`new`](RequestError::new) and [`from_diagnose`](RequestError::from_diagnose) check the message
/// and the span count against the [`Limits`] they are handed **before** allocating either, so a
/// refusal costs nothing. The [`RequestErrorResult`] that lists the entry then re-checks every
/// quantity it reports against its own ceilings, which is what lets an entry built under one budget
/// be listed under another without the result retaining more than its own budget allows.
///
/// # Worked example
///
/// See [`RequestErrorResult`]'s second example, which builds two of these and lists them.
pub struct RequestError<V> {
  /// Draft §7.1.6's `message`, boxed at its exact length.
  message: std::boxed::Box<str>,
  /// Draft §7.1.6's `locations`, as spans into the requested document, boxed at their exact count.
  locations: std::boxed::Box<[SimpleSpan]>,
  /// Draft §7.1.6's per-error `extensions`, absent until the driver attaches one.
  extensions: Option<Extensions<V>>,
}

impl<V> RequestError<V> {
  /// Builds an entry from the driver's own `message` and the spans it points at.
  ///
  /// `locations` are spans into the requested document — the text the driver parsed, or failed to
  /// parse — in the order §7.1.6's `locations` should list them, and they are copied as they are.
  /// Empty is honest when the error has no point in the text: §7.1.6's `locations` is a *should*,
  /// conditional on there being one.
  ///
  /// # Refusal
  ///
  /// [`MessageBytes`](RequestErrorCeiling::MessageBytes) when `message` is longer than
  /// [`Limits::max_request_error_message_bytes`], and then
  /// [`Locations`](RequestErrorCeiling::Locations) when there are more spans than
  /// [`Limits::max_request_error_locations`]. Nothing is allocated on either path: both are
  /// borrowed and both ceilings are checked first, which is the difference between a ceiling and a
  /// receipt. There is nothing to hand back, for the reason [`Full`](crate::Full) does not return
  /// a key: nothing was taken.
  pub fn new(
    limits: &Limits,
    message: &str,
    locations: &[SimpleSpan],
  ) -> Result<Self, RequestErrorCeiling> {
    if message.len() as u64 > u64::from(limits.max_request_error_message_bytes.get()) {
      return Err(RequestErrorCeiling::MessageBytes);
    }
    if locations.len() as u64 > u64::from(limits.max_request_error_locations.get()) {
      return Err(RequestErrorCeiling::Locations);
    }
    Ok(Self {
      message: message.into(),
      locations: locations.into(),
      extensions: None,
    })
  }

  /// Builds an entry from a diagnostic: its message, and the positions it points at.
  ///
  /// Draft §5's validation diagnostics are what this is for — pass the view `smear-compiler`'s
  /// `Diagnostic::display` returns, which is its [`Diagnose`] implementor — and any other
  /// implementor is read the same way. The spans are taken as positions in the requested document,
  /// so a diagnostic about some other input belongs in an entry only if the caller resolves its
  /// positions against that input.
  ///
  /// - **The message** is the diagnostic's `Display`, as it renders. The position travels in the
  ///   entry's `locations`, in the form a client can use.
  /// - **The locations** are the primary span and then each label's, in order, skipping a
  ///   [`Location::entire`](smear_schema::diagnostic::Location::entire), which is no point in the
  ///   text.
  /// - **No `extensions`.** The diagnostic's code is the driver's to publish or not, through
  ///   [`set_extensions`](RequestError::set_extensions); `request_error.rs`'s header has why this
  ///   crate does not invent an `extensions.code`.
  ///
  /// # Refusal
  ///
  /// As [`new`](RequestError::new), and with the same order and the same guarantee: the message is
  /// rendered once to measure it and once to keep it, so a message over
  /// [`Limits::max_request_error_message_bytes`] is refused without having been allocated, and an
  /// accepted one is allocated once, at its exact length.
  pub fn from_diagnose<D>(limits: &Limits, diagnostic: &D) -> Result<Self, RequestErrorCeiling>
  where
    D: Diagnose + ?Sized,
  {
    // The first rendering keeps nothing but its length. A `Display` reports an error only when its
    // sink does, and neither sink here ever does; one that fails anyway is measured as far as it
    // got, and the second rendering below stops at the same place.
    let mut measure = Measure(0);
    let _ = fmt::write(&mut measure, format_args!("{diagnostic}"));
    let len = measure.0;
    if len as u64 > u64::from(limits.max_request_error_message_bytes.get()) {
      return Err(RequestErrorCeiling::MessageBytes);
    }

    // Counted before anything is allocated for them, through the same walk that collects them.
    let primary = diagnostic.primary().span();
    let count = usize::from(primary.is_some()) + label_spans(diagnostic).count();
    if count as u64 > u64::from(limits.max_request_error_locations.get()) {
      return Err(RequestErrorCeiling::Locations);
    }
    let mut locations = std::vec::Vec::with_capacity(count);
    locations.extend(primary);
    locations.extend(label_spans(diagnostic).take(count - locations.len()));

    let mut message = Bounded {
      text: std::string::String::with_capacity(len),
      room: len,
    };
    let _ = fmt::write(&mut message, format_args!("{diagnostic}"));

    Ok(Self {
      message: message.text.into_boxed_str(),
      locations: locations.into_boxed_slice(),
      extensions: None,
    })
  }

  /// Returns draft §7.1.6's `message`.
  #[inline]
  pub fn message(&self) -> &str {
    &self.message
  }

  /// Returns draft §7.1.6's `locations`, as spans into the requested document.
  ///
  /// Empty when the entry was built with none, and then a serialiser writes no `locations` entry
  /// at all — §7.1.6's *should* is conditional on there being a point to name.
  #[inline]
  pub fn locations(&self) -> &[SimpleSpan] {
    &self.locations
  }

  /// Returns the entry's draft §7.1.6 `extensions`, or `None` when it has none.
  ///
  /// `None` is an absent key and never an empty map, exactly as it is on
  /// [`Error::extensions`](crate::Error::extensions): `Some` holding an empty [`Extensions`] is a
  /// *present* empty map, and a serialiser writes it as `{}`.
  #[inline]
  pub const fn extensions(&self) -> Option<&Extensions<V>> {
    self.extensions.as_ref()
  }

  /// Attaches draft §7.1.6's per-error `extensions` map, returning whatever map it replaced.
  ///
  /// Not refused here, and that is not an unbudgeted door: attaching moves a map the driver already
  /// built — under the ceilings [`Extensions::new`] was handed — and allocates nothing, and the
  /// entry is still the driver's own value. The ceilings that bound what is *retained* are the
  /// result's, and [`RequestErrorResult::from_driver`] and [`RequestErrorResult::push`] check the
  /// map against them on the way in, handing the whole entry back if it does not fit.
  #[inline]
  pub fn set_extensions(&mut self, extensions: Extensions<V>) -> Option<Extensions<V>> {
    self.extensions.replace(extensions)
  }

  /// Removes the entry's `extensions` map, returning it, so the entry carries no such key.
  ///
  /// The counterpart of [`set_extensions`](RequestError::set_extensions), for the reason
  /// [`RequestErrorResult::take_extensions`] gives: absence must not be a one-way door.
  #[inline]
  pub fn take_extensions(&mut self) -> Option<Extensions<V>> {
    self.extensions.take()
  }
}

impl<V> fmt::Debug for RequestError<V> {
  /// Unbounded in `V`, for the reason [`TooLarge`]'s is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RequestError")
      .field("message", &self.message)
      .field("locations", &self.locations)
      .field("extensions", &self.extensions)
      .finish()
  }
}

impl<V> fmt::Display for RequestError<V> {
  /// The `message`, as §7.1.6 asks for it.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(&self.message)
  }
}

/// A [`RequestError`] a result refused to list, carrying it back.
///
/// [`TooLarge`]'s counterpart for an entry, and returned for its reason: the entry's own
/// `extensions` may hold a wasm or FFI handle, and a refusal that dropped the entry would close
/// it. [`ceiling`](RefusedEntry::ceiling) says which way to shrink, and [`RequestErrorCeiling`]
/// has which door can refuse with which.
pub struct RefusedEntry<V> {
  ceiling: RequestErrorCeiling,
  error: RequestError<V>,
}

impl<V> RefusedEntry<V> {
  /// Returns which ceiling refused, so the caller knows which way to shrink.
  #[inline]
  pub const fn ceiling(&self) -> RequestErrorCeiling {
    self.ceiling
  }

  /// Returns the entry that was refused, giving up ownership.
  ///
  /// Untouched: a refusal is decided before anything about the entry is normalised, so its map
  /// comes back with the allocation it arrived with.
  #[inline]
  pub fn into_error(self) -> RequestError<V> {
    self.error
  }
}

impl<V> fmt::Debug for RefusedEntry<V> {
  /// Unbounded in `V`, for the reason [`TooLarge`]'s is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RefusedEntry")
      .field("ceiling", &self.ceiling)
      .field("error", &self.error)
      .finish()
  }
}

impl<V> fmt::Display for RefusedEntry<V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&self.ceiling, f)
  }
}

#[cfg(feature = "std")]
impl<V> std::error::Error for RefusedEntry<V> {}

/// One entry of a draft §7.1.3 result's `errors`, as [`RequestErrorResult::errors`] reads it.
///
/// Two kinds, because two parties raise request errors — `request_error.rs`'s module header has
/// which raises which. A serialiser need not tell them apart: [`Display`](fmt::Display) is
/// §7.1.6's `message` for both, and for the executor's refusal
/// [`locations`](RequestErrorEntry::locations) is empty and
/// [`extensions`](RequestErrorEntry::extensions) is `None`, so one writer that handles a driver's
/// entry handles a refusal unchanged. A driver that branches on the refusal matches
/// [`Start`](RequestErrorEntry::Start), or reads [`RequestErrorResult::error`].
///
/// **Not `#[non_exhaustive]`**, for the rule [`Node`](crate::Node) records: whatever matches on it
/// has to answer for every kind of entry, and a third kind must break that `match` rather than fall
/// into a catch-all.
pub enum RequestErrorEntry<'r, V> {
  /// The executor's draft §6.1 refusal, as [`Executor::start`](crate::Executor::start) returned
  /// it.
  Start(StartError),
  /// An entry the driver listed.
  Driver(&'r RequestError<V>),
}

impl<'r, V> RequestErrorEntry<'r, V> {
  /// Returns draft §7.1.6's `locations`, as spans into the requested document.
  ///
  /// Empty for the executor's refusal, which is associable to no point in the text — the module
  /// header has the argument, and its one exception.
  #[inline]
  pub fn locations(&self) -> &'r [SimpleSpan] {
    match *self {
      Self::Start(_) => &[],
      Self::Driver(error) => error.locations(),
    }
  }

  /// Returns the entry's draft §7.1.6 `extensions`, or `None` when it has none.
  ///
  /// Always `None` for the executor's refusal, which is raised before the driver has answered
  /// anything there could be a map about.
  #[inline]
  pub fn extensions(&self) -> Option<&'r Extensions<V>> {
    match *self {
      Self::Start(_) => None,
      Self::Driver(error) => error.extensions(),
    }
  }
}

impl<V> Clone for RequestErrorEntry<'_, V> {
  /// Written out rather than derived, because a derive would bound `V: Clone`, and the entry holds a
  /// reference.
  #[inline]
  fn clone(&self) -> Self {
    *self
  }
}

impl<V> Copy for RequestErrorEntry<'_, V> {}

impl<V> fmt::Debug for RequestErrorEntry<'_, V> {
  /// Unbounded in `V`, for the reason [`TooLarge`]'s is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Start(error) => f.debug_tuple("Start").field(error).finish(),
      Self::Driver(error) => f.debug_tuple("Driver").field(error).finish(),
    }
  }
}

impl<V> fmt::Display for RequestErrorEntry<'_, V> {
  /// Draft §7.1.6's `message`, whichever kind of entry this is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Start(error) => fmt::Display::fmt(error, f),
      Self::Driver(error) => f.write_str(error.message()),
    }
  }
}

/// Draft §7.1.3's *request error result*: the response for a request that failed before execution.
///
/// `request_error.rs`'s module header has why this is a type, which two kinds of entry its
/// `errors` list holds and who raises each, and what bounds what it retains.
///
/// Named as a file rather than linked, because the module is private — the type reaches a consumer
/// through the crate root's `pub use`, so rustdoc renders no page for the module and a `[…](self)`
/// link is a public item pointing at a private one. `extensions.rs` refers to its own header the
/// same way, for the same reason.
///
/// # `data` is absent, and absence is the point
///
/// §7.1.3: "The *request error result* map must not contain an entry with key {"data"}." There is
/// no accessor for one here and there must never be, because a driver serialising from this type
/// has nothing to write it from. That is a stronger guarantee than a documented rule and a weaker
/// thing to state, so it is stated as a gate instead:
///
/// ```compile_fail,E0599
/// use graphql_proto::{Limits, RequestErrorResult, StartError};
///
/// let result: RequestErrorResult<()> =
///   RequestErrorResult::new(&Limits::default(), StartError::AmbiguousOperation);
/// // §7.1.3 forbids the entry, so the accessor does not exist.
/// let _ = result.data();
/// ```
///
/// # Worked example: the executor refused
///
/// The refusal below is a **valid** document's: draft §6.1 `GetOperation` chooses which operation
/// to run from the request's operation name, so draft §5 validation cannot pre-empt an ambiguity
/// between two named operations. That is what makes this shape reachable rather than theoretical.
///
/// ```
/// use graphql_proto::{Executor, Extensions, RequestErrorResult, StartError, Values};
/// # use graphql_proto::Leaf;
/// use smear_parser::{
///   graphql::{
///     GraphQL,
///     ast::{ExecutableDocument, TypeSystemDocument},
///     error::GraphqlErrors,
///     syntactic::{GraphqlLexer, executable_document, type_system_document},
///   },
///   lexer::tokora::{Parse as _, Parser},
/// };
/// use smear_schema::Schema;
///
/// # struct Space;
/// # impl Values for Space {
/// #   type Value = String;
/// #   fn is_null(&self, _: &String) -> bool { false }
/// #   fn as_bool(&self, _: &String) -> Option<bool> { None }
/// #   fn as_str<'a>(&'a self, value: &'a String) -> Option<&'a str> { Some(value) }
/// #   fn list_len(&self, _: &String) -> Option<usize> { None }
/// #   fn list_item(&mut self, _: &String, _: usize) -> String { String::new() }
/// #   fn type_name<'a>(&'a self, _: &'a String) -> Option<&'a str> { None }
/// #   fn coerce_leaf(&mut self, value: String, _: Leaf<'_>) -> Option<String> { Some(value) }
/// #   fn variable(&mut self, _: &str) -> Option<String> { None }
/// # }
/// let schema = Schema::build(
///   &Parser::with_parser::<GraphqlLexer<'_, str>, TypeSystemDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///     type_system_document,
///   )
///   .parse_str("type Query { greeting: String }")
///   .expect("the SDL parses"),
/// )
/// .expect("the SDL is a schema");
///
/// let document = Parser::with_parser::<GraphqlLexer<'_, str>, ExecutableDocument<&str>, GraphqlErrors<&str>, _, GraphQL>(
///   executable_document,
/// )
/// .parse_str("query A { greeting } query B { greeting }")
/// .expect("the query parses");
///
/// let mut space = Space;
/// let mut executor = Executor::new(&schema, &document);
/// let Err(error) = executor.start(&mut space, None, String::new()) else {
///   panic!("two named operations and no name is draft §6.1's ambiguity")
/// };
/// assert_eq!(error, StartError::AmbiguousOperation);
///
/// // The ceilings are the refusing executor's, so the map this result may retain is bounded by
/// // the same budget the operation would have run under.
/// let mut result = RequestErrorResult::new(executor.limits(), error);
/// let mut extensions = Extensions::new(executor.limits());
/// extensions
///   .insert("traceId", "9f2c".to_owned())
///   .expect("well under `max_extension_entries`");
/// result
///   .set_extensions(extensions)
///   .expect("built under the same ceilings this result carries");
///
/// // §7.1.3's map, entry by entry: a non-empty `errors`, an optional `extensions`, and no `data`.
/// assert_eq!(result.error(), Some(StartError::AmbiguousOperation));
/// let mut errors = result.errors();
/// assert_eq!(errors.len(), 1);
/// assert_eq!(
///   errors.next().expect("non-empty by construction").to_string(),
///   "the document contains more than one operation and none was named",
/// );
/// assert_eq!(
///   result.extensions().and_then(|map| map.get("traceId")).map(String::as_str),
///   Some("9f2c"),
/// );
/// ```
///
/// # Worked example: the driver's own errors
///
/// No executor exists here: these are draft §5 refusals of a document that will never run, so the
/// ceilings are the ones the deployment runs its executors under. A driver takes the spans from
/// its validator — they are written out below — and `smear::json` turns them into §7.1.6's `line`
/// and `column` against the document.
///
/// ```
/// use core::num::NonZeroU32;
///
/// use graphql_proto::{
///   Limits, RequestError, RequestErrorCeiling, RequestErrorEntry, RequestErrorResult,
/// };
/// use tokora::SimpleSpan;
///
/// // `{ hero(id: $id) { nickname } }`, where the operation declares no `$id` and `Character`
/// // has no `nickname`.
/// let limits = Limits::default()
///   .with_max_request_errors(NonZeroU32::new(2).expect("two is not zero"));
/// let nickname = RequestError::<()>::new(
///   &limits,
///   "Cannot query field \"nickname\" on type \"Character\".",
///   &[SimpleSpan::new(18, 26)],
/// )
/// .expect("well under both ceilings");
/// let mut result = RequestErrorResult::from_driver(&limits, nickname)
///   .expect("built under the ceilings this result carries");
///
/// let id = RequestError::new(
///   &limits,
///   "Variable \"$id\" is not defined.",
///   &[SimpleSpan::new(11, 14)],
/// )
/// .expect("well under both ceilings");
/// result.push(id).expect("room for a second entry");
///
/// // A third is over `max_request_errors`, and comes back whole rather than being dropped.
/// let third = RequestError::new(&limits, "Unknown argument \"first\".", &[]).expect("it fits");
/// let refused = result.push(third).expect_err("two is the ceiling");
/// assert_eq!(refused.ceiling(), RequestErrorCeiling::Errors);
/// assert_eq!(refused.into_error().message(), "Unknown argument \"first\".");
///
/// // §7.1.3's list, entry by entry, and no executor's refusal among them.
/// assert_eq!(result.error(), None);
/// let messages: Vec<String> = result.errors().map(|entry| entry.to_string()).collect();
/// assert_eq!(
///   messages,
///   [
///     "Cannot query field \"nickname\" on type \"Character\".",
///     "Variable \"$id\" is not defined.",
///   ],
/// );
/// assert!(
///   result
///     .errors()
///     .all(|entry| matches!(entry, RequestErrorEntry::Driver(_)))
/// );
/// ```
pub struct RequestErrorResult<V> {
  /// The first entry of `errors`, which every result has.
  ///
  /// A field rather than the head of a `Vec`, which is what makes "non-empty" a property of the
  /// type instead of an invariant a constructor has to keep.
  head: Head<V>,
  /// Every entry after the first, all of them the driver's.
  ///
  /// Grown by [`push`](RequestErrorResult::push) alone, by doubling clamped to the count ceiling,
  /// so its capacity never passes the `max_request_errors - 1` entries it may hold.
  rest: std::vec::Vec<RequestError<V>>,
  extensions: Option<Extensions<V>>,
  /// The ceilings this result was created under, copied out of the [`Limits`] it was handed.
  ceilings: Ceilings,
}

/// The first entry of a result's `errors`, which is one of two kinds.
enum Head<V> {
  /// The executor's refusal.
  Start(StartError),
  /// The driver's entry.
  Driver(RequestError<V>),
}

/// The five [`Limits`] fields a [`RequestErrorResult`] reads, as they stood when it was built.
///
/// Copied rather than borrowed so the result outlives whatever produced them: a driver commonly
/// answers the request long after dropping the executor that refused it, and a `&Limits` would tie
/// the response's lifetime to the executor's for no benefit — `Limits` is `Copy`, and five `u32`
/// are all of it this type reads.
#[derive(Clone, Copy)]
struct Ceilings {
  /// [`Limits::max_extension_entries`], for the result's map and for every entry's.
  extension_entries: u32,
  /// [`Limits::max_extension_key_bytes`], for the result's map and for every entry's.
  extension_key_bytes: u32,
  /// [`Limits::max_request_errors`].
  errors: u32,
  /// [`Limits::max_request_error_message_bytes`].
  message_bytes: u32,
  /// [`Limits::max_request_error_locations`].
  locations: u32,
}

impl Ceilings {
  #[inline]
  const fn new(limits: &Limits) -> Self {
    Self {
      extension_entries: limits.max_extension_entries.get(),
      extension_key_bytes: limits.max_extension_key_bytes.get(),
      errors: limits.max_request_errors.get(),
      message_bytes: limits.max_request_error_message_bytes.get(),
      locations: limits.max_request_error_locations.get(),
    }
  }

  /// Which extension ceiling `extensions` is over, if either.
  ///
  /// Entries first, then key bytes, which is the order [`Extensions::insert`] checks them in, so a
  /// map over both is refused by the same ceiling at every door.
  #[inline]
  fn extensions_over<V>(self, extensions: &Extensions<V>) -> Option<Ceiling> {
    if extensions.len() as u64 > u64::from(self.extension_entries) {
      Some(Ceiling::Entries)
    } else if u64::from(extensions.key_bytes()) > u64::from(self.extension_key_bytes) {
      Some(Ceiling::KeyBytes)
    } else {
      None
    }
  }

  /// Re-checks every quantity an entry reports, and re-derives its map's spine on acceptance.
  ///
  /// The order is the entry's own construction order — message, then spans — and then the map,
  /// so the same entry is refused by the same ceiling whichever door it meets. The spine is
  /// re-derived only after every check has passed, so a refused entry goes back to the driver with
  /// the allocation it arrived with.
  fn admit<V>(self, mut error: RequestError<V>) -> Result<RequestError<V>, RefusedEntry<V>> {
    let refusal = if error.message.len() as u64 > u64::from(self.message_bytes) {
      Some(RequestErrorCeiling::MessageBytes)
    } else if error.locations.len() as u64 > u64::from(self.locations) {
      Some(RequestErrorCeiling::Locations)
    } else {
      error
        .extensions
        .as_ref()
        .and_then(|extensions| self.extensions_over(extensions))
        .map(RequestErrorCeiling::Extensions)
    };
    if let Some(ceiling) = refusal {
      return Err(RefusedEntry { ceiling, error });
    }
    if let Some(extensions) = error.extensions.as_mut() {
      extensions.shrink_to_ceiling(self.extension_entries);
    }
    Ok(error)
  }
}

/// The smallest spine [`RequestErrorResult::push`] grows `rest` to, where the ceiling allows it.
///
/// `Vec`'s own first allocation for an element this size, so clamping the growth changes nothing
/// about a short list.
const MIN_SPINE: usize = 4;

impl<V> RequestErrorResult<V> {
  /// Builds the result for a [`StartError`], under `limits`.
  ///
  /// Pass [`Executor::limits`](crate::Executor::limits) — the ceilings of the executor that
  /// refused — so that the map this result may retain, and any entry the driver pushes after the
  /// refusal, are bounded by the budget the operation itself would have run under. Nothing forces
  /// that, for the same reason nothing forces [`Extensions::new`]'s argument: this value is the
  /// caller's and the caller drops it, so the ceilings bound what *this* result retains and are not
  /// a claim about anyone else's memory.
  ///
  /// Nothing is allocated. An `extensions` entry is absent until
  /// [`set_extensions`](RequestErrorResult::set_extensions) is called, which is a different
  /// response from a present empty map — the distinction
  /// [`Extensions::is_empty`] exists to keep.
  #[inline]
  #[must_use]
  pub const fn new(limits: &Limits, error: StartError) -> Self {
    Self {
      head: Head::Start(error),
      rest: std::vec::Vec::new(),
      extensions: None,
      ceilings: Ceilings::new(limits),
    }
  }

  /// Builds the result for the driver's own request errors, starting with `first`, under `limits`.
  ///
  /// The door for the three request-error sources that never reach an executor — a syntax error,
  /// a validation failure, a variable coercion failure — and [`push`](RequestErrorResult::push)
  /// lists the rest. Taking the first entry as an argument is what makes the list non-empty by
  /// construction: there is no result without one.
  ///
  /// `limits` is the budget this result retains under, for the reasons [`new`](Self::new) gives;
  /// a driver with no executor yet passes the [`Limits`] it runs its executors under. Nothing else
  /// is allocated: the list's spine is empty until a second entry is pushed.
  ///
  /// # Refusal
  ///
  /// [`RefusedEntry`], carrying `first` back, when one of the quantities it reports is over this
  /// result's ceilings: [`MessageBytes`](RequestErrorCeiling::MessageBytes),
  /// [`Locations`](RequestErrorCeiling::Locations), or
  /// [`Extensions`](RequestErrorCeiling::Extensions) for its map. An entry built under the same
  /// `limits` passes every one of them. Never [`Errors`](RequestErrorCeiling::Errors):
  /// [`Limits::max_request_errors`] is never below one.
  ///
  /// On acceptance the entry's map has its spine re-derived, for the reason
  /// [`set_extensions`](RequestErrorResult::set_extensions) gives.
  pub fn from_driver(limits: &Limits, first: RequestError<V>) -> Result<Self, RefusedEntry<V>> {
    let ceilings = Ceilings::new(limits);
    let first = ceilings.admit(first)?;
    Ok(Self {
      head: Head::Driver(first),
      rest: std::vec::Vec::new(),
      extensions: None,
      ceilings,
    })
  }

  /// Returns the executor's refusal, when this result reports one.
  ///
  /// `Some` for a result [`new`](RequestErrorResult::new) built — the same value
  /// [`start`](crate::Executor::start) returned — and `None` for one
  /// [`from_driver`](RequestErrorResult::from_driver) built, whose entries are all the driver's.
  /// Kept as an accessor rather than left to `errors().next()` because branching on the failure is
  /// what a driver does with it, and a classification reached through an iterator reads as though
  /// there might be another.
  #[inline]
  pub const fn error(&self) -> Option<StartError> {
    match &self.head {
      Head::Start(error) => Some(*error),
      Head::Driver(_) => None,
    }
  }

  /// Returns the result's `errors` entry: draft §7.1.3's non-empty list.
  ///
  /// At least one item, always, and the first is the one the result was built with — the
  /// executor's refusal for [`new`](RequestErrorResult::new), the driver's first entry for
  /// [`from_driver`](RequestErrorResult::from_driver) — followed by every entry
  /// [`push`](RequestErrorResult::push) accepted, in order. The length is in the type —
  /// [`ExactSizeIterator`] — so a serialiser writing this entry never has to check for the empty
  /// list §7.1.3 forbids.
  ///
  /// Each item is a [`RequestErrorEntry`], which reads either kind the same way: §7.1.6's
  /// `message` through `Display`, and `locations` and `extensions` through its accessors.
  #[inline]
  pub fn errors(&self) -> impl ExactSizeIterator<Item = RequestErrorEntry<'_, V>> {
    let head = match &self.head {
      Head::Start(error) => RequestErrorEntry::Start(*error),
      Head::Driver(error) => RequestErrorEntry::Driver(error),
    };
    Entries {
      head: Some(head),
      rest: self.rest.iter(),
    }
  }

  /// Lists another of the driver's request errors after the ones already here.
  ///
  /// It appends to either kind of result: §7.1.3's list holds the request errors raised during the
  /// request, whoever raised them.
  ///
  /// # Refusal
  ///
  /// [`RefusedEntry`], carrying `error` back, and nothing is retained:
  ///
  /// - [`Errors`](RequestErrorCeiling::Errors) when the list already holds
  ///   [`Limits::max_request_errors`] entries — the first included, whichever kind it is — and
  ///   then, as [`from_driver`](RequestErrorResult::from_driver) checks them,
  /// - [`MessageBytes`](RequestErrorCeiling::MessageBytes),
  ///   [`Locations`](RequestErrorCeiling::Locations) or
  ///   [`Extensions`](RequestErrorCeiling::Extensions) when a quantity the entry reports is over
  ///   this result's ceilings, which an entry built under a laxer [`Limits`] can be.
  ///
  /// # On acceptance two allocations are bounded, and neither is a refusal
  ///
  /// The entry's map has its spine re-derived, for the reason
  /// [`set_extensions`](RequestErrorResult::set_extensions) gives. And the list's own spine grows
  /// by doubling clamped to the count ceiling, so it never holds a slot
  /// [`Limits::max_request_errors`] would refuse — `Vec`'s plain doubling would leave a result
  /// with a ceiling of a hundred holding a hundred and twenty-eight.
  pub fn push(&mut self, error: RequestError<V>) -> Result<(), RefusedEntry<V>> {
    // The head is the `1`: every result has one, whichever kind it is.
    if 1 + self.rest.len() as u64 >= u64::from(self.ceilings.errors) {
      return Err(RefusedEntry {
        ceiling: RequestErrorCeiling::Errors,
        error,
      });
    }
    let error = self.ceilings.admit(error)?;
    if self.rest.len() == self.rest.capacity() {
      // Positive: the check above leaves room for at least this entry, and `rest` may hold
      // `max_request_errors - 1` of them, the head being the other.
      let room = self.ceilings.errors as usize - 1 - self.rest.len();
      self
        .rest
        .reserve_exact(self.rest.capacity().max(MIN_SPINE).min(room));
    }
    self.rest.push(error);
    Ok(())
  }

  /// Returns the result's draft §7.1.7 *Extensions* entry, or `None` when it has none.
  ///
  /// `None` is an absent key and never an empty map, exactly as it is on
  /// [`Response::extensions`](crate::Response::extensions): §7.1.3 makes the entry optional
  /// ("**may** also contain"), and `Some` holding an empty [`Extensions`] is the *present* empty
  /// map, which is a different response.
  #[inline]
  pub const fn extensions(&self) -> Option<&Extensions<V>> {
    self.extensions.as_ref()
  }

  /// Attaches the draft §7.1.7 *Extensions* map, returning whatever map it replaced.
  ///
  /// This is the **second** of §7.1.7's two sites — "The `extensions` entry in an *execution
  /// result* or *request error result*" — and the first is
  /// [`Executor::set_extensions`](crate::Executor::set_extensions). The two behave alike on
  /// purpose, because the resource is the same one.
  ///
  /// # Refusal
  ///
  /// One way, and it hands the map back: [`TooLarge`], when the map's entries or key bytes exceed
  /// the ceilings this result was created under. There is no lifecycle refusal, because a result
  /// has no phases — it is already the response.
  ///
  /// # On acceptance the allocation is re-derived, and that is not a refusal
  ///
  /// The two ceilings bound what the map *reports*; this bounds what it *holds*. A map grown to a
  /// million entries under a lax [`Limits`] and emptied back down reports `len() == 0` and
  /// `key_bytes() == 0` through both checks above while still holding a million entry slots,
  /// because [`Extensions::remove`] refunds the budget and returns no slot to the allocator. So an
  /// accepted map's spine is re-derived from the entries it actually carries whenever it is over
  /// [`Limits::max_extension_entries`]. Nothing is cloned, nothing is dropped, and no observable
  /// of [`Extensions`] changes — which is why it is a normalization and not a fourth refusal a
  /// caller could not act on anyway, there being no `shrink` out here to act with.
  ///
  /// It runs **after** the refusal check, so a map this result is not going to keep is not
  /// reallocated on its way back out to the driver.
  pub fn set_extensions(
    &mut self,
    mut extensions: Extensions<V>,
  ) -> Result<Option<Extensions<V>>, TooLarge<V>> {
    if let Some(ceiling) = self.ceilings.extensions_over(&extensions) {
      return Err(TooLarge {
        ceiling,
        extensions,
      });
    }
    extensions.shrink_to_ceiling(self.ceilings.extension_entries);
    Ok(self.extensions.replace(extensions))
  }

  /// Removes the draft §7.1.7 *Extensions* map, returning it, so the result carries no such entry.
  ///
  /// The counterpart to [`Executor::take_extensions`](crate::Executor::take_extensions) and there
  /// for the same reason: absence must not be a one-way door, and a driver that changes its mind
  /// gets its values back rather than having to drop the whole result to release them — which
  /// matters when one of them is a handle.
  #[inline]
  pub fn take_extensions(&mut self) -> Option<Extensions<V>> {
    self.extensions.take()
  }
}

impl<V> fmt::Debug for RequestErrorResult<V> {
  /// Unbounded in `V`, for the reason [`TooLarge`]'s is.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("RequestErrorResult")
      .field("errors", &Listed(self))
      // `Extensions`'s own `Debug` renders keys without requiring `V: Debug`.
      .field("extensions", &self.extensions)
      .finish()
  }
}

/// A result's `errors`, as a `Debug` list, without collecting them anywhere first.
struct Listed<'a, V>(&'a RequestErrorResult<V>);

impl<V> fmt::Debug for Listed<'_, V> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.0.errors()).finish()
  }
}

/// [`RequestErrorResult::errors`]'s iterator: the head, then the rest.
///
/// Its own type rather than a `Chain`, which is not an [`ExactSizeIterator`] — the sum of two
/// lengths can overflow in general — and the length is what lets a serialiser write the list
/// without checking for the empty one §7.1.3 forbids.
struct Entries<'r, V> {
  head: Option<RequestErrorEntry<'r, V>>,
  rest: core::slice::Iter<'r, RequestError<V>>,
}

impl<'r, V> Iterator for Entries<'r, V> {
  type Item = RequestErrorEntry<'r, V>;

  #[inline]
  fn next(&mut self) -> Option<Self::Item> {
    match self.head.take() {
      Some(head) => Some(head),
      None => self.rest.next().map(RequestErrorEntry::Driver),
    }
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    // Cannot overflow: `rest` holds fewer than `max_request_errors` entries, a `u32`.
    let len = usize::from(self.head.is_some()) + self.rest.len();
    (len, Some(len))
  }
}

impl<V> ExactSizeIterator for Entries<'_, V> {}

/// The spans a diagnostic's labels point at, in the diagnostic's order, skipping a label whose
/// location is [`Location::entire`](smear_schema::diagnostic::Location::entire).
///
/// The walk stops at the first index with no label, which is where [`Diagnose::labels`] promises
/// the count ends, so an implementor that broke that promise is read no further than it kept it.
fn label_spans<D>(diagnostic: &D) -> impl Iterator<Item = SimpleSpan>
where
  D: Diagnose + ?Sized,
{
  (0..diagnostic.labels())
    .map_while(move |index| diagnostic.label(index))
    .filter_map(|label| label.location().span())
}

/// A sink that keeps nothing but how many bytes were written to it — the measuring half of
/// [`RequestError::from_diagnose`]'s two renderings.
struct Measure(usize);

impl fmt::Write for Measure {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    self.0 = self.0.saturating_add(s.len());
    Ok(())
  }
}

/// A sink that keeps at most `room` more bytes, cut at a character boundary — the keeping half of
/// [`RequestError::from_diagnose`]'s two renderings.
///
/// `room` starts at the length the measuring half counted, so the text is never grown past the
/// allocation it was given. A `Display` that renders longer the second time is cut off at that
/// length rather than trusted.
struct Bounded {
  text: std::string::String,
  room: usize,
}

impl fmt::Write for Bounded {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    let mut cut = s.len().min(self.room);
    while !s.is_char_boundary(cut) {
      cut -= 1;
    }
    self.text.push_str(&s[..cut]);
    self.room = if cut == s.len() { self.room - cut } else { 0 };
    Ok(())
  }
}

#[cfg(test)]
mod tests;

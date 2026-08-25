//! What execution does with a name it cannot read.
//!
//! [`Executor::new`] accepts any `&ExecutableDocument<S>` for any `S: AsRef<[u8]>`, and says
//! nothing about what those bytes are. Draft §2.1.9 makes a *lexed* name `[_A-Za-z][_0-9A-Za-z]*`,
//! so a document that came out of the parser can only hold ASCII whatever `S` is — but a document
//! that was **assembled** is the executor's input too, and a persisted-query store, a query
//! rewriter or an FFI bridge holding foreign bytes assembles one.
//!
//! So the fixtures here are built rather than parsed. That is not a way of reaching a private
//! branch: it is the only way to write down the input the executor's own signature admits.
//!
//! What can be written down is narrower than that signature, and worth knowing while reading
//! these fixtures: `smear-parser` keeps `new` crate-private on every value leaf — `IntValue`,
//! `FloatValue`, `StringValue`, `BooleanValue`, `NullValue`, `EnumValue` — so an assembled
//! argument value can only be a variable, a list or an input object, never `1` or `"x"`. Nothing
//! here needs a scalar literal, but the gap is why `VariableValue::new` is public at all; its own
//! documentation carries the reason.
//!
//! Two paths read a variable's spelling — draft §6.4.1's `CoerceArgumentValues` at a field
//! argument, and draft §6.3's `@skip`/`@include` condition — and they must reach the same
//! conclusion about the same spelling. A conversion failure that yields a *different valid name*
//! is worse than one that raises: it asks the driver about a variable the document does not
//! contain, hands the resolver that invented name, and prints it in a diagnostic.

use graphql_proto::{ArgumentSource, Executor, Kind, Leaf, Node, Segment, Values, variable_key};
use smear_parser::{
  graphql::{
    GraphQL,
    ast::{
      Alias, Argument, ArgumentList, Described, Directive, Directives, ExecutableDefinition,
      ExecutableDocument, Field, InputValue, List, Name, Object, ObjectField, OperationDefinition,
      Selection, SelectionSet, TypeSystemDocument, VariableValue,
    },
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, type_system_document},
  },
  lexer::tokora::{Parse as _, Parser},
};
use smear_schema::Schema;
use tokora::SimpleSpan;

/// A spelling no `S` can turn into a `&str`: `0xff` never begins a UTF-8 sequence.
const UNREADABLE: &[u8] = b"\xff\x9f";

/// A second one, so a repair that collapses every unreadable name onto one readable name is
/// distinguishable from a repair that refuses to read either.
const ALSO_UNREADABLE: &[u8] = b"\xfe\x9f";

/// The spelling the control uses, which every conversion succeeds on.
const READABLE: &[u8] = b"flagged";

/// Spellings a `&str` renders perfectly and draft §2.1.9 does not admit.
///
/// The half of al8n/smear#139 a UTF-8 check cannot answer. Each of these *converts*, so a
/// conversion-shaped guard hands it to the driver as a lookup key — and a driver holding that key
/// then satisfies an argument with a value no variable in the document declared. The empty
/// spelling is the sharpest, because it is the exact key `from_utf8(..).unwrap_or("")` invented:
/// the fallback is gone, and a `$` with nothing after it must not arrive by the honest route
/// either.
const NOT_NAMES: &[&[u8]] = &[
  b"",                    // `$`
  b"1abc",                // draft §2.1.9's first character is `[_A-Za-z]`
  b"a b",                 // a space is in no position of the production
  b"a-b",                 // and neither is a hyphen
  "\u{1f642}".as_bytes(), // `$🙂`: four bytes, one `char`, and not ASCII
];

/// One spelling in its two Unicode normalisations, neither of which is a `Name`.
///
/// Two distinct `&str` keys here and one key in any driver that normalises before it looks up, so
/// under a UTF-8 check these are two document variables that can resolve to one runtime value.
/// That is the collapse this issue is about, arriving through the branch that *succeeds*.
const NFC: &[u8] = "caf\u{e9}".as_bytes();
const NFD: &[u8] = "cafe\u{301}".as_bytes();

const SDL: &str = r"
type Query {
  greeting: String
  guarded(flag: Boolean!): String
  filtered(filter: Filter): String
  listed(flags: [Boolean!]): String
}

input Filter {
  flag: Boolean
}
";

/// The driver's values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
  Text,
  True,
}

/// A driver that records every name it is asked about.
///
/// `supplied` is what it answers with, for **every** name. A driver that answers `Some` for a name
/// it was never given is not a misbehaving driver here — it is the instrument: it makes the
/// difference between "the executor asked" and "the executor did not" visible in the response as
/// well as in the log.
struct Space {
  asked: Vec<Vec<u8>>,
  supplied: Option<Value>,
  /// Every `ArgumentSource::Variable` name the driver was handed, for the arguments it resolved.
  received: Vec<Vec<u8>>,
}

impl Space {
  fn new(supplied: Option<Value>) -> Self {
    Self {
      asked: Vec::new(),
      supplied,
      received: Vec::new(),
    }
  }
}

impl Values for Space {
  type Value = Value;

  fn is_null(&self, _: &Value) -> bool {
    false
  }

  fn as_bool(&self, value: &Value) -> Option<bool> {
    matches!(value, Value::True).then_some(true)
  }

  fn list_len(&self, _: &Value) -> Option<usize> {
    None
  }

  fn list_item(&mut self, _: &Value, _: usize) -> Value {
    unreachable!("no position in this fixture has a list type")
  }

  fn type_name<'a>(&'a self, _: &'a Value) -> Option<&'a str> {
    None
  }

  fn coerce_leaf(&mut self, value: Value, _: Leaf<'_>) -> Option<Value> {
    Some(value)
  }

  fn variable(&mut self, name: &str) -> Option<Value> {
    self.asked.push(name.as_bytes().to_vec());
    self.supplied
  }
}

// ------------------------------------------------------------------------------------------
// the fixtures, assembled
// ------------------------------------------------------------------------------------------

fn span() -> SimpleSpan {
  SimpleSpan::new(0, 0)
}

fn name(source: &'static [u8]) -> Name<&'static [u8]> {
  Name::new(span(), source)
}

fn variable(spelling: &'static [u8]) -> InputValue<&'static [u8]> {
  InputValue::Variable(VariableValue::new(span(), name(spelling)))
}

fn document(selections: Vec<Selection<&'static [u8]>>) -> ExecutableDocument<&'static [u8]> {
  ExecutableDocument::new(
    span(),
    std::vec![Described::new(
      span(),
      None,
      ExecutableDefinition::Operation(OperationDefinition::Shorthand(SelectionSet::new(
        span(),
        selections.into(),
      ))),
    )],
  )
}

/// `{ guarded(flag: $spelling) }` — draft §6.4.1 step 5.f's variable read.
fn at_an_argument(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![Selection::Field(Field::new(
    span(),
    None,
    name(b"guarded"),
    Some(ArgumentList::new(
      span(),
      std::vec![Argument::new(span(), name(b"flag"), variable(spelling))],
    )),
    None,
    None,
  ))])
}

/// `{ greeting @skip(if: $spelling) }` — draft §6.3 step 3.a's condition.
fn at_a_condition(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![Selection::Field(Field::new(
    span(),
    None,
    name(b"greeting"),
    None,
    Some(Directives::new(
      span(),
      std::vec![Directive::new(
        span(),
        name(b"skip"),
        Some(ArgumentList::new(
          span(),
          std::vec![Argument::new(span(), name(b"if"), variable(spelling))],
        )),
      )],
    )),
    None,
  ))])
}

/// `{ spelling: greeting }` — draft §7.1.2's response key, which is the alias verbatim.
///
/// The *second* place a document's bytes have to become a `&str`, and the one the issue's census
/// does not name. An alias rather than the field's own name because it is the case that survives
/// every other guard: `greeting` resolves, the schema lookup succeeds, and the only thing standing
/// between these bytes and a key in `data` is the admission rule at the collection site. The
/// field's own name reaches that same rule — [`at_a_field_name`] is that branch — but a reader
/// could believe the schema miss was what stopped it.
fn at_a_response_key(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![Selection::Field(Field::new(
    span(),
    Some(Alias::new(span(), name(spelling))),
    name(b"greeting"),
    None,
    None,
    None,
  ))])
}

/// `{ spelling }` — the same response key, taken from the field's **own** name.
///
/// The other arm of the one `match field.alias()` that decides a response key, and not a second
/// site: draft §7.1.2 makes the key the alias *or* the name, so both arms hand their bytes to the
/// same admission rule. Worth writing down because the refusal here is the collection's and not the
/// schema's — draft §6.3 collects before draft §6.4 resolves, so a field name that is not a `Name`
/// is refused with an error rather than silently omitted by a `sym` miss it never reaches.
fn at_a_field_name(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![Selection::Field(Field::new(
    span(),
    None,
    name(spelling),
    None,
    None,
    None,
  ))])
}

/// `{ first: greeting second: greeting }` — two sibling response keys.
///
/// One key can be lost; two are what *collapse*, and the collapse is the finding. A response
/// carrying one key where the document asked for two is indistinguishable, from the client's side,
/// from a response that answered both.
fn two_response_keys(
  first: &'static [u8],
  second: &'static [u8],
) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![
    Selection::Field(Field::new(
      span(),
      Some(Alias::new(span(), name(first))),
      name(b"greeting"),
      None,
      None,
      None,
    )),
    Selection::Field(Field::new(
      span(),
      Some(Alias::new(span(), name(second))),
      name(b"greeting"),
      None,
      None,
      None,
    )),
  ])
}

/// `{ filtered(filter: {flag: $spelling}) }` — a variable draft §6.4.1 step 5.j leaves to the
/// driver.
///
/// The argument as a whole is a literal, so step 5's control flow decides about *it* and never
/// looks inside. Coercing the contents is input coercion, whose product is a value in the
/// service's representation, which is the one thing this crate cannot build — see
/// [`ArgumentSource::Literal`].
fn nested_in_an_object(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  argument_value(
    b"filtered",
    b"filter",
    InputValue::Object(Object::new(
      span(),
      std::vec![ObjectField::new(span(), name(b"flag"), variable(spelling))].into(),
    )),
  )
}

/// `{ listed(flags: [$spelling]) }` — the same boundary, one literal variant over.
fn nested_in_a_list(spelling: &'static [u8]) -> ExecutableDocument<&'static [u8]> {
  argument_value(
    b"listed",
    b"flags",
    InputValue::List(List::new(span(), std::vec![variable(spelling)].into())),
  )
}

fn argument_value(
  field: &'static [u8],
  argument: &'static [u8],
  value: InputValue<&'static [u8]>,
) -> ExecutableDocument<&'static [u8]> {
  document(std::vec![Selection::Field(Field::new(
    span(),
    None,
    name(field),
    Some(ArgumentList::new(
      span(),
      std::vec![Argument::new(span(), name(argument), value)],
    )),
    None,
    None,
  ))])
}

// ------------------------------------------------------------------------------------------
// driving
// ------------------------------------------------------------------------------------------

/// What one execution did, from the driver's side and from the response's.
struct Run {
  /// Every name [`Values::variable`] was asked about, in order.
  asked: Vec<Vec<u8>>,
  /// Every `ArgumentSource::Variable` name handed to the driver with a resolved argument.
  received: Vec<Vec<u8>>,
  /// Every variable spelling that reached the driver *inside* an
  /// [`ArgumentSource::Literal`](graphql_proto::ArgumentSource::Literal), in document order.
  ///
  /// Not a second reading of `received`: these are the ones the executor deliberately did not
  /// read, so this is the walk draft §6.4.1 step 5.j puts on the driver, performed here because
  /// nothing else performs it.
  nested: Vec<Vec<u8>>,
  /// The response's `errors`, classified.
  kinds: Vec<Kind>,
  /// The response's `errors`, rendered.
  messages: Vec<String>,
  /// Every key of `data`, in response order.
  keys: Vec<String>,
}

/// Collects every variable spelling nested inside a literal, in document order.
fn nested_variables<'a>(value: &InputValue<&'a [u8]>, out: &mut Vec<&'a [u8]>) {
  match value {
    InputValue::Variable(spelled) => out.push(spelled.name().source()),
    InputValue::List(list) => {
      for item in list.values() {
        nested_variables(item, out);
      }
    }
    InputValue::Object(object) => {
      for field in object.fields() {
        nested_variables(field.value(), out);
      }
    }
    _ => {}
  }
}

fn run(
  schema: &Schema,
  document: &ExecutableDocument<&'static [u8]>,
  supplied: Option<Value>,
) -> Run {
  let mut space = Space::new(supplied);
  let mut nested: Vec<Vec<u8>> = Vec::new();
  let mut executor = Executor::new(schema, document);
  executor
    .start(&mut space, None, Value::Text)
    .expect("the operation resolves");
  while let Some(request) = executor.poll_resolve(&mut space) {
    let id = request.id();
    let received: Vec<Vec<u8>> = request
      .arguments()
      .iter()
      .filter_map(|argument| match argument.source() {
        ArgumentSource::Variable { name, .. } => Some(name.as_bytes().to_vec()),
        _ => None,
      })
      .collect();
    for argument in request.arguments() {
      if let ArgumentSource::Literal(literal) = argument.source() {
        let mut spellings = Vec::new();
        nested_variables(literal, &mut spellings);
        nested.extend(spellings.into_iter().map(<[u8]>::to_vec));
      }
    }
    space.received.extend(received);
    executor.handle_resolved(&mut space, id, Value::Text);
  }
  let response = executor.poll_response().expect("nothing is outstanding");
  let kinds = response.errors().map(|error| error.kind()).collect();
  let messages = response.errors().map(|error| error.to_string()).collect();
  let keys = match response.data() {
    Node::Object(children) => children
      .map(|(key, _)| match key {
        Segment::Field(key) => key.to_owned(),
        Segment::Index(index) => index.to_string(),
      })
      .collect(),
    _ => Vec::new(),
  };
  Run {
    asked: space.asked,
    received: space.received,
    nested,
    kinds,
    messages,
    keys,
  }
}

fn schema() -> Schema {
  let sdl = Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str(SDL)
  .expect("the SDL parses");
  Schema::build(&sdl).expect("the SDL is a schema")
}

// ------------------------------------------------------------------------------------------
// the properties
// ------------------------------------------------------------------------------------------

/// The control: a readable spelling reaches the driver, unchanged, from **both** paths.
///
/// Without it every assertion below is satisfiable by a harness that drives nothing, and the
/// interesting ones are all assertions that something did *not* happen.
#[test]
fn a_readable_variable_name_reaches_the_driver_from_both_paths() {
  let schema = schema();
  let argument = run(&schema, &at_an_argument(READABLE), Some(Value::True));
  let condition = run(&schema, &at_a_condition(READABLE), Some(Value::True));

  assert_eq!(argument.asked, std::vec![READABLE.to_vec()]);
  assert_eq!(condition.asked, std::vec![READABLE.to_vec()]);
  assert_eq!(argument.asked, condition.asked);
  assert_eq!(
    argument.received,
    std::vec![READABLE.to_vec()],
    "the resolver is handed the spelling the document wrote"
  );
  assert!(argument.kinds.is_empty(), "{:?}", argument.messages);
  assert!(condition.kinds.is_empty(), "{:?}", condition.messages);
}

/// The two paths take the same branch for the same unreadable spelling.
///
/// "The same branch" is *not* the same [`Kind`]: draft §6.4.1's error is raised at the field whose
/// argument it is and draft §6.3's at the object whose selection set was being collected, so the
/// classification differs by position and always has. What must agree is the decision — whether
/// the driver is asked at all, and about what.
#[test]
fn the_two_paths_agree_about_an_unreadable_variable_name() {
  let schema = schema();
  let argument = run(&schema, &at_an_argument(UNREADABLE), Some(Value::True));
  let condition = run(&schema, &at_a_condition(UNREADABLE), Some(Value::True));

  assert_eq!(
    argument.asked, condition.asked,
    "draft §6.4.1 asked the driver {:?} and draft §6.3 asked it {:?} about the same spelling",
    argument.asked, condition.asked
  );
  assert!(
    argument.asked.is_empty(),
    "the driver was asked about {:?}, and this document names no such variable",
    argument.asked
  );
  assert_eq!(argument.kinds, std::vec![Kind::ArgumentVariableMissing]);
  assert_eq!(condition.kinds, std::vec![Kind::DirectiveCondition]);
}

/// An unreadable name does not become a name the driver has a value for.
///
/// The driver here answers `Some` for every name it is asked, so a substituted spelling does not
/// merely reach it — it *satisfies* draft §6.4.1 step 5.f, and the field resolves with a value no
/// variable in the document supplied.
#[test]
fn an_unreadable_variable_name_does_not_supply_an_argument() {
  let schema = schema();
  let argument = run(&schema, &at_an_argument(UNREADABLE), Some(Value::True));

  assert!(
    argument.received.is_empty(),
    "the resolver was handed a variable named {:?}",
    argument.received
  );
  assert_eq!(argument.kinds, std::vec![Kind::ArgumentVariableMissing]);
}

/// Two unreadable spellings are two variables, not one.
///
/// The defect a fallback introduces is not that a name is lost; it is that **every** unreadable
/// name becomes the *same* readable one, so distinct variables merge. Whatever the driver is told
/// about one, it must not be the same thing it is told about the other by virtue of the failure.
#[test]
fn two_unreadable_names_do_not_collapse_onto_one() {
  let schema = schema();
  let first = run(&schema, &at_an_argument(UNREADABLE), Some(Value::True));
  let second = run(&schema, &at_an_argument(ALSO_UNREADABLE), Some(Value::True));

  assert!(
    first.asked.is_empty() && second.asked.is_empty(),
    "two distinct spellings reached the driver as {:?} and {:?}",
    first.asked,
    second.asked
  );
}

/// Neither diagnostic quotes a variable it could not read.
///
/// Both messages have a spelling-free form for exactly this: "was provided a variable which was
/// not provided a runtime value". Rendering `"$"` instead points the reader at a variable whose
/// name is nothing.
#[test]
fn neither_diagnostic_renders_a_dollar_with_nothing_after_it() {
  let schema = schema();
  let argument = run(&schema, &at_an_argument(UNREADABLE), None);
  let condition = run(&schema, &at_a_condition(UNREADABLE), None);

  // Collected rather than asserted one at a time, because the two paths reach the same rendering
  // by different routes — draft §6.4.1's through the substituted lookup key and draft §6.3's
  // through the raw spelling it interns for the message — and stopping at the first would leave
  // the second unmeasured and its repair unpinned.
  let named_nothing: Vec<&String> = argument
    .messages
    .iter()
    .chain(condition.messages.iter())
    .filter(|message| message.contains("\"$\""))
    .collect();
  assert!(
    named_nothing.is_empty(),
    "these messages named a variable with the empty name: {named_nothing:#?}"
  );
  assert_eq!(argument.kinds, std::vec![Kind::ArgumentVariableMissing]);
  assert_eq!(condition.kinds, std::vec![Kind::DirectiveCondition]);
}

/// A response key that is not a name is reported, and no key stands in for it.
///
/// The second half of the census, and the one the issue does not name: `Segment::Field` handed
/// back a `&str` read out of the executor's arena through `unwrap_or("")`, so two sibling keys it
/// could not read became the same key — the same collapse as the variable, one step further on and
/// reachable without a variable at all.
#[test]
fn an_unreadable_response_key_is_reported_rather_than_rendered_empty() {
  let schema = schema();
  let readable = run(&schema, &at_a_response_key(READABLE), None);
  let unreadable = run(&schema, &at_a_response_key(UNREADABLE), None);

  assert_eq!(
    readable.keys,
    std::vec![String::from_utf8(READABLE.to_vec()).expect("ASCII")],
    "the control's alias is the response key, verbatim"
  );
  assert!(readable.kinds.is_empty(), "{:?}", readable.messages);

  assert_eq!(unreadable.kinds, std::vec![Kind::ResponseKeyUnreadable]);
  assert!(
    !unreadable.keys.iter().any(String::is_empty),
    "a key stood in for one that could not be read: {:?}",
    unreadable.keys
  );
}

// ------------------------------------------------------------------------------------------
// the key space is draft §2.1.9, and not UTF-8
// ------------------------------------------------------------------------------------------

/// A spelling that converts but is not a `Name` does not supply an argument.
///
/// The half a conversion-shaped guard cannot answer. Each of [`NOT_NAMES`] renders perfectly, so
/// `from_utf8(..).ok()` hands every one of them to the driver as a lookup key — and this driver
/// answers `Some` for every name it is asked, so reaching it does not merely leak a spelling, it
/// *satisfies* draft §6.4.1 step 5.f and resolves the field with a value no variable in the
/// document declared. The question the key space asks is not "can these bytes be printed" but
/// "can these bytes be a key draft §6.1 could have bound", and only draft §2.1.9 answers it.
#[test]
fn a_variable_name_that_is_not_a_name_does_not_supply_an_argument() {
  let schema = schema();
  for spelling in NOT_NAMES {
    let argument = run(&schema, &at_an_argument(spelling), Some(Value::True));
    let condition = run(&schema, &at_a_condition(spelling), Some(Value::True));

    assert!(
      argument.asked.is_empty(),
      "draft §6.4.1 asked the driver about {:?}, which is not a name",
      argument.asked
    );
    assert_eq!(
      argument.asked, condition.asked,
      "the two readers disagreed about {spelling:?}"
    );
    assert!(
      argument.received.is_empty(),
      "the resolver was handed a variable named {:?}",
      argument.received
    );
    assert_eq!(
      argument.kinds,
      std::vec![Kind::ArgumentVariableMissing],
      "{:?}",
      argument.messages
    );
    assert_eq!(
      condition.kinds,
      std::vec![Kind::DirectiveCondition],
      "{:?}",
      condition.messages
    );
  }
}

/// Two normalisations of one spelling are two variables, and neither is a key.
///
/// The empty-name collapse arriving through the branch that *succeeds*: both of these convert, so
/// a UTF-8 guard hands the driver two distinct `&str` keys that any driver normalising before it
/// looks up merges into one. Refusing both is what keeps "a spelling that is not a key names no
/// variable" true of a pair as well as of a single name.
#[test]
fn two_normalisations_of_one_spelling_are_neither_of_them_a_key() {
  assert_ne!(NFC, NFD, "the fixture must be two distinct byte strings");
  assert!(
    core::str::from_utf8(NFC).is_ok() && core::str::from_utf8(NFD).is_ok(),
    "both must convert, or this tests the unreadable case over again"
  );

  let schema = schema();
  let composed = run(&schema, &at_an_argument(NFC), Some(Value::True));
  let decomposed = run(&schema, &at_an_argument(NFD), Some(Value::True));

  assert!(
    composed.asked.is_empty() && decomposed.asked.is_empty(),
    "two normalisations reached the driver as {:?} and {:?}",
    composed.asked,
    decomposed.asked
  );
  assert_eq!(composed.kinds, std::vec![Kind::ArgumentVariableMissing]);
  assert_eq!(decomposed.kinds, std::vec![Kind::ArgumentVariableMissing]);
}

/// A response key that is not a name is refused, exactly as a variable key is.
///
/// The same predicate applied to the *other* name space, and the reason these cells exist as their
/// own test rather than as a line in the variable one: the round that introduced the response key's
/// guard checked UTF-8 there and draft §2.1.9 at the variable, while claiming in prose that both
/// spaces shared one predicate. Every spelling below converts, so a conversion-shaped guard interns
/// it and hands it back as a [`Segment::Field`] — an empty JSON object key for `b""`, and `1abc`,
/// `a b` or `🙂` for the rest, none of which any client asked a GraphQL service for.
#[test]
fn a_response_key_that_is_not_a_name_is_refused() {
  let schema = schema();
  for spelling in NOT_NAMES {
    let rendered = core::str::from_utf8(spelling).expect("every NOT_NAMES entry converts");

    for (shape, arm) in [
      (
        at_a_response_key as fn(&'static [u8]) -> ExecutableDocument<&'static [u8]>,
        "alias",
      ),
      (at_a_field_name, "field name"),
    ] {
      let run = run(&schema, &shape(spelling), None);

      assert_eq!(
        run.kinds,
        std::vec![Kind::ResponseKeyUnreadable],
        "{arm} {spelling:?} was admitted as a response key: {:?}",
        run.messages
      );
      assert!(
        !run.keys.iter().any(|key| key == rendered),
        "{arm} {spelling:?} became a key in `data`: {:?}",
        run.keys
      );
      assert!(
        !run.keys.iter().any(String::is_empty),
        "a key stood in for {arm} {spelling:?}: {:?}",
        run.keys
      );
    }
  }
}

/// Two normalisations of one spelling are two response keys, and neither is one.
///
/// The response side of the pair, and the sharper half of it: two distinct `&str` keys reach a
/// client as two entries of one JSON object, and a client that normalises before it indexes reads
/// one. That is a response whose shape does not match the document, produced with nothing in
/// `errors` to say so — so refusing both is what a UTF-8 guard cannot do, since both convert.
#[test]
fn two_normalisations_of_one_response_key_are_neither_of_them_a_key() {
  let schema = schema();

  let control = run(&schema, &two_response_keys(b"a", b"b"), None);
  assert_eq!(
    control.keys,
    std::vec![String::from("a"), String::from("b")],
    "two readable aliases are two keys, in document order: {:?}",
    control.messages
  );
  assert!(control.kinds.is_empty(), "{:?}", control.messages);

  let pair = run(&schema, &two_response_keys(NFC, NFD), None);
  assert_eq!(
    pair.kinds,
    std::vec![Kind::ResponseKeyUnreadable],
    "{:?}",
    pair.messages
  );
  assert!(
    pair.keys.is_empty(),
    "a normalisation of `café` became a response key: {:?}",
    pair.keys
  );
}

/// [`variable_key`] is the whole key space, and a caller can ask it the same question.
///
/// The function is `pub` because the executor is not its only reader — see the nested-literal
/// test below — so what it admits is API and pinned here rather than inferred from a response.
#[test]
fn variable_key_admits_exactly_the_name_production() {
  assert_eq!(variable_key(READABLE), Some("flagged"));
  assert_eq!(variable_key(b"_"), Some("_"));
  assert_eq!(variable_key(b"_0aZ"), Some("_0aZ"));
  for spelling in NOT_NAMES {
    assert_eq!(variable_key(spelling), None, "admitted {spelling:?}");
  }
  assert_eq!(variable_key(UNREADABLE), None);
  assert_eq!(variable_key(NFC), None);
  assert_eq!(variable_key(NFD), None);
}

// ------------------------------------------------------------------------------------------
// the boundary the executor does not cross
// ------------------------------------------------------------------------------------------

/// A variable inside a list or an input object reaches the driver unread, whatever it spells.
///
/// Draft §6.4.1 step 5.j — coercing a literal's contents — is the driver's, because its product is
/// a value in the service's representation and this crate builds none. So the executor decides
/// about the argument *as a whole*, hands the literal over, and never asks
/// [`Values::variable`] about anything inside it. That is the contract
/// [`ArgumentSource::Literal`](graphql_proto::ArgumentSource::Literal) states, and it predates
/// this repair: it is where `ArgumentSource` has drawn the line since the executor was written.
///
/// What is pinned here is that the executor does not quietly start reading them — which would
/// change *which* variables a request must supply — and that the spelling arrives verbatim, so
/// the driver's own read is a read of the document's bytes and not of something substituted on
/// the way. [`variable_key`] is what that read must be: the same refusal, in the same function,
/// rather than a second `from_utf8` per driver.
#[test]
fn a_variable_nested_in_a_literal_reaches_the_driver_unread() {
  let schema = schema();
  for shape in [
    nested_in_an_object as fn(&'static [u8]) -> ExecutableDocument<&'static [u8]>,
    nested_in_a_list,
  ] {
    for spelling in [READABLE, UNREADABLE] {
      let run = run(&schema, &shape(spelling), Some(Value::True));

      assert!(
        run.asked.is_empty(),
        "the executor read a nested variable and asked the driver about {:?}",
        run.asked
      );
      assert_eq!(
        run.nested,
        std::vec![spelling.to_vec()],
        "the driver was handed a literal whose variable is not the one the document wrote"
      );
      assert!(run.kinds.is_empty(), "{:?}", run.messages);
    }
  }

  // And the read the driver then performs is this one, which refuses the spelling the executor's
  // own readers refuse. Without it every driver writes the conversion again, which is the shape
  // al8n/smear#139 is about rather than any one occurrence of it.
  assert_eq!(variable_key(READABLE), Some("flagged"));
  assert_eq!(variable_key(UNREADABLE), None);
}

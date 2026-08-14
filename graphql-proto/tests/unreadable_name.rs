//! What execution does with a name it cannot read.
//!
//! [`Executor::new`] accepts any `&ExecutableDocument<S>` for any `S: AsRef<[u8]>`, and says
//! nothing about what those bytes are. Draft §2.1.9 makes a *lexed* name `[_A-Za-z][_0-9A-Za-z]*`,
//! so a document that came out of the parser can only hold ASCII whatever `S` is — but a document
//! that was **assembled** is the executor's input too, and a persisted-query store, a query
//! rewriter or an FFI bridge holding foreign bytes assembles one. Every node of the AST has a
//! public constructor for exactly that reason.
//!
//! So the fixtures here are built rather than parsed. That is not a way of reaching a private
//! branch: it is the only way to write down the input the executor's own signature admits.
//!
//! Two paths read a variable's spelling — draft §6.4.1's `CoerceArgumentValues` at a field
//! argument, and draft §6.3's `@skip`/`@include` condition — and they must reach the same
//! conclusion about the same spelling. A conversion failure that yields a *different valid name*
//! is worse than one that raises: it asks the driver about a variable the document does not
//! contain, hands the resolver that invented name, and prints it in a diagnostic.

use graphql_proto::{ArgumentSource, Executor, Kind, Leaf, Node, Segment, Values};
use smear_parser::{
  graphql::{
    GraphQL,
    ast::{
      Alias, Argument, ArgumentList, Described, Directive, Directives, ExecutableDefinition,
      ExecutableDocument, Field, InputValue, Name, OperationDefinition, Selection, SelectionSet,
      TypeSystemDocument, VariableValue,
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

const SDL: &str = r"
type Query {
  greeting: String
  guarded(flag: Boolean!): String
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
        selections,
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
/// does not name. The field's own name is looked up in the schema and a miss simply omits the
/// position, so an unreadable field name never reaches a response — an unreadable **alias** on a
/// resolvable field does.
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

// ------------------------------------------------------------------------------------------
// driving
// ------------------------------------------------------------------------------------------

/// What one execution did, from the driver's side and from the response's.
struct Run {
  /// Every name [`Values::variable`] was asked about, in order.
  asked: Vec<Vec<u8>>,
  /// Every `ArgumentSource::Variable` name handed to the driver with a resolved argument.
  received: Vec<Vec<u8>>,
  /// The response's `errors`, classified.
  kinds: Vec<Kind>,
  /// The response's `errors`, rendered.
  messages: Vec<String>,
  /// Every key of `data`, in response order.
  keys: Vec<String>,
}

fn run(
  schema: &Schema,
  document: &ExecutableDocument<&'static [u8]>,
  supplied: Option<Value>,
) -> Run {
  let mut space = Space::new(supplied);
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

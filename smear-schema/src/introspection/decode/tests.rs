use std::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use super::read;
use crate::introspection::{
  ResponseErrorKind,
  json::{Text, visited},
  model::{IntrospectedSchema, IntrospectedType, Name, TypeRef},
};

// ---------------------------------------------------------------------------------------------
// fixtures
// ---------------------------------------------------------------------------------------------

/// A minimal query root, so a fixture's defect is the only thing wrong with it.
const OK_QUERY: &str = r#"{"kind":"OBJECT","name":"Query","fields":[
  {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
]}"#;

/// The members of a bare `__Schema`, without the braces, so a root can carry them beside a `data`.
const BARE_BODY: &str = r#""queryType":{"name":"Query"},"directives":[],"types":[
  {"kind":"OBJECT","name":"Query","fields":[
    {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
  ]}
]"#;

fn response(types: &str) -> String {
  format!(r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],"types":[{types}]}}}}"#)
}

fn with_directives(directives: &str) -> String {
  format!(
    r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[{directives}],"types":[{OK_QUERY}]}}}}"#
  )
}

/// The kind and the owner of the one refusal a response produces.
fn refused(response: &str) -> (ResponseErrorKind, Option<String>) {
  match read(response) {
    Err(error) => (error.kind(), error.owner().map(String::from)),
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{response}"),
  }
}

fn subject_of(response: &str) -> String {
  match read(response) {
    Err(error) => error.subject().to_string(),
    Ok(_) => panic!("expected a refusal, got a schema\n---\n{response}"),
  }
}

// ---------------------------------------------------------------------------------------------
// nothing is copied, proved over every borrow rather than sampled
// ---------------------------------------------------------------------------------------------

/// Whether `borrowed` is a slice of `response` rather than merely equal to one.
fn slice_of(response: &str, borrowed: &str) -> bool {
  let base = response.as_ptr() as usize;
  let at = borrowed.as_ptr() as usize;
  at >= base && at + borrowed.len() <= base + response.len()
}

/// Visits every borrow in a decoded model and counts what it saw.
///
/// A census rather than a spot check: "names are not copied" is a claim about *every* name, and a
/// test that reads one of them would keep passing after the other twenty-seven started to
/// allocate.
#[derive(Debug)]
struct Census<'a> {
  response: &'a str,
  names: usize,
  borrowed_texts: usize,
  copied_texts: usize,
}

impl<'a> Census<'a> {
  fn over(response: &'a str, schema: &IntrospectedSchema<'a>) -> Self {
    let mut census = Self {
      response,
      names: 0,
      borrowed_texts: 0,
      copied_texts: 0,
    };
    census.name(schema.query_type.name);
    census.name(schema.mutation_type.as_ref().and_then(|root| root.name));
    census.name(schema.subscription_type.as_ref().and_then(|root| root.name));
    for ty in &schema.types {
      census.ty(ty);
    }
    for directive in &schema.directives {
      census.name(Some(directive.name));
      for arg in &directive.args {
        census.name(Some(arg.name));
        census.type_ref(&arg.ty);
        census.text(arg.default_value.as_ref());
      }
    }
    census
  }

  fn ty(&mut self, ty: &IntrospectedType<'a>) {
    self.name(ty.name);
    for field in ty.fields.iter().flatten() {
      self.name(Some(field.name));
      self.type_ref(&field.ty);
      for arg in &field.args {
        self.name(Some(arg.name));
        self.type_ref(&arg.ty);
        self.text(arg.default_value.as_ref());
      }
    }
    for input in ty.input_fields.iter().flatten() {
      self.name(Some(input.name));
      self.type_ref(&input.ty);
      self.text(input.default_value.as_ref());
    }
    for reference in ty.interfaces.iter().flatten() {
      self.type_ref(reference);
    }
    for reference in ty.possible_types.iter().flatten() {
      self.type_ref(reference);
    }
    for value in ty.enum_values.iter().flatten() {
      self.name(Some(value.name));
    }
  }

  fn type_ref(&mut self, reference: &TypeRef<'a>) {
    self.name(reference.name);
    if let Some(inner) = reference.of_type.as_deref() {
      self.type_ref(inner);
    }
  }

  fn name(&mut self, name: Option<Name<'a>>) {
    let Some(name) = name else { return };
    assert!(
      slice_of(self.response, name.as_str()),
      "`{}` is not a slice of the response it was read from",
      name.as_str()
    );
    self.names += 1;
  }

  fn text(&mut self, text: Option<&Text<'a>>) {
    let Some(text) = text else { return };
    if text.is_borrowed() {
      assert!(
        slice_of(self.response, text.as_str()),
        "`{}` says it is borrowed and is not a slice of the response",
        text.as_str()
      );
      self.borrowed_texts += 1;
    } else {
      self.copied_texts += 1;
    }
  }
}

/// A response exercising every slot the model has, so a census over it is not thin.
///
/// `pad` lengthens every name, which is what a copying reader would pay for and a borrowing one
/// does not. `default` is the one prose member.
fn corpus(pad: usize, default: &str) -> String {
  let p = "z".repeat(pad);
  format!(
    r#"{{"data":{{"__schema":{{
      "queryType":{{"name":"Query{p}"}},
      "mutationType":{{"name":"Mut{p}"}},
      "subscriptionType":null,
      "directives":[
        {{"name":"tag{p}","locations":["FIELD_DEFINITION","OBJECT"],"args":[
          {{"name":"note{p}","type":{{"kind":"SCALAR","name":"String{p}"}},"defaultValue":null}}
        ],"isRepeatable":true}}
      ],
      "types":[
        {{"kind":"OBJECT","name":"Query{p}","interfaces":[{{"kind":"INTERFACE","name":"Node{p}"}}],
         "fields":[
          {{"name":"ok{p}","args":[
            {{"name":"arg{p}","type":{{"kind":"SCALAR","name":"String{p}"}},"defaultValue":{default}}}
          ],"type":{{"kind":"NON_NULL","name":null,"ofType":
            {{"kind":"LIST","name":null,"ofType":{{"kind":"SCALAR","name":"Int{p}"}}}}}}}}
        ]}},
        {{"kind":"OBJECT","name":"Mut{p}","fields":[
          {{"name":"go{p}","args":[],"type":{{"kind":"SCALAR","name":"Int{p}"}}}}
        ]}},
        {{"kind":"INTERFACE","name":"Node{p}","interfaces":[],"fields":[
          {{"name":"id{p}","args":[],"type":{{"kind":"SCALAR","name":"ID{p}"}}}}
        ]}},
        {{"kind":"ENUM","name":"Verdict{p}","enumValues":[{{"name":"YES{p}"}},{{"name":"NO{p}"}}]}},
        {{"kind":"UNION","name":"U{p}","possibleTypes":[{{"kind":"OBJECT","name":"Query{p}"}}]}},
        {{"kind":"INPUT_OBJECT","name":"In{p}","isOneOf":true,"inputFields":[
          {{"name":"f{p}","type":{{"kind":"SCALAR","name":"String{p}"}},"defaultValue":"1"}}
        ]}}
      ]
    }}}}}}"#
  )
}

/// Every name a response of names alone carries comes back as a slice of the response.
///
/// This is the allocation property said the strongest way it can be said: not "the counter did not
/// move" but "these bytes are the response's own bytes". A reader that copied a name would hand
/// back a pointer outside the buffer and fail here whatever an allocator reported.
#[test]
fn a_document_of_only_names_copies_nothing() {
  let response = corpus(0, "null");
  let schema = read(&response).expect("a schema");
  let census = Census::over(&response, &schema);

  assert_eq!(
    census.copied_texts, 0,
    "a response with no escape in it produced a copy"
  );
  // The census must not pass by having looked at almost nothing.
  assert!(
    census.names >= 25,
    "the census reached only {} names; the corpus shrank",
    census.names
  );
}

/// The same, with names long enough that a copy would be unmissable.
///
/// Nothing about the assertion changes with `pad`, which is the point: the reader's output is the
/// input's bytes at every length, so there is no size at which it starts copying.
#[test]
fn borrowing_does_not_depend_on_how_long_a_name_is() {
  for pad in [0, 1, 64, 4096] {
    let response = corpus(pad, "null");
    let schema = read(&response).expect("a schema");
    let census = Census::over(&response, &schema);
    assert_eq!(census.copied_texts, 0, "pad {pad}");
    assert!(census.names >= 25, "pad {pad}");
  }
}

/// Prose borrows too, right up until an escape makes borrowing impossible.
#[test]
fn prose_allocates_only_when_a_backslash_is_present() {
  // No backslash, however long the literal: borrowed.
  for default in [
    "\"1\"",
    "\"null\"",
    "\"ENUM_MEMBER\"",
    &format!("\"{}\"", "4".repeat(4096)),
  ] {
    let response = corpus(0, default);
    let schema = read(&response).expect("a schema");
    let census = Census::over(&response, &schema);
    assert_eq!(
      census.copied_texts, 0,
      "an escape-free default value was copied: {default}"
    );
    assert!(census.borrowed_texts >= 2, "{default}");
  }

  // One backslash, and exactly one copy — the value it appears in, and nothing else.
  let response = corpus(0, r#""\"world\"""#);
  let schema = read(&response).expect("a schema");
  let census = Census::over(&response, &schema);
  assert_eq!(census.copied_texts, 1);
  assert!(census.borrowed_texts >= 1, "only the escaped value copied");

  let decoded = schema.types[0].fields.as_ref().expect("fields")[0].args[0]
    .default_value
    .as_ref()
    .expect("a default value");
  assert!(!decoded.is_borrowed());
  assert_eq!(decoded.as_str(), "\"world\"");
}

// ---------------------------------------------------------------------------------------------
// the envelope, decided by key
// ---------------------------------------------------------------------------------------------

/// A `data` with a malformed `__schema` reports the malformation.
///
/// The root here carries a **complete and valid bare `__Schema`** beside the `data`, so a reader
/// that tried each envelope in turn would fall through to it and return a schema. This is the
/// discrimination the ordering exists for, and the second assertion is what makes the first one
/// evidence rather than a tautology: the fall-through reading really would have succeeded.
#[test]
fn a_malformed_schema_under_data_is_not_re_read_as_a_bare_one() {
  let fallible = format!(r#"{{"data":{{"__schema":5}},{BARE_BODY}}}"#);
  assert_eq!(
    refused(&fallible).0,
    ResponseErrorKind::MalformedResponse,
    "the malformed `data.__schema` was not reported"
  );
  assert!(
    read(&format!("{{{BARE_BODY}}}")).is_ok(),
    "the fall-through reading does not succeed, so the test above proves nothing"
  );

  // The same, with a `__schema` that is an object and missing a required member.
  let missing_types = format!(
    r#"{{"data":{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[]}}}},{BARE_BODY}}}"#
  );
  assert_eq!(
    refused(&missing_types).0,
    ResponseErrorKind::MalformedResponse
  );
}

/// A `data` shadows a root-level `__schema` whether or not it carries one.
#[test]
fn data_is_the_only_place_looked_when_it_is_present() {
  // Both present: `data.__schema` wins, and the two name different roots so the winner is visible.
  let both = r#"{"data":{"__schema":{"queryType":{"name":"Chosen"},"directives":[],"types":[]}},
     "__schema":{"queryType":{"name":"Ignored"},"directives":[],"types":[]}}"#;
  let schema = read(both).expect("a schema");
  assert_eq!(
    schema.query_type.name.expect("a root name").as_str(),
    "Chosen"
  );

  // `data` present and carrying no `__schema`, with a root-level one beside it: the root-level one
  // is not a second place to look, and the root itself is not a `__Schema` either.
  let shadowed =
    r#"{"data":{"nope":1},"__schema":{"queryType":{"name":"Q"},"directives":[],"types":[]}}"#;
  assert_eq!(refused(shadowed).0, ResponseErrorKind::MissingSchema);
}

/// The bare envelope is reached exactly when `types` is at the root and no `__schema` was found.
#[test]
fn the_bare_envelope_is_the_last_reading() {
  assert!(read(&format!("{{{BARE_BODY}}}")).is_ok());
  // A `data` with no `__schema` under it still falls through to the root, because the root has
  // `types` — the discriminator is the key, not the absence of a `data`.
  assert!(read(&format!(r#"{{"data":{{"nope":1}},{BARE_BODY}}}"#)).is_ok());
  // And with no `types` at the root there is nothing left to read.
  assert_eq!(
    refused(r#"{"data":{"nope":1}}"#).0,
    ResponseErrorKind::MissingSchema
  );
  assert_eq!(refused("[1,2]").0, ResponseErrorKind::MissingSchema);
  assert_eq!(refused("5").0, ResponseErrorKind::MissingSchema);
}

/// The envelope walk is also what proves the response is JSON, and it says so first.
#[test]
fn a_syntax_error_anywhere_outranks_a_question_about_the_shape() {
  // A malformed `__schema`, and a defect in a member the door never reads: the syntax error is
  // what comes back, because the walk that decided the envelope is also the walk that proved the
  // response was JSON.
  let with_bad_errors = r#"{"errors":[{"message":"boom}],"data":{"__schema":5}}"#;
  assert_eq!(refused(with_bad_errors).0, ResponseErrorKind::MalformedJson);
  // Trailing text after an otherwise valid response.
  assert_eq!(
    refused(&format!("{{{BARE_BODY}}} and more")).0,
    ResponseErrorKind::MalformedJson
  );
  assert_eq!(refused("not json").0, ResponseErrorKind::MalformedJson);
}

// ---------------------------------------------------------------------------------------------
// the refusals that moved into the reader keep their subject and their owner
// ---------------------------------------------------------------------------------------------

/// `UnknownTypeKind`, `UnknownDirectiveLocation` and `InvalidName` are now refused where the
/// literal is read rather than where a rendered fragment is compared. Which artifact each one
/// names, and what owns it, is part of the surface and is pinned here position by position.
#[test]
fn a_refusal_from_the_reader_names_the_same_owner_the_renderer_named() {
  let cases: &[(&str, String, ResponseErrorKind, Option<&str>, &str)] = &[
    (
      "an unknown kind on a member of `types`",
      response(r#"{"kind":"WIDGET","name":"Widget"}"#),
      ResponseErrorKind::UnknownTypeKind,
      None,
      "WIDGET",
    ),
    (
      "an unknown kind in a field's type",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[],"type":{"kind":"WIDGET","name":"W"}}
        ]}"#,
      ),
      ResponseErrorKind::UnknownTypeKind,
      Some("Query"),
      "WIDGET",
    ),
    (
      "an unknown kind in an argument's type",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[
            {"name":"arg","type":{"kind":"WIDGET","name":"W"},"defaultValue":null}
          ],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
      ResponseErrorKind::UnknownTypeKind,
      Some("Query.ok"),
      "WIDGET",
    ),
    (
      "an unknown kind in an implemented interface",
      response(
        r#"{"kind":"OBJECT","name":"Query","interfaces":[{"kind":"WIDGET","name":"W"}],
           "fields":[{"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}]}"#,
      ),
      ResponseErrorKind::UnknownTypeKind,
      Some("Query"),
      "WIDGET",
    ),
    (
      "an unknown kind in a directive argument's type",
      with_directives(
        r#"{"name":"weird","locations":["FIELD"],"args":[
          {"name":"arg","type":{"kind":"WIDGET","name":"W"},"defaultValue":null}
        ]}"#,
      ),
      ResponseErrorKind::UnknownTypeKind,
      Some("weird"),
      "WIDGET",
    ),
    (
      "an unknown directive location",
      with_directives(r#"{"name":"weird","locations":["NOWHERE"],"args":[]}"#),
      ResponseErrorKind::UnknownDirectiveLocation,
      Some("weird"),
      "NOWHERE",
    ),
    (
      "a type named something the grammar cannot spell",
      response(r#"{"kind":"OBJECT","name":"Not A Name","fields":[]}"#),
      ResponseErrorKind::InvalidName,
      None,
      "Not A Name",
    ),
    (
      "a field named something the grammar cannot spell",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"not ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
      ResponseErrorKind::InvalidName,
      Some("Query"),
      "not ok",
    ),
    (
      "an argument named something the grammar cannot spell",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[
            {"name":"not arg","type":{"kind":"SCALAR","name":"Int"},"defaultValue":null}
          ],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
      ResponseErrorKind::InvalidName,
      Some("Query.ok"),
      "not arg",
    ),
    (
      "an input-object field named something the grammar cannot spell",
      response(&format!(
        r#"{OK_QUERY},{{"kind":"INPUT_OBJECT","name":"In","inputFields":[
          {{"name":"not f","type":{{"kind":"SCALAR","name":"Int"}},"defaultValue":null}}
        ]}}"#
      )),
      ResponseErrorKind::InvalidName,
      Some("In"),
      "not f",
    ),
    (
      "an enum value named something the grammar cannot spell",
      response(&format!(
        r#"{OK_QUERY},{{"kind":"ENUM","name":"Verdict","enumValues":[{{"name":"not ok"}}]}}"#
      )),
      ResponseErrorKind::InvalidName,
      Some("Verdict"),
      "not ok",
    ),
    (
      "a base type named something the grammar cannot spell",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Not A Name"}}
        ]}"#,
      ),
      ResponseErrorKind::InvalidName,
      Some("Query"),
      "Not A Name",
    ),
    (
      "a directive named something the grammar cannot spell",
      with_directives(r#"{"name":"not ok","locations":["FIELD"],"args":[]}"#),
      ResponseErrorKind::InvalidName,
      None,
      "not ok",
    ),
    (
      "a directive argument named something the grammar cannot spell",
      with_directives(
        r#"{"name":"weird","locations":["FIELD"],"args":[
          {"name":"not arg","type":{"kind":"SCALAR","name":"Int"},"defaultValue":null}
        ]}"#,
      ),
      ResponseErrorKind::InvalidName,
      Some("weird"),
      "not arg",
    ),
    (
      "a root operation named something the grammar cannot spell",
      r#"{"__schema":{"queryType":{"name":"Not A Name"},"directives":[],"types":[]}}"#.to_string(),
      ResponseErrorKind::InvalidName,
      Some("schema"),
      "Not A Name",
    ),
  ];

  for (what, json, kind, owner, subject) in cases {
    let (found, found_owner) = refused(json);
    assert_eq!(found, *kind, "{what}\n---\n{json}");
    assert_eq!(found_owner.as_deref(), *owner, "{what}\n---\n{json}");
    assert_eq!(subject_of(json), *subject, "{what}\n---\n{json}");
  }
}

/// A refusal the scanner raised says where in the response it stopped, and staging it does not
/// move where that is.
///
/// **This is a test the previous shape did not need.** The line and column used to be formatted
/// into the subject at the refusal, in one expression, so there was no way to lose them that was
/// not also a way to change the message. They are counted at the door now — once, on the refusal
/// that escapes, because counting them per refusal is quadratic in a duplicated bad member — and a
/// door that forgot to count them would produce a message missing its position and nothing else in
/// the suite would notice.
///
/// Two lines and one defect, counted by hand: line 1 ends after `"queryType":{"name":"Query"},`,
/// and on line 2 the `5` that should have been a list is the 27th byte after the newline.
#[test]
fn a_scanner_refusal_carries_the_line_and_column_it_stopped_at() {
  let json =
    "{\"__schema\":{\"queryType\":{\"name\":\"Query\"},\n \"directives\":[], \"types\":5}}";
  assert_eq!(
    subject_of(json),
    "invalid type: a number, expected a sequence for `__Schema.types` at line 2 column 27"
  );

  // The same defect one line further down: the line moves and the column does not, so the
  // assertion above is about a count and not about a constant that happens to match.
  let lower = json.replacen('\n', "\n\n", 1);
  assert_eq!(
    subject_of(&lower),
    "invalid type: a number, expected a sequence for `__Schema.types` at line 3 column 27"
  );

  // And with no newline at all, where the column is the offset: every real response is one line.
  let flat = json.replacen('\n', "", 1);
  assert_eq!(
    subject_of(&flat),
    "invalid type: a number, expected a sequence for `__Schema.types` at line 1 column 69"
  );
}

/// An owner is found whatever order the response wrote its members in.
///
/// This is the whole reason an [`Owner`] is an offset rather than a name. JSON does not promise
/// that an object writes its `name` before the member that turns out to be defective, so a reader
/// that captured the owner as it went would have nothing to attach when the defect came first. The
/// offset is resolved afterwards, against the object as a whole.
#[test]
fn an_owner_is_found_when_the_response_wrote_its_name_last() {
  // The bad field precedes the type's own `name`.
  let json = response(
    r#"{"fields":[{"name":"not ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}],
       "kind":"OBJECT","name":"Query"}"#,
  );
  assert_eq!(
    refused(&json),
    (ResponseErrorKind::InvalidName, Some(String::from("Query")))
  );

  // Both segments of a two-segment path written after the member that fails.
  let json = response(
    r#"{"fields":[{"args":[{"name":"not arg","type":{"kind":"SCALAR","name":"Int"}}],
       "name":"ok","type":{"kind":"SCALAR","name":"Int"}}],
       "kind":"OBJECT","name":"Query"}"#,
  );
  assert_eq!(
    refused(&json),
    (
      ResponseErrorKind::InvalidName,
      Some(String::from("Query.ok"))
    )
  );

  // An owner with no name to give is left unowned rather than given an invented one — including
  // when its `name` is present and is not a string at all, which the walk has to step over
  // without losing its place.
  for owner in ["null", "42", r#"{"deep":[1,2]}"#] {
    let json = response(&std::format!(
      r#"{{"fields":[{{"name":"not ok","args":[],"type":{{"kind":"SCALAR","name":"Int"}}}}],
         "kind":"OBJECT","name":{owner}}}"#
    ));
    assert_eq!(
      refused(&json),
      (ResponseErrorKind::InvalidName, None),
      "owner name {owner}"
    );
  }
}

/// A JSON `\uXXXX` escape, assembled rather than written, so nothing between this source and the
/// fixture can normalise it into the character it stands for.
fn escaped(code: &str, rest: &str) -> String {
  let spelling = format!("\\u{code}{rest}");
  assert!(spelling.contains('\\'), "the fixture lost its escape");
  spelling
}

/// An escape is decoded in four of [the five kinds of string](crate::introspection::json), and
/// refused in the fifth.
///
/// The taxonomy is what this pins, one row per kind, because "names borrow, prose may not" is a
/// statement about two of the five and the reader meets all five. A **dispatch key** and a
/// **closed vocabulary** are read in order to be matched, so the escape names the member or the
/// enum value it spells; **prose** is decoded and kept; a string in a **skipped** member is
/// validated and never read. Only a **`Name`** is matched raw, and that is the borrow's price,
/// charged where the borrow is.
#[test]
fn an_escape_is_decoded_everywhere_but_a_name() {
  // A dispatch key, at the two places one selects a whole reading.
  let bare_under_escaped_data = format!(
    r#"{{"{}":{{"__schema":{{{BARE_BODY}}}}}}}"#,
    escaped("0064", "ata")
  );
  assert!(
    read(&bare_under_escaped_data).is_ok(),
    "an escaped `data` was not recognised as `data`"
  );
  let escaped_schema_key = format!(r#"{{"{}":{{{BARE_BODY}}}}}"#, escaped("005F", "_schema"));
  assert!(read(&escaped_schema_key).is_ok());

  // A dispatch key on an ordinary member, and the member is read rather than skipped.
  let escaped_name_key = response(&format!(
    r#"{{"kind":"OBJECT","{}":"Query","fields":[]}}"#,
    escaped("006E", "ame")
  ));
  let schema = read(&escaped_name_key).expect("a schema");
  assert_eq!(
    schema.types[0].name.expect("a name").as_str(),
    "Query",
    "an escaped `name` key was skipped as an unknown member"
  );

  // The two closed vocabularies.
  let escaped_kind = response(&format!(
    r#"{{"kind":"{}","name":"Query","fields":[]}}"#,
    escaped("004F", "BJECT")
  ));
  assert!(
    read(&escaped_kind).is_ok(),
    "an escaped `OBJECT` was not `OBJECT`"
  );
  let escaped_location = with_directives(&format!(
    r#"{{"name":"weird","locations":["{}"],"args":[]}}"#,
    escaped("0046", "IELD")
  ));
  assert!(
    read(&escaped_location).is_ok(),
    "an escaped `FIELD` was not `FIELD`"
  );

  // A `Name` is the one kind read raw. The literal decodes to `Query` and is refused anyway, with
  // the literal as the subject so whoever reads the message can see why.
  let query = escaped("0051", "uery");
  let escaped_name = response(&format!(
    r#"{{"kind":"OBJECT","name":"{query}","fields":[]}}"#
  ));
  assert_eq!(refused(&escaped_name).0, ResponseErrorKind::InvalidName);
  assert_eq!(subject_of(&escaped_name), query);

  // Prose is decoded and kept, which it always was.
  let benign = response(
    r#"{"kind":"OBJECT","name":"Query","fields":[
      {"name":"ok","args":[
        {"name":"arg","type":{"kind":"SCALAR","name":"String"},"defaultValue":"\"x\""}
      ],"type":{"kind":"SCALAR","name":"Int"}}
    ]}"#,
  );
  let schema = read(&benign).expect("a schema");
  assert_eq!(
    schema.types[0].fields.as_ref().expect("fields")[0].args[0]
      .default_value
      .as_ref()
      .expect("a default")
      .as_str(),
    "\"x\""
  );

  // A skipped member's key is neither decoded nor dispatched on, and an escape in one changes
  // nothing about the response it sits in.
  let escaped_unread = response(&format!(
    r#"{{"kind":"OBJECT","name":"Query","{}":"prose","fields":[]}}"#,
    escaped("0064", "escription")
  ));
  assert!(read(&escaped_unread).is_ok());
}

/// An escaped `data` still shadows every other reading, malformed or not.
///
/// This is the load-bearing property of [`locate`](super::locate) said against the spelling rather
/// than against the order: the root carries a **complete and valid bare `__Schema`** beside a
/// `data` whose `__schema` cannot be read, so a reader that failed to recognise the key would fall
/// through to it and return a schema. `a_malformed_schema_under_data_is_not_re_read_as_a_bare_one`
/// pins the same discrimination for the plain spelling; both spellings name the same key, so both
/// have to reach the same refusal.
#[test]
fn an_escaped_data_is_still_the_only_place_looked() {
  for spelling in ["data", &escaped("0064", "ata")] {
    let fallible = format!(r#"{{"{spelling}":{{"__schema":5}},{BARE_BODY}}}"#);
    assert_eq!(
      refused(&fallible).0,
      ResponseErrorKind::MalformedResponse,
      "`{spelling}` did not shadow the bare reading beside it"
    );

    // A `data` carrying no `__schema` at all shadows a root-level one just the same.
    let shadowed = format!(
      r#"{{"{spelling}":{{"nope":1}},
         "__schema":{{"queryType":{{"name":"Q"}},"directives":[],"types":[]}}}}"#
    );
    assert_eq!(refused(&shadowed).0, ResponseErrorKind::MissingSchema);
  }

  // And the control that makes both of the above evidence: the fall-through reading succeeds.
  assert!(
    read(&format!("{{{BARE_BODY}}}")).is_ok(),
    "the fall-through reading does not succeed, so the assertions above prove nothing"
  );
}

// ---------------------------------------------------------------------------------------------
// what the shape admits, and what it does not
// ---------------------------------------------------------------------------------------------

/// Absent, null and defaulted are three different things, and the reader keeps them apart.
#[test]
fn the_three_ways_a_member_can_be_missing() {
  // `mutationType` absent and `mutationType: null` both mean no mutation root.
  for json in [
    r#"{"__schema":{"queryType":{"name":"Query"},"directives":[],"types":[]}}"#,
    r#"{"__schema":{"queryType":{"name":"Query"},"mutationType":null,"directives":[],"types":[]}}"#,
  ] {
    assert!(read(json).expect("a schema").mutation_type.is_none());
  }

  // `args` is defaulted, so absent is the empty list — and `null` is still refused, because the
  // meta-schema declares it non-null and a server that writes `null` is describing nothing.
  let no_args = response(
    r#"{"kind":"OBJECT","name":"Query","fields":[
    {"name":"ok","type":{"kind":"SCALAR","name":"Int"}}
  ]}"#,
  );
  assert!(read(&no_args).is_ok());
  let null_args = response(
    r#"{"kind":"OBJECT","name":"Query","fields":[
    {"name":"ok","args":null,"type":{"kind":"SCALAR","name":"Int"}}
  ]}"#,
  );
  assert_eq!(refused(&null_args).0, ResponseErrorKind::MalformedResponse);

  // A member draft §4 declares non-null, absent.
  for json in [
    r#"{"__schema":{"queryType":{"name":"Query"},"directives":[]}}"#,
    r#"{"__schema":{"queryType":{"name":"Query"},"types":[]}}"#,
    r#"{"__schema":{"directives":[],"types":[]}}"#,
  ] {
    assert_eq!(
      refused(json).0,
      ResponseErrorKind::MalformedResponse,
      "{json}"
    );
  }

  // ... and present with the wrong JSON type.
  for json in [
    r#"{"__schema":{"queryType":{"name":"Query"},"directives":[],"types":null}}"#,
    r#"{"__schema":{"queryType":{"name":"Query"},"directives":[],"types":42}}"#,
    r#"{"__schema":{"queryType":null,"directives":[],"types":[]}}"#,
    r#"{"__schema":{"queryType":{"name":5},"directives":[],"types":[]}}"#,
  ] {
    assert_eq!(
      refused(json).0,
      ResponseErrorKind::MalformedResponse,
      "{json}"
    );
  }
}

/// One member written twice takes the last, which is what the value tree the previous reader
/// built already did.
///
/// Pinned rather than left to fall out, because it is the one place the shape-directed reading
/// could quietly have become stricter than the reading it replaced: a derived deserializer refuses
/// a duplicate field, and the reason the old door did not is that the tree collapsed the pair into
/// one member before the shape was ever consulted.
#[test]
fn a_member_written_twice_takes_the_last() {
  let json = response(r#"{"kind":"OBJECT","name":"A","name":"B","fields":[]}"#);
  let schema = read(&json).expect("a schema");
  assert_eq!(
    schema.types[0].name.expect("a name").as_str(),
    "B",
    "the reader took a member other than the last"
  );
}

/// Last-wins covers **whether the member could be read**, and not only which value the slot ends
/// up with.
///
/// The test above pins the slot half, and the slot half alone is what a reader gets for free by
/// assigning in document order — its first occurrence is still the one that decides, because a
/// refusal on it never reaches the second. So `{"name":"bad-name","name":"ok"}` came back as
/// `InvalidName` while the tree the previous reader built had kept `ok` and accepted it. Every row
/// here is a response the old door accepted, one per shape of failure a first occurrence can have:
/// an invalid name, a value of the wrong JSON type, an unknown member of a closed vocabulary, and
/// a defect nested inside the member's value.
#[test]
fn a_member_written_twice_takes_the_last_even_when_the_first_cannot_be_read() {
  let cases: &[(&str, String)] = &[
    (
      "a `__Type.name` the grammar cannot spell",
      response(r#"{"kind":"OBJECT","name":"bad-name","name":"Ok","fields":[]}"#),
    ),
    (
      "a `__Type.name` that is not a string",
      response(r#"{"kind":"OBJECT","name":5,"name":"Ok","fields":[]}"#),
    ),
    (
      "a `__Type.kind` nobody defines",
      response(r#"{"kind":"WIDGET","kind":"OBJECT","name":"Ok","fields":[]}"#),
    ),
    (
      "a `__Type.fields` that is not a list",
      response(r#"{"kind":"OBJECT","name":"Ok","fields":5,"fields":[]}"#),
    ),
    (
      "a `__Schema.types` that is not a list",
      format!(
        r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],
           "types":5,"types":[{OK_QUERY}]}}}}"#
      ),
    ),
    (
      "a `__Schema.queryType` naming something unspellable",
      r#"{"__schema":{"queryType":{"name":"Not A Name"},"queryType":{"name":"Query"},
         "directives":[],"types":[]}}"#
        .to_string(),
    ),
    (
      "a `__Field.name` the grammar cannot spell",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"not ok","name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
    ),
    (
      "a `__Field.type` whose kind nobody defines",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[],"type":{"kind":"WIDGET","name":"W"},
           "type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
    ),
    (
      "an `__InputValue.name` the grammar cannot spell",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[
            {"name":"not arg","name":"arg","type":{"kind":"SCALAR","name":"Int"}}
          ],"type":{"kind":"SCALAR","name":"Int"}}
        ]}"#,
      ),
    ),
    (
      "an `__EnumValue.name` the grammar cannot spell",
      response(&format!(
        r#"{OK_QUERY},{{"kind":"ENUM","name":"Verdict","enumValues":[
          {{"name":"not ok","name":"YES"}}
        ]}}"#
      )),
    ),
    (
      "a `__Directive.name` the grammar cannot spell",
      with_directives(r#"{"name":"not ok","name":"weird","locations":["FIELD"],"args":[]}"#),
    ),
    (
      "a `__Directive.locations` entry nobody defines",
      with_directives(
        r#"{"name":"weird","locations":["NOWHERE"],"locations":["FIELD"],"args":[]}"#,
      ),
    ),
    (
      "an `ofType` whose kind nobody defines",
      response(
        r#"{"kind":"OBJECT","name":"Query","fields":[
          {"name":"ok","args":[],"type":{"kind":"LIST","name":null,
            "ofType":{"kind":"WIDGET","name":"W"},"ofType":{"kind":"SCALAR","name":"Int"}}}
        ]}"#,
      ),
    ),
  ];

  for (what, json) in cases {
    assert!(
      read(json).is_ok(),
      "the first occurrence decided: {what}\n---\n{json}"
    );
  }
}

/// A member written twice and unreadable **both** times still refuses, and names the last.
///
/// The complement of the test above, and what keeps it from being a hole: deferring a refusal
/// until the object closes must lose the *superseded* one and nothing else.
#[test]
fn a_member_unreadable_twice_reports_the_last_occurrence() {
  let json = response(r#"{"kind":"OBJECT","name":"bad-name","name":"worse name","fields":[]}"#);
  assert_eq!(refused(&json).0, ResponseErrorKind::InvalidName);
  assert_eq!(subject_of(&json), "worse name");
}

/// Two defective members in one object report whichever the *response* wrote first, and a
/// duplicate does not change which one that is.
///
/// Holding a member's refusal until the object closes is what makes last-wins reach validation,
/// and the risk it carries is that the refusal a response gets stops being the first one in it.
/// It does not: the surviving refusals are compared by where their member's value began, and
/// members are read left to right.
#[test]
fn the_refusal_is_the_one_the_response_wrote_first() {
  // Neither member duplicated: whichever comes first is the answer, both ways round.
  let kind_first = response(r#"{"kind":"WIDGET","name":"Not A Name"}"#);
  assert_eq!(subject_of(&kind_first), "WIDGET");
  let name_first = response(r#"{"name":"Not A Name","kind":"WIDGET"}"#);
  assert_eq!(subject_of(&name_first), "Not A Name");

  // The first of the two repaired by a duplicate: the second one is what is left.
  let kind_repaired = response(r#"{"kind":"WIDGET","kind":"OBJECT","name":"Not A Name"}"#);
  assert_eq!(subject_of(&kind_repaired), "Not A Name");
  let name_repaired = response(r#"{"name":"Not A Name","name":"Ok","kind":"WIDGET"}"#);
  assert_eq!(subject_of(&name_repaired), "WIDGET");
}

/// A member that could not be read is walked to its end, so the members after it are still found.
///
/// The mechanism last-wins rests on: a refusal leaves the cursor wherever it happened, and the
/// reader puts it back and steps over the value as an uninterpreted one. A reader that did not
/// would lose its place and refuse the object for a reason that is not in it — and the deeper the
/// defect, the further from the truth the second refusal would be.
#[test]
fn a_refused_member_is_stepped_over_and_the_object_read_on() {
  // The defect is four containers deep inside the first `fields`, and the members after it — the
  // duplicate `fields`, the `name` — are still read.
  let json = response(
    r#"{"kind":"OBJECT",
       "fields":[{"name":"ok","args":[
         {"name":"arg","type":{"kind":"LIST","name":null,"ofType":{"kind":"WIDGET","name":"W"}}}
       ],"type":{"kind":"SCALAR","name":"Int"}}],
       "fields":[{"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"}}],
       "name":"Query"}"#,
  );
  let schema = read(&json).expect("a schema");
  assert_eq!(schema.types[0].name.expect("a name").as_str(), "Query");
  assert_eq!(
    schema.types[0].fields.as_ref().expect("fields")[0]
      .args
      .len(),
    0,
    "the surviving `fields` is not the last one written"
  );
}

/// Stepping over a refused member gives back the containers it left open, not only the offset.
///
/// A refusal stops the cursor *inside* whatever it was reading, so the reader is several
/// containers deep when it decides to walk the value instead. Restoring the offset alone would
/// leave those containers counted against the 128-deep nesting bound for the rest of the
/// document — a leak of a few per refusal, invisible until enough of them accumulate and then
/// reported as `recursion limit exceeded`, which is a statement about a response that does not
/// nest at all.
///
/// Two hundred refused occurrences is well past the 128 the bound allows and nowhere near it,
/// which is the whole point: the response below nests five deep and has to be read.
#[test]
fn stepping_over_a_refused_member_gives_back_its_containers() {
  let refused = r#""fields":[{"name":"ok","args":[],"type":{"kind":"WIDGET","name":"W"}}],"#;
  let json = response(&format!(
    r#"{{"kind":"OBJECT","name":"Query",{}
       "fields":[{{"name":"ok","args":[],"type":{{"kind":"SCALAR","name":"Int"}}}}]}}"#,
    refused.repeat(200)
  ));
  let schema = read(&json).expect("a schema");
  assert_eq!(
    schema.types[0].fields.as_ref().expect("fields").len(),
    1,
    "the surviving `fields` is not the last one written"
  );
}

/// Members the door does not read are ignored however strange they are.
#[test]
fn an_unknown_member_is_ignored() {
  let json = response(
    r#"{"kind":"OBJECT","name":"Query","description":"prose with \"escapes\" in it",
       "specifiedByURL":null,"unheardOf":{"deeply":[1,2,{"nested":true}]},
       "fields":[{"name":"ok","args":[],"type":{"kind":"SCALAR","name":"Int"},
                  "isDeprecated":false,"deprecationReason":null}]}"#,
  );
  let schema = read(&json).expect("a schema");
  assert_eq!(schema.types.len(), 1);
  // Nothing was copied on the way past them.
  assert_eq!(Census::over(&json, &schema).copied_texts, 0);
}

/// The wrapper chain a hostile response can build is bounded before the renderer walks it.
#[test]
fn a_reference_chain_cannot_be_driven_off_the_stack() {
  let mut chain = String::from(r#"{"kind":"SCALAR","name":"Int"}"#);
  for _ in 0..200 {
    chain = format!(r#"{{"kind":"LIST","name":null,"ofType":{chain}}}"#);
  }
  let json = response(&format!(
    r#"{{"kind":"OBJECT","name":"Query","fields":[
      {{"name":"ok","args":[],"type":{chain}}}
    ]}}"#
  ));
  assert_eq!(refused(&json).0, ResponseErrorKind::MalformedJson);
}

/// A response is read once, and the reading stops at the first thing that is not a §4 result.
#[test]
fn reading_is_fail_fast() {
  let json = response(&format!(
    r#"{OK_QUERY},{{"kind":"WIDGET","name":"First"}},{{"kind":"GADGET","name":"Second"}}"#
  ));
  assert_eq!(subject_of(&json), "WIDGET");
}

/// The three envelopes name the same schema.
#[test]
fn every_envelope_reads_the_same_document() {
  let bare = format!("{{{BARE_BODY}}}");
  let unwrapped = format!(r#"{{"__schema":{{{BARE_BODY}}}}}"#);
  let full = format!(r#"{{"data":{{"__schema":{{{BARE_BODY}}}}}}}"#);

  let names = |json: &str| -> Vec<String> {
    read(json)
      .expect("a schema")
      .types
      .iter()
      .map(|ty| ty.name.expect("a name").as_str().to_string())
      .collect()
  };
  assert_eq!(names(&bare), names(&unwrapped));
  assert_eq!(names(&bare), names(&full));
  assert_eq!(names(&bare), ["Query"]);
}

// ---------------------------------------------------------------------------------------------
// what a refusal costs, counted in the bytes the reader looks at
// ---------------------------------------------------------------------------------------------
//
// ── WHY THIS IS COUNTED AND NOT TIMED, AND NOT MEASURED AS ALLOCATION EITHER ──────────────────
//
// `introspection_allocation.rs` pins that a discarded refusal does not resolve its owner, and it
// pins it in bytes allocated because a threshold on a clock is a hope. That instrument is
// structurally blind to the two properties below: deriving a line and column walks a prefix, and
// recovering from a refused member walks a subtree, AND NEITHER ALLOCATES ANYTHING. A pin chosen
// for good reasons was incapable of observing the sibling of the defect it was written for.
//
// Bytes visited is the quantity both properties are stated in. `json::visited` counts them, and
// what follows is a bound on the *marginal* cost — how much more scanning one more occurrence of a
// bad member, or one more `ofType` level, is allowed to cost. A bound on the total would pass a
// reader that scanned the response a fixed forty times; a bound on the margin says the cost of what
// was added is proportional to what was added.

/// How many times the reader may look at a byte, for each byte *added* to a response.
///
/// A ratio, and that is the point: it says the cost of one more occurrence of a bad member, or one
/// more `ofType` level, is proportional to the bytes that occurrence or that level is written in and
/// not to the response those bytes were added to. A derivation that walks the response once per
/// refusal fails it by an order of magnitude however the constant is tuned, and nothing that does
/// not walk the response comes near it.
///
/// Measured on the tree that introduced this gate: 4.7 for an added occurrence, 8.5 for an added
/// `ofType` level. Measured for the two defects it was written against: 1 300 and 470.
const LOOKS_PER_ADDED_BYTE: u64 = 32;

/// Bytes of the response the reader looked at while reading it.
fn visits(response: &str) -> u64 {
  let before = visited();
  drop(read(response));
  visited() - before
}

/// `k` occurrences of a `__Field.name` that is not a string, and then one that is.
///
/// **Accepted.** Every one of the `k` builds a refusal that the final occurrence takes away, which
/// is what makes it the shape a hostile response would use: the door returns a schema, so nothing
/// about the outcome says how much work the response cost.
fn discarded_refusals(k: usize) -> String {
  let discarded = r#""name":0,"#.repeat(k);
  format!(
    r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],"types":[
      {{"kind":"OBJECT","name":"Query","fields":[
        {{{discarded}"name":"ok","args":[],"type":{{"kind":"SCALAR","name":"Int"}}}}
      ]}}
    ]}}}}"#
  )
}

/// An `ofType` chain `depth` wrappers deep, bottoming out in a kind nobody defines beside a
/// `description` the door does not read.
///
/// **Refused**, and refused by the innermost object — so the failure is deferred out through every
/// enclosing `ofType` member, each of which has the whole subtree in front of it.
fn deep_refusal(depth: usize, prose: usize) -> String {
  let description = "p".repeat(prose);
  let mut reference = format!(r#"{{"kind":"WIDGET","name":"W","description":"{description}"}}"#);
  for _ in 0..depth {
    reference = format!(r#"{{"kind":"LIST","name":null,"ofType":{reference}}}"#);
  }
  format!(
    r#"{{"__schema":{{"queryType":{{"name":"Query"}},"directives":[],"types":[
      {{"kind":"OBJECT","name":"Query","fields":[
        {{"name":"ok","args":[],"type":{reference}}}
      ]}}
    ]}}}}"#
  )
}

/// Reports the marginal cost of the bytes one fixture has and a smaller one does not.
fn per_added_byte(small: &str, large: &str) -> u64 {
  let added = (large.len() - small.len()) as u64;
  assert!(added > 0, "the two fixtures are the same size");
  (visits(large) - visits(small)) / added
}

/// A refusal the reader builds and throws away costs the bytes it names, and not a piece of the
/// response.
///
/// The response is **accepted**: `k` occurrences of `"name":0` inside one `__Field`, and then a
/// `name` that reads. Each of the `k` builds a refusal that the last one replaces, so a derivation
/// performed at construction is performed `k` times for an answer thrown away `k` times — and if it
/// walks the response, that is Θ(k²) byte visits for a schema the door goes on to return. A network
/// peer chooses `k`.
#[test]
fn a_discarded_refusal_costs_a_constant_number_of_bytes() {
  // The control that makes the measurement mean what it says: this is the accepted path, so the
  // cost below is paid by a response nothing else about the door would flag.
  assert!(
    read(&discarded_refusals(4)).is_ok(),
    "the fixture is refused, so it is not the shape this gate is about"
  );

  // Four points two orders of magnitude apart. A quadratic term shows up as a marginal cost that
  // grows from one interval to the next, and the assertion is on each interval rather than on the
  // total so that a large constant cannot hide one.
  let mut previous = discarded_refusals(8);
  for k in [32usize, 128, 512, 2048] {
    let next = discarded_refusals(k);
    let looks = per_added_byte(&previous, &next);
    assert!(
      looks <= LOOKS_PER_ADDED_BYTE,
      "up to k={k}, each byte of a discarded refusal cost {looks} byte visits; a refusal is \
       deriving something by walking the response"
    );
    previous = next;
  }
}

/// A refusal deferred to an object's closing brace is stepped over once, not once per ancestor.
///
/// Reading holds a member's failure until its object closes, so the object has been walked to its
/// end by the time anything knows it failed — and the enclosing member would then rewind and walk
/// the whole of it again, once for every level of `ofType` between the defect and the document.
/// Within the 128-container bound that is up to 128 walks of one subtree, and the subtree here is a
/// string the door does not even read.
#[test]
fn a_deferred_refusal_is_not_re_walked_by_every_enclosing_member() {
  const PROSE: usize = 16 * 1024;

  assert_eq!(
    refused(&deep_refusal(8, PROSE)).0,
    ResponseErrorKind::UnknownTypeKind,
    "the fixture is not refused where this gate assumes it is"
  );
  // Without this the bound below could be met by a subtree too small to be worth re-walking.
  assert!(deep_refusal(8, PROSE).len() > PROSE);

  let mut previous = deep_refusal(1, PROSE);
  for depth in [8usize, 32, 120] {
    let next = deep_refusal(depth, PROSE);
    let looks = per_added_byte(&previous, &next);
    assert!(
      looks <= LOOKS_PER_ADDED_BYTE,
      "up to depth={depth}, each byte of an added `ofType` level cost {looks} byte visits; the \
       enclosing members are walking the refused subtree again"
    );
    previous = next;
  }
}

/// The counter is wired to the reader, and it moves by what the reader reads.
///
/// Without this, both gates above could pass because nothing was being counted.
#[test]
fn the_byte_gate_counts() {
  // Every byte of the document is proved to be JSON, so reading one cannot visit fewer bytes than
  // it has.
  let response = discarded_refusals(64);
  let visited = visits(&response);
  assert!(
    visited >= response.len() as u64,
    "reading {} bytes registered as {visited} byte visits",
    response.len()
  );

  // And it moves for the thing the gates measure: a response with more in it costs more to read,
  // so a flat marginal cost above is a fact about the reader and not about the instrument.
  assert!(
    visits(&discarded_refusals(2048)) > visits(&discarded_refusals(8)),
    "a longer response did not cost more, so the counter is not tracking the reader"
  );
  assert!(
    visits(&deep_refusal(120, 1024)) > visits(&deep_refusal(1, 1024)),
    "a deeper response did not cost more, so the counter is not tracking the reader"
  );
}

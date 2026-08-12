use std::{
  format,
  string::{String, ToString},
  vec::Vec,
};

use super::read;
use crate::introspection::{
  ResponseErrorKind,
  json::Text,
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

/// The closed vocabularies are matched against the literal's own bytes, so an escape in one is a
/// refusal rather than a copy.
///
/// This is the price of the borrow being unconditional, and it is a price nothing real pays: a
/// GraphQL name, a `__TypeKind` and a `__DirectiveLocation` are ASCII identifiers, and no server
/// has a reason to escape one.
#[test]
fn a_closed_vocabulary_spelled_with_an_escape_is_refused() {
  // A JSON `\uXXXX` escape, assembled rather than written, so nothing between this source and the
  // fixture can normalise it into the character it stands for.
  let hex = |code: &str, rest: &str| format!("\\u{code}{rest}");
  let query = hex("0051", "uery");
  let object = hex("004F", "BJECT");
  let field = hex("0046", "IELD");

  // The literal decodes to `Query`, and is refused anyway — the check ran on the literal, and the
  // subject is the literal so whoever reads the message can see why.
  let escaped_name = response(&format!(
    r#"{{"kind":"OBJECT","name":"{query}","fields":[]}}"#
  ));
  assert_eq!(refused(&escaped_name).0, ResponseErrorKind::InvalidName);
  assert_eq!(subject_of(&escaped_name), query);

  let escaped_kind = response(&format!(r#"{{"kind":"{object}","name":"Query"}}"#));
  assert_eq!(refused(&escaped_kind).0, ResponseErrorKind::UnknownTypeKind);
  assert_eq!(subject_of(&escaped_kind), object);

  let escaped_location = with_directives(&format!(
    r#"{{"name":"weird","locations":["{field}"],"args":[]}}"#
  ));
  assert_eq!(
    refused(&escaped_location).0,
    ResponseErrorKind::UnknownDirectiveLocation
  );
  assert_eq!(subject_of(&escaped_location), field);

  // A default value is the one place an escape is honoured, and it still is.
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

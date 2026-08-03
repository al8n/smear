#![cfg(feature = "rowan")]

//! Gate 4 — the typed accessor layer over the GraphQL lossless CST.
//!
//! One test per grammar area, each walking a real tree through the wrappers rather than through
//! [`SyntaxNode`](smear_parser::graphql::lossless::SyntaxNode). Task 14 extends this file; what is
//! here is Task 10's own gate.
//!
//! **Every fixture is chosen so each getter has something to get wrong**, which is the lesson
//! Task 9 recorded: over `"{ a }"` a `cast::child` is indistinguishable from "take the first
//! child" and a `cast::token` from "take the first token", so a fixture of that shape proves
//! nothing about the getter it exercises. Concretely, every assertion below is of one of three
//! sharp forms:
//!
//! - a cast answers `Some` for its own kind **and `None` for a near neighbour** — `Description`
//!   and `StringValue` both wrap a bare string token, and `ObjectTypeDefinition` and
//!   `ObjectTypeExtension` differ only by an `extend`;
//! - an `opt` getter answers **the specific** child rather than merely *a* child — asserted by
//!   the child's text, with a sibling of a different kind sitting before it;
//! - a `tok` getter is proved where the wanted token is **neither the node's first token nor the
//!   only one of its kind** — a field's own name behind an `Alias` that holds another `Name`, a
//!   definition's name behind the keyword that introduces it.
//!
//! Every tree shape asserted here was probed, not inferred.
//!
//! # Two halves, and why the second one exists
//!
//! The twelve tests in the first half are sharp and narrow: each picks a fixture in which one
//! getter has something specific to get wrong, and asserts the answer by text. What a sharp test
//! cannot say is whether a *sixtieth* wrapper, or a two-hundred-and-fifth getter, exists and is
//! touched by nobody — and an untested wrapper is indistinguishable from a broken one, because a
//! wrapper pointed at a kind that never occurs compiles, navigates and answers `None` forever.
//!
//! The second half is that totality statement, made in two independent registers:
//!
//! - a **hand-maintained table** naming all 59 wrappers and all 204 getters, which is what the
//!   corpus sweep actually calls — Task 14's "a list you maintain by hand is the point";
//! - the same inventory **parsed back out of `src/graphql/lossless/ast/*.rs`**, which is what the
//!   crate ships.
//!
//! The two are compared as sets, so the hand-maintained table cannot go stale: a wrapper or a
//! getter added to the sources and not to the table reds this file. That closes the one hole in
//! the plan's own instruction — a hand-maintained list is exactly as good as the memory of
//! whoever last edited the thing it lists.

use std::{
  collections::{BTreeMap, BTreeSet},
  path::PathBuf,
};

use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    GraphQLLang, SyntaxNode, SyntaxToken,
    ast::{
      Alias, Argument, Arguments, ArgumentsDefinition, AstChildren, AstTokens, BooleanValue,
      CastNode, DefaultValue, Description, Directive, DirectiveDefinition, DirectiveLocations,
      Directives, Document, EnumTypeDefinition, EnumTypeExtension, EnumValue, EnumValueDefinition,
      EnumValuesDefinition, ExecutableDocument, Field, FieldDefinition, FieldsDefinition,
      FloatValue, FragmentDefinition, FragmentSpread, ImplementsInterfaces, InlineFragment,
      InputFieldsDefinition, InputObjectTypeDefinition, InputObjectTypeExtension,
      InputValueDefinition, IntValue, InterfaceTypeDefinition, InterfaceTypeExtension, ListType,
      ListValue, NamedType, NonNullType, NullValue, ObjectField, ObjectTypeDefinition,
      ObjectTypeExtension, ObjectValue, OperationDefinition, OperationType,
      RootOperationTypeDefinition, RootOperationTypeDefinitions, ScalarTypeDefinition,
      ScalarTypeExtension, SchemaDefinition, SchemaExtension, SelectionSet, StringValue,
      TypeSystemDocument, UnionMemberTypes, UnionTypeDefinition, UnionTypeExtension, Variable,
      VariableDefinition, VariablesDefinition, cast, token_any, tokens,
    },
    document::test_support::parse_type_system_document,
    executable::test_support::parse_executable_document,
    parse_str,
  },
};

/// The text of a token getter's answer, or `None`.
fn tok(t: Option<SyntaxToken>) -> Option<String> {
  t.map(|t| t.text().to_string())
}

/// The one `Document` a [`parse_str`] tree carries, typed.
fn document(src: &str) -> Document {
  let root = parse_str(src).syntax();
  cast::child(&root).expect("Root wraps exactly one Document")
}

#[test]
fn a_document_sorts_its_definitions_by_kind_and_declines_the_others() {
  let doc = document(
    "type T { a: Int } type V { c: Int } extend type T @d union U = A scalar S \
     query Q { a } fragment F on T { b }",
  );

  // Fixture guard first: an unfiltered `many` and a filtered one agree whenever every child
  // matches, so each count below is worthless unless the Document really carries siblings of
  // other kinds.
  assert_eq!(
    doc.syntax().children().count(),
    7,
    "fixture must carry seven definitions of six different kinds"
  );

  // Two of one kind and one of each of five others: a getter pointed at the wrong kind then
  // reports the wrong *count*, which a fixture of one-per-kind could not have caught.
  assert_eq!(
    doc
      .object_type_definitions()
      .filter_map(|t| tok(t.name()))
      .collect::<Vec<_>>(),
    ["T", "V"]
  );
  assert_eq!(doc.object_type_extensions().count(), 1);
  assert_eq!(doc.union_type_definitions().count(), 1);
  assert_eq!(doc.scalar_type_definitions().count(), 1);
  assert_eq!(doc.operation_definitions().count(), 1);
  assert_eq!(doc.fragment_definitions().count(), 1);
  // Kinds this document does not contain must answer empty rather than "whatever is first".
  assert_eq!(doc.interface_type_definitions().count(), 0);
  assert_eq!(doc.schema_definitions().count(), 0);
  assert_eq!(doc.schema_extensions().count(), 0);

  // A definition and its extension differ only by a leading `extend`, so this is the sharpest
  // near-neighbour decline in the whole kind space.
  let ext = doc.syntax().children().nth(2).expect("the extension");
  assert_eq!(ext.kind(), K::ObjectTypeExtension);
  assert!(ObjectTypeExtension::cast_node(ext.clone()).is_some());
  assert!(
    ObjectTypeDefinition::cast_node(ext).is_none(),
    "an ObjectTypeExtension must not cast to an ObjectTypeDefinition"
  );
}

#[test]
fn a_value_position_answers_with_the_value_kind_that_is_there() {
  let doc = document("{ f(a: [1, 2.5, \"s\", true, null, EV, [7], {k: $v}], b: {x: 1}) }");
  let field = doc
    .operation_definitions()
    .next()
    .and_then(|op| op.selection_set())
    .and_then(|set| set.fields().next())
    .expect("the field");
  let args = field.arguments().expect("the argument list");
  let mut arguments = args.arguments();
  let a: Argument = arguments.next().expect("argument `a`");
  let b: Argument = arguments.next().expect("argument `b`");

  // `tok`: `a` is the argument's own name, and it is the node's first token — but `b`'s is not
  // the *only* `Name` beneath its argument, so the pair together rules out a descendant search.
  assert_eq!(tok(a.name()).as_deref(), Some("a"));
  assert_eq!(tok(b.name()).as_deref(), Some("b"));

  // One value kind answers and the other eight decline. Without the declines a getter that
  // returned "the first child node whatever it is" would pass every positive assertion here.
  let list: ListValue = a.list_value().expect("argument `a` holds a list");
  assert!(a.object_value().is_none());
  assert!(a.int_value().is_none());
  assert!(a.variable().is_none());
  let obj: ObjectValue = b.object_value().expect("argument `b` holds an object");
  assert!(b.list_value().is_none());

  // `many`, per kind, over a list whose members are eight different kinds.
  assert_eq!(list.syntax().children().count(), 8, "fixture guard");
  assert_eq!(
    list
      .int_values()
      .filter_map(|v| tok(v.int_token()))
      .collect::<Vec<_>>(),
    ["1"]
  );
  assert_eq!(
    list
      .float_values()
      .filter_map(|v| tok(v.float_token()))
      .collect::<Vec<_>>(),
    ["2.5"]
  );
  assert_eq!(
    list
      .boolean_values()
      .filter_map(|v| tok(v.name()))
      .collect::<Vec<_>>(),
    ["true"]
  );
  assert_eq!(
    list
      .null_values()
      .filter_map(|v| tok(v.name()))
      .collect::<Vec<_>>(),
    ["null"]
  );
  assert_eq!(
    list
      .enum_values()
      .filter_map(|v| tok(v.name()))
      .collect::<Vec<_>>(),
    ["EV"],
    "`true`, `null` and `EV` are all bare Name tokens; only the node kind separates them"
  );
  assert_eq!(list.list_values().count(), 1, "the nested `[7]`");
  assert_eq!(list.object_values().count(), 1, "the nested object value");
  assert_eq!(
    list.variables().count(),
    0,
    "the `$v` belongs to the object"
  );

  // And through the nested object, whose own `ObjectField` holds the variable.
  let f: ObjectField = obj.object_fields().next().expect("`x: 1`");
  assert_eq!(tok(f.name()).as_deref(), Some("x"));
  assert_eq!(
    f.int_value().map(|v| v.syntax().text().to_string()),
    Some("1".into())
  );
  let nested: ObjectField = list
    .object_values()
    .next()
    .and_then(|o| o.object_fields().next())
    .expect("`k: $v`");
  assert_eq!(
    tok(nested.variable().and_then(|v| v.name())).as_deref(),
    Some("v")
  );
}

#[test]
fn a_string_value_and_a_description_each_reach_both_of_their_token_kinds() {
  // `cast::token` takes one kind, and a string literal has two images — `K::String` for `"s"`
  // and `K::BlockString` for `"""s"""`. One getter must answer for both, which is what the
  // local `tok_any` arm exists for. Four assertions: two nodes x two spellings.
  let inline = document("query { f(s: \"in\") } \"desc\" scalar S");
  let block = document("query { f(s: \"\"\"blk\"\"\") } \"\"\"doc\"\"\" scalar S");

  let string_of = |doc: &Document| -> StringValue {
    doc
      .operation_definitions()
      .next()
      .and_then(|op| op.selection_set())
      .and_then(|set| set.fields().next())
      .and_then(|f| f.arguments())
      .and_then(|a| a.arguments().next())
      .and_then(|a| a.string_value())
      .expect("the string argument")
  };
  let description_of = |doc: &Document| -> Description {
    doc
      .scalar_type_definitions()
      .next()
      .and_then(|s| s.description())
      .expect("the description")
  };

  assert_eq!(
    tok(string_of(&inline).string_token()).as_deref(),
    Some("\"in\"")
  );
  assert_eq!(
    tok(string_of(&block).string_token()).as_deref(),
    Some("\"\"\"blk\"\"\"")
  );
  assert_eq!(
    tok(description_of(&inline).string_token()).as_deref(),
    Some("\"desc\"")
  );
  assert_eq!(
    tok(description_of(&block).string_token()).as_deref(),
    Some("\"\"\"doc\"\"\"")
  );

  // The kind is still on the token, so the block/inline distinction survives the one getter.
  assert_eq!(
    string_of(&block).string_token().map(|t| t.kind()),
    Some(K::BlockString)
  );
  assert_eq!(
    string_of(&inline).string_token().map(|t| t.kind()),
    Some(K::String)
  );

  // Near neighbours: `Description` and `StringValue` are the two node kinds that wrap a bare
  // string token and nothing else, so neither may cast to the other.
  let desc_node = description_of(&block).syntax().clone();
  assert!(Description::cast_node(desc_node.clone()).is_some());
  assert!(
    StringValue::cast_node(desc_node).is_none(),
    "a Description must not cast to a StringValue"
  );
}

#[test]
fn a_type_reference_nests_by_kind_rather_than_by_position() {
  // `[Int!]!` — a NonNullType over a ListType over a NonNullType over a NamedType. Every level
  // has exactly one node child, so a getter that returned "the first child" would pass; what it
  // could not do is answer `None` for the two kinds that are *not* there at each level.
  let doc = document("type T { f: [Int!]! }");
  let field: FieldDefinition = doc
    .object_type_definitions()
    .next()
    .and_then(|t| t.fields_definition())
    .and_then(|f| f.field_definitions().next())
    .expect("the field definition");

  let outer: NonNullType = field.non_null_type().expect("the outer `!`");
  assert!(
    field.named_type().is_none(),
    "the type ref is not a NamedType"
  );
  assert!(
    field.list_type().is_none(),
    "the outer node is not a ListType"
  );
  assert_eq!(outer.syntax().text().to_string(), "[Int!]!");

  let list: ListType = outer.list_type().expect("the `[…]`");
  assert!(outer.named_type().is_none());
  assert_eq!(list.syntax().text().to_string(), "[Int!]");

  let inner: NonNullType = list.non_null_type().expect("the inner `!`");
  assert!(
    list.named_type().is_none(),
    "the element is wrapped, not bare"
  );
  assert!(list.list_type().is_none());

  let named: NamedType = inner.named_type().expect("`Int`");
  assert_eq!(tok(named.name()).as_deref(), Some("Int"));
  assert!(
    NamedType::cast_node(list.syntax().clone()).is_none(),
    "a ListType must not cast to a NamedType"
  );
}

#[test]
fn a_field_reaches_past_its_alias_for_its_own_name() {
  // `a: b(…) @d { c }` — the `Name` token `a` lives inside the `Alias` node, so the field's own
  // `Name` is neither its first descendant `Name` nor its first token. And its `SelectionSet`
  // sits behind an `Alias`, an `Arguments` and a `Directives` that all decline the cast.
  let doc = document("{ a: b(x: 1) @d { c } }");
  let set: SelectionSet = doc
    .operation_definitions()
    .next()
    .and_then(|op| op.selection_set())
    .expect("the selection set");
  let field: Field = set.fields().next().expect("the field");

  assert_eq!(
    tok(field.alias().and_then(|a| a.name())).as_deref(),
    Some("a"),
    "the alias owns the first Name"
  );
  assert_eq!(
    tok(field.name()).as_deref(),
    Some("b"),
    "`tok` must not reach into the Alias for the field's own name"
  );
  assert_eq!(
    field.arguments().map(|a| a.syntax().text().to_string()),
    Some("(x: 1)".into())
  );
  assert_eq!(
    field.directives().map(|d| d.syntax().text().to_string()),
    Some("@d ".into())
  );
  assert_eq!(
    field
      .selection_set()
      .map(|s| s.syntax().text().to_string())
      .as_deref(),
    Some("{ c }"),
    "`opt` must scan past the alias, arguments and directives"
  );

  // The inner field has no alias at all, which is the negative half of the same getter.
  let inner: Field = field
    .selection_set()
    .and_then(|s| s.fields().next())
    .expect("the inner field");
  assert!(inner.alias().is_none());
  assert_eq!(tok(inner.name()).as_deref(), Some("c"));
}

#[test]
fn a_selection_set_separates_the_three_selection_kinds() {
  let doc = document("{ ... on T { a } ...Frag @d f g }");
  let set: SelectionSet = doc
    .operation_definitions()
    .next()
    .and_then(|op| op.selection_set())
    .expect("the selection set");

  assert_eq!(
    set.syntax().children().count(),
    4,
    "fixture guard: four selections of three different kinds"
  );
  // Two fields and one of each other kind: with one of each, a getter pointed at the wrong kind
  // would answer the same count and the test could not tell.
  assert_eq!(
    set
      .fields()
      .filter_map(|f| tok(f.name()))
      .collect::<Vec<_>>(),
    ["f", "g"]
  );
  assert_eq!(set.fragment_spreads().count(), 1);
  assert_eq!(set.inline_fragments().count(), 1);
  assert_eq!(
    tok(set.fragment_spreads().next().and_then(|s| s.name())).as_deref(),
    Some("Frag")
  );

  let frag: InlineFragment = set.inline_fragments().next().expect("the inline fragment");
  // The type condition has no node kind of its own — it surfaces as the `NamedType` after the
  // `on`, which is also the fragment's only node child besides its selection set.
  assert_eq!(
    tok(frag.type_condition().and_then(|t| t.name())).as_deref(),
    Some("T")
  );
  assert_eq!(
    frag.selection_set().map(|s| s.syntax().text().to_string()),
    Some("{ a }".into())
  );
  assert!(frag.directives().is_none());
}

#[test]
fn an_executable_definition_names_itself_past_its_keyword() {
  // `fragment F on T` puts three `Name` tokens directly under the definition — `fragment`, `F`
  // and `on` — so "the first Name" answers `fragment` and only a positional getter answers `F`.
  let doc = document("query Q($v: Int! = 3, $w: [String] @k) @d { f } fragment F on T @e { c }");

  let op: OperationDefinition = doc.operation_definitions().next().expect("the operation");
  // The operation's own `Name` is not its first token (a `Space` is) and not the first `Name`
  // beneath it (`query`, inside the `OperationType` node, is).
  assert_eq!(tok(op.name()).as_deref(), Some("Q"));
  let op_type: OperationType = op.operation_type().expect("the operation type");
  assert_eq!(tok(op_type.name()).as_deref(), Some("query"));
  assert!(op.description().is_none());

  let vars = op.variables_definition().expect("the variable definitions");
  assert_eq!(vars.variable_definitions().count(), 2);
  let mut defs = vars.variable_definitions();
  let v: VariableDefinition = defs.next().expect("`$v`");
  let w: VariableDefinition = defs.next().expect("`$w`");
  assert_eq!(
    tok(v.variable().and_then(|x| x.name())).as_deref(),
    Some("v")
  );
  assert!(v.non_null_type().is_some(), "`Int!`");
  assert!(v.named_type().is_none());
  assert_eq!(
    v.default_value().map(|d| d.syntax().text().to_string()),
    Some("= 3".into())
  );
  assert!(v.directives().is_none());
  assert!(w.list_type().is_some(), "`[String]`");
  assert!(w.default_value().is_none());
  assert!(w.directives().is_some());

  let frag = doc.fragment_definitions().next().expect("the fragment");
  assert_eq!(
    tok(frag.name()).as_deref(),
    Some("F"),
    "`fragment` and `on` are Name tokens too; only the second one is the fragment's name"
  );
  assert_eq!(
    tok(frag.type_condition().and_then(|t| t.name())).as_deref(),
    Some("T")
  );
  assert!(frag.directives().is_some());
  assert_eq!(
    frag.selection_set().map(|s| s.syntax().text().to_string()),
    Some("{ c }".into())
  );
}

#[test]
fn a_type_system_definition_exposes_its_parts_by_name() {
  let doc = document(
    "\"\"\"doc\"\"\" type T implements I & J @d { f(x: Int = 1): [Int!]! @e } \
     schema @s { query: Q mutation: M } \
     union U = A | B enum E { X Y } input In { a: Int } \
     directive @dd(y: Int) repeatable on FIELD | QUERY",
  );

  let obj: ObjectTypeDefinition = doc
    .object_type_definitions()
    .next()
    .expect("the object type");
  assert_eq!(
    tok(obj.name()).as_deref(),
    Some("T"),
    "`type` is a Name token too; the type's own name is the second"
  );
  assert_eq!(
    tok(obj.description().and_then(|d| d.string_token())).as_deref(),
    Some("\"\"\"doc\"\"\"")
  );
  let implements = obj.implements_interfaces().expect("`implements I & J`");
  assert_eq!(
    implements
      .interfaces()
      .filter_map(|t| tok(t.name()))
      .collect::<Vec<_>>(),
    ["I", "J"],
    "both interfaces, in order"
  );
  assert!(obj.directives().is_some());
  let fields = obj.fields_definition().expect("the fields block");

  let f: FieldDefinition = fields.field_definitions().next().expect("the field");
  assert_eq!(tok(f.name()).as_deref(), Some("f"));
  let arg_defs = f.arguments_definition().expect("`(x: Int = 1)`");
  let x = arg_defs
    .input_value_definitions()
    .next()
    .expect("the input value");
  assert_eq!(tok(x.name()).as_deref(), Some("x"));
  assert_eq!(
    tok(x.named_type().and_then(|t| t.name())).as_deref(),
    Some("Int")
  );
  assert_eq!(
    x.default_value().map(|d| d.syntax().text().to_string()),
    Some("= 1".into())
  );
  assert!(
    f.non_null_type().is_some(),
    "the field's own type is `[Int!]!`"
  );
  assert_eq!(
    f.directives().map(|d| d.syntax().text().to_string()),
    Some("@e ".into())
  );

  let schema: SchemaDefinition = doc.schema_definitions().next().expect("the schema");
  assert!(schema.directives().is_some());
  let roots = schema
    .root_operation_type_definitions()
    .expect("the root block");
  assert_eq!(
    roots
      .root_operation_type_definitions()
      .filter_map(|r| Some((
        tok(r.operation_type().and_then(|o| o.name()))?,
        tok(r.named_type().and_then(|n| n.name()))?
      )))
      .collect::<Vec<_>>(),
    [
      ("query".to_string(), "Q".to_string()),
      ("mutation".to_string(), "M".to_string())
    ]
  );

  let union: UnionTypeDefinition = doc.union_type_definitions().next().expect("the union");
  assert_eq!(tok(union.name()).as_deref(), Some("U"));
  assert_eq!(
    union
      .union_member_types()
      .expect("`= A | B`")
      .member_types()
      .filter_map(|t| tok(t.name()))
      .collect::<Vec<_>>(),
    ["A", "B"]
  );

  let enum_ty: EnumTypeDefinition = doc.enum_type_definitions().next().expect("the enum");
  assert_eq!(tok(enum_ty.name()).as_deref(), Some("E"));
  assert_eq!(
    enum_ty
      .enum_values_definition()
      .expect("`{ X Y }`")
      .enum_value_definitions()
      .filter_map(|v| tok(v.enum_value().and_then(|e| e.name())))
      .collect::<Vec<_>>(),
    ["X", "Y"]
  );

  let input: InputObjectTypeDefinition = doc
    .input_object_type_definitions()
    .next()
    .expect("the input object");
  assert_eq!(tok(input.name()).as_deref(), Some("In"));
  assert_eq!(
    input
      .input_fields_definition()
      .expect("`{ a: Int }`")
      .input_value_definitions()
      .filter_map(|v| tok(v.name()))
      .collect::<Vec<_>>(),
    ["a"]
  );

  let dd: DirectiveDefinition = doc
    .directive_definitions()
    .next()
    .expect("the directive def");
  assert_eq!(
    tok(dd.name()).as_deref(),
    Some("dd"),
    "`directive`, `repeatable` and `on` are all Name tokens; the name is the second"
  );
  assert!(dd.arguments_definition().is_some());
  // `cast::token` answers only its first match and there is no `cast::tokens`, so the second
  // location would be unreachable without the local plural form.
  let locations: DirectiveLocations = dd.directive_locations().expect("the locations");
  assert_eq!(
    locations
      .locations()
      .map(|t| t.text().to_string())
      .collect::<Vec<_>>(),
    ["FIELD", "QUERY"],
    "both locations, in order — they are bare Name tokens with no node kind of their own"
  );

  let scalar_less = document("scalar S @sd");
  let s: ScalarTypeDefinition = scalar_less
    .scalar_type_definitions()
    .next()
    .expect("the scalar");
  assert_eq!(tok(s.name()).as_deref(), Some("S"));
  assert!(s.description().is_none());
  assert!(s.directives().is_some());
}

#[test]
fn a_type_system_extension_names_itself_past_two_keywords() {
  // An extension carries one more leading `Name` than a definition — `extend`, then the shape
  // keyword, then the name — so a positional getter tuned for a definition answers the shape
  // keyword here. Both positions are asserted, in one document.
  let doc = document(
    "extend type T implements I @d { g: Int } extend scalar S @y \
     extend union U = C extend enum E @z extend input In { b: Int } \
     extend interface N @w extend schema @x",
  );

  let obj: ObjectTypeExtension = doc
    .object_type_extensions()
    .next()
    .expect("the type extension");
  assert_eq!(
    tok(obj.name()).as_deref(),
    Some("T"),
    "`extend` and `type` are Name tokens too; the name is the third"
  );
  assert!(obj.implements_interfaces().is_some());
  assert!(obj.directives().is_some());
  assert_eq!(
    obj
      .fields_definition()
      .and_then(|f| f.field_definitions().next())
      .and_then(|f| tok(f.name()))
      .as_deref(),
    Some("g")
  );

  assert_eq!(
    tok(doc.scalar_type_extensions().next().and_then(|s| s.name())).as_deref(),
    Some("S")
  );
  assert_eq!(
    tok(doc.union_type_extensions().next().and_then(|u| u.name())).as_deref(),
    Some("U")
  );
  assert_eq!(
    tok(doc.enum_type_extensions().next().and_then(|e| e.name())).as_deref(),
    Some("E")
  );
  assert_eq!(
    tok(
      doc
        .input_object_type_extensions()
        .next()
        .and_then(|i| i.name())
    )
    .as_deref(),
    Some("In")
  );
  assert_eq!(
    tok(
      doc
        .interface_type_extensions()
        .next()
        .and_then(|i| i.name())
    )
    .as_deref(),
    Some("N")
  );

  // The one extension with nothing to be called, so it gets no name getter at all.
  let schema: SchemaExtension = doc
    .schema_extensions()
    .next()
    .expect("the schema extension");
  assert!(schema.directives().is_some());
  assert!(schema.root_operation_type_definitions().is_none());
}

#[test]
fn the_two_alternative_document_roots_wrap_their_own_definitions() {
  // Neither root is reachable from `parse_str`, which parses the mixed `Document`; both have a
  // driver, and both are among the fifty-nine.
  let exec = parse_executable_document("query Q { a } fragment F on T { b } query R { c }");
  let root = exec.syntax();
  let doc: ExecutableDocument = cast::child(&root).expect("the executable document");
  assert_eq!(doc.syntax().children().count(), 3, "fixture guard");
  assert_eq!(doc.operation_definitions().count(), 2);
  assert_eq!(doc.fragment_definitions().count(), 1);
  assert!(
    Document::cast_node(doc.syntax().clone()).is_none(),
    "an ExecutableDocument must not cast to a Document"
  );

  let sdl = parse_type_system_document("scalar S scalar S2 type T { a: Int } extend type T @d");
  let root = sdl.syntax();
  let doc: TypeSystemDocument = cast::child(&root).expect("the type-system document");
  assert_eq!(doc.syntax().children().count(), 4, "fixture guard");
  assert_eq!(
    doc
      .scalar_type_definitions()
      .filter_map(|s| tok(s.name()))
      .collect::<Vec<_>>(),
    ["S", "S2"]
  );
  assert_eq!(
    tok(doc.object_type_definitions().next().and_then(|t| t.name())).as_deref(),
    Some("T")
  );
  assert_eq!(
    tok(doc.object_type_extensions().next().and_then(|t| t.name())).as_deref(),
    Some("T")
  );
  assert_eq!(doc.interface_type_definitions().count(), 0);
}

#[test]
fn the_local_token_helpers_scan_direct_children_only() {
  // `token_any` and `tokens` are this crate's own, written because `cast` offers no multi-kind
  // and no plural token getter. They owe the same contract `cast::token` keeps — direct token
  // children, never a descendant — and **no wrapper among the fifty-nine can prove it**: every
  // `toks` and `tok_nth` site happens to carry its own tokens before any child node that holds a
  // token of the same kind, so a descendant scan would answer identically at all of them. The
  // two helpers are therefore asserted here directly, against a node that does have such a
  // child.
  let doc = document("\"\"\"doc\"\"\" type T implements I & J { a: Int }");
  let obj = doc
    .object_type_definitions()
    .next()
    .expect("the object type");

  // `implements` is a direct `Name`; `I` and `J` are `Name`s one level down, inside `NamedType`.
  let implements = obj.implements_interfaces().expect("the implements clause");
  assert_eq!(
    implements
      .syntax()
      .descendants_with_tokens()
      .filter(|e| e.kind() == K::Name)
      .count(),
    3,
    "fixture guard: three Name tokens beneath the clause, only one of them direct"
  );
  assert_eq!(
    tokens(implements.syntax(), K::Name)
      .map(|t| t.text().to_string())
      .collect::<Vec<_>>(),
    ["implements"],
    "`tokens` must not reach into a child node"
  );

  // The block string belongs to the `Description` node, not to the type definition above it.
  assert!(
    obj
      .syntax()
      .descendants_with_tokens()
      .any(|e| e.kind() == K::BlockString),
    "fixture guard: the block string is there, one level down"
  );
  assert_eq!(
    token_any(obj.syntax(), &[K::String, K::BlockString]),
    None,
    "`token_any` must not reach into a child node"
  );
  assert!(
    token_any(
      obj.description().expect("the description").syntax(),
      &[K::String, K::BlockString]
    )
    .is_some(),
    "and the node that does own the token still answers"
  );
}

#[test]
fn a_directive_and_its_arguments_keep_their_wrapper_levels() {
  // The plan's recorded decision: `Directives` is not skipped. A `Field` answers
  // `Option<Directives>`, and the walk to a single `Directive` goes through it — which is what
  // makes "no directives written at all" distinguishable from "an empty list", the property
  // Task 6 built the node placement for.
  let doc = document("{ f @a @b(x: 1) g }");
  let set = doc
    .operation_definitions()
    .next()
    .and_then(|op| op.selection_set())
    .expect("the selection set");
  let mut fields = set.fields();
  let f: Field = fields.next().expect("`f`");
  let g: Field = fields.next().expect("`g`");

  let dirs = f.directives().expect("`f` has directives");
  assert_eq!(dirs.directives().count(), 2);
  assert!(
    g.directives().is_none(),
    "a field with no `@` opens no Directives node at all"
  );

  let b: Directive = dirs.directives().nth(1).expect("the second directive");
  assert_eq!(tok(b.name()).as_deref(), Some("b"));
  assert_eq!(
    b.arguments()
      .and_then(|a| a.arguments().next())
      .and_then(|a| tok(a.name()))
      .as_deref(),
    Some("x")
  );
  assert!(
    dirs
      .directives()
      .next()
      .expect("the first directive")
      .arguments()
      .is_none()
  );
}

// ===========================================================================================
// Task 14 — the totality half
// ===========================================================================================

/// The two ends of a `rowan::TextRange`.
type Span = (u32, u32);

/// What a getter projected, reduced to something a **kind-blind rival** can be compared against.
///
/// The rival is the whole point. "This getter answered `Some`" is nearly free — over `{ a }` a
/// `cast::child` is indistinguishable from "take the first child", which is the fixture weakness
/// Task 9 recorded. What is not free is answering *differently* from a getter that ignored kinds
/// altogether, and that is a property of the material this gate sweeps rather than of the getter,
/// so it is measured rather than claimed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Projection {
  /// A singular getter's answer: the span of what it found, if it found anything.
  One(Option<Span>),
  /// A plural getter's answer: how many it yielded, and where the first one starts.
  Many(usize, Option<Span>),
}

impl Projection {
  /// Whether the getter projected anything at all.
  fn answered(self) -> bool {
    match self {
      Self::One(found) => found.is_some(),
      Self::Many(count, _) => count > 0,
    }
  }
}

/// Something a getter can project — one of the 59 wrappers, or a token.
///
/// The trait exists for its two associated functions: an element type knows whether its getters
/// look at a node's *children* or at its *tokens*, and therefore what the kind-blind rival on that
/// side is.
trait Element: Sized {
  /// This element's own span.
  fn span(&self) -> Span;
  /// "Take the first one, whatever kind it is."
  fn rival_one(parent: &SyntaxNode) -> Projection;
  /// "Take all of them, whatever kind they are."
  fn rival_many(parent: &SyntaxNode) -> Projection;
}

fn span_of(range: rowan::TextRange) -> Span {
  (range.start().into(), range.end().into())
}

impl Element for SyntaxToken {
  fn span(&self) -> Span {
    span_of(self.text_range())
  }

  fn rival_one(parent: &SyntaxNode) -> Projection {
    Projection::One(
      parent
        .children_with_tokens()
        .find_map(|child| child.into_token())
        .map(|t| span_of(t.text_range())),
    )
  }

  fn rival_many(parent: &SyntaxNode) -> Projection {
    let spans: Vec<Span> = parent
      .children_with_tokens()
      .filter_map(|child| child.into_token())
      .map(|t| span_of(t.text_range()))
      .collect();
    Projection::Many(spans.len(), spans.first().copied())
  }
}

/// The node-side rival for [`Element::rival_one`], shared by all 59 wrappers.
fn first_child_node(parent: &SyntaxNode) -> Projection {
  Projection::One(parent.children().next().map(|n| span_of(n.text_range())))
}

/// The node-side rival for [`Element::rival_many`], shared by all 59 wrappers.
fn all_child_nodes(parent: &SyntaxNode) -> Projection {
  let spans: Vec<Span> = parent.children().map(|n| span_of(n.text_range())).collect();
  Projection::Many(spans.len(), spans.first().copied())
}

/// A getter's return type, reduced to a [`Projection`] alongside the rival it must beat.
trait Projected {
  /// What this answer projected.
  fn projection(self) -> Projection;
  /// What a kind-blind getter of the same arity would have answered on the same node.
  fn rival(parent: &SyntaxNode) -> Projection;
}

impl<T: Element> Projected for Option<T> {
  fn projection(self) -> Projection {
    Projection::One(self.map(|found| found.span()))
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    T::rival_one(parent)
  }
}

impl<N: Element + CastNode<GraphQLLang>> Projected for AstChildren<N> {
  fn projection(self) -> Projection {
    let spans: Vec<Span> = self.map(|found| found.span()).collect();
    Projection::Many(spans.len(), spans.first().copied())
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    N::rival_many(parent)
  }
}

impl Projected for AstTokens {
  fn projection(self) -> Projection {
    let spans: Vec<Span> = self.map(|found| found.span()).collect();
    Projection::Many(spans.len(), spans.first().copied())
  }

  fn rival(parent: &SyntaxNode) -> Projection {
    SyntaxToken::rival_many(parent)
  }
}

/// Declare the wrapper/getter table this gate walks, and generate the three things read off it.
///
/// Entries mirror the `ast_node!` invocations exactly — the same wrapper name, the same getter
/// names, the same forms, in the same order — minus the target kinds, which
/// [`the_probe_table_is_exactly_the_shipped_inventory`] reads out of the sources instead.
///
/// Each getter is named **once**, and the name is both the method called and the label recorded,
/// through `stringify!`. That is not a stylistic choice: a table that spelled the label
/// separately could record `Field::name` against a call to `Field::alias()` and report full
/// coverage while leaving a getter untouched.
macro_rules! wrapper_table {
  ($( $ty:ident { $( $form:ident $getter:ident ),* $(,)? } )*) => {
    /// Every `(wrapper, getter, form)` triple the table declares.
    const DECLARED: &[(&str, &str, &str)] = &[
      $( $( (stringify!($ty), stringify!($getter), stringify!($form)), )* )*
    ];

    /// Every wrapper the table declares, in declaration order.
    const TABLE_WRAPPERS: &[&str] = &[ $( stringify!($ty), )* ];

    $(
      impl Element for $ty {
        fn span(&self) -> Span {
          span_of(self.syntax().text_range())
        }

        fn rival_one(parent: &SyntaxNode) -> Projection {
          first_child_node(parent)
        }

        fn rival_many(parent: &SyntaxNode) -> Projection {
          all_child_nodes(parent)
        }
      }
    )*

    /// Cast `node` to every wrapper that accepts it, and call every one of that wrapper's
    /// getters.
    ///
    /// Deliberately not `else if`: a node accepted by two wrappers is a defect
    /// ([`each_node_kind_casts_to_exactly_the_wrapper_that_names_it`]), and a chain that stopped
    /// at the first match would hide it here.
    fn probe(node: &SyntaxNode, reg: &mut Registry) {
      $(
        if let Some(typed) = $ty::cast_node(node.clone()) {
          reg.cast(stringify!($ty));
          $(
            reg.record(stringify!($ty), stringify!($getter), node, typed.$getter());
          )*
        }
      )*
    }

    /// Every wrapper whose cast accepts `node` — one row of the 59x59 cast matrix.
    fn accepting_wrappers(node: &SyntaxNode) -> Vec<&'static str> {
      let mut out = Vec::new();
      $( if $ty::cast_node(node.clone()).is_some() { out.push(stringify!($ty)); } )*
      out
    }
  };
}

wrapper_table! {
  Variable { tok name }
  IntValue { tok int_token }
  FloatValue { tok float_token }
  StringValue { tok_any string_token }
  BooleanValue { tok name }
  NullValue { tok name }
  EnumValue { tok name }
  ListValue {
    many variables, many int_values, many float_values, many string_values, many boolean_values,
    many null_values, many enum_values, many list_values, many object_values,
  }
  ObjectValue { many object_fields }
  ObjectField {
    tok name, opt variable, opt int_value, opt float_value, opt string_value, opt boolean_value,
    opt null_value, opt enum_value, opt list_value, opt object_value,
  }
  DefaultValue {
    opt variable, opt int_value, opt float_value, opt string_value, opt boolean_value,
    opt null_value, opt enum_value, opt list_value, opt object_value,
  }
  NamedType { tok name }
  ListType { opt named_type, opt list_type, opt non_null_type }
  NonNullType { opt named_type, opt list_type }
  Argument {
    tok name, opt variable, opt int_value, opt float_value, opt string_value, opt boolean_value,
    opt null_value, opt enum_value, opt list_value, opt object_value,
  }
  Arguments { many arguments }
  Directive { tok name, opt arguments }
  Directives { many directives }
  Alias { tok name }
  Field { opt alias, tok name, opt arguments, opt directives, opt selection_set }
  SelectionSet { many fields, many fragment_spreads, many inline_fragments }
  FragmentSpread { tok name, opt directives }
  InlineFragment { opt type_condition, opt directives, opt selection_set }
  VariableDefinition {
    opt variable, opt named_type, opt list_type, opt non_null_type, opt default_value,
    opt directives,
  }
  VariablesDefinition { many variable_definitions }
  OperationDefinition {
    opt description, opt operation_type, tok name, opt variables_definition, opt directives,
    opt selection_set,
  }
  FragmentDefinition {
    opt description, tok_nth name, opt type_condition, opt directives, opt selection_set,
  }
  ExecutableDocument { many operation_definitions, many fragment_definitions }
  Description { tok_any string_token }
  InputValueDefinition {
    opt description, tok name, opt named_type, opt list_type, opt non_null_type,
    opt default_value, opt directives,
  }
  ArgumentsDefinition { many input_value_definitions }
  FieldDefinition {
    opt description, tok name, opt arguments_definition, opt named_type, opt list_type,
    opt non_null_type, opt directives,
  }
  FieldsDefinition { many field_definitions }
  InputFieldsDefinition { many input_value_definitions }
  ImplementsInterfaces { many interfaces }
  UnionMemberTypes { many member_types }
  DirectiveLocations { toks locations }
  EnumValueDefinition { opt description, opt enum_value, opt directives }
  EnumValuesDefinition { many enum_value_definitions }
  OperationType { tok name }
  RootOperationTypeDefinition { opt operation_type, opt named_type }
  RootOperationTypeDefinitions { many root_operation_type_definitions }
  ScalarTypeDefinition { opt description, tok_nth name, opt directives }
  ObjectTypeDefinition {
    opt description, tok_nth name, opt implements_interfaces, opt directives,
    opt fields_definition,
  }
  InterfaceTypeDefinition {
    opt description, tok_nth name, opt implements_interfaces, opt directives,
    opt fields_definition,
  }
  UnionTypeDefinition { opt description, tok_nth name, opt directives, opt union_member_types }
  EnumTypeDefinition { opt description, tok_nth name, opt directives, opt enum_values_definition }
  InputObjectTypeDefinition {
    opt description, tok_nth name, opt directives, opt input_fields_definition,
  }
  DirectiveDefinition {
    opt description, tok_nth name, opt arguments_definition, opt directive_locations,
  }
  SchemaDefinition { opt description, opt directives, opt root_operation_type_definitions }
  ScalarTypeExtension { tok_nth name, opt directives }
  ObjectTypeExtension {
    tok_nth name, opt implements_interfaces, opt directives, opt fields_definition,
  }
  InterfaceTypeExtension {
    tok_nth name, opt implements_interfaces, opt directives, opt fields_definition,
  }
  UnionTypeExtension { tok_nth name, opt directives, opt union_member_types }
  EnumTypeExtension { tok_nth name, opt directives, opt enum_values_definition }
  InputObjectTypeExtension { tok_nth name, opt directives, opt input_fields_definition }
  SchemaExtension { opt directives, opt root_operation_type_definitions }
  Document {
    many operation_definitions, many fragment_definitions, many scalar_type_definitions,
    many object_type_definitions, many interface_type_definitions, many union_type_definitions,
    many enum_type_definitions, many input_object_type_definitions, many directive_definitions,
    many schema_definitions, many scalar_type_extensions, many object_type_extensions,
    many interface_type_extensions, many union_type_extensions, many enum_type_extensions,
    many input_object_type_extensions, many schema_extensions,
  }
  TypeSystemDocument {
    many scalar_type_definitions, many object_type_definitions, many interface_type_definitions,
    many union_type_definitions, many enum_type_definitions, many input_object_type_definitions,
    many directive_definitions, many schema_definitions, many scalar_type_extensions,
    many object_type_extensions, many interface_type_extensions, many union_type_extensions,
    many enum_type_extensions, many input_object_type_extensions, many schema_extensions,
  }
}

/// How often one getter was called, and how often it projected something.
#[derive(Debug, Default, Clone, Copy)]
struct Answers {
  /// How many nodes of the getter's own kind it ran on.
  calls: usize,
  /// On how many of those it returned `Some`, or a non-empty iterator.
  hits: usize,
}

/// What one sweep observed.
#[derive(Default)]
struct Registry {
  /// Wrapper name -> how many nodes it cast.
  casts: BTreeMap<&'static str, usize>,
  /// `(wrapper, getter)` -> its call and hit counts.
  getters: BTreeMap<(&'static str, &'static str), Answers>,
  /// The getters seen, at least once, to answer differently from their kind-blind rival.
  discriminated: BTreeSet<(&'static str, &'static str)>,
  /// One real node per node kind seen, for the cast matrix.
  representatives: BTreeMap<K, SyntaxNode>,
}

impl Registry {
  fn cast(&mut self, wrapper: &'static str) {
    *self.casts.entry(wrapper).or_default() += 1;
  }

  fn record<P: Projected>(
    &mut self,
    wrapper: &'static str,
    getter: &'static str,
    parent: &SyntaxNode,
    answer: P,
  ) {
    let projection = answer.projection();
    let entry = self.getters.entry((wrapper, getter)).or_default();
    entry.calls += 1;
    entry.hits += usize::from(projection.answered());
    if projection != P::rival(parent) {
      self.discriminated.insert((wrapper, getter));
    }
  }

  /// Probe every node in one tree, root included.
  fn walk(&mut self, root: &SyntaxNode) {
    for node in root.descendants() {
      self
        .representatives
        .entry(node.kind())
        .or_insert_with(|| node.clone());
      probe(&node, self);
    }
  }

  /// Parse one source through all three roots and probe every tree.
  ///
  /// All three, because `parse_str` reaches 57 of the 59 wrappers structurally: `ExecutableDocument`
  /// and `TypeSystemDocument` are *roots*, and a parse has one. Driving the other two over the
  /// same bytes is what Task 12 does for the same reason.
  fn sweep_source(&mut self, src: &str) {
    self.walk(&parse_str(src).syntax());
    self.walk(&parse_executable_document(src).syntax());
    self.walk(&parse_type_system_document(src).syntax());
  }

  /// The wrappers this sweep never cast.
  fn uncast(&self) -> BTreeSet<&'static str> {
    TABLE_WRAPPERS
      .iter()
      .copied()
      .filter(|w| !self.casts.contains_key(w))
      .collect()
  }

  /// The getters this sweep never called at all.
  fn uncalled(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| !self.getters.contains_key(key))
      .collect()
  }

  /// The getters this sweep called but which never projected anything.
  fn unanswered(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| self.getters.get(key).is_none_or(|a| a.hits == 0))
      .collect()
  }

  /// The getters this sweep never saw answer differently from a kind-blind rival.
  fn undiscriminated(&self) -> BTreeSet<(&'static str, &'static str)> {
    DECLARED
      .iter()
      .map(|(w, g, _)| (*w, *g))
      .filter(|key| !self.discriminated.contains(key))
      .collect()
  }
}

/// Every `.graphql` file in the shared corpus, in a deterministic order.
///
/// Read unchanged, exactly as gates 1, 2, 3 and 5 read it — this gate adds no corpus entry, so a
/// green here is a statement about the same material the other four hold.
fn corpus() -> Vec<(String, String)> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("tests")
    .join("corpus");
  let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
    .unwrap_or_else(|e| panic!("the shared corpus at {} is unreadable: {e}", dir.display()))
    .map(|entry| entry.expect("a corpus directory entry").path())
    .filter(|path| path.extension().is_some_and(|ext| ext == "graphql"))
    .collect();
  files.sort();
  files
    .into_iter()
    .map(|p| {
      let name = p.file_name().unwrap().to_string_lossy().to_string();
      let src = std::fs::read_to_string(&p)
        .unwrap_or_else(|e| panic!("{} is unreadable: {e}", p.display()));
      (name, src)
    })
    .collect()
}

/// Sources this gate adds on top of the corpus, and what each one is for.
///
/// The corpus reaches **every one of the 59 wrappers** on its own and calls all 204 getters, but
/// 31 of those getters had nothing to project from it: a getter is only exercised by a document
/// that writes the shape it reaches for, and no corpus entry writes an enum default, a boolean
/// inside a list, a described union or an interface extension that implements. The first three
/// fixtures are that complement, and nothing else — they were written against the measured list
/// in [`CORPUS_CANNOT_PROJECT`], which is what keeps them honest. The fourth is for a different
/// gap: [`UNDISCRIMINATED`], where the corpus's material could not separate four getters from a
/// kind-blind rival.
///
/// They are fixtures rather than corpus entries on purpose: the corpus is shared with gates 1, 2,
/// 3 and 5, and a file added here would move gate 5's golden set for a reason that has nothing to
/// do with tree shape.
const FIXTURES: &[(&str, &str)] = &[
  // Nine value kinds in each of the three value positions that can hold them — a list's members,
  // an object's fields and a variable's default — plus the two type-reference forms and the
  // directive position a variable definition has.
  (
    "every_value_kind_in_every_value_position",
    "query Q($a: E = EV, $b: Float = 1.5, $c: Int = null, $d: In = {x: 1}, $e: String = \"s\", \
     $f: [Int] @dv) @dq { \
     h(list: [$v, 1, 1.5, \"s\", true, null, EV, [7], {k: 1}], \
     obj: {v: $x, i: 1, f: 1.5, s: \"s\", b: true, n: null, e: EV, l: [1], o: {y: 2}}) }",
  ),
  // A description on every definition that takes one, and a directive on the two definition
  // kinds no corpus entry decorates.
  (
    "a_description_on_every_definition_that_takes_one",
    "\"scalar\" scalar S @d \
     \"interface\" interface I implements J @d { a: Int } \
     \"union\" union U @d = A \
     \"enum\" enum E @d { X } \
     \"input\" input In @d { a: Int } \
     \"directive\" directive @dd(x: Int) on FIELD \
     \"schema\" schema @d { query: Q } \
     \"fragment\" fragment F on T { a } \
     \"query\" query Q @dq { f }",
  ),
  // The interface extension's `implements`, and a bare list type in an input value definition —
  // the corpus writes `[String!]!`, whose outer node is a `NonNullType`.
  (
    "an_interface_extension_and_a_bare_list_type",
    "extend interface I implements J @d { a: Int } \
     type T { f(x: [Int] = [1]): Int }",
  ),
  // An extension whose body is written and whose directives are not. Every extension the corpus
  // writes carries directives, and they are the first child node when they are there, so
  // `directives()` and "take the first child node" agree on all of them — see
  // [`UNDISCRIMINATED`].
  (
    "extensions_that_carry_a_body_and_no_directives",
    "extend union U = C extend enum E { X } extend input In { a: Int } extend schema { query: Q }",
  ),
];

/// A sweep over the corpus alone.
fn sweep_corpus() -> Registry {
  let mut reg = Registry::default();
  for (_, src) in corpus() {
    reg.sweep_source(&src);
  }
  reg
}

/// A sweep over the corpus and this gate's own fixtures, valid and malformed alike.
fn sweep_everything() -> Registry {
  let mut reg = sweep_corpus();
  for (_, src) in FIXTURES.iter().chain(RECOVERY_FIXTURES) {
    reg.sweep_source(src);
  }
  reg
}

// ---- the shipped inventory, read back out of the wrapper sources ----

/// One getter as the wrapper sources actually spell it.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct SourceGetter {
  wrapper: String,
  name: String,
  form: String,
  /// Everything after the form: a wrapper type, a `K::` path, `K::A | K::B`, or `n K::A`.
  target: String,
}

/// The seven files the 59 `ast_node!` invocations live in.
const AST_FILES: &[&str] = &[
  "value.rs",
  "ty.rs",
  "directive.rs",
  "selection.rs",
  "executable.rs",
  "definition.rs",
  "document.rs",
];

/// Strip every `//`-introduced comment, line by line.
///
/// Enough for these seven files, which hold `ast_node!` invocations and doc comments and no
/// string literals at all. If one ever gains a literal containing `//` the parse below breaks
/// loudly, in [`the_probe_table_is_exactly_the_shipped_inventory`], rather than quietly.
fn without_comments(src: &str) -> String {
  src
    .lines()
    .map(|line| match line.find("//") {
      Some(at) => &line[..at],
      None => line,
    })
    .collect::<Vec<_>>()
    .join("\n")
}

/// Every getter the crate ships, parsed out of the `ast_node!` invocations themselves.
///
/// The hand-maintained table above is what this gate *walks*; this is what the crate *exposes*,
/// and the two are compared. Parsing source text in a test is unusual and is the point: no
/// declaration a human maintains can be the sole witness for "nothing is missing", because the
/// thing that goes missing is the declaration.
fn source_inventory() -> Vec<SourceGetter> {
  let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("src")
    .join("graphql")
    .join("lossless")
    .join("ast");
  let mut out = Vec::new();
  for file in AST_FILES {
    let path = dir.join(file);
    let raw = std::fs::read_to_string(&path)
      .unwrap_or_else(|e| panic!("{} is unreadable: {e}", path.display()));
    let src = without_comments(&raw);
    let mut rest = src.as_str();
    while let Some(at) = rest.find("ast_node!(") {
      rest = &rest[at + "ast_node!(".len()..];
      let mut depth = 1usize;
      let mut end = rest.len();
      for (i, ch) in rest.char_indices() {
        match ch {
          '(' => depth += 1,
          ')' => {
            depth -= 1;
            if depth == 0 {
              end = i;
              break;
            }
          }
          _ => {}
        }
      }
      let body = &rest[..end];
      rest = &rest[end..];

      let (header, tail) = body
        .split_once('{')
        .unwrap_or_else(|| panic!("{file}: an ast_node! body opens a brace"));
      let getters = tail
        .rsplit_once('}')
        .unwrap_or_else(|| panic!("{file}: an ast_node! body closes its brace"))
        .0;
      let wrapper = header
        .split_once("=>")
        .unwrap_or_else(|| panic!("{file}: an ast_node! header reads `Name => K::Kind`"))
        .0
        .trim()
        .to_string();

      for entry in getters.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
          continue;
        }
        let (name, spec) = entry
          .split_once(':')
          .unwrap_or_else(|| panic!("{file}: `{entry}` is not `getter: form target`"));
        let mut words = spec.split_whitespace();
        let form = words
          .next()
          .unwrap_or_else(|| panic!("{file}: `{entry}` names no getter form"))
          .to_string();
        out.push(SourceGetter {
          wrapper: wrapper.clone(),
          name: name.trim().to_string(),
          form,
          target: words.collect::<Vec<_>>().join(" "),
        });
      }
    }
  }
  out
}

/// `InputValueDefinition` -> `input_value_definition`.
fn snake(camel: &str) -> String {
  let mut out = String::with_capacity(camel.len() + 4);
  for (i, ch) in camel.char_indices() {
    if ch.is_ascii_uppercase() {
      if i != 0 {
        out.push('_');
      }
      out.push(ch.to_ascii_lowercase());
    } else {
      out.push(ch);
    }
  }
  out
}

/// The kind or wrapper a getter projects, out of its raw target text.
fn projected(form: &str, target: &str) -> String {
  let word = match form {
    // `tok_nth 2 K::Name` — the index comes first.
    "tok_nth" => target.split_whitespace().nth(1),
    // `tok_any K::String | K::BlockString` — the first of the alternatives names the getter.
    _ => target.split_whitespace().next(),
  }
  .unwrap_or_else(|| panic!("`{form} {target}` names no target"));
  word.strip_prefix("K::").unwrap_or(word).to_string()
}

/// Getters whose name states a grammar **role** rather than the kind they project, with the
/// projection each one is pinned to.
///
/// Five, and every one of them is a position the grammar names and the kind space does not: a
/// type condition, an interface list, a union's members and a directive's locations are all
/// spelled out of `NamedType` and `K::Name`. Everywhere else the name *is* the kind, which
/// [`every_getter_is_named_for_what_it_projects`] turns into a total check.
const ROLE_NAMED_GETTERS: &[(&str, &str, &str, &str)] = &[
  ("DirectiveLocations", "locations", "toks", "Name"),
  ("FragmentDefinition", "type_condition", "opt", "NamedType"),
  ("ImplementsInterfaces", "interfaces", "many", "NamedType"),
  ("InlineFragment", "type_condition", "opt", "NamedType"),
  ("UnionMemberTypes", "member_types", "many", "NamedType"),
];

/// Print a set one entry per line, for a failure message worth reading.
fn listed<T: core::fmt::Debug>(items: impl IntoIterator<Item = T>) -> String {
  items
    .into_iter()
    .map(|item| format!("\n  {item:?}"))
    .collect::<String>()
}

#[test]
fn the_probe_table_is_exactly_the_shipped_inventory() {
  let shipped = source_inventory();

  // Positive control on the parser itself. A source scan that silently found nothing would make
  // every comparison below vacuous, so the counts are pinned before they are used.
  assert_eq!(
    shipped.len(),
    204,
    "the wrapper sources should declare 204 getters"
  );
  let shipped_wrappers: BTreeSet<&str> = shipped.iter().map(|g| g.wrapper.as_str()).collect();
  assert_eq!(shipped_wrappers.len(), 59, "…over 59 wrappers");
  assert_eq!(DECLARED.len(), 204, "the table declares 204 getters");
  assert_eq!(TABLE_WRAPPERS.len(), 59, "…over 59 wrappers");

  let table: BTreeSet<(&str, &str, &str)> = DECLARED.iter().copied().collect();
  assert_eq!(table.len(), DECLARED.len(), "the table repeats an entry");

  let shipped_set: BTreeSet<(&str, &str, &str)> = shipped
    .iter()
    .map(|g| (g.wrapper.as_str(), g.name.as_str(), g.form.as_str()))
    .collect();

  let missing: BTreeSet<_> = shipped_set.difference(&table).collect();
  let extra: BTreeSet<_> = table.difference(&shipped_set).collect();
  assert!(
    missing.is_empty(),
    "the crate ships getters this gate never calls, because they are absent from the \
     table:{}",
    listed(missing)
  );
  assert!(
    extra.is_empty(),
    "the table names getters the crate does not ship:{}",
    listed(extra)
  );

  let table_wrappers: BTreeSet<&str> = TABLE_WRAPPERS.iter().copied().collect();
  assert_eq!(table_wrappers, shipped_wrappers);
}

#[test]
fn every_getter_is_named_for_what_it_projects() {
  // The defect this catches is the one no other gate can see: a getter aimed at a **plausible
  // neighbour** — `interface_type_definitions: many ObjectTypeDefinition` — which compiles,
  // navigates, answers `Some`, round-trips and keeps every byte. Only the name disagrees with the
  // kind, so the name is what has to be checked, for all 204 rather than for the handful a
  // fixture happens to count.
  let mut role_named: BTreeSet<(String, String, String, String)> = ROLE_NAMED_GETTERS
    .iter()
    .map(|(w, n, f, t)| (w.to_string(), n.to_string(), f.to_string(), t.to_string()))
    .collect();
  let mut offenders = Vec::new();

  for g in source_inventory() {
    let projects = projected(&g.form, &g.target);
    let key = (
      g.wrapper.clone(),
      g.name.clone(),
      g.form.clone(),
      projects.clone(),
    );
    if ROLE_NAMED_GETTERS
      .iter()
      .any(|(w, n, _, _)| *w == g.wrapper && *n == g.name)
    {
      assert!(
        role_named.remove(&key),
        "{}::{} is pinned as a role-named getter, but it now reads `{} {}`",
        g.wrapper,
        g.name,
        g.form,
        g.target
      );
      continue;
    }

    let base = snake(&projects);
    let expected: Vec<String> = match g.form.as_str() {
      "opt" => vec![base],
      "many" => vec![format!("{base}s")],
      // A token getter is named for its token, optionally with a `_token` suffix where the bare
      // name would collide with a node getter (`int_token`, not `int`).
      _ => vec![base.clone(), format!("{base}_token")],
    };
    if !expected.contains(&g.name) {
      offenders.push(format!(
        "{}::{} projects {} but is not named any of {:?}",
        g.wrapper, g.name, projects, expected
      ));
    }
  }

  assert!(
    offenders.is_empty(),
    "a getter's name must state the kind it projects:{}",
    listed(offenders)
  );
  assert!(
    role_named.is_empty(),
    "these role-named getters are pinned but no longer exist:{}",
    listed(role_named)
  );
}

#[test]
fn a_positional_name_getter_counts_past_exactly_its_own_keywords() {
  // Task 10's ruling 2, as a rule rather than as fourteen separate assertions: this kind space
  // has no `Name` node and no per-keyword token kind, so a definition's keyword and its name are
  // both `K::Name` tokens under the same parent. `type T` puts the name second; `extend type T`
  // puts it third, the `extend` landing inside the extension's own node. An index that is right
  // for a definition is therefore wrong for an extension, and vice versa — which is exactly what
  // Task 10's mutations 2 and 3 measured.
  let mut positional = 0;
  for g in source_inventory().iter().filter(|g| g.form == "tok_nth") {
    positional += 1;
    assert_eq!(g.name, "name", "only a name getter is positional");
    assert_eq!(projected(&g.form, &g.target), "Name");
    let index = g
      .target
      .split_whitespace()
      .next()
      .expect("`tok_nth n K::Name`");
    let expected = if g.wrapper.ends_with("Extension") {
      "2"
    } else {
      "1"
    };
    assert_eq!(
      index, expected,
      "{} counts past the wrong number of keywords",
      g.wrapper
    );
  }
  assert_eq!(positional, 14, "fourteen wrappers name themselves by index");
}

#[test]
fn every_wrapper_casts_from_a_real_parse() {
  let reg = sweep_everything();
  assert!(
    reg.uncast().is_empty(),
    "these wrappers never cast a single node in the whole sweep, which is indistinguishable \
     from their kind being misspelled:{}",
    listed(reg.uncast())
  );
  assert_eq!(reg.casts.len(), 59);
}

#[test]
fn every_getter_is_called_and_projects_something() {
  let reg = sweep_everything();
  assert!(
    reg.uncalled().is_empty(),
    "these getters were never called — no node of their wrapper's kind occurs in the \
     sweep:{}",
    listed(reg.uncalled())
  );
  assert!(
    reg.unanswered().is_empty(),
    "these getters were called but never projected anything, so nothing here distinguishes \
     them from a getter pointed at a kind that cannot occur:{}",
    listed(reg.unanswered())
  );
  assert_eq!(reg.getters.len(), 204);
}

#[test]
fn each_node_kind_casts_to_exactly_the_wrapper_that_names_it() {
  // The near-neighbour decline, made total. The twelve tests above assert it for the three
  // sharpest pairs by hand; this asserts it for every kind against all 59 wrappers at once, and
  // it is the check a wrapper pointed at a *neighbouring* kind fails — that wrapper then accepts
  // a node another wrapper already answers for, and the row carries two names instead of one.
  let reg = sweep_everything();
  let mut checked = 0;
  for (kind, node) in &reg.representatives {
    let named = format!("{kind:?}");
    let expected: Vec<&str> = TABLE_WRAPPERS
      .iter()
      .copied()
      .filter(|w| *w == named)
      .collect();
    assert_eq!(
      accepting_wrappers(node),
      expected,
      "a {named} node must cast to {expected:?} and to nothing else"
    );
    checked += 1;
  }
  // The three bookkeeping kinds have no wrapper by design, and `Gap` is a token rather than a
  // node, so a sweep sees at most 61 node kinds.
  assert!(
    reg.representatives.contains_key(&K::Root) && reg.representatives.contains_key(&K::Error),
    "the sweep must reach the two bookkeeping node kinds too, or their empty rows prove nothing"
  );
  assert_eq!(checked, 61);
}

/// Every getter the shared corpus alone calls but never gives anything to project.
///
/// The census behind [`FIXTURES`], and the reason each of those three exists. It is pinned rather
/// than described because the alternative is a comment: a corpus entry added later that happens to
/// write an enum default would make one fixture redundant, and nothing would say so.
///
/// Note what is *not* here. The corpus reaches all 59 wrappers and calls all 204 getters on its
/// own — every entry below is a getter with a node to run on and nothing in it to find, which is
/// a statement about what GraphQL the corpus writes rather than about what the typed layer can
/// reach.
const CORPUS_CANNOT_PROJECT: &[(&str, &str)] = &[
  ("DefaultValue", "enum_value"),
  ("DefaultValue", "float_value"),
  ("DefaultValue", "null_value"),
  ("DefaultValue", "object_value"),
  ("DefaultValue", "string_value"),
  ("DirectiveDefinition", "description"),
  ("EnumTypeDefinition", "description"),
  ("FragmentDefinition", "description"),
  ("InputObjectTypeDefinition", "description"),
  ("InputValueDefinition", "list_type"),
  ("InterfaceTypeDefinition", "description"),
  ("InterfaceTypeDefinition", "directives"),
  ("InterfaceTypeExtension", "implements_interfaces"),
  ("ListValue", "boolean_values"),
  ("ListValue", "enum_values"),
  ("ListValue", "float_values"),
  ("ListValue", "null_values"),
  ("ListValue", "object_values"),
  ("ListValue", "variables"),
  ("ObjectField", "boolean_value"),
  ("ObjectField", "enum_value"),
  ("ObjectField", "float_value"),
  ("ObjectField", "list_value"),
  ("ObjectField", "null_value"),
  ("ObjectField", "variable"),
  ("OperationDefinition", "directives"),
  ("ScalarTypeDefinition", "description"),
  ("SchemaDefinition", "description"),
  ("UnionTypeDefinition", "description"),
  ("VariableDefinition", "directives"),
  ("VariableDefinition", "list_type"),
];

#[test]
fn the_shared_corpus_alone_reaches_every_wrapper_and_calls_every_getter() {
  let reg = sweep_corpus();
  assert!(
    reg.uncast().is_empty(),
    "the corpus alone must reach every wrapper — these need a fixture, and that is a corpus \
     gap worth knowing about:{}",
    listed(reg.uncast())
  );
  assert!(
    reg.uncalled().is_empty(),
    "the corpus alone must call every getter:{}",
    listed(reg.uncalled())
  );
  assert_eq!(
    reg.unanswered(),
    CORPUS_CANNOT_PROJECT
      .iter()
      .copied()
      .collect::<BTreeSet<_>>(),
    "the corpus's projection gap moved; re-check whether FIXTURES still earns its keep"
  );
}

#[test]
fn every_fixture_is_a_document_the_suite_accepts() {
  // A fixture that quietly failed to parse would still make getters answer — recovery keeps the
  // subtrees it managed to build — so "the sweep is green" would then rest on trees nobody meant
  // to write. Each fixture is a document this suite accepts outright.
  for (name, src) in FIXTURES {
    let parse = parse_str(src);
    assert!(
      !parse.has_errors(),
      "fixture `{name}` does not parse cleanly: {:?}",
      parse
        .diagnostics()
        .iter()
        .map(|d| d.span())
        .collect::<Vec<_>>()
    );
  }
}

/// Deliberately malformed sources, swept for one property nothing well-formed can show.
///
/// A list wrapper's children are all of one kind by construction — a `FieldsDefinition` holds
/// `FieldDefinition`s and nothing else — so on every valid document `field_definitions()` and
/// "hand back every child node" are the same function. Recovery is the only thing that ever puts
/// a foreign child in such a list, and these three are where it does.
///
/// **They are three and not ten because that is what was measured.** Twelve malformed shapes were
/// probed, one per list wrapper; nine of them lose the enclosing definition altogether rather than
/// keeping a list with an `Error` inside it, which is Task 10's recorded structural finding
/// (a definition failing mid-body unwinds past its own `node_at` mark) showing up again. The
/// wrappers those nine would have covered are in [`UNDISCRIMINATED`].
const RECOVERY_FIXTURES: &[(&str, &str)] = &[
  // An `Error` child inside an `EnumValuesDefinition`, beside two real `EnumValueDefinition`s.
  ("an_enum_body_with_a_hole", "enum E { A ! B }"),
  // …inside an `ObjectValue`.
  ("an_object_value_with_a_hole", "{ f(o: {a: 1 ! b: 2}) }"),
  // …inside a `RootOperationTypeDefinitions`.
  (
    "a_schema_body_with_a_hole",
    "schema { query: Q ! mutation: M }",
  ),
];

/// The getters no tree this gate sweeps ever separates from a kind-blind rival, and why.
///
/// 24 of 204. Every other getter is **measured** to answer differently, at least once, from a
/// getter of the same arity that ignored kinds entirely — which is the property that makes the
/// sweep's fixtures worth anything: over `"{ a }"` a `cast::child` is indistinguishable from
/// "take the first child", and a suite built out of fixtures like that reports full coverage of a
/// layer it never tested.
///
/// The 24 fall into three shapes, and none of them is a weak fixture:
///
/// - **The wanted element is always the node's first one.** An `Alias` is `name :`, an `Argument`
///   is `name : value`, a `VariableDefinition` opens on its `Variable`. "The first token" and
///   "the `Name` token" are the same token in every tree the grammar builds.
/// - **The node holds exactly one token, and the getter wants it.** `IntValue`, `FloatValue`,
///   `StringValue`, `Description`, `BooleanValue`, `NullValue`, `EnumValue`, `OperationType`,
///   `NamedType`.
/// - **Every child the node can have is of the kind the getter projects.** The list wrappers, plus
///   `Directive::arguments`, `FragmentSpread::directives` and `ScalarTypeExtension::directives`,
///   whose parents admit exactly one child kind each. [`RECOVERY_FIXTURES`] closes three of these
///   with an `Error` child; the rest cannot be closed, because the shapes that would produce one
///   lose the enclosing definition instead.
///
/// This is the same species as Task 10's surviving mutation 8 — a real property of the typed layer
/// that no `SyntaxNode` this crate can build makes observable — and it is recorded rather than
/// asserted away.
///
/// **One entry is here because a defect that used to discriminate it was fixed, and that has to
/// be on the page or it reads as an unexplained weakening.** `NamedType::name` was never
/// separable on its own merits: over `type T { f: Int }`, `query Q { a(x: 1) }` and
/// `fragment F on T { b }` it is indistinguishable from "take the first token". It separated in
/// exactly one shape, `schema { query: Q }`, and only because `root_operation_type_definition`
/// opened its `NamedType` on the wrong side of the leading trivia, so that node's first token was
/// a `Space` rather than the `Name`. That is the defect gate 5 pinned in
/// `OPENS_ON_LEADING_TRIVIA`, and `named_type` now forecloses it by committing the leading trivia
/// before it opens its node — which makes `NamedType` the one-token node the second bullet
/// describes, and turns a pass this census had been buying from a bug into an admission it can
/// defend.
///
/// So this entry is a real, if small, loss of coverage, and it is meant to be visible as one.
/// **It is not dead weight to delete on sight**: the only thing that earns its removal is a tree
/// in which a `NamedType` legitimately holds something before its `Name`, which no production
/// here builds today.
const UNDISCRIMINATED: &[(&str, &str)] = &[
  ("Alias", "name"),
  ("Argument", "name"),
  ("ArgumentsDefinition", "input_value_definitions"),
  ("BooleanValue", "name"),
  ("Description", "string_token"),
  ("Directive", "arguments"),
  ("Directives", "directives"),
  ("EnumValue", "name"),
  ("FieldsDefinition", "field_definitions"),
  ("FloatValue", "float_token"),
  ("FragmentSpread", "directives"),
  ("ImplementsInterfaces", "interfaces"),
  ("InputFieldsDefinition", "input_value_definitions"),
  ("IntValue", "int_token"),
  // Here only because fixing the `root_operation_type_definition` trivia defect removed the one
  // tree that separated it from "take the first token" — see the paragraph above.
  ("NamedType", "name"),
  ("NullValue", "name"),
  ("ObjectField", "name"),
  ("OperationType", "name"),
  ("RootOperationTypeDefinition", "operation_type"),
  ("ScalarTypeExtension", "directives"),
  ("StringValue", "string_token"),
  ("UnionMemberTypes", "member_types"),
  ("VariableDefinition", "variable"),
  ("VariablesDefinition", "variable_definitions"),
];

#[test]
fn every_getter_but_these_answers_differently_from_a_kind_blind_rival() {
  let reg = sweep_everything();
  let undiscriminated = reg.undiscriminated();
  let pinned: BTreeSet<(&str, &str)> = UNDISCRIMINATED.iter().copied().collect();
  let regressed: BTreeSet<_> = undiscriminated.difference(&pinned).collect();
  let improved: BTreeSet<_> = pinned.difference(&undiscriminated).collect();
  assert!(
    regressed.is_empty(),
    "these getters stopped being separable from a kind-blind rival, which means the material \
     this gate sweeps got weaker rather than the layer getting better:{}",
    listed(regressed)
  );
  assert!(
    improved.is_empty(),
    "these getters are now separable from a kind-blind rival — good news, and the census has to \
     say so:{}",
    listed(improved)
  );
  assert_eq!(
    DECLARED.len() - undiscriminated.len(),
    180,
    "180 of the 204 getters are proved to beat a kind-blind rival"
  );
}

#[test]
fn every_recovery_fixture_really_is_malformed() {
  // The mirror of [`every_fixture_is_a_document_the_suite_accepts`]. These three exist to put an
  // `Error` node inside a list wrapper; one that quietly started parsing cleanly would stop doing
  // that, and [`UNDISCRIMINATED`] would grow three entries with no explanation attached.
  for (name, src) in RECOVERY_FIXTURES {
    assert!(
      parse_str(src).has_errors(),
      "recovery fixture `{name}` parses cleanly, so it no longer holds a recovery hole"
    );
  }
}

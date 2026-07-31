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

use smear_parser::graphql::{
  kinds::SyntaxKind as K,
  lossless::{
    SyntaxToken,
    ast::{
      Argument, CastNode, Description, Directive, DirectiveDefinition, DirectiveLocations,
      Document, EnumTypeDefinition, ExecutableDocument, Field, FieldDefinition, InlineFragment,
      InputObjectTypeDefinition, ListType, ListValue, NamedType, NonNullType, ObjectField,
      ObjectTypeDefinition, ObjectTypeExtension, ObjectValue, OperationDefinition, OperationType,
      ScalarTypeDefinition, SchemaDefinition, SchemaExtension, SelectionSet, StringValue,
      TypeSystemDocument, UnionTypeDefinition, VariableDefinition, cast, token_any, tokens,
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

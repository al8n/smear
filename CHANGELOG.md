# Changelog

All notable changes to Smear are documented in this file.

## [Unreleased]

- `smear-lexer`!: both dialects' `LexerErrors` and `StringErrors` keep their errors in a `Vec`
  in every configuration, where they held them in a one-slot `SmallVec` while the `smallvec`
  feature was on, as it was by default. A token's `Result` no longer carries room for a whole
  error when it holds none. Measured against the `SmallVec` containers, the three parse
  workloads ran in 14 % to 15.5 % less time, the six schema and validation workloads moved by
  no more than 0.81 %, and peak allocation was unchanged. Each container derives `From`,
  `Into`, `Deref`, `DerefMut`, `AsRef` and `AsMut` over its storage, so its deref target and
  its conversions are now `Vec`'s, and `StringErrors`' `IntoIterator::IntoIter` is
  `vec::IntoIter`.
- `smear`!, `smear-lexer`! and `smear-parser`!: the `smallvec` integration is removed. Each of
  the three loses its `smallvec_1` feature, the `smallvec` alias and their place in its
  `default`, and `smear-lexer` its optional `smallvec` dependency. The feature switched only the
  lexers' error containers, which are a `Vec` in every configuration now. 0.0.1 released the
  `smallvec` feature, in each crate's `default`, and no alias is left in its place: a dependent
  that names it drops it from its feature list.
- `smear-semantics` and `smear-analysis`: two new crates. `smear-semantics` holds the
  algorithms smear's two semantic layers share: the byte-spelling interner, duplicate-name
  detection and the graph-cycle kernel. `smear` does not re-export it. `smear-analysis` is
  the GraphQLx schema analyzer, reached as `smear::analysis` behind the new `analysis`
  capability feature, which implies `parser` and `graphqlx` and no tier; the analyzer itself
  needs `std` or `alloc`. Its source features `bytes_1`, `bstr_1`, `hipstr_0_8` and
  `smol-bytes_0_2`, with the aliases `bytes`, `bstr`, `hipstr` and `smol-bytes`, forward
  `smear-parser`'s and imply no tier, and `smear`'s features of the same names forward to them.
  `smear-schema`'s builder now takes those three kernels from `smear-semantics`, behind
  `build`, with no change in behaviour.
- `smear-analysis`: the analyzer's entrance, its exits and its rules. `graphqlx::analyze`
  takes a root `Package` and its dependencies, each file's text any `SourceText` (`&str`,
  `&[u8]`, or a source feature's type), within `Limits`, and answers an `Analysis` with no
  type parameter: a `Model` of the packages, the `Diagnostics` and a `SourceMap`. Packages and
  sources are numbered deterministically, whatever order they are passed in. The package
  set's file paths, package names, dependencies and total size are checked, under the new
  `smear::graphqlx` codes `DiagnosticKind` lists; a message quotes a name or a path whole when
  it is at most 128 bytes, and otherwise as its first and last 64 bytes around `…`, each cut
  moved back to the start of a character, unless that would leave nothing out, so no message
  grows with what it quotes. Every file is parsed with the GraphQLx syntactic parser,
  its syntax errors reported at that file. `Diagnostic::builder` is public, so a consumer's
  findings join smear's in one ordered list, and `SourceMap::bind` pairs an analysis with the
  texts it was made from, finding each file again by its package and its path alone: it
  answers no text for a file whose path another file of its package has, for the files of a
  dependency whose name another dependency has, or for the root's files when the root is
  given under another name. The `Model` is dense tables read through `Copy` views, each
  numbered by its own id: packages, modules, definitions and their contributions, members,
  type occurrences and interned types, values, directive applications, the schema and the
  built-ins, with `canonical_name`, `lookup` and `module_by_path`; `lookup` keeps a type's and
  a directive's names apart, the second names a module gives included. Rules are visitors: a
  `Visitor` implements the hooks it needs, each with an empty default body, from `begin`
  through entering and leaving packages, modules, definitions and members and reaching type
  occurrences, values, directive applications and the schema, to `end`; `visit` walks an
  analysis's model for one over the packages `Packages` selects, hands each hook a `Cx` that
  says where the walk is (its `Site` says where an occurrence sits) and takes what the visitor
  reports, and answers those diagnostics under the analysis's ceiling. A consumer's walk marks
  nothing and holds memory bounded by that ceiling. Visitors compose at compile time: a tuple
  of up to twelve, an `Option` of one, a `&mut V`, a `Box<V>` and `()` are each a visitor.
  `analyze` runs smear's own rules through the same walk over the supplied packages, and an
  error that a phase or a rule reports marks the contributions,
  members and definitions that hold where it points, as their `has_errors()` says, whether the
  ceiling keeps the diagnostic or not. Every analysis parses the built-in library too:
  `core`, `alloc` and `std`, one GraphQLx file each, embedded in smear. So far `analyze` fills
  in the packages and their modules: a module for every directory of a `Directory` package
  that holds a file or a directory that does, every file of a `Flat` package in its root
  module, and a module for every inline `mod`. A directory that is not a name is
  `invalid-directory-name`, a second declaration of one module is `module-defined-twice`, and
  a module nested past `Limits::max_module_depth` is `resource-limit`; the files under a refused
  directory, and a file whose path the package set refused, are parsed but place nothing in any
  module. Every type and directive definition that parses is a definition of the model, with its
  kind, name, module, location, origin and description, numbered by package, module path, source
  and position, and so is every intrinsic primitive (`bool`, `f32`, `f64`, `iN`, `uN`) a program
  writes where its package may have it: `core`'s own widths 8 to 128, or a width its
  `IntegerProvider` gives. `Int`, `Float`, `Boolean`, `String` and `ID` are second names of
  `i32`, `f64`, `bool`, `string` and `id`; a root package's own scalar of one of those names, or
  directive named as one of GraphQL's five, in its root module, replaces the built-in, which keeps
  its id. `Builtins`' accessors answer a `DefId`. Two definitions of one name in one namespace of a
  module (`smear::schema::duplicate-type-name`, `duplicate-directive-definition`), a supplied name
  that begins with `__` (`reserved-type-name`, `reserved-directive-name`), and a type named as a
  child module (`module-type-collision`) are reported. A file that holds only white space,
  commas and comments is accepted and declares nothing. `Limits::max_definitions` bounds every
  definition the model would hold, the built-in library's and the primitives included: the first
  one a supplied package would add past it is `resource-limit`, and nothing after it is
  collected. A new `Limits::max_modules`, 2^16 by default, bounds every module the model would
  hold, every package's root module included, which is always held: the first module a supplied
  package's directory or `mod` would add past it is `resource-limit`, and no module is added
  after it. A file holding an item of a kind the analyzer does not know, which only a newer
  parser can make, is `unknown-item`, once per file. Every module has a scope: its own
  declarations and child modules, then the prelude, the root modules of `core`, `alloc` and `std`
  whatever the package's tier, and last, where a module is wanted, the names of the packages it
  may reach, which are `core`, `alloc`, `std` and its declared dependencies; types and modules
  share one namespace and directives have their own. A type that a supplied package names as a
  primitive of the prelude (`bool`, `f32`, `f64`, `iN`, `uN`, `string`, `bytes`, `NonZeroIN`,
  `NonZeroUN`) shadows it in its module and is the warning `shadowed-primitive`; a path that does
  not resolve is `unresolved-path`, or `unknown-package` for a package it may not reach, and a
  name of one segment in a place GraphQL has keeps GraphQL's own code. Every `use` item is
  resolved, each module's to a fixpoint, with a module's explicit `use` bindings before its
  globs, which give the declarations and child modules of the modules they name; `use` cycles are
  legal. A new `Limits::max_globs`, 64 by default, bounds the modules the globs of one module may
  name, and so what one lookup searches: the first glob past it is `resource-limit`, once for its
  module. A name two globs give two items is `ambiguous-glob` where a path meets it; a string
  source names its directory's module, relative to the file it is written in, and is
  `file-path-use` when it names a file and `url-unsupported` when it is a URL. A `use` item that
  names one thing while its module's items resolve and another once all have is
  `use-resolution-changed`, a `use` of a name its module declares is
  `use-declaration-collision`, and two that give one name two items are `use-name-collision`. A
  name that does not resolve gets a "did you mean" in its help: the nearest name visible where
  it is written, of the namespace wanted, within `Limits::max_near_miss_comparisons` names
  looked at over the whole analysis. Lowering then writes what each definition holds beyond its
  identity. Each extension's target is resolved where the extension is written; one that names a
  definition of another kind than its keyword is `smear::schema::extension-kind-mismatch`, one
  that is an alias such as `NonZeroU32`, which spells an instantiation of `NonZero` and is no
  definition of its own, is `smear::schema::undefined-extension-target`, and these and one that
  names nothing contribute nothing. A definition holds its contributions, its own,
  named where its name is written, then each extension's, by source and position, named where
  its target path is written; its type parameters; a directive definition's locations and whether
  it is `repeatable`; and its members, the direct members of each contribution in turn as written
  and then each field's arguments, field by field, each with its description. A GraphQL built-in
  the root package replaces holds what the root package's definition writes, and the root may
  extend a built-in. `Contribution::is_extension()` says whether a contribution is a definition
  or one of its extensions. Every written type is a type occurrence, numbered in the order the
  model's walk reaches it, each before the types it nests: a type parameter's default, a bound,
  an `implements` entry, a union member, a member's type. It keeps its path as written, `Int`
  staying `Int`, and its written arguments, however many its definition has parameters, and its
  path is resolved where it is written, with the generic parameters of its definition, or of its
  extension by position, in scope; one that names nothing is reported with the code of its place
  and stays unresolved. A `where` predicate bounds one of its definition's own type parameters;
  any other left side, and any `where` clause on an extension, is the new
  `smear::graphqlx::invalid-bound`. Every directive application, on a definition, an extension, a
  member or an argument, is numbered in the walk's order too, with its directive's path as
  written and resolved where it is written; one whose directive names nothing keeps no type
  argument and no argument. The others keep their type arguments, each an occurrence, and the
  arguments their directive declares, in the directive's order, an argument written twice
  twice; one the directive does not declare is `smear::schema::undefined-directive-argument`,
  with the nearest name it does declare in its help. A written value, a default or an
  argument's, is one `Literal` that keeps its syntactic kind and its text as written, and a
  value that is a path keeps its path. The schema has a contribution for every schema item any
  package writes, its definitions first and then its extensions, each by source and position
  and named at its `schema` or its `extend`. The root package's first schema definition gives it
  its description, its roots and its applications, the root package's schema extensions give it
  their roots and their applications, and every other schema item is a contribution alone. The
  first root written for an operation is the schema's, a type occurrence, and a second one is
  `smear::schema::duplicate-root-operation-type`; with no schema definition of the root
  package's, an object type the root package's root module declares named `Query`, `Mutation` or
  `Subscription` is presumed that operation's root. Names and texts past what the model's `u32`
  offsets address are `resource-limit`. Then smear's rules walk the model, each a visitor on the
  public trait. A program defines its schema at most once: each schema definition the root
  package writes after its first is `smear::schema::duplicate-schema-definition`, GraphQL's own
  code, with a label at the first. And the schema is the root package's: each schema definition
  and each schema extension a dependency writes is the new
  `smear::graphqlx::schema-outside-root`, and no duplicate of the root package's. A rule says
  nothing about a contribution a phase already refused something in. A new `test-support`
  feature, which `smear/test-support` forwards, compiles a hidden door to the crate-private
  model builder, and to analyses of the models it builds, for `smear`'s own tests; it is not API.
- `smear`!, `smear-lexer`!, `smear-parser`! and `smear-compiler`!: the five features
  that enable an optional dependency are versioned, as tokora's are, and the
  dependency is named with its version. `bytes`, `bstr`, `hipstr`, `smol-bytes`
  and `rowan` become `bytes_1`, `bstr_1`, `hipstr_0_8`, `smol-bytes_0_2` and
  `rowan_0_17`; the unversioned names remain as aliases that float with the
  current major, each enabling its versioned feature and nothing else, so code
  gated on them is unaffected. The optional dependencies are keyed by the
  versioned name too (`bytes_1`, `bstr_1`, `hipstr_0_8`, `smol-bytes_0_2`,
  `rowan_0_17`), so a downstream crate that named smear's optional dependency by
  its old key (`smear/dep:bytes` style, or a `[patch]`) must respell it.
- `smear-lexer`: string literals in byte sources (`[u8]`, `Bytes`, `BStr`,
  `HipByt`) must be well-formed UTF-8. Ill-formed content is refused with
  `StringError::UnsupportedCharacter`, carrying each sequence's span.
  `LitStr::try_from(&[u8])` refuses likewise. `str` and `HipStr` sources are
  unchanged (#246).
- `smear-lexer`: comments in byte sources (`[u8]`, `Bytes`, `BStr`, `HipByt`)
  must be well-formed UTF-8. Both the syntactic and the lossless lexer refuse an
  ill-formed comment with one `LexerErrorData::UnknownLexeme` per ill-formed
  sequence, at that sequence's span, and consume the whole comment; the lossless
  error token covers the whole comment. `str` and `HipStr` sources are unchanged
  (#251).
- `smear-schema`: `SchemaErrorKind::UnreadableText` is no longer reachable from
  a parsed document. The kind is kept.
- Builds on tokora 0.13 and smol-bytes 0.2. Paths through the
  `smear::lexer::tokora` re-export follow the layout tokora adopted in 0.12. The
  `smol-bytes` feature of `smear`, `smear-lexer` and `smear-parser` now enables
  tokora's `smol-bytes_0_2` integration instead of `smol_bytes_0_1`, so the four
  smol-bytes source types are smol-bytes 0.2's. Supersedes #235.
- **Diagnostics follow tokora 0.13: label and help text is borrowed from the
  diagnostic.** `Diagnose::label` on every smear error type returns
  `Option<Label<'_>>`; `primary_label` and `help` are unchanged in what they
  return, and no smear diagnostic composes a label or help text yet. A caller
  that named the type `Label` writes `Label<'_>`.
- `smear-lexer`, `smear-parser` and `smear`: under `smol-bytes`, the four
  smol-bytes 0.2 types (`Bytes` and `Utf8Bytes`, each shared and compact) are
  now accepted as sources by both dialects' syntactic lexers and parsers, and as
  slice types by both dialects' lossless lexers. The two `Bytes` are byte
  sources and are held to the UTF-8 rule for string literals and comments; the
  two `Utf8Bytes` are text sources, like `str` and `HipStr`.
- `smear-compiler`: `Rule::NoSchemaIntrospection`, an opt-in policy rule that
  refuses every `__schema` and `__type` field selection an operation reaches,
  at the field's name. It is the policy graphql-js ships as
  `NoSchemaIntrospectionCustomRule`, matched by name rather than by resolved
  type. It is outside `RuleSet::ALL`, so `validate_executable` still accepts
  introspection; opt in with `RuleSet::ALL.with(Rule::NoSchemaIntrospection)`.
  `__typename` is never refused. Code
  `smear::validation::no-schema-introspection`.
- `smear-lexer`: a backslash that no character follows in an inline string
  literal is refused as `StringError::IncompleteEscape`, at the backslash's
  byte, instead of the positionless `StringError::Other("unknown")`: the input
  ends right after it, or, in a byte source, the bytes after it are not
  well-formed UTF-8.
- `smear-schema`!: the diagnostic contract is tokora's. `smear_schema::diagnostic`,
  and through it `smear::diagnostic`, re-exports `tokora::diagnostic`'s `Diagnose`,
  `DiagnoseExt`, `Code`, `Severity`, `Location`, `Label`, `PathSegment`, `Labels`
  and `PathSegments` instead of declaring copies of them, so `SchemaError`, the
  introspection door's `ResponseError` and `DiagnosticDisplay` implement
  `tokora::diagnostic::Diagnose` and painty's `tokora` adapter renders them.
  Paths, names and method signatures are unchanged. What changed for a caller:
  each item is now tokora's own rather than a structurally identical twin, so a
  type that implemented both `Diagnose` traits now implements one trait twice;
  `Labels` and `PathSegments` no longer implement `ExactSizeIterator` (count them
  with `count()`, not `len()`), report a `size_hint` lower bound of `0`, and stop
  for good at the first index whose accessor answers `None` before the count is
  reached; and `Severity` implements `From<tokora::emitter::Severity>`.
- `smear-compiler`: a draft §5 diagnostic's message no longer ends in its byte
  range. `DiagnosticDisplay`'s `Display` wrote the sentence followed by
  ` (start..end)`; it now writes the sentence alone, so a renderer that draws the
  position from the contract does not print it twice. The position is where it
  already was: `Diagnose::primary()` and the labels, and `Diagnostic::span()` and
  `related_span()`.
- `graphql-proto`: a request error result carries the driver's own request
  errors — the syntax errors, validation failures and variable coercion
  failures raised where no executor exists. `RequestError` is one entry, with
  its message, its §7.1.6 `locations` as spans and its own `extensions`, built
  with `RequestError::new` or from any `Diagnose` with
  `RequestError::from_diagnose`, which takes the diagnostic's rendering as the
  message and its primary and label spans as the `locations`.
  `RequestErrorResult::from_driver` and `RequestErrorResult::push` list them; a
  refused entry comes back in a `RefusedEntry`, naming its
  `RequestErrorCeiling`. Three new `Limits` ceilings bound them:
  `max_request_errors` (128), `max_request_error_message_bytes` (4096) and
  `max_request_error_locations` (64).
- `graphql-proto`: `RequestErrorResult::errors` yields a `RequestErrorEntry`,
  which is the executor's refusal or the driver's entry, rather than a
  `StartError`, and `RequestErrorResult::error` returns `Option<StartError>`,
  `None` for a result built from the driver's entries. Both break code that
  reads them.
- `graphql-proto`!: `Limits`'s fields are no longer public. Build one with
  `Limits::new()` or `Limits::default()` and the `with_*` methods, one per
  ceiling, and read a ceiling with the getter of the same name; a struct
  literal, `..Limits::default()` included, and a field read no longer compile.
  Adding a ceiling is therefore no longer a breaking change.
- `smear`: `json::write_request_error_result` and
  `json::write_request_error_result_with` take the request's `document: &str`,
  as `write_response` does, and write each driver entry's `locations`, resolved
  against it, and its `extensions`. The new parameter breaks every existing
  call.
- `smear-lexer`!: `SyntaxErrorKind`, one fieldless kind for every draft §2
  refusal the lexer or the parser reports, in both dialects, with a code in the
  new `smear::syntax` family (23 of them), a lowercase message, an optional help
  line and `Severity::Error`. It is re-exported at the crate root and through
  `smear::lexer`. Each dialect's `LexerError` implements `Display`,
  `core::error::Error` and tokora's `Diagnose`: the code is its kind's, the
  primary location is its own span, and the message names no position. A
  `State` refusal — a tripped nesting or token ceiling — reports
  `smear::syntax::resource-limit`. `LexerErrorData`, `LexerError` and every
  nested string, Unicode-escape and number error gain `kind()`. Each dialect's
  `LexerErrorData` gains `NotOneToken(&'static str)`: what a whole-slice door
  (`identifier`, `int_literal` and `float_literal`, and GraphQLx's `identifier`
  and its `LitInt` and `LitFloat` `TryFrom<&str>`) answers when the slice is not
  exactly one token of the kind it asked for, rendered "not exactly one name",
  "… integer" or "… float" and reported as `smear::syntax::unexpected-token`.
  Those doors answered `UnexpectedLexeme` over the slice, which is now only the
  scanner's refused character.
- `smear-lexer`!: token kinds render as a diagnostic names them: punctuation in
  backticks and every class as a bare noun. GraphQL's `SyntacticTokenKind` and
  `LosslessTokenKind` render `name` and `integer` where they rendered
  `identifier` and `int`, and `` `{` `` where they rendered `{`; GraphQLx's
  rendered their `Debug` names. `FloatHint::Exponent` and
  `HexFloatHint::Exponent` render the exponent hint they carry instead of the
  literal text `_0`.
- `smear-parser`!: a syntax error answers the diagnostic contract. Each
  dialect's `Error` implements `Display`, `core::error::Error` and `Diagnose`,
  and `Error::kind` and `ErrorData::kind` name its `SyntaxErrorKind`. The
  syntactic `From<LexerErrors>` keeps the first lexer error's kind as the new
  `ErrorData::Lexical(SyntaxErrorKind)`, at that error's span, where every
  lexical refusal used to become `ErrorData::Other("lexer error")`; an empty
  batch still does, and a `State` refusal is still `TerminalEndOfInput`. A match
  on `Other("lexer error")` stops matching.
- `smear-parser`!: the fourteen GraphQL `ErrorData` variants no parse produced
  are removed — `InvalidEnumValue`, `InvalidBooleanValue`, `InvalidNullValue`,
  `InvalidFragmentName`, `UnexpectedKeyword`, `UnexpectedEndOfVariableValue`,
  `UnknownDirectiveLocation`, `UnknownOperationType` and the six
  `UnexpectedEndOf…Extension` variants — with their constructors and the types
  only they used: `UnexpectedKeyword`, `VariableValueHint` and the six
  `…ExtensionHint` enums. GraphQLx's `ErrorData::UnexpectedEnd` and
  `Error::unexpected_end` are removed for the same reason.
- `smear-parser`!: `Expectation`, in both dialects, implements `Display` and
  gains `Production(&'static str)` and `Unspecified`. A production description
  such as "a type definition" is a `Production` where it was a `Keyword`, which
  now carries real keywords only. An absent or multi-kind expected set from
  tokora maps to `Unspecified` where it mapped to `Name`, and so does GraphQLx's
  genuine end of input where it named `InputValue`; a message then has no
  "expected …" clause.
- `smear-lexer`!: fifteen public error enums are now `#[non_exhaustive]`:
  `StringError`, `UnicodeError`, `FixedUnicodeEscapeError`,
  `BracedUnicodeEscapeError` and `StringValueError`; in `graphql`,
  `LexerErrorData`, `FloatError` and `DecimalError`; in `graphqlx`,
  `LexerErrorData`, `FloatError`, `HexFloatError`, `DecimalError`, `HexError`,
  `OctalError` and `BinaryError`. A `match` on one of them outside `smear-lexer`
  needs a wildcard arm. Adding a refusal variant is no longer a breaking change.
  Two closed sets stay exhaustive. One is `InvalidUnicodeScalarKind`: a value
  that is not a Unicode scalar is a surrogate or lies above `0x10_FFFF`, and the
  Unicode Standard fixes both ranges. The other is `LengthError`: converting a
  list to one error fails only when the list holds none or more than one.
- `smear-parser`!: GraphQLx's `Unclosed` is now `#[non_exhaustive]`. A `match`
  on it outside `smear-parser` needs a wildcard arm. Adding a delimiter kind is
  no longer a breaking change. GraphQL's `Unclosed` stays exhaustive: its three
  delimiter pairs are the specification's.
- `smear`!: `json::Error` is now `#[non_exhaustive]`. A `match` on it outside
  `smear` needs a wildcard arm. Adding a refusal variant is no longer a breaking
  change.
- `smear-lexer`: a GraphQLx radix prefix with no digit, `0x`, `0o` or `0b`,
  refuses as an incomplete number wherever it stands: `HexError::UnexpectedEnd`,
  `OctalError::UnexpectedEnd` or `BinaryError::UnexpectedEnd` at the prefix's
  span, as at the end of input. Before a delimiter, as in `0x)`, it produced an
  empty error batch, which the syntactic parser reported as
  `Other("lexer error")` at `0..0`.
- `smear-lexer`: a braced Unicode escape whose brace is closed but whose content
  is not hex, such as `\u{G}` or `\u{12G4}`, refuses as
  `BracedUnicodeEscapeError::InvalidSequence` over the whole escape in a text
  source too, where it refused as `Unclosed`. The search for the brace stops at
  the string's closing quote, at a line break and at the next escape, any of
  which leaves the escape `Unclosed`, so in a byte source it no longer runs past
  the string to a `}` later in the document. A `\u{` followed directly by a
  quote, a backslash or a line break no longer swallows it.
- `smear-schema`!: the default root operation type names `Query`, `Mutation`
  and `Subscription` apply only when the document has no `schema` definition
  (draft §3.3.1). Beside a `schema` definition, an object type of a default name
  that the definition does not list is an ordinary type: `schema { query: Query }`
  beside `type Mutation` has no mutation root, as in the draft's Example 42, where
  `Mutation` used to become one. An `extend schema` is not a definition, so a
  document with one and no `schema` definition still takes the defaults.
- `smear-schema`: an `extend schema` with no schema to extend is refused as
  `SchemaErrorKind::SchemaExtensionWithoutSchema`, at the extension (draft
  §3.3.2: "The Schema must already be defined"). Omitting the `schema`
  definition still defines a schema when there is an object type named `Query`
  (Example 41), and an extension may extend that one. The presumed default roots
  are now in place before any extension is applied, so an `extend schema` that
  provides one of them again is `DuplicateRootOperationType`, as it is against a
  written definition's root. Code `smear::schema::schema-extension-without-schema`.
- `smear-schema`: the input-object cycle rule is the draft's
  `InputObjectHasUnbreakableCycle` (§3.10), which a `@oneOf` input object can
  hold closed: it is given exactly one field, so it breaks a cycle only through
  a field that can be given a finite value. A cycle a OneOf input object holds
  closed is refused as `SchemaErrorKind::CircularOneOfInputField`, at the field
  that closes it, as in the draft's Counter Example 86;
  `input Filter @oneOf { not: Filter, eq: String }` still builds. A cycle through
  a OneOf input object that has such a field is no longer refused as
  `CircularNonNullInputField`. Code `smear::schema::circular-one-of-input-field`.
- `smear-schema`: a constant value offered to a `@oneOf` input object must hold
  exactly one entry, and not a `null` one (draft §3.10.1). A default value or a
  directive argument whose literal gives a OneOf input object no field, two
  fields or one `null` field, at any depth of lists and input objects, is refused
  as `InvalidDefaultValue` or `InvalidDirectiveArgumentValue`, at the literal or
  at the `null`.
- `smear-schema`: `@specifiedBy` on a built-in scalar is refused as
  `SchemaErrorKind::SpecifiedByOnBuiltInScalar`, at the directive, whether a
  definition that spells the scalar out applies it or an `extend scalar` does
  (draft §3.13.4: it "must not appear on built-in scalar types"). graphql-js
  accepts it and discards it. `Schema::from_introspection` refuses a response
  that gives a built-in scalar a `specifiedByURL` the same way, because the door
  writes it out as the directive. Code
  `smear::schema::specified-by-on-built-in-scalar`.
- `smear-schema`: a related position carries its own document, which
  `SchemaError::related_document` returns and the related label is rendered in.
  A duplicate across the builder's documents points at the first definition in
  the document it was read from, rather than at the same range of the
  duplicate's document, and `MissingQueryRootOperationType` is reported in the
  `schema` definition's document rather than in document 0. A related position
  inside a built-in definition is no longer reported, since no document the
  caller supplied holds it: `extend type Int { f: Int }` is refused with no label.
- `smear-lexer`!: `DIRECTIVE_DEFINITION` is a contextual keyword in both
  dialects, the twentieth `DirectiveLocation` of draft §3.13. Each dialect's
  `ContextualKeyword` gains `DirectiveDefinitionLocation`, after
  `InputFieldDefinitionLocation` (and, in GraphQLx, before `Import`), and
  `keywords::DirectiveDefinitionLocation` is its keyword token. An exhaustive
  match on either `ContextualKeyword` needs the new arm.
- `smear-parser`!: a directive definition takes directives and a directive is
  extended with `extend directive`, in both dialects (draft §3.13 and §3.13.6).
  `DirectiveDefinition` gains a `Directives` type parameter after `Arguments`:
  `DirectiveDefinition::new` takes the definition's directives after its
  arguments definition, `directives()` returns them, both dialects'
  `DirectiveDefinition` aliases bind `ConstDirectives`, and both parsers read
  `Directives[Const]?` after the arguments and before `repeatable`.
  `DirectiveExtension` is `extend directive @Name Directives[Const]`, whose
  directives are mandatory; GraphQL's alias names the directive with a `Name`
  and GraphQLx's with an `ExtensionName`, and each dialect has a
  `directive_extension` production and `DirectiveExtension::graphql` or
  `::graphqlx`. `TypeSystemExtension` is now
  `TypeSystemExtension<TypeExtension, DirectiveExtension, SchemaExtension>`
  with a `Directive` variant, and both dialects' aliases follow; an exhaustive
  match needs the new arm. `TypeSystemDirectiveLocation::DirectiveDefinition`
  is the `DIRECTIVE_DEFINITION` location. In the lossless layer each dialect's
  `SyntaxKind` gains `DirectiveExtension` (GraphQL's after `SchemaExtension`,
  GraphQLx's after `InputObjectTypeExtension`), which moves the raw value of
  every later kind by one; the typed layer adds the `DirectiveExtension`
  wrapper, `DirectiveDefinition::directives` and `directive_extensions` on both
  document roots, and both projections build the new AST.
- `smear-schema`!: `Schema::build` reads the directives on a directive
  definition and `extend directive` (draft §3.13 and §3.13.6).
  `DirectiveLocation` gains `DirectiveDefinition`, the `DIRECTIVE_DEFINITION`
  location, which `DirectiveLocation::from_name` resolves, and
  `DirectiveLocation::ALL` becomes `[DirectiveLocation; 20]`; an exhaustive
  match needs the new arm. A directive written on a directive definition, or
  provided by an `extend directive`, is checked at `DIRECTIVE_DEFINITION` like
  any other usage, and a definition and its extensions are one location, so a
  non-repeatable directive applied by both is `DuplicateDirectiveUse`. An
  `extend directive` of a directive no document defines is
  `UndefinedExtensionTarget`. A directive that reaches itself through the
  directives on its definition or its extensions, directly or through other
  directives and types, is `SelfReferentialDirective`.
- `smear-schema`!: the meta-schema and the specified `@deprecated` follow the
  draft (§3.13.3 and §4.2). `@deprecated(reason: String! = "No longer supported")`
  may also stand on a directive definition, and `@deprecated(reason: null)` is
  refused as `InvalidDirectiveArgumentValue` where it built as deprecated with no
  reason. `__DirectiveLocation` gains `DIRECTIVE_DEFINITION`. `__Directive` lists
  its fields in the draft's order — `name`, `description`, `isRepeatable`,
  `locations`, `args`, then the new `isDeprecated` and `deprecationReason` — and
  `__Schema.directives` takes `includeDeprecated`. Every `includeDeprecated` is
  `Boolean! = false`, so `includeDeprecated: null` no longer validates. A
  directive definition or an `extend directive` carrying `@deprecated` deprecates
  the directive: `DirectiveMeta::new` takes its `Deprecation` as a second
  argument, and `DirectiveMeta::deprecation` returns it.
  `Schema::from_introspection` reads a directive's `isDeprecated` and
  `deprecationReason` and writes them back as the `@deprecated` on its
  definition, and accepts an `includeDeprecated` typed `Boolean`, as the October
  2021 specification and graphql-js 16 type it, beside the draft's `Boolean!`;
  it refuses an explicit `null` for any `isDeprecated`, on a field, an input
  value, an enum value or a directive (draft §4 types all four `Boolean!`), as
  `MalformedResponse` where it read as unwritten, while an omitted
  `isDeprecated` still falls back to the reason as graphql-js reads it, and a
  deprecated element whose `deprecationReason` the response omits is written as
  a bare `@deprecated`, so the definition's default reason applies, where a
  `null` one is refused. A bare `@deprecated` now takes its reason from the
  effective definition's default (a replacement's own string or `null`), where
  it always read "No longer supported", so the door's omitted reasons resolve
  the same way.
- `graphql-proto`: introspection answers `__Directive.isDeprecated` and
  `deprecationReason`, and `__Schema.directives` leaves a deprecated directive
  out unless `includeDeprecated` is `true`. `includeDeprecated: null`, written or
  as a variable's value, is draft §6.4.1's `Kind::ArgumentNull` field error where
  it read as `false`, and the error is raised on every `__Type`, including the
  kinds that have no such list and the `LIST` and `NON_NULL` wrapper types.
- The introspection parity oracle compares the executor's answers with graphql-js
  17.1.0's instead of 16.11.0's, directive deprecation included, and its eight
  recorded differences are redrawn for that release.
- All six crates gain an `alloc` feature beside `std`, and the two are
  independent tiers: `std` does not imply `alloc`, and the allocating API exists
  when either is on. Every combination is a valid build: neither, the core tier;
  `alloc` alone; `std` alone, the default; and both. With neither, a crate
  compiles in every feature combination and exports only what needs no
  allocation: in `smear-lexer`, `hints`, `keywords` (with `keyword!`), `limits`
  and `punctuator`; in `smear-parser`, `combinator` (with
  `typed_keyword_atom!`); in `smear-schema`, `diagnostic` and `builtin`;
  `smear-compiler` re-exports `smear-schema` as `schema` and `graphql-proto`
  exports nothing. `smear` exposes the same items through `smear::lexer`,
  `smear::parser`, `smear::diagnostic` and `smear::validator::schema`. Each
  crate's `__features` gains `ALLOC`, which `smear` asserts equal to every
  member's. The workspace no longer pins tokora's `alloc`: each crate's `alloc`
  forwards `tokora/alloc`.
- All six crates!: a build with `default-features = false` no longer includes
  the allocating API: the dialect lexers and parsers, the schema
  representation and builder, the validator, the executor and `smear::json`. A
  capability feature (`graphql`, `graphqlx`, `parser`, `validator`, `proto`,
  `introspection`, …) never switches a tier on, so such a consumer names `std`
  or `alloc` itself; with neither the build compiles and lacks those items.
- `smear`!, `smear-lexer`, `smear-parser`! and `smear-compiler`!: a feature
  that enables an optional dependency implies no tier. `bytes_1`, `hipstr_0_8`
  and `smol-bytes_0_2` do not imply `alloc`, although each of those crates
  links `alloc` itself, and `rowan_0_17` no longer implies `std`. So a
  `default-features = false` consumer names `std` or `alloc` beside a source
  feature, and `std` beside `rowan_0_17`: the lossless tower is gated on `std`.
- `smear`! and `smear-compiler`!: the `introspection` feature no longer implies
  `std`. The draft §4 reader is `core` and `alloc` only, so it needs `std` or
  `alloc`, neither of which `introspection` implies.
- `graphql-proto` and `smear`: `StartError`, `SetExtensionsError`,
  `UnattachedExtensions`, `Full`, `TooLarge`, `RequestErrorCeiling`,
  `RefusedEntry`, `SourceEventError` and `json::Error` implement
  `core::error::Error` in every configuration, where they implemented
  `std::error::Error` only with `std`.
- `smear-parser`!: GraphQLx drops its collection type syntax and respells its
  collection literals. `<T>` and `<K => V>` no longer parse as types: a `<`
  only follows a path, as its type arguments, so a set or a map is an ordinary
  generic scalar (`HashSet<T>`, `HashMap<K, V>`). The literals are `set!{…}`
  and `map!{…}`: `set` and `map` open one only when the next significant token
  is `!`, a `{` must follow the `!`, and trivia may stand between the three.
  Anywhere else they are the enum value or path they always were, so
  `[set {a: 1}]` is now the enum value `set` followed by an object, as in
  GraphQL. The AST `Type` loses its `Set` and `Map` variants, and the
  `SetType` and `MapType` carriers go with them. The standalone `set_value`,
  `map_value` and their const twins report `Expectation::Bang` where the `!` is
  missing. In the lossless layer `SyntaxKind` loses `SetType` and `MapType`,
  which moves the raw value of every later kind by two; their typed wrappers go,
  and with them every `set_type`, `map_type`, `set_types` and `map_types`
  getter. `SetValue` and `MapValue` keep their kinds and hold the `!` as a
  token between the keyword and the `{`.
- `smear-parser`: under a recovering emitter, an inline fragment whose `{` is
  missing is refused at the token standing in its place, in both GraphQL and
  GraphQLx, with the expected-`{` error the fail-fast emitter already raised
  there. The brace delimiter reported the missing opener and then read members,
  so `{ ... on Q ... on Q … }` recursed once per fragment with one brace open
  for the nesting limit to count, and a long enough chain overflowed the native
  stack. Parses under the `Fatal` emitter the shipped entry points use are
  unchanged.
- `smear-lexer`! and `smear-parser`!: a GraphQLx path takes a root, and no path
  starts with `::`. Both GraphQLx lexers gain three punctuators, `~` (`Tilde`),
  `..` (`DotDot`) and `^` (`Caret`), after `PathSeparator` in the token and
  token-kind enums, which moves the raw value of every later
  `LosslessTokenKind` by three. `..` is taken by longest match after `...`, so
  `....` is `...` then a stray `.`, `.....` is `...` then `..`, and a lone `.`
  is the only unterminated spread left. A path is `PathRoot? Name (:: Name)*`:
  `~::` roots it at the own package, `^::` at a dependency its first segment
  names, and `..::`, once per level, at the parent module, with trivia allowed
  between a root's tokens. The leading `::` of a fully qualified path is gone:
  a `::` where a path begins is refused, and the syntactic refusal is the new
  `ErrorData::LeadingPathSeparator`, of the new
  `SyntaxErrorKind::LeadingPathSeparator` (`smear::syntax::leading-path-separator`),
  whose help says that a dependency path starts with `^::`. Every path position
  — a type, a value, a directive name, an `implements` entry or union member,
  an extension's target, a root operation type, a `where` predicate and its
  bounds, a type condition, a spread's target — opens on a name or a root's
  sigil and no longer on `::`, so `[^::a::B, ^::c::D]` and
  `IndexMap<string, ^::geo::Point>` hold two members each where a leading `::`
  joined them into one. A `where` clause continues on a name followed by `:`,
  `::` or `<`, or on a root's sigil followed by `::`. The AST `Path` trades
  `fully_qualified: bool` for `root: PathRoot`, an enum of `Relative`,
  `Package(Span)`, `Dependency(Span)` and `Parent { span, count }` re-exported
  from `graphqlx::ast`: `Path::new` takes the root before the segments,
  `root()` replaces `is_fully_qualified()`, `IntoComponents` yields
  `(Span, PathRoot<Span>, Container)`, and a path's span starts at its root. The
  reserved first segments — `true`, `false` and `null` of an enum value, `on`
  of a spread's target — are reserved in a relative path only. In the lossless
  layer a root's tokens are the leading children of its `Path` node, the typed
  `Path` gains `tilde_token`, `caret_token` and `dot_dot_tokens`, and
  `SyntaxKind` gains the images `Tilde`, `DotDot` and `Caret` after
  `PathSeparator`, which moves the raw value of every later kind by three.
  `smear_lexer::punctuator` gains `Tilde` and `Caret`, and `smear-parser`'s
  `combinator` gains `tilde`, `try_tilde`, `caret` and `try_caret`. The
  syntactic `path`, `try_path`, `Path::graphqlx` and `Path::try_graphqlx`
  require the dialect error conversion every other path production already
  required.
- `smear-lexer`! and `smear-parser`!: GraphQLx's `import … from` becomes `use`,
  a tree of paths, and `use` and `mod` are its keywords. `ContextualKeyword::Use`
  and `::Mod` take the places of `Import` and `From`, so the list stays at 45 and
  only those two discriminants change meaning, and
  `smear_lexer::keywords::{Import, From}` become `{Use, Mod}`. `mod` opens an
  inline module, in the entry below. A `use` item is
  `use UseTree`, and a tree is a path with an optional `as Name`, or a scope
  followed by a glob `*` or by a group `{ UseTree+ }`; the scope is a bare root
  (`~::`, `^::` or `..::`), a path and its `::`, or an inline string source with
  the names after it and a `::`. One token decides at every step, a tree ends at
  the first token that cannot continue it, `as` after a complete path is always
  the keyword, a group's trees start with a name, and no description may
  precede the item. A `use` item stands anywhere among a document's entries, in
  all three document kinds, before or after a definition. Every form `import`
  accepted, and its spelling now:

  | before | after |
  |---|---|
  | `import * from "m"` | `use "m"::*` |
  | `import * as ns from "m"` | `use "m" as ns` |
  | `import * as a::b from "m"` | `use "m" as b`: an alias is one name |
  | `import { A } from "m"` | `use "m"::{A}`, or `use "m"::A` |
  | `import { A, B } from "m"` | `use "m"::{A, B}` |
  | `import { A as B } from "m"` | `use "m"::{A as B}` |
  | `import { A as ns::B } from "m"` | `use "m"::{A as B}`: an alias is one name |
  | `import { * } from "m"` | `use "m"::*` |
  | `import { * as ns } from "m"` | `use "m" as ns` |
  | `import { A, * as ns } from "m"` | two items, `use "m"::{A}` and `use "m" as ns`: a bare `*` is no tree |

  In the AST, `UseDefinition { span, tree }` and `UseTree { span, prefix, kind }`
  replace `ImportDefinition`, `ImportClause`, `ImportList`, `ImportMember`,
  `NamedSpecifier` and `WildcardSpecifier`. `UsePrefix` is `Root(PathRoot)`,
  `Path(Path)` or `Source { source, segments }`, and `UseTreeKind` is
  `Simple { alias }`, `Glob` or `Group(Nested<UseTree>)`; both are
  `#[non_exhaustive]`, `UseTree::new` takes `Leaf` names and spans, and a tree
  is released by a loop, as a selection set is. The document entries
  `ImportOrTypeSystemDefinitionOrExtension`, `ImportOrExecutableDefinition` and
  `ImportOrDefinitionOrExtension` become `TypeSystemItem` (`#[non_exhaustive]`),
  `UseOrExecutableDefinition` and `DocumentItem`, each with a `Use` variant, and
  their dispatchers `type_system_item`, `use_or_executable_definition` and
  `document_item`; the productions are `syntactic::use_tree::{use_tree,
  use_definition}`. `Expectation` trades `Import`, `From`, `ImportClause` and
  `ImportMember` for `Use`, `UseTree`, `NameGlobOrGroup`, `Tilde`, `DotDot` and
  `Caret`, and `As` now names the alias's keyword. In the lossless layer
  `SyntaxKind`'s `ImportDefinition` (96) and `ImportList` (97) become
  `UseDefinition` and `UseTree` in place, and `NamedSpecifier` and
  `WildcardSpecifier` go, which moves the raw value of every later kind down by
  two: `DefinitionTypeParam` is 98 where it was 100, and `Root` is 112 where it
  was 114. The typed `UseDefinition` and `UseTree` replace the four import
  wrappers, every document root's `import_definitions` is `use_definitions`, and
  the lossless module is `use_tree`.
- `smear-parser`!: GraphQLx gains inline modules, `implements` on scalars,
  enums, unions and input objects, and `where` on a scalar definition.
  `Description? mod Name
  { ModItem* }` is an entry of a type-system document and of a mixed document,
  and an item of another module's body. A body holds `use` items, modules,
  type-system definitions and extensions, and may be empty; an operation or a
  fragment in one is a syntax error. An executable document has no `mod`
  entry: the syntactic parser refuses one, and the lossless parser reports one
  and still builds it, which its projection refuses. Every body's `{` is a
  counted delimiter, so the nesting limit bounds how deep modules nest. In the
  AST, `ModDefinition { span, description, name, items }` is new in
  `graphqlx::ast`, with a public `ModDefinition::new`, and a deep module is
  released by a loop, as a selection set is. `TypeSystemItem` and
  `DocumentItem` gain a `Mod` variant, and `ModDefinition::items` holds
  `TypeSystemItem`s, so a consumer whose wildcard arm stood for "nothing else"
  now receives modules there and adds a `Mod` arm to read them;
  `UseOrExecutableDefinition` is unchanged. `syntactic::definition::mod_definition`
  and `ModDefinition::graphqlx` parse one module, and `Expectation` gains `Mod`.
  In the lossless layer `SyntaxKind` gains `ModDefinition` (98) after
  `UseTree`, which moves the raw value of every later kind up by one:
  `DefinitionTypeParam` is 99 and `Root` is 113. A module's description,
  braces and items are its own children, the typed `ModDefinition` reads its
  description, its `mod` and name tokens and one iterator per item kind, and
  the typed `Document` and `TypeSystemDocument` gain `mod_definitions`.
  A scalar, enum, union or input object definition or extension takes an
  `implements` clause after its name and generics and before its directives,
  and an extension of one must still add something, an `implements` clause
  counting; an empty one's refusal names `implements` among what it expected.
  In the AST the eight `graphqlx::ast` aliases keep their names and stand for
  carriers of GraphQLx's own, each with an `implements` slot after the name and
  an `implements()` accessor, so each `new` takes the clause after the name;
  the carriers the GraphQL dialect binds do not change. The four
  extensions' payloads are GraphQLx's own too, under the names they had:
  `UnionTypeExtensionData`, `EnumTypeExtensionData` and
  `InputObjectTypeExtensionData` gain an `Implements(_)` variant, their
  `Directives` variant becomes `Directives { implements, directives }`, and
  their remaining variant gains `implements`. The scalar extension's payload is
  the new `ScalarTypeExtensionData`, `Implements(_)` or
  `Directives { implements, directives }`, so `ScalarTypeExtension::new` takes
  it in place of the directives and `directives()` answers an `Option`; a
  `match` on any of the four payloads needs the new arms. `ScalarTypeExtension`
  and `EnumTypeExtension` gain the `Ty` parameter the other aliases carry. In
  the lossless layer each of the eight nodes may hold an `ImplementInterfaces`
  child after its name, and their typed wrappers gain `implement_interfaces`.
  A scalar definition ends on an optional `where` clause after its directives,
  which forces nothing after it, as a directive definition's does; a scalar
  extension takes none. Its carrier gains a `where_clause` slot after the
  directives and a `where_clause()` accessor, so `ScalarTypeDefinition::new`
  takes the clause last, and the lossless node may hold a `WhereClause` child,
  read by the typed wrapper's `where_clause`.

## [0.0.1] - 2026-10-30

- Coordinated the first `0.0.1` release of the six publishable crates:
  `smear-lexer`, `smear-parser`, `smear-schema`, `smear-compiler`,
  `graphql-proto`, and `smear`.

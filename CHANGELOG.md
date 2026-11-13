# Changelog

All notable changes to Smear are documented in this file.

## [Unreleased]

- `smear-lexer`: `normalize_decimal_float` reads a decimal float literal's spelling, its `-`
  included, as a binary float would hold it: `DecimalFloat::Zero`, `DecimalFloat::Overflow`, or
  `DecimalFloat::Spelled`, a spelling `str::parse::<f64>` and `::<f32>` read exactly, written into
  the caller's `[u8; NORMALIZED_FLOAT_LEN]`. It needs no allocator and sits behind no feature.
  Those parsers stop reading an exponent's digits once it reaches 65 536, so a literal whose long
  run of digits offsets a written exponent past 655 359 is misread when handed to them as written:
  `0.`, 99 998 zeros and `1e999999` read as 1.0. The spelling keeps the first 800 significant
  digits and a final `1` for a later digit that is not zero, so it rounds as the literal does.
- `smear-parser`, `smear-schema` and `smear-compiler`: a `Float` literal is read through
  `normalize_decimal_float`, and no longer by `str::parse` as written. Only a literal whose digits
  offset a written exponent past 655 359 reads differently, now as its value: `0.`, 99 998 zeros
  and `1e999999`, 10^900000, which read as 1.0, is past every double, so `Schema::build` and
  `Rule::ValuesOfCorrectType` refuse it and the `materialized-numbers` conversion answers
  `OutOfRange::Float`; and `1`, 99 999 zeros and `e-999999`, which read as 1.0, converts to 0.0.
- `smear-parser`!: `lossless::runner::DiagnosticSource::collect_diagnostics_retaining` takes a
  second argument, `visit`, which is lent the span and the payload of each record the projection
  keeps at error severity, in emission order. 0.0.1 released the method with `retains` alone, and
  the trait is not sealed: an implementation outside the crate must take the argument, and a
  caller that wants nothing lent passes `|_, _| {}`.
- `smear-parser`: the parser layers, as values, behind one trait per dialect. `layer` holds what
  both dialects share: `Report`, the sink a layer lends each syntax error to, of which `()`,
  `Vec<E>` and `&mut K` are three and which `from_fn` makes of a closure; `Parsed`, a layer's
  answer, with the document, its `Recovery`, `error_count()`, `is_valid()` and the tree the layer
  kept; `Rejected`, the syntactic layer's failure; the layer values `Syntactic<D>` and, behind
  `rowan_0_17` and `std`, `Lossless<D>`; and `SyntaxTree`, which the lossless `Parse` implements
  through the new `Parse::covering_node`. `graphqlx::Layer`, over type-system documents, and
  `graphql::Layer`, over both document kinds, are sealed for each layer and source, and so is
  `SyntaxTree`: no other crate implements them. Each dialect names its two layers `Syntactic` and
  `Lossless` and gives them the trait's methods as inherent methods. The syntactic layer reads
  exactly the sources its lexer reads: `str`, `[u8]`, and behind their features `BStr`, `Bytes`,
  `HipStr`, `HipByt` and the four smol-bytes forms, and `&str` as the source itself is not one. It
  lends each error of a text it rejects. The lossless layer reads `str`, and answers `Refused`
  before any scan for a text past `Refused::MAX_SOURCE_LEN`. It lends every error its tree keeps
  at error severity but a token-budget member the grammar wrote, one `invalid-syntax` error for
  such a record that holds none, and its token-budget refusal, once and last; a recovery hole and
  a warning are not lent. `error_count()` is how many errors the parse found: a sink that never
  breaks is lent that many, less the error records the lossless layer refused past its limits'
  ceiling on kept diagnostics, which it counts one for each. For the lossless layer the count is
  zero exactly when the tree's `has_errors()` is false, and never less than the number of the
  tree's error diagnostics. `Recovery` now lives in `layer`, and `lossless::project::Recovery`,
  where 0.0.1 released it, re-exports it.
- `smear-lexer` and `smear-parser`!: a ceiling on the diagnostics a lossless parse keeps.
  `LosslessLimits::max_diagnostics()` is the most records a lossless parse keeps, errors,
  warnings and recovery holes together: the first it records, none dropped later to make room.
  It is 32 768 by default, `LosslessLimits::with_max_diagnostics(max)` sets it, `usize::MAX` is
  no ceiling, and `0` keeps nothing. Every lossless door and the lossless layer run under it:
  each door reads it off the limits it is handed and makes the parse's recording emitter with it.
  Past it the parse goes on, and its tree, its document and its recovery are what they would
  have been; a further error is counted and not kept, and a further warning or recovery hole is
  dropped. 0.0.1 released the doors keeping every record: a caller that wants them all passes
  `LosslessLimits::new().with_max_diagnostics(usize::MAX)` to a `_with_limits` door or to
  `Lossless::with_limits`. `lossless::runner::Parse::refused_errors()` answers how many errors a
  parse refused, and `Parse::has_errors()` counts them, whatever its list holds.
  `DiagnosticSource::refused_errors` is a required method: an implementation outside smear must
  add it, answering 0 when its emitter has no ceiling. The lossless state grows by a word more,
  which every token the parser caches carries: with the brace count's, it is two words larger
  than 0.0.1's. What the ceiling bounds is how many records a parse keeps, and with them their
  bytes: a lexer error's record keeps at most eighteen entries however long its token, which only
  a string literal's reaches, with its first sixteen faults, one that counts the rest, and the
  `Unterminated` that ends it where its line or the text does. It does not bound the tree or the
  parse's log of events, which grow with the text, and a caller that bounds a parse's whole
  memory bounds the text's length.
- `smear-lexer`: an inline string literal ends at its first line terminator, `\n`, `\r` or
  `\r\n`, before it. It is refused with the errors of what came before and
  `StringError::Unterminated`, and the terminator and the next line lex as they do anywhere
  else: trivia in the lossless layer, skipped in the syntactic one. A `\` at the end of the line
  is `StringError::IncompleteEscape`, and a `\u` escape takes no digit from the next line. The
  literal used to run on to the next `"` or to the end of the text, with an error for each line
  break, and the quotes after it paired up the other way. `StringError::UnexpectedLineTerminator`,
  its three constructors and `SyntaxErrorKind::LineTerminatorInString` stay, and no lexer reports
  them. `SyntaxErrorKind::UnterminatedString`'s help now says that an inline string ends with its
  line, and that text spanning lines needs a block string.
- `smear-lexer`: a string literal's record keeps at most sixteen errors, inline or block: the first
  sixteen in the order found, then, when `n` more were found, one `StringError::Omitted(n)` that
  counts them. A literal its line or the input ends still closes its record with
  `StringError::Unterminated`, which the count never takes, because it is why the token ends
  where it does. So a record holds at most eighteen entries however long its literal, where it
  held one for each fault. `StringError::Omitted` is a new variant: it renders as "n more errors
  in this string literal were found and not kept" and reports `SyntaxErrorKind::InvalidSyntax`.
- `smear-lexer`: a comment in a byte source keeps at most sixteen errors the same way: an
  `UnknownLexeme` for each of its first sixteen ill-formed sequences, then one error of the new
  `LexerErrorData::Omitted(n)`, in either dialect, counting the rest and spanning from the first
  of them to the last. It renders as "n more errors in this comment were found and not kept" and
  reports `SyntaxErrorKind::InvalidSyntax`.
- `smear-lexer`: a `\u` escape in a text source with fewer than four hex digits, before characters
  of more than one byte, as in `"\u€€€€"`, no longer panics. The escape takes up to four
  characters whole as its digits, and reports each one that is not hex at its own byte.
- `smear-parser`!: `Recovery` counts at every module depth. 0.0.1 released it counting a
  document's top-level elements, so that `projected()` was the length of the document's
  definitions. GraphQLx's recovering type-system doors, and both layers, now count every item
  with an AST image at every depth, an inline module counting one beside the items inside it and
  a `use` item one, and `skipped` every element without one, a module whose header has no image
  counting one with nothing inside it counted. So in GraphQLx a document that holds an inline
  module counts more than its top-level items. GraphQL has no modules, and its counts keep their
  meaning.
- `smear-compiler`! and `smear`!: the validator's lossless doors are removed. A document comes
  from a parser layer, and the one validator reads it:
  `Lossless::new().executable_document(text, &mut syntax)`, then `validate_executable` or
  `validate_executable_with` on `parsed.document()`;
  `Lossless::new().type_system_document(text, &mut syntax)`, then
  `Schema::build(parsed.document())`. Syntax errors leave by the layer's sink and findings by the
  validator's; `parsed.is_valid()` and `parsed.recovery()` are the layer's, whatever the
  validator answers. Removed, each released in 0.0.1:
  - `validate_executable_lossless`, `validate_executable_lossless_with` and
    `validate_executable_lossless_verified_with`. A caller that kept a `Parse` projects it with
    `project_executable_document_recovered` and validates the document;
  - `validate_schema_lossless`. A caller reads the text with the `Lossless` layer's
    `type_system_document`, or, having kept a `Parse`, projects it with
    `project_type_system_document_recovered`, and builds the schema from the document;
  - `LosslessInvalid` and `LosslessSchemaErrors`;
  - the re-exports `Recovery` and `Verified`, which are `smear_parser::graphql::Recovery` and
    `smear_parser::graphql::lossless::Verified`;
  - `Refusal::SourceMismatch`, `Refusal::TooDeep` and `Refusal::WrongRoot`: no pair reaches the
    validator, and `Refusal::Budget` is the one refusal left;
  - `smear-compiler`'s `rowan` feature: the lossless layer is `smear-parser`'s, and `smear`'s
    `rowan_0_17` no longer forwards to `smear-compiler`.

  `Budget::validation_work` prices validation alone: a lossless parse and its projection are
  bounded by the layer's `LosslessLimits`.
- `smear-parser`: lossless recovery leaves a closing delimiter to the block it closes, in both
  dialects, and while a brace is open only the production that owns a closing brace takes it. A
  type or a value missing in front of a closer is reported and the closer left, so the block
  closes on it and the definition after the block is read whole; a list opened by `(`, `[` or,
  in GraphQLx, `<` that meets a `}` closing a brace open before it reports its own closer missing
  and stops there; and the skip over junk keeps no `}` it did not open: where what it committed
  held one, taken for the closer of a `(`, `[` or `<` it had open, it keeps what stands in front
  of the first such `}`, whether a restart point, a limit or an error stopped it, and everywhere
  else it keeps what it kept before. That skip is `lossless::recover::unexpected`'s, which 0.0.1
  released: it now also watches the braces it opens, the pair whose identity in its `delimiters`
  table is `b'{'`, and a table with no such pair skips as it did. To tell a skip that finds no
  restart point from one that finds one past such a `}`, it reads past the `}` and then reads the
  prefix again, so under a produce-event budget, which no rollback refunds and whose default is
  none, that prefix is paid for twice: where the budget runs out first, the skip keeps part of
  the prefix or nothing, and the parse ends there. The trees and the diagnostics of broken
  documents change accordingly.
- `smear-parser`: in GraphQLx, a module's body recovers its own items. An item that fails costs
  itself, and the module keeps its other items and its node; the body ends at the `}` the braces
  as written give it, which no item inside it can take; junk between two items is reported once
  and skipped as one run when an item or the body's `}` follows it; a refusal that ends the
  document inside a module still leaves every module around it its node; and a `}` after a
  description or an `extend` inside a module is left to close the module. The recovering
  type-system doors descend into the modules whose header reads, keep every item there that
  projects, and cover what they skip inside a module with that module's span; when nothing is
  skipped they answer what the fail-fast door answers.
- `smear-parser`: `lossless::recover::resync_to_closing`, a public helper beside `resync_to`,
  skips wreckage that left delimiters open: it spends its first `left` closers on the wreckage and
  stops before the next restart point outside it. One call costs at most one scan and one token;
  a caller that loops on it bounds the loop with the allowance flag it passes.
- `smear-lexer`: `LosslessLimits::brace_depth()` answers how many `{` are open at the state's
  position. Both dialects' lossless lexers keep the count, over every source. It is no budget and
  nothing refuses on it; it lives in the rewindable state, so a rewind takes it back; and it joins
  `LosslessLimits`' equality and hash. The lossless state grows by one word, which every token the
  parser caches carries. `SyntacticLimits` is unchanged.
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
- `smear-semantics`: `scc::components` answers, for each node of a directed graph, the least
  node of its strongly connected component, over the same compressed rows and iterative walk as
  `scc::on_cycle`, which is unchanged.
- `smear-analysis`: the analyzer's entrance, its exits and its rules. `graphqlx::analyze`
  takes a root `Package` and its dependencies, each file's text any `SourceText` (`&str`,
  `&[u8]`, or a source feature's type), within `Limits`, and answers an `Analysis` with no
  type parameter: a `Model` of the packages, the `Diagnostics` and a `SourceMap`.
  `graphqlx::analyze_with` is the same entrance with the parser layer the caller passes, and it
  answers beside the `Analysis` a `Trees`: what that layer kept for each supplied file it read,
  by source id, found by `get` and `node` and walked by `TreesIter` and `TreesIntoIter`.
  `Analysis` and `Model` implement `PartialEq`, and `SourceMap` `PartialEq` and `Eq`: two
  analyses are equal when their models, every row and every id, their diagnostics, their source
  maps and the diagnostic ceilings they were made under are, and for a package set no layer
  reports a syntax error for, every parser layer answers an equal analysis. Packages
  and sources are numbered deterministically, whatever order they are passed in. The package
  set's file paths, package names, dependencies and total size are checked, under the new
  `smear::graphqlx` codes `DiagnosticKind` lists; a message quotes a name or a path whole when
  it is at most 128 bytes, and otherwise as its first and last 64 bytes around `…`, each cut
  moved back to the start of a character, unless that would leave nothing out, so no message
  grows with what it quotes. Every file is parsed with the GraphQLx syntactic parser, or,
  through `analyze_with`, with the parser layer the caller passes, its syntax errors reported at
  that file. A parser that keeps a bounded number of records, as the lossless layer does under
  its own limits, lends no more than it kept; the errors it found beyond them are counted, and
  the list is truncated. `Diagnostic::builder` is public, so a consumer's
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
  `analyze` runs smear's own rules through the same walk over the supplied packages,
  and an error that a phase or a rule reports marks the contributions, members and
  definitions that hold where it points, as their `has_errors()` says, whether the
  ceiling keeps the diagnostic or not; a syntax error marks nothing. Every analysis parses the
  built-in library too:
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
  commas, byte order marks and comments is accepted and declares nothing, before any parser
  layer reads it, so it has no entry in `Trees`; the lossless layer, handed such a text, answers a
  tree with one syntax error. `Limits::max_definitions` bounds every
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
  names nothing contribute nothing. An extension's parameter list names its target's type
  parameters by position, one name each, and a list of another length is the new
  `smear::graphqlx::generic-arity`, though the extension still contributes what it writes; an
  extension that writes no list names none of them, and is legal whatever their count. A
  definition holds its contributions, its own,
  named where its name is written, then each extension's, by source and position, named where
  its target path is written; its type parameters; a directive definition's locations and whether
  it is `repeatable`; and its members, the direct members of each contribution in turn as written
  and then each field's arguments, field by field, each with its description. A GraphQL
  built-in the root package replaces holds what the root package's definition
  writes, and the root package may add directives to a built-in by an extension; it declares no
  type parameter, since the built-in keeps its identity, and a list it writes is `generic-arity`.
  `Contribution::is_extension()` says whether a contribution is a definition or one of its
  extensions. `Contribution::extent()`, `Member::extent()` and `Module::extent()` answer where
  each contribution, member and inline module is written, in its own file, from its description,
  or its first token, to its last token; the own contribution of a primitive `core` materialises,
  when no definition of the root package's replaces it, answers the whole of `core`'s package
  source, which has no span; and through `analyze_with` with a layer that keeps a tree,
  `Trees::node` answers the node an extent in a file the layer read was written as. Every written
  type is a type occurrence, numbered in the order the
  model's walk reaches it, each before the types it nests: a type parameter's default, a bound,
  an `implements` entry, a union member, a member's type. It keeps its path as written, `Int`
  staying `Int`, and its written arguments, however many its definition has parameters, and its
  path is resolved where it is written, with the generic parameters of its definition, or of its
  extension by position, in scope; one that names nothing is reported with the code of its place
  and stays unresolved. A `where` predicate bounds one of its definition's own type parameters;
  any other left side, and any `where` clause on an extension, is the new
  `smear::graphqlx::invalid-bound`, and so is a right side that names no interface, which is kept
  as written. Every directive application, on a definition, an extension, a
  member or an argument, is numbered in the walk's order too, with its directive's path as
  written and resolved where it is written; one whose directive names nothing keeps no type
  argument and no argument. The others keep their type arguments, each an occurrence, and the
  arguments their directive declares, in the directive's order, an argument written twice
  twice; one the directive does not declare is `smear::schema::undefined-directive-argument`,
  with the nearest name it does declare in its help. The schema has a contribution for every
  schema item any package writes, its definitions first and then its extensions, each by source
  and position
  and named at its `schema` or its `extend`. The root package's first schema definition gives it
  its description, its roots and its applications, the root package's schema extensions give it
  their roots and their applications, and every other schema item is a contribution alone. The
  first root written for an operation is the schema's, a type occurrence, and a second one is
  `smear::schema::duplicate-root-operation-type`; with no schema definition of the root
  package's, an object type the root package's root module declares named `Query`, `Mutation` or
  `Subscription` is presumed that operation's root. Names and texts past what the model's `u32`
  offsets address are `resource-limit`. Then each definition gets its built-in meaning, read off
  what its applications resolved to and never off how they are spelled, and only off those the
  package that wrote the definition writes: an interface that applies `core`'s `@marker` is a
  marker, as `Def::is_marker()` says, and a scalar that applies `core`'s `@set` or `@map` stands for
  that collection, as `Def::collection()` says. Each definition's type parameter defaults are
  checked where they are written: they are trailing, a parameter with no default after one with a
  default being the new `smear::graphqlx::generic-default`; a default
  may name any parameter of its definition, written before it or after it; and each group of a
  definition's defaults that need each other, a default that names its own parameter included, is
  `generic-default` once, at the default of the group's lowest parameter, which a use that leaves
  that parameter out can never fill. Then every type occurrence gets its type, `TypeExpr::ty()`,
  each type interned once, so that `==` on `TyId` is type equality, and where the model's walk
  first needs it, its arguments before it: a list's, a type parameter's, of its own definition,
  and a definition's with every argument, those written and then each one left out filled from its
  default, substituted, a default that names a parameter written after it filled first; a
  collection's key or element is non-null in it however it is written, and an alias such as
  `NonZeroU32` is the `NonZero<u32>` it spells. More type arguments than a definition has type
  parameters, fewer than it takes, type arguments on a type parameter or an alias, and an argument
  left to a default that cannot fill it, are `generic-arity`, each where it is written; an
  occurrence inside a default whose filling leads back to a default being filled is
  `generic-default`; and an application's count of type arguments is checked against its
  directive's. An occurrence that cannot be typed, or holds one that cannot, has the unresolved
  type. `Model::instantiations` lists the closed types a generic definition's written occurrences
  resolve to. `Model::satisfies` answers whether a type satisfies an interface or a marker, and the
  new `Model::satisfies_ty` whether it satisfies one interface type exactly, the `!` of its
  arguments included, both read off what the model holds and nothing stored: by declaration,
  through the `implements` entries of a named type's definition under its arguments and the bounds
  of a type parameter, with `core`'s `MapKey` satisfied intrinsically by every integer primitive,
  `bool`, `string`, `id`, every enum and `NonZero` of an integer primitive. A new
  `Limits::max_substitutions`, 2^20 by default, bounds the steps of substitution
  filling defaults takes, and `Limits::max_generic_depth`, a list counting a level, and
  `Limits::max_instantiations`, every distinct instantiation counting once whatever its nullness,
  are enforced: each occurrence past one is `resource-limit` where it is written, and the built-in
  library is typed whatever the three are. Last, every written occurrence of a definition with a
  bound is judged by its complete type, a slot a default filled included: the argument for each
  bounded parameter, without its own `!`, satisfies the bound's interface type with the type's
  arguments in place of the definition's parameters, each with its `!`, or the occurrence is the
  new `smear::graphqlx::unsatisfied-bound`, for the first bound that fails, and keeps its type; an
  occurrence that names a parameter of the definition it is written in is judged through that
  parameter's bounds. A new `Limits::max_bound_steps`, 2^20 by default, bounds the steps judging
  takes, each type judged once whatever its nullness; past it, and past the depth or the
  instantiations the types it derives may reach, each occurrence is `resource-limit` and keeps its
  type. Then every value is made, in the model's walk, each by the type expected where it stands,
  with its `!`, as `Value::ty()` answers, each value nested in it a value of its own at its own
  location: `null` where the type is nullable; a list's elements each against its element, and any
  other value where a list is expected the one-element list that holds it, a list for each level;
  a boolean; an integer of any length, range-checked against its width by smear itself, in every
  radix; a float, or an integer, finite in `f32` or `f64`, `f32`'s range judged on the literal
  itself, and a decimal one's exponent read at any size; a string, cooked; for `id` a string, or an
  integer in `i32`'s range; for `bytes` a string's UTF-8 encoding or a list of integers from 0 to
  255, either one `ValueKind::Bytes`, a new kind; for `NonZero` an integer that is not zero; and for
  an enum one of its values, as `ValueKind::Enum`, a bare name looked up under the enum expected
  alone, and a qualified one whose prefix names that enum where the value is written, through any
  path or alias, a name that is none of its values having the nearest in its help. A custom scalar's
  value but `null` is its `Literal`, and so, in this version, is an input object's and a
  collection's, and a value whose type names a type parameter or can take no value. A value that
  does not fit is unresolved and reported where it is written, with GraphQL's
  `smear::schema::invalid-default-value` or `invalid-directive-argument-value`, but for a number out
  of the range of any width but the 32-bit signed one, or of `f32`, the new
  `smear::graphqlx::width-overflow`, and a zero where `NonZero` is expected, the new
  `smear::graphqlx::nonzero-zero`; a required argument given `null` is
  `smear::schema::missing-required-directive-argument`, at its name. A row made of a written path
  keeps its path. An application has its arguments in its directive's order: each one written,
  and each one left out whose definition has a default, which a copy of the default fills, each
  of its values not `written()` and located at the default, the copy silent and the default's own
  diagnostic saying once what does not fit; a required argument left out is
  `missing-required-directive-argument`, at the application's path, with a label at the argument.
  A new `Limits::max_value_steps`, 2^20 by default, bounds what no literal writes: the lists a
  value written alone stands for, each value of a copy, charged before the copy's first, and each
  required argument found left out; and the one reading more than linear, the conversion of a
  decimal integer of more than 38 significant digits, charged before it as
  `ceil(d / 9) × (d × 3322 / 32000 + 2)` steps for `d` digits, past which the integer is
  `resource-limit`; a hexadecimal, octal or binary integer costs nothing. Past it a value is
  `resource-limit` where it is written, and an application once, at its path, its written arguments
  kept and a copy made whole or not at all; the built-in library's own values cost nothing, and a
  copy made for an application it writes neither. Then smear's rules walk the model, each a visitor
  on the public trait. A program defines its schema at most once: each schema definition the root
  package writes after its first is `smear::schema::duplicate-schema-definition`, GraphQL's own
  code, with a label at the first. And the schema is the root package's: each schema definition
  and each schema extension a dependency writes is the new
  `smear::graphqlx::schema-outside-root`, and no duplicate of the root package's. And a package
  extends its own definitions alone: an extension of a type or directive another supplied
  package defines is the new `smear::graphqlx::orphan-extension`, at its target path, with a
  label at the definition's name; and so is an extension of a definition of `core`, `alloc` or
  `std`, a GraphQL built-in the root package replaces among them, unless the root package writes it
  and it adds directives and nothing else; in the root package's extension of a built-in it did not
  write, each application of `core`'s `@marker`, `@set` or `@map` is `orphan-extension` too, at its
  path, since what a built-in is is the language's to say. And an enum takes no type parameter: an
  enum that declares one is the new `smear::graphqlx::generic-enum`, at its first type parameter's
  name, and each use of it is still typed. And a marker is used as a marker: one that declares a
  field, once, at the first its own package wrote; one named anywhere but as the outermost type of
  an `implements` entry or of a bound, a type argument and a list's element included; and an
  `implements` entry of a scalar, an enum, an input object or a union that names what is no marker,
  a type parameter among them, are each the new `smear::graphqlx::marker-misuse`. And a set has one
  type parameter and a map two: the first `@set` of a scalar its own package writes that has not
  exactly one, or `@map` not exactly two, is the new `smear::graphqlx::collection-arity`, at the
  application, and a scalar that applies both is so once, at the later. And `TypeRef` stands only as
  the type of a directive definition's argument or of an input field, under lists alone: anywhere
  else, a type argument of any generic definition included, it is the new
  `smear::graphqlx::typeref-misuse`. And `NonZero`'s type argument is an integer primitive at least
  one bit wide, written without `!`: `NonZero` of a float, `string`, an object, a list, `u0`, a type
  parameter or an integer written with `!` is the new `smear::graphqlx::nonzero-argument`, where it
  is written, which is then all a map or a set keyed by it is refused with. And a definition's type
  parameters have distinct names, and so do the names an extension's own list gives them: a type
  parameter with the name of an earlier one is the new `smear::graphqlx::duplicate-type-parameter`,
  at its name, with a label at the first of that name. A rule says nothing about a contribution a
  phase already refused something in. A new `test-support` feature, which `smear/test-support`
  forwards, compiles a hidden door to the crate-private model builder, and to analyses of the models
  it builds, for `smear`'s own tests; it is not API.
- `smear`!, `smear-lexer`! and `smear-parser`!: the five features
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
  `StringError::UnsupportedCharacter`, carrying each sequence's span, for the
  first sixteen sequences, and `StringError::Omitted` counts the rest.
  `LitStr::try_from(&[u8])` refuses likewise. `str` and `HipStr` sources are
  unchanged (#246).
- `smear-lexer`: comments in byte sources (`[u8]`, `Bytes`, `BStr`, `HipByt`)
  must be well-formed UTF-8. Both the syntactic and the lossless lexer refuse an
  ill-formed comment with one `LexerErrorData::UnknownLexeme` per ill-formed
  sequence, at that sequence's span, for the first sixteen, and one
  `LexerErrorData::Omitted` counting the rest, and consume the whole comment;
  the lossless error token covers the whole comment. `str` and `HipStr` sources
  are unchanged (#251).
- `smear-schema`: `SchemaErrorKind::UnreadableText` is no longer reachable from
  a parsed document. The kind is kept.
- Builds on tokora 0.13.1 and smol-bytes 0.2. Paths through the
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
- `smear`!, `smear-lexer` and `smear-parser`!: a feature
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

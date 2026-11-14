//! The GraphQLx schema analyzer.
//!
//! Packages of GraphQLx source go in. A resolved model of what they declare comes out, with the
//! diagnostics that say what could not be resolved or was refused.
//!
//! # What goes in
//!
//! A [`Package`] is a name, a [`Layout`], a [`Tier`], its [`SourceFile`]s, the names of the
//! packages it depends on, and optionally an [`IntegerProvider`]. A file's text is any
//! [`SourceText`]: `&str`, `&[u8]`, or one of the owned or shared texts behind the source
//! features. [`Limits`] bounds what an analysis may spend.
//!
//! The analysis is Sans-I/O: the caller walks its directories, applies its include rules, reads
//! its files and reads its manifest, and supplies every package up front.
//!
//! # What comes out
//!
//! [`analyze`] answers an [`Analysis`]: the [`Model`], its [`Diagnostics`] and its [`SourceMap`].
//! None of the three has a type parameter or a lifetime, whatever text type went in.
//!
//! [`Analysis::is_ok`] is the verdict: whether no error was reported, counted whether the
//! diagnostic ceiling kept it or not. On every GraphQL document of smear's corpora, each given as
//! the one file of a `Flat` package, it is the verdict `smear-schema` gives, but for three capacity
//! ceilings of `smear-schema`'s representation that GraphQLx does not have. A differential test
//! holds that. It counts each of GraphQL's codes on both sides, and lists by document each count
//! it lets differ, every one on a document both refuse. That is what this release's rules give,
//! and no promise: GraphQLx is not bound to GraphQL's rules, and a GraphQL document whose one
//! mistake is a type the prelude declares, `string` or `i32`, is ok here and refused there. A root
//! package with no query root is not ok, as in GraphQL: a library has none, and a consumer that
//! analyzes one as the root package waives `smear::schema::missing-query-root-operation-type`, as
//! it waives any code (see "Waiving a rule" below).
//!
//! ```
//! use smear_analysis::graphqlx::{Layout, Limits, Package, SourceFile, Tier, analyze};
//!
//! let root = Package::new("app", Layout::Directory, Tier::Std)
//!   .with_files([SourceFile::new("schema.graphqlx", "type Query { ok: Int }")]);
//! let analysis = analyze(&root, &[], &Limits::new());
//! assert!(analysis.is_ok());
//!
//! // The texts stay with the caller, and are paired with the analysis again to render.
//! let model = analysis.model();
//! let file = model.package(model.root_package()).sources().into_iter().nth(1);
//! let texts = analysis.sources().bind(&root, &[]);
//! assert_eq!(file.and_then(|id| texts.text(id)), Some("type Query { ok: Int }"));
//! ```
//!
//! # The phases
//!
//! [`analyze`] runs these in order, each writing part of the model, and then smear's rules, which
//! only read it; a phase reports what could not be built as written.
//!
//! | Step | What it writes or judges | Its codes |
//! |---|---|---|
//! | The package set | The packages and their sources, numbered; each supplied package's name, file paths and dependencies, and the supplied sources' size, past which no supplied file is parsed | `smear::graphqlx`: `duplicate-file-path`, `invalid-file-path`, `invalid-package-name`, `reserved-package-name`, `duplicate-package-name`, `missing-dependency`, `resource-limit` |
//! | Parsing | Every supplied file, read by the parser layer chosen into the document the model is lowered from; a file of nothing but what the lexer skips declares nothing | `smear::syntax`: the parser's own; `smear::graphqlx`: `resource-limit`, for a file a layer answered neither a document nor an error for |
//! | Collection | Each package's module tree, from its directories and its inline `mod`s, and every item each module declares | `smear::graphqlx`: `invalid-directory-name`, `module-defined-twice`, `unknown-item`, `resource-limit` |
//! | Declaration | Every module and every definition, numbered, the built-in library's and the primitives the program writes included; a GraphQL built-in the root package's root module defines again, replaced; each module's names | `smear::schema`: `duplicate-type-name`, `duplicate-directive-definition`, `reserved-type-name`, `reserved-directive-name`, `redefined-built-in-type`; `smear::graphqlx`: `module-type-collision` |
//! | Name resolution | Every module's scope, its `use` items resolved to a fixpoint; and each path, resolved where it stands for the phases after it | `smear::schema`: `undefined-type`, `undefined-directive`, `undefined-extension-target`, `undefined-implements-interface`, `undefined-union-member`, `undefined-root-operation-type`; `smear::graphqlx`: `unknown-package`, `unresolved-path`, `ambiguous-glob`, `url-unsupported`, `file-path-use`, `use-resolution-changed`, `use-declaration-collision`, `use-name-collision`, `shadowed-primitive`, `unknown-item`, `resource-limit` |
//! | Lowering | What each definition holds beyond its identity: its contributions, its type parameters and bounds, a directive's locations, and its members; every type occurrence and application, its path resolved where it is written; every value, as written; and the schema's contributions and roots | `smear::schema`: `extension-kind-mismatch`, `undefined-extension-target`, `undefined-directive-argument`, `duplicate-root-operation-type`; `smear::graphqlx`: `generic-arity`, `invalid-bound`, `unresolved-path`, `unknown-item` |
//! | Instantiation | Each definition's built-in meaning and its defaults; every occurrence's type, each type interned once; each application's complete type arguments; and the bounds of every instantiation the program writes | `smear::graphqlx`: `generic-arity`, `generic-default`, `unsatisfied-bound`, `resource-limit` |
//! | Coercion | Every value, by the type expected where it stands, an argument or a field left out filled from its default; and a generic definition's values, at each closed instantiation of it | `smear::schema`: `invalid-default-value`, `invalid-directive-argument-value`, `missing-required-directive-argument`, `undefined-input-object-field`, `missing-required-input-object-field`, `input-object-default-value-cycle`; `smear::graphqlx`: `width-overflow`, `nonzero-zero`, `collection-literal`, `resource-limit` |
//! | Binding | Each scalar's host types, from the `@extern`s its own package writes on it | none |
//! | Implementation | What each definition says it is: its `implements` entries, transitively, with the fields and arguments each interface asks for, and a union's members; and each instantiation's entries and members, and the arguments it makes required | `smear::schema`: `duplicate-implements-interface`, `duplicate-union-member`, `missing-transitive-interface`, `missing-interface-field`, `invalid-interface-field-type`, `interface-field-not-deprecated`, `missing-interface-field-argument`, `invalid-interface-field-argument-type`, `unexpected-required-argument`; `smear::graphqlx`: `resource-limit` |
//! | Tiers | The tier every definition and every type requires | none |
//!
//! # The model
//!
//! The [`Model`] is dense tables, read through views, each a `Copy` pair of the model and an id
//! that answers borrows of the model. It owns every spelling it answers with, so a consumer that
//! reads the model alone never holds the sources.
//!
//! [`Model::packages`] and [`Model::package`] answer each package, a [`PackageInfo`]: its name,
//! kind, tier, layout, sources, dependencies, integer provider and root module;
//! [`Model::root_package`] is the one the analysis was asked about. [`Model::modules`] and
//! [`Model::module`] answer each module, a [`Module`], from its package's directories and its
//! inline `mod`s: its package, path, parent, children and kind, the definitions it holds, and an
//! inline `mod`'s location, extent and description.
//!
//! [`Model::defs`] and [`Model::def`] answer every definition, a [`Def`], the built-in library's
//! and the primitives the program writes included: its kind, name, module, location, origin and
//! description; its contributions, its own and then its extensions', each a [`Contribution`] with
//! its location, its extent and what it wrote; its type parameters with their defaults, and its
//! bounds; its `implements` entries, union members and applications; a directive definition's
//! locations and whether it is repeatable; its members, by kind; what the language makes of it, an
//! intrinsic primitive, or a marker or a collection when its own package says so; a scalar's host
//! types, each `@extern` its own package writes on it, an [`Extern`]; and its
//! [`tier`](Def::tier), the least of `core`, `alloc` and `std` that holds everything it uses. A
//! definition, a contribution and a member each answer `has_errors()`: whether a phase or a rule
//! refused something in it. GraphQL's introspection meta-types are definitions of
//! [`Origin::Compat`], which a code generator skips.
//!
//! [`Model::member`] answers each field, argument, input field and enum value, a [`Member`]: its
//! kind, name, owner, parent, location, extent and description, its type, its default, a field's
//! arguments, and its applications.
//!
//! [`Model::type_expr`] answers each written type occurrence, a [`TypeExpr`]: what it resolved to,
//! a [`TypeExprKind`], its path and its arguments as written, whether it is written non-null, its
//! location, and its type. [`Model::ty`] answers each type, a [`Ty`], interned once, so `==` on
//! [`TyId`] is type equality: a [`TyKind`], the complete instantiation with defaults filled and
//! substituted and an alias such as `NonZeroU32` expanded, with its nullness, its tier and whether
//! it is closed. [`Model::instantiations`] lists the closed instantiations of a generic definition
//! that the program's occurrences resolve to, and [`Model::satisfies`] and [`Model::satisfies_ty`]
//! answer what a type satisfies, read off what it declares.
//!
//! [`Model::value`] answers each value, a [`Value`], coerced to the type expected where it stands,
//! each nested value one of its own: a [`ValueKind`], with its location, its type, the path a
//! value written as a path writes, and whether the author wrote it or a default filled it. An input
//! object's fields are found by name, a set's elements and a map's entries are each coerced against
//! their collection's types, a `TypeRef` value is the definition its path names, and a custom
//! scalar's value but `null` is kept as written.
//!
//! [`Model::application`] answers each directive application, an [`Application`]: its directive,
//! its type arguments and its path as written, its location, and its arguments in its directive's
//! order, each written or filled from its definition's default, an [`ApplicationArgument`]. An
//! application of a generic directive has a type, its directive's instantiation,
//! [`Application::ty`], and complete type arguments, [`Application::ty_args`]: the written ones,
//! then those its directive's defaults fill.
//!
//! [`Model::schema`] answers the schema, a [`Schema`], the root package's whether or not it writes
//! one: its roots, written or presumed, its applications, its description and its contributions.
//! [`Model::builtins`] answers the built-in definitions a consumer keys on in place of a spelling,
//! [`Builtins`]: `core`'s directives, `TypeRef`, `MapKey`, `NonZero`, `string`, `bytes`, `id`, and
//! each primitive the program writes.
//!
//! [`Model::canonical_name`] names a definition by the path that reaches it from any package that
//! may reach its own, `^::package::module::path::Name`, and `@^::package::module::path::name` for a
//! directive: its identity across analyses and versions. [`Model::lookup`] finds a definition by
//! that name in a [`Namespace`], and [`Model::name`] spells a [`Sym`].
//!
//! # When the analysis is ok
//!
//! On an analysis that [`is_ok`](Analysis::is_ok), the model holds these, and a census holds them
//! over smear's corpora:
//!
//! - no [`TypeExprKind::Unresolved`], [`ValueKind::Unresolved`] or [`Resolution::Unresolved`]
//!   appears, and no occurrence or value has the unresolved type, [`TyKind::Unresolved`];
//! - the members of a definition have distinct names in each of its lists, and so do a field's
//!   arguments;
//! - every named occurrence writes a count of type arguments its definition takes, so its type has
//!   one argument for each type parameter, and every bound holds at every instantiation the
//!   program writes;
//! - every application's arguments are its directive's, in its order, each at most once, every
//!   required one present, each value coerced to its argument's type under the application's type
//!   arguments;
//! - every definition's [`tier`](Def::tier) is at most its package's, GraphQL's meta-types aside;
//! - every rule of the table under "Rules" holds, the type-system rules GraphQLx shares with
//!   GraphQL in this release among them.
//!
//! An instantiation that only a default's filling derives, which no occurrence writes, is not
//! judged for itself, neither its bounds, nor its entries and members, nor the arguments it makes
//! required, nor its definition's values under it, each default being judged where it is written;
//! the rules on the kinds a type parameter takes, on `TypeRef`, on a OneOf's fields and on
//! deprecated members judge what it derives, at the occurrence that leaves the slot out, but for a
//! default such a rule refuses as written, which it reports where it is written and at no use; and
//! a code generator that monomorphises judges the closure it computes.
//!
//! # When it is not ok
//!
//! The model is still complete: every definition that parsed is in it, a refused one included,
//! with its members, applications and values. What could not be built is one of four shapes:
//! [`TypeExprKind::Unresolved`], [`ValueKind::Unresolved`], [`Resolution::Unresolved`], and the
//! unresolved type, [`TyKind::Unresolved`], which a resolved occurrence also has when it holds one
//! that has it, or when its type could not be built. Each lies within the extent of an error a
//! phase reported, which says why: within the contribution that holds it, but for a copy of a
//! default, whose error is where the default is written, or at the application, the literal or the
//! instantiation's occurrence whose type arguments it does not fit, which may stand in another
//! definition. So a consumer that waives only codes that rules report meets none of them once no
//! other error remains.
//!
//! An error that a phase or one of smear's rules reports marks the contributions, members and
//! definitions that hold where it points, whether the ceiling keeps the diagnostic or not, and
//! their `has_errors()` says so. A syntax error marks nothing: a definition the lossless parser
//! keeps despite one is the image of its repaired text, and what is found inside it is reported. A
//! consumer's walk marks nothing.
//!
//! # Codes
//!
//! A [`Diagnostic`] answers tokora's diagnostic contract, and [`Diagnostic::builder`] is public: a
//! consumer reports its own findings with its own codes, at the analysis's sources or at source
//! ids of its own, and they render exactly as smear's do. [`Diagnostics`] keeps them in one total
//! order, under a ceiling that keeps the least of them. A message of smear's quotes a name or a
//! path from the input only by a bounded excerpt of it.
//!
//! Every diagnostic has a code, `smear::<family>::<rule>`, and the code is its key: a consumer
//! filters and waives by code, and a message, a label or a help may be reworded in any release.
//! The analysis reports three families: its own, `smear::graphqlx`, which [`DiagnosticKind`] lists
//! in [`DiagnosticKind::ALL`]; GraphQL's own, `smear::schema`, for each rule that is GraphQL's rule
//! too, under `smear-schema`'s code so that a consumer sees one vocabulary across the two
//! dialects, each a constant of [`schema`] and all of them in [`schema::ALL`]; and the parser's,
//! `smear::syntax`, for a file that does not parse. A code is a phase's or a rule's, never both.
//! Every code is an error but `smear::graphqlx::shadowed-primitive`, a warning, and
//! [`DiagnosticKind::UnboundedInstantiation`] is the one `analyze` never reports.
//!
//! A consumer of this crate alone names `tokora`'s `Code`, `Location` and `Severity` itself, so the
//! major version of `tokora` this crate asks for is part of its contract; a consumer of `smear`
//! names them through `smear::diagnostic`.
//!
//! # Waiving a rule
//!
//! There is no waiver API: a consumer waives a rule by filtering its code out of
//! [`Analysis::diagnostics`]. Its own verdict is then that no error remains but of the codes it
//! waives, and [`Analysis::is_ok`] stays smear's.
//!
//! The list keeps the [`max_diagnostics`](Limits::max_diagnostics) least diagnostics in its order,
//! and [`Diagnostics::error_count`] counts every error, kept or dropped. So a waiver is sound only
//! on a list that is not [`truncated`](Diagnostics::truncated): once the ceiling cut it, an error
//! of a code the consumer does not waive may be among those dropped, and the consumer raises the
//! ceiling and analyzes again. The analysis keeps no count by code.
//!
//! ```
//! use smear_analysis::graphqlx::{
//!   Diagnostic, Layout, Limits, Package, SourceFile, Tier, analyze, schema,
//! };
//! use tokora::diagnostic::Diagnose;
//!
//! // A library has no query root, so analyzed as the root package it is not ok.
//! let library = Package::new("geo", Layout::Flat, Tier::Std)
//!   .with_files([SourceFile::new("geo.graphqlx", "type Point { x: Float! y: Float! }")]);
//! let analysis = analyze(&library, &[], &Limits::new());
//! assert!(!analysis.is_ok());
//!
//! // Waived by its code, on a list the ceiling did not cut, nothing else remains.
//! let diagnostics = analysis.diagnostics();
//! assert!(!diagnostics.truncated());
//! let rest = diagnostics
//!   .iter()
//!   .filter(|diagnostic: &&Diagnostic| {
//!     diagnostic.code() != schema::MISSING_QUERY_ROOT_OPERATION_TYPE
//!   })
//!   .count();
//! assert_eq!(rest, 0);
//! ```
//!
//! # Limits
//!
//! [`Limits`] bounds every resource untrusted input could grow, and a bound passed is
//! `smear::graphqlx::resource-limit`, never a panic, but for the diagnostic ceiling and the
//! near-miss budget, which limit what the analysis says and not what it accepts. Each is a `u32`
//! count, and `0` admits none of what it counts, but that every package has its root module and
//! the built-in library is always declared whole, its types, bounds, values and implementations
//! costing nothing against these bounds.
//!
//! | Bound | Default | What it counts | At `0` |
//! |---|---|---|---|
//! | [`max_definitions`](Limits::max_definitions) | 2^20 | The definitions the model holds, over every package, the built-in library's included | No supplied definition |
//! | [`max_modules`](Limits::max_modules) | 2^16 | The modules the model holds, over every package | No module but the built-in packages' and the root modules |
//! | [`max_module_depth`](Limits::max_module_depth) | 64 | How deep a module sits below its package's root module | No directory module and no `mod` |
//! | [`max_path_segments`](Limits::max_path_segments) | 64 | The segments of one written path | No path |
//! | [`max_use_depth`](Limits::max_use_depth) | 32 | How deeply one `use` tree nests its braces | No braced group in a `use` item |
//! | [`max_glob_iterations`](Limits::max_glob_iterations) | 2^10 | The rounds the glob fixpoint of one module's `use` items runs | No round |
//! | [`max_globs`](Limits::max_globs) | 64 | The modules the glob `use` items of one module name | No glob |
//! | [`max_generic_depth`](Limits::max_generic_depth) | 64 | How deeply one type nests once substitution has filled it | No list, and no type with an argument, that a supplied package writes |
//! | [`max_instantiations`](Limits::max_instantiations) | 2^16 | The distinct instantiations of generic definitions the analysis interns | No instantiation a supplied package needs |
//! | [`max_substitutions`](Limits::max_substitutions) | 2^20 | The steps of substitution, filling the type arguments a use leaves out | No type argument a supplied package leaves out is filled |
//! | [`max_bound_steps`](Limits::max_bound_steps) | 2^20 | The steps judging bounds takes | Every instantiation of a definition with a bound is reported, a `HashMap` among them |
//! | [`max_value_steps`](Limits::max_value_steps) | 2^20 | The steps completing values beyond what is written takes | No default fills an argument or a field, no written value stands for a list, and no decimal integer of more than 38 significant digits is read |
//! | [`max_implementation_steps`](Limits::max_implementation_steps) | 2^20 | The steps checking implementations takes | Every definition that implements an interface or a marker with an entry or a field to look at is reported |
//! | [`max_source_bytes`](Limits::max_source_bytes) | 2^26, 64 MiB | The bytes of source the supplied packages hold together | No supplied byte of source |
//! | [`max_diagnostics`](Limits::max_diagnostics) | 16 384 | The diagnostics one analysis keeps, the least of them | None is kept, and every error is still counted |
//! | [`max_near_miss_comparisons`](Limits::max_near_miss_comparisons) | 2^20 | The name comparisons spent looking for a "did you mean" suggestion | No suggestion |
//!
//! The parser's own ceilings are not among these: they belong to the parser value, as [`Limits`]
//! says.
//!
//! # Rules
//!
//! A rule is a [`Visitor`]: it implements the hooks it needs, and [`visit`] walks the model for
//! it, reaching every entity the chosen [`Packages`] wrote in id order and handing each hook a
//! [`Cx`] that says where the walk is, an occurrence's [`Site`] included, and takes what the rule
//! reports. Rules compose at compile time: a tuple of up to twelve visitors is a visitor, and so
//! are an `Option` of one, a borrowed or boxed one, and `()`. A consumer's walk answers its own
//! [`Diagnostics`] under the analysis's ceiling, which [`Diagnostics::merge`] joins with the
//! analysis's, and holds memory bounded by that ceiling however much its visitor reports.
//!
//! [`analyze`] runs smear's own rules through the same walk over the supplied packages, once its
//! phases have written the model. Each refuses what this table says, under its codes, and says
//! nothing about a contribution a phase already refused something in.
//!
//! | Its codes | What it refuses |
//! |---|---|
//! | `smear::schema`: `duplicate-schema-definition` | Each schema definition the root package writes after its first: a program defines its schema at most once |
//! | `smear::graphqlx`: `schema-outside-root` | A schema definition or extension a dependency writes: the schema is the root package's |
//! | `smear::graphqlx`: `orphan-extension`; `smear::schema`: `introspection-type-extension` | An extension of a definition another supplied package defines, or of a definition of `core`, `alloc` or `std` unless the root package writes it and adds directives alone, none of them `core`'s `@marker`, `@set`, `@map` or `@extern` on a built-in it did not write; one that adds more to one of GraphQL's introspection types keeps GraphQL's code |
//! | `smear::graphqlx`: `generic-enum` | An enum that declares a type parameter |
//! | `smear::graphqlx`: `marker-misuse` | A marker that declares a field, or is named anywhere but as the outermost type of an `implements` entry or of a bound; and an `implements` entry of a scalar, an enum, an input object or a union that names what is no marker |
//! | `smear::graphqlx`: `collection-arity` | A scalar its own package makes a set with other than one type parameter, or a map with other than two, or both |
//! | `smear::graphqlx`: `typeref-misuse` | `TypeRef` anywhere but as the type of a directive definition's argument or of an input field, under lists alone; a field's argument whose value can hold an input field of type `TypeRef`; and an instantiation that gives such a type to a type parameter an operation's argument reaches |
//! | `smear::graphqlx`: `nonzero-argument` | `NonZero` of what is no integer primitive at least one bit wide, written without `!` |
//! | `smear::graphqlx`: `duplicate-type-parameter` | A type parameter with the name of an earlier one of its definition, or of its extension's own list |
//! | `smear::schema`: `reserved-field-name`, `reserved-argument-name`, `reserved-enum-value-name`, `reserved-input-field-name`, `reserved-directive-argument-name` | A member whose name begins with `__` |
//! | `smear::schema`: `duplicate-field-name`, `duplicate-argument-name`, `duplicate-enum-value`, `duplicate-input-field-name`, `duplicate-directive-argument-name` | A member with the name of an earlier one of its list, a definition's own members and its extensions' being one list and each field's arguments another |
//! | `smear::schema`: `empty-fields-definition`, `empty-union-members`, `empty-enum-values`, `empty-input-fields` | An object, an interface that is no marker, a union, an enum or an input object with no member, its extensions' counted |
//! | `smear::schema`: `field-type-not-output-type`, `argument-type-not-input-type`, `input-field-type-not-input-type`, `directive-argument-type-not-input-type` | A member typed by a definition of a kind its position does not take, a collection's arguments there included; and an instantiation, or an application, that gives a type parameter such a type where the parameter stands |
//! | `smear::schema`: `implements-non-interface` | An `implements` entry of an object or an interface that names what is no interface |
//! | `smear::schema`: `self-implementing-interface` | An interface on a cycle of `implements` entries |
//! | `smear::schema`: `union-member-not-object` | A union member that is no object type |
//! | `smear::schema`: `deprecated-required-argument`, `deprecated-required-input-field` | A required argument or input field that applies `@deprecated`, and an instantiation that makes such a member required |
//! | `smear::schema`: `one-of-field-not-nullable` | A field of a OneOf input object written non-null, and an instantiation that makes one non-null |
//! | `smear::schema`: `one-of-field-has-default` | A field of a OneOf input object with a default |
//! | `smear::schema`: `one-of-on-input-object-extension` | `@oneOf` in an extension of an input object |
//! | `smear::schema`: `unsupported-directive-location` | An application at a location its directive's definition does not list |
//! | `smear::schema`: `duplicate-directive-use` | A directive that is not repeatable, applied twice to one location, a definition and its extensions being one |
//! | `smear::schema`: `duplicate-directive-argument-use` | An argument an application writes twice |
//! | `smear::schema`: `self-referential-directive` | A directive that refers to itself, through the directives applied where it reaches and the types it names |
//! | `smear::schema`: `specified-by-on-built-in-scalar` | `@specifiedBy` on a scalar of the built-in library |
//! | `smear::schema`: `root-operation-type-not-object` | A root operation type that is no object type |
//! | `smear::schema`: `shared-root-operation-type` | Two roots of one type |
//! | `smear::schema`: `schema-extension-without-schema` | A schema extension the root package writes when it defines no schema |
//! | `smear::schema`: `missing-query-root-operation-type` | A schema with no query root, neither written nor presumed |
//! | `smear::graphqlx`: `extern-misuse` | An `@extern` whose `lang` an earlier one of its scalar has, and one the root package writes on a GraphQL built-in it replaces |
//! | `smear::graphqlx`: `repr-misuse` | An `@repr` on the schema with no `for`, one elsewhere that writes `for`, and one that chooses twice for a language at a position, or for a type and a language on the schema |
//! | `smear::graphqlx`: `tier` | Below `std`, a use above the tier its package declares: a list in a `core` package, another package's definition that needs more, `@heap` in a `core` package, an extern's `tier` above its package's. What a directive definition writes and an application's type arguments are compile-time metadata and count nowhere, an input object a directive's argument names being judged where it is written |
//! | `smear::schema`: `circular-non-null-input-field`, `circular-one-of-input-field` | An input object that requires a value of itself, through fields each non-null and no list, or by way of a OneOf none of whose fields can be given a finite value, a generic input object requiring what its type arguments must hold |
//!
//! # A rule a backend composes
//!
//! [`UnboundedInstantiation`] refuses a generic definition that reaches itself with a type argument
//! that grows, which a backend that makes one copy per instantiation could never finish.
//! [`analyze`] never runs it, so [`Analysis::is_ok`] means what it meant. A backend composes it into
//! its own [`visit`], and its verdict is the conjunction of the two.
//!
//! # Sources and rendering
//!
//! Every package has a package source, numbered before its files, and every file is a source. A
//! [`SourceId`] numbers them densely, and the [`SourceMap`] records each one's package, path and
//! length, never its text: the texts stay with the caller, and [`SourceMap::bind`] pairs the map
//! with them again for a renderer, finding each file by its package and its path. The built-in
//! packages `core`, `alloc` and `std` have [`PackageId`]s and sources like any other, and smear
//! supplies their texts itself. A renderer is handed each source's path and text through
//! [`SourceMap::bind`]'s [`Bound`], in id order, the consumer's own sources after them.
//!
//! ```
//! use smear_analysis::graphqlx::{Layout, Limits, Package, SourceFile, SourceId, Tier, analyze};
//! use tokora::diagnostic::Diagnose;
//!
//! let text = "type Query { a: Missing }";
//! let root = Package::new("app", Layout::Flat, Tier::Std)
//!   .with_files([SourceFile::new("schema.graphqlx", text)]);
//! let analysis = analyze(&root, &[], &Limits::new());
//!
//! // A renderer is handed each diagnostic's source by its path and its text.
//! let texts = analysis.sources().bind(&root, &[]);
//! let mut shown = 0;
//! for diagnostic in analysis.diagnostics() {
//!   let source = SourceId::new(diagnostic.primary().source());
//!   assert_eq!(texts.path(source), Some("schema.graphqlx"));
//!   assert_eq!(texts.text(source), Some(text));
//!   shown += 1;
//! }
//! assert_eq!(shown, 1);
//! ```
//!
//! # Layers and trees
//!
//! [`analyze_with`] answers the same analysis with the parser the caller chose, a GraphQLx parser
//! layer, and the [`Trees`] that layer kept beside it, one for each file it read. Each
//! contribution, member and inline module answers its extent, where it is written in its file
//! ([`Contribution::extent`], [`Member::extent`], [`Module::extent`]), and [`Trees::node`] answers
//! the node of a kept tree that an extent was written as.
//!
//! # Determinism
//!
//! [`analyze`] is a function of its inputs alone: it reads no file, no clock and no global state,
//! and what it answers is `Send + Sync`. Every id is a dense `u32` in a fixed order: packages
//! `core`, `alloc` and `std`, then the root, then the dependencies by name; sources by package,
//! each package's package source first, then its files by path; modules by package and path;
//! definitions by package, module path, source and position; members by their owner, its direct
//! members first and then each field's arguments; type occurrences, applications and values in the
//! order the walk of [`visit`] reaches them under [`Packages::All`]; and types as they are
//! interned, a type's arguments before it. A [`Sym`] orders nothing. Permuting the dependencies,
//! or a package's files, changes no id and no diagnostic. Ids are stable within one analysis, and
//! not across versions of smear: across versions a definition is known by its canonical name.
//!
//! # What is not here
//!
//! The model does not hold: `use` bindings and an index of every path and what it resolved to,
//! which a language server will add; derived types such as filters and orderings; wire ids or
//! stable numbers; GraphQLx's own introspection; a type that keeps the source texts beside the
//! analysis; a canonical coordinate for a member, since canonical names are definitions'; and the
//! closure of instantiations under substitution: [`Model::instantiations`] lists the ones the
//! program writes, a code generator that monomorphises computes the rest, and
//! [`UnboundedInstantiation`] says whether that closure is finite.
//!
//! # Stability
//!
//! This crate makes no semver promise yet: smear has no downstream user, so a release may change
//! any path, signature or answer of it, and which programs it accepts, and the CHANGELOG says each
//! change. What a reader can lean on in the release it reads: on an analysis that is ok the model
//! holds the invariants above, which a census holds over smear's corpora; and a diagnostic's code
//! is its key, which no rewording of its message changes.

mod analysis;
mod bind;
mod builtin;
mod check;
mod code;
mod coerce;
mod collect;
mod declare;
mod diagnostic;
mod id;
mod implement;
mod instantiate;
mod limits;
mod lower;
mod model;
mod package;
mod parse;
mod positions;
mod raw;
mod reach;
mod resolve;
mod rules;
mod set;
mod source;
mod text;
mod tiers;
mod tree;
mod types;
mod visitor;
mod widths;

// The model's builder, and analyses of the models it builds, for `smear`'s integration tests.
// Hidden, and compiled only with the feature: the builder is crate-private API.
#[cfg(feature = "test-support")]
#[doc(hidden)]
pub mod test_support;

pub use analysis::{Analysis, Trees, TreesIntoIter, TreesIter, analyze, analyze_with};
pub use code::{DiagnosticKind, schema};
pub use diagnostic::{Diagnostic, DiagnosticBuilder, Diagnostics};
pub use id::{
  ApplicationId, DefId, IdIter, IdRange, MemberId, ModuleId, PackageId, SourceId, Sym, TyId,
  TypeExprId, ValueId,
};
pub use limits::Limits;
pub use model::{
  Application, ApplicationArgument, Builtins, CanonicalName, Collection, Contribution,
  Contributions, Def, DefKind, Description, DirectiveLocation, Extern, Externs, FloatValue,
  IntValue, Literal, LiteralKind, Member, MemberKind, Model, Module, ModuleKind, Namespace,
  ObjectEntry, Origin, PackageInfo, PackageKind, Path, PathRoot, Primitive, Resolution, Root,
  RootOperation, Schema, Ty, TyKind, TypeBound, TypeExpr, TypeExprKind, TypeParam, TypeParams,
  Value, ValueKind,
};
pub use package::{IntegerProvider, Layout, Package, SourceFile, Tier};
pub use rules::UnboundedInstantiation;
pub use source::{Bound, Source, SourceMap};
pub use text::SourceText;
pub use visitor::{Cx, Packages, Site, Visitor, visit};

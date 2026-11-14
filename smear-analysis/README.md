# `smear-analysis`

[![Crates.io](https://img.shields.io/crates/v/smear-analysis.svg)](https://crates.io/crates/smear-analysis)
[![Documentation](https://docs.rs/smear-analysis/badge.svg)](https://docs.rs/smear-analysis)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The GraphQLx schema analyzer: GraphQLx packages in, a resolved model and its diagnostics out.

GraphQLx only. Standard GraphQL is validated against the draft specification by `smear-compiler`;
the dialect's modules, imports, generics and packages are what this crate resolves.

`graphqlx::analyze` takes a root package and its dependencies, numbers their sources, checks the
package set, parses every file and runs smear's own rules over the supplied packages. It answers an
`Analysis`: the model, the diagnostics in one total order, and the source map that pairs the
analysis with the caller's texts again. The model is dense tables read through `Copy` views over
packages, modules, definitions, members, type occurrences and types, values, directive applications,
the schema and the built-ins, with canonical names and lookup; so far `analyze` fills in the
packages, their modules, one for every directory of a `Directory` package and one for every inline
`mod`, and every definition: each type and directive a package declares, and each intrinsic
primitive the program writes, with its identity, its contributions, its own and then each
extension's, its type parameters with their defaults and its bounds, its `implements` entries and
union members, its directive applications, a directive definition's locations, whether it is a
marker or a collection, read off `core`'s `@marker`, `@set` and `@map` where the package that wrote
it applies them, a scalar's host types, each `@extern` its own package writes on it, with its
effective tier, the closed instantiations its written occurrences resolve to, and its members with
their descriptions, types, defaults and applications; every written type an occurrence with its path
and arguments as written, what it resolves to, and its type: the complete instantiation, with each
argument left out filled from its default and an alias such as `NonZeroU32` expanded, each type
interned once, so that `==` on two type ids is type equality; every application with its type
arguments and its arguments; every value, coerced to the type expected where it stands, each value
nested in it one of its own, an input object's fields found by name and kept in its order, a set's
elements and a map's entries, a `TypeRef` value the definition its path names, and a custom
scalar's but `null` kept as written; and the schema, with its contributions, one for each schema
item, its description, its roots, written or presumed, and its applications.
`Model::satisfies` answers whether a type satisfies an interface or a marker, and
`Model::satisfies_ty` whether it satisfies one interface type exactly, its arguments' `!` included:
by declaration, read off the `implements` entries and the bounds the model holds, with `core`'s
`MapKey` satisfied intrinsically by every integer primitive, `bool`, `string`, `id`, every enum and
`NonZero` of an integer primitive. Each definition's type parameter defaults are checked where they
are written, and each use's count of type arguments where it is written. Filling defaults is bounded
by `Limits::max_substitutions`, and the types it derives by `Limits::max_generic_depth` and
`Limits::max_instantiations`. Every instantiation the program writes has its bounds judged, exactly:
a type argument that does not satisfy a bound's interface type, with the instantiation's arguments
in its parameters' place, is `smear::graphqlx::unsatisfied-bound` where the instantiation is
written, and judging is bounded by `Limits::max_bound_steps`. A value that does not fit the type
expected of it is refused where it is written, with GraphQL's `smear::schema::invalid-default-value`
or `invalid-directive-argument-value`, an integer read at any length, a decimal one past 38
significant digits within `Limits::max_value_steps`, and range-checked against its width,
`smear::graphqlx::width-overflow` past any width but the 32-bit signed one or past `f32`, and
`smear::graphqlx::nonzero-zero` for a zero where `NonZero` is expected. An object literal's field
the input object does not declare is `smear::schema::undefined-input-object-field`, a required field
given `null` or left out is `smear::schema::missing-required-input-object-field`, and an input
object its own package makes OneOf takes exactly one field, not `null`. A set takes a `set!{…}` and
a map a `map!{…}`, and nothing else does but a custom scalar: any other literal where a collection
is expected, and a collection literal where a value of another type is, is
`smear::graphqlx::collection-literal`, and so is an element of a set, or a key of a map, written
twice, told by its coerced value; where the type takes no value, or still names a type parameter, a
collection literal is kept as written, and nothing is said. An
application's argument left out takes a copy of its default, and so does an object literal's field,
but a OneOf input object's; a required argument left out is
`smear::schema::missing-required-directive-argument`. A
copy is counted before it is made, and a default whose copy would hold a copy of itself is not
copied, the cycle being `smear::schema::input-object-default-value-cycle` where the count meets
itself, at one field of it, under type arguments too; the copies, the reads of a default's count, by another's and by each later copy of it, the
required arguments and fields left out, the lists a value written alone stands for where a list is
expected, the conversion of a decimal integer of more than 38 significant digits, each field a
count reads of an object literal against an input object it had not read it against, and the
identities of a set's elements or a map's keys a count under type arguments renders to search them
for one held twice, a step for each 64 bytes, once for each literal and the type they are read
against, are bounded by `Limits::max_value_steps`. A directive application has its complete type
arguments, filled from
its directive's defaults as an occurrence's are, and its directive's bounds are judged at it; its
arguments are read under them, a generic input object's fields under its type's arguments, and a
copy of a default made there is judged as a written value is, the first that does not fit
reported once, at the application or at the literal whose type the arguments made. A value of a
generic type definition whose type names its type parameters stays a literal, and is checked at
each closed instantiation of the definition the program writes, reported once at each occurrence
of one under which it does not fit; so is an argument an application on it leaves out whose type
is its bare type parameter, reported where an instantiation makes it required. The values of the
applications on a generic directive and on its arguments are checked likewise at each closed
application of the directive. Each definition is checked against what it says it is, reading
of it what its own package writes: no two of its `implements` entries, nor of a union's members,
are one type, as written or under the type arguments of an instantiation the program writes; it
implements every interface the interfaces it implements implement, compared as types under its
entries' type arguments, on every kind that may write `implements`, so a type that omits one is
refused; and an object or an interface declares each field of the interfaces it implements, with a
type that is a subtype of the interface's under those arguments, covariant through lists, non-null,
declared interfaces, union members and type arguments, a type parameter a subtype of itself and of
its bounds alone, deprecated only where the interface's is, and taking each of its arguments with
exactly its type and requiring no other. The refusals are `smear-schema`'s nine codes,
`smear::schema::duplicate-implements-interface`, `duplicate-union-member`,
`missing-transitive-interface`, `missing-interface-field`, `invalid-interface-field-type`,
`interface-field-not-deprecated`, `missing-interface-field-argument`,
`invalid-interface-field-argument-type` and `unexpected-required-argument`, and the steps the check
takes are bounded by `Limits::max_implementation_steps`, a budget of its own, which a valid GraphQL
document can meet. Every definition and every type has the tier it requires, the
least of `core`, `alloc` and `std` that holds what it uses: a built-in its package's tag, a scalar
its externs' greatest tier, an object, an interface, an input object or a union the greatest of its
positions' and members' types', a position `@heap` marks `alloc` at least, and a type its
definition's and its arguments', a list `alloc` at least. It
resolves every module's `use` items too, globs and string sources included, against scopes that
end with the prelude and the packages each package may reach. The built-in packages, `core`, `alloc`
and `std`, are one embedded GraphQLx file each, parsed on every analysis. `graphqlx::analyze_with`
is the same analysis with the parser layer the caller passes, such as `smear-parser`'s lossless
layer, which recovers, so the well-formed rest of a broken file is analyzed, and it answers `Trees`
beside the `Analysis`: what that layer kept for each file it read. Each contribution, member and
inline module answers its extent, where it is written in its file, and `Trees::node` answers the
node of a kept tree that an extent was written as.

Rules are visitors. A `graphqlx::Visitor` implements the hooks it needs, one for each entity a walk
enters, leaves or reaches, and `graphqlx::visit` walks an analysis's model for it over the packages
`Packages` selects, handing each hook a `Cx` that says where the walk is and takes what the visitor
reports. Visitors compose at compile time: a tuple of up to twelve, an `Option` of one, a `&mut` or
a `Box` of one, and `()`. `analyze` runs smear's own rules through the same walk: a program defines
its schema at most once, each later schema definition of the root package being
`smear::schema::duplicate-schema-definition`; only the root package defines or extends it, a
dependency's schema item being `smear::graphqlx::schema-outside-root`; and a package extends its own
definitions alone, an extension of another package's being `smear::graphqlx::orphan-extension`, but
for the directives the root package adds to a definition of `core`, `alloc` or `std`, none of which
may be `@marker`, `@set` or `@map` unless the root package wrote that definition, nor `@extern`; an
enum takes no
type parameter, an enum that declares one being `smear::graphqlx::generic-enum`; and a marker
declares no field, is named only as the outermost type of an `implements` entry or of a bound, and
is all a scalar, an enum, an input object or a union implements, each other use being
`smear::graphqlx::marker-misuse`; and a set has one type parameter and a map two, a scalar its own
package makes a collection with another count, or makes both, being
`smear::graphqlx::collection-arity`; and `TypeRef` stands only as the type of a directive
definition's argument or of an input field, under lists alone, anywhere else being
`smear::graphqlx::typeref-misuse`, and so is a field's argument whose value can hold an input field
of type `TypeRef`, through lists, collections and the input objects of every package, and each
instantiation that gives such a type to a type parameter an operation's argument reaches; and
`NonZero`'s type argument is an integer primitive at least
one bit wide, written without `!`, `NonZero` of anything else being
`smear::graphqlx::nonzero-argument`; and a definition's type parameters have distinct names, and so
do the names an extension's own list gives them, a type parameter with the name of an earlier one
being `smear::graphqlx::duplicate-type-parameter`; and `@extern` binds a scalar its own package
defines, once for each language, an extern whose `lang` an earlier one of its scalar has, and an
`@extern` the root package writes on a GraphQL built-in its own definition replaces, being
`smear::graphqlx::extern-misuse`; and `@repr` writes `for` on the schema alone, where it names a
type, and chooses once for each language at a position and for each type and language on the schema,
anything else being `smear::graphqlx::repr-misuse`; and a package uses nothing above the tier it
declares, a list in a `core` package, another package's definition that needs more, `@heap` in a
`core` package and an extern's `tier` above its package's each being `smear::graphqlx::tier`, where
it is written, but for compile-time metadata, which counts nowhere: a directive definition's types
and an application's type arguments are not judged for their tiers, though an input object a
directive's argument names is, where it is written. And what a definition a supplied package wrote holds keeps GraphQL's
own rules, with GraphQL's codes: a member whose name begins with `__` is
`smear::schema::reserved-field-name`, or the code of what else it is, and a member whose name an
earlier member of its list has is `smear::schema::duplicate-field-name`, or the code of its list,
and a definition that holds members and holds none, its extensions' counted, is
`smear::schema::empty-fields-definition`, or the code of its kind, a marker holding none, and a
member typed by a definition of a kind its position does not take is
`smear::schema::field-type-not-output-type`, or the code of its position, and so is one typed by a
collection that holds one, a set or a map being of the kinds of its arguments, and so is an
instantiation, or a directive application, that gives a type parameter a type of a kind where the
parameter stands does not take, a parameter standing where its definition and each definition it is
passed to put it, what a default fills judged at the occurrence that leaves the slot out, and an
`implements` entry of an object or an interface that names what is no interface is
`smear::schema::implements-non-interface`, and an interface on a cycle of `implements` entries is
`smear::schema::self-implementing-interface`, and a union member that is no object type is
`smear::schema::union-member-not-object`, and a required argument or input field that applies
`core`'s `@deprecated` is `smear::schema::deprecated-required-argument`, or the code of an input
field, and a field written non-null of an input object its own package makes OneOf is
`smear::schema::one-of-field-not-nullable`, and so is an instantiation that gives a non-null type
to a type parameter that is such a field's type, and one with a default
`smear::schema::one-of-field-has-default`, and `@oneOf` in an extension of an input object is
`smear::schema::one-of-on-input-object-extension`, and an input object that requires a value of
itself, through fields each non-null and no list, is `smear::schema::circular-non-null-input-field`,
and one that does by way of a OneOf none of whose fields can be given a finite value
`smear::schema::circular-one-of-input-field`, at the field that closes the cycle, a generic input
object requiring what its type arguments must hold. And every application a supplied package
writes keeps GraphQL's own rules on directives, with GraphQL's codes: one at a location its
directive's definition does not list is `smear::schema::unsupported-directive-location`, and one
of a directive that is not repeatable, applied again where it is applied, a definition and its
extensions being one location, is `smear::schema::duplicate-directive-use`, and an argument an
application writes again is `smear::schema::duplicate-directive-argument-use`, and a directive
that refers to itself, through the directives applied where it reaches and the types it names, is
`smear::schema::self-referential-directive`, and `core`'s `@specifiedBy` on a scalar of the
built-in library, not only on GraphQL's five, is `smear::schema::specified-by-on-built-in-scalar`.
And the schema keeps GraphQL's own rules on its roots, with GraphQL's codes: a root operation type
that is no object type is `smear::schema::root-operation-type-not-object`, and two roots of one
type are `smear::schema::shared-root-operation-type`, and a schema extension the root package
writes when it defines no schema, by a definition or a presumed query root, is
`smear::schema::schema-extension-without-schema`, and a schema with no query root, neither written
nor presumed, is `smear::schema::missing-query-root-operation-type`. So a root package with no
query root is not ok, as in GraphQL: a library has none, and a consumer that analyzes one as the
root package filters that code, as it filters any code it waives.
An error that a phase or a rule reports marks the contributions, members and definitions that hold
it, as their `has_errors()` says, whether the diagnostic ceiling keeps it or not. A syntax error
marks nothing. A consumer's walk marks nothing, answers its own diagnostics under the same
ceiling, and holds memory bounded by it.

On every GraphQL document of smear's corpora the analyzer is held to `smear-schema`'s verdict, code
for code, by a differential test that lists each divergence it allows and why.

One rule is a backend's to compose, and `analyze` never runs it: `graphqlx::UnboundedInstantiation`
refuses a generic definition that reaches itself with a type argument that grows, as
`smear::graphqlx::unbounded-instantiation`, for a backend that makes one copy per instantiation and
so needs their set to be finite. The `smear::schema` codes the analyzer shares with GraphQL's own
validator are public constants of `graphqlx::schema`, which `graphqlx::schema::ALL` lists.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The analyzer, the `graphqlx` module, without the standard library. `std` gives the same API; with neither, the crate is `no_std` and exports nothing. Implied by no feature | |
| `std` | Standard library support, and with it the analyzer; it does not turn `alloc` on. Off, the crate is `no_std` | ✓ |
| `bytes_1` | Read `bytes::Bytes` sources, from `bytes` 1. Implies no tier | |
| `bstr_1` | Read `bstr::BStr` sources, from `bstr` 1. Implies no tier | |
| `hipstr_0_8` | Read the `hipstr::{HipStr, HipByt}` sources, from `hipstr` 0.8. Implies no tier | |
| `smol-bytes_0_2` | Read the smol-bytes 0.2 sources. Implies no tier | |
| `test-support` | `graphqlx::test_support`: a hidden door to the crate-private model builder, and to analyses of the models it builds, for `smear`'s own tests. Not API; `smear`'s `test-support` forwards it | |

Each source feature forwards `smear-parser`'s of the same name and implies no tier, so a
`default-features = false` consumer names `std` or `alloc` beside it. `bytes`, `bstr`, `hipstr` and
`smol-bytes` are aliases for the current version: each enables its versioned feature and nothing
else.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-semantics` — the algorithms the semantic layers share
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear-analysis` — the GraphQLx schema analyzer (this crate)
- `smear` — the umbrella that re-exports the layers, each behind a feature

A consumer of the umbrella reaches this crate as `smear::analysis`, behind its `analysis` feature.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.

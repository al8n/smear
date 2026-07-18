//! Typed keyword atoms for the GraphQLx dialect.
//!
//! Each keyword the grammar names gets a committed/declining atom pair through
//! [`typed_keyword_atom!`](crate::typed_keyword_atom), which maps the matched
//! keyword onto its typed node. Every GraphQLx keyword is soft/contextual — the
//! dialect has no strict keywords, so `ident` still accepts these spellings — and
//! these atoms only match a keyword *where a production asks for it*: the committed
//! form errors on anything else, the `try_` form declines without consuming.
//!
//! The set is the GraphQL keyword vocabulary plus the GraphQLx-only
//! `import`/`from`/`as`/`where`/`set`/`map`. The typed nodes ([`On`], [`Query`],
//! [`Import`], …) are the lexer's keyword vocabulary (`smear_lexer::keywords`),
//! re-exported here so the productions and their AST node types can name them; the
//! atoms feed each its matched span through the node's `new` constructor.

pub use smear_lexer::keywords::{
  As, Directive, Enum, Extend, Fragment, From, Implements, Import, Input, Interface, Map, Mutation,
  On, Query, Repeatable, Scalar, Schema, Set, Subscription, Type, Union, Where,
};

crate::typed_keyword_atom!(
  query / try_query => "query" => Query,
  mutation / try_mutation => "mutation" => Mutation,
  subscription / try_subscription => "subscription" => Subscription,
  fragment / try_fragment => "fragment" => Fragment,
  on / try_on => "on" => On,
  schema / try_schema => "schema" => Schema,
  extend / try_extend => "extend" => Extend,
  scalar / try_scalar => "scalar" => Scalar,
  r#type / try_type => "type" => Type,
  interface / try_interface => "interface" => Interface,
  union / try_union => "union" => Union,
  r#enum / try_enum => "enum" => Enum,
  input / try_input => "input" => Input,
  directive / try_directive => "directive" => Directive,
  implements / try_implements => "implements" => Implements,
  repeatable / try_repeatable => "repeatable" => Repeatable,
  import / try_import => "import" => Import,
  from / try_from => "from" => From,
  r#as / try_as => "as" => As,
  r#where / try_where => "where" => Where,
  set / try_set => "set" => Set,
  map / try_map => "map" => Map,
);

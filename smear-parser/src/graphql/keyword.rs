//! Typed keyword atoms for the GraphQL dialect.
//!
//! Each keyword the grammar names gets a committed/declining atom pair through
//! [`typed_keyword_atom!`](crate::typed_keyword_atom), which maps the matched
//! keyword onto its typed node. Every GraphQL keyword is soft/contextual — the
//! dialect has no strict keywords, so `ident` still accepts these spellings — and
//! these atoms only match a keyword *where a production asks for it*: the committed
//! form errors on anything else, the `try_` form declines without consuming.
//!
//! The typed nodes ([`On`], [`Query`], …) are the lexer's keyword vocabulary
//! (`smear_lexer::keywords`), re-exported here so the productions and their AST
//! node types can name them; the atoms feed each its matched span through the
//! node's `new` constructor.

pub use smear_lexer::keywords::{
  Directive, Enum, Extend, Fragment, Implements, Input, Interface, Mutation, On, Query, Repeatable,
  Scalar, Schema, Subscription, Type, Union,
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
);

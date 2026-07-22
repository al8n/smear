//! Value-node carriers shared by the dialect ASTs.
//!
//! Copied type-only from the frozen `smear-parser` crate (`src/value/`): the
//! boolean / enum / int / float / string / null / variable value nodes the
//! dialect input-value enums are built from. Each is generic over the source slice
//! `S` where applicable and over `Span`, which defaults to
//! [`SimpleSpan`](tokora::SimpleSpan). Literal carriers also have a `Lang` marker
//! for dialect type safety; variables instead inherit their dialect from the name
//! node they contain. The carriers expose only span and source accessors — the
//! assemblies construct them, and the productions and tests read them.

// The `pub(crate)` `new` constructors are the substrate the Wave 1+ value
// productions build these nodes with; until those productions land they have no
// in-crate caller. Kept crate-private (per the frozen crate) so only the parser
// mints nodes — external users read them out of parse results.
#![allow(dead_code)]

pub use boolean_value::*;
pub use enum_value::*;
pub use float::*;
pub use int::*;
pub use null_value::*;
pub use string::*;
pub use variable::*;

mod boolean_value;
mod enum_value;
mod float;
mod int;
mod null_value;
mod string;
mod variable;

#[cfg(test)]
mod tests {
  use smear_lexer::LitStr;
  use tokora::{
    span::{AsSpan, IntoSpan},
    utils::IntoComponents,
  };

  use super::{
    BlockStringValue, BooleanValue, EnumValue, FloatValue, InlineStringValue, IntValue, NullValue,
    StringValue, VariableValue,
  };

  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  struct CustomSpan(u8);

  trait CustomLang {}

  #[test]
  fn carriers_support_custom_spans_and_language_markers() {
    let boolean = BooleanValue::<CustomSpan, dyn CustomLang>::new(CustomSpan(1), true);
    assert_eq!(boolean.as_span(), &CustomSpan(1));
    assert_eq!(
      BooleanValue::<CustomSpan, dyn CustomLang>::new(CustomSpan(1), true).into_span(),
      CustomSpan(1)
    );
    assert_eq!(boolean.into_components(), (CustomSpan(1), true));

    let enum_value = EnumValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(2), "ACTIVE");
    assert_eq!(enum_value.as_span(), &CustomSpan(2));
    assert_eq!(
      EnumValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(2), "ACTIVE").into_span(),
      CustomSpan(2)
    );
    assert_eq!(enum_value.into_components(), (CustomSpan(2), "ACTIVE"));

    let float = FloatValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(3), "1.5");
    assert_eq!(float.as_span(), &CustomSpan(3));
    assert_eq!(
      FloatValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(3), "1.5").into_span(),
      CustomSpan(3)
    );
    assert_eq!(float.into_components(), (CustomSpan(3), "1.5"));

    let int = IntValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(4), "1");
    assert_eq!(int.as_span(), &CustomSpan(4));
    assert_eq!(
      IntValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(4), "1").into_span(),
      CustomSpan(4)
    );
    assert_eq!(int.into_components(), (CustomSpan(4), "1"));

    let null = NullValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(5), "null");
    assert_eq!(null.as_span(), &CustomSpan(5));
    assert_eq!(
      NullValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(5), "null").into_span(),
      CustomSpan(5)
    );
    assert_eq!(null.into_components(), (CustomSpan(5), "null"));

    let inline_lit = match LitStr::try_from("\"inline\"").unwrap() {
      LitStr::Inline(lit) => lit,
      LitStr::Block(_) => panic!("inline spelling produced a block literal"),
    };
    let string =
      StringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(6), LitStr::Inline(inline_lit));
    assert_eq!(string.as_span(), &CustomSpan(6));
    assert_eq!(
      StringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(6), LitStr::Inline(inline_lit),)
        .into_span(),
      CustomSpan(6)
    );
    let (span, lit) = string.into_components();
    assert_eq!(span, CustomSpan(6));
    assert_eq!(lit.source_ref(), &"\"inline\"");

    let inline = InlineStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(7), inline_lit);
    assert_eq!(inline.as_span(), &CustomSpan(7));
    assert_eq!(
      InlineStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(7), inline_lit)
        .into_span(),
      CustomSpan(7)
    );
    let (span, lit) = inline.into_components();
    assert_eq!(span, CustomSpan(7));
    assert_eq!(lit.source_ref(), &"\"inline\"");

    let block_lit = match LitStr::try_from("\"\"\"block\"\"\"").unwrap() {
      LitStr::Block(lit) => lit,
      LitStr::Inline(_) => panic!("block spelling produced an inline literal"),
    };
    let block = BlockStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(8), block_lit);
    assert_eq!(block.as_span(), &CustomSpan(8));
    assert_eq!(
      BlockStringValue::<_, CustomSpan, dyn CustomLang>::new(CustomSpan(8), block_lit).into_span(),
      CustomSpan(8)
    );
    let (span, lit) = block.into_components();
    assert_eq!(span, CustomSpan(8));
    assert_eq!(lit.source_ref(), &"\"\"\"block\"\"\"");

    let variable = VariableValue::<_, CustomSpan>::new(CustomSpan(9), "value");
    assert_eq!(variable.as_span(), &CustomSpan(9));
    assert_eq!(
      VariableValue::<_, CustomSpan>::new(CustomSpan(9), "value").into_span(),
      CustomSpan(9)
    );
    assert_eq!(variable.into_components(), (CustomSpan(9), "value"));

    #[cfg(feature = "graphql")]
    {
      let branded: crate::graphql::ast::BooleanValue<CustomSpan> =
        BooleanValue::<CustomSpan, crate::graphql::GraphQL>::new(CustomSpan(10), false);
      let _: crate::value::BooleanValue<CustomSpan, crate::graphql::GraphQL> = branded;
    }
  }
}

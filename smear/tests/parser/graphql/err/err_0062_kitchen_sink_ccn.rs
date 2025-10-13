use smear::parser::graphql::ast::{Document, ParseStr};


const ALL: &str = include_str!("../../../fixtures/parser/err/0062_kitchen_sink_ccn.graphql");


#[test]
fn kitchen_sink_ccn() {
  let _err = Document::<&str>::parse_str(ALL).into_result().unwrap_err();
}

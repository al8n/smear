
const ALL: &str = include_str!("../../fixtures/parser/err/0062_kitchen_sink_ccn.graphql");


#[test]
fn kitchen_sink_ccn() {
  // let err = Document::<WithSource<&str, SimpleSpan>>::parse_str_padded::<
  //   extra::Err<Simple<'_, char>>,
  // >(ALL)
  // .unwrap();

  let parser = apollo_parser::Parser::new(ALL);
  let tree = parser.parse();
  let err = tree.errors().collect::<Vec<_>>();
  println!("{:#?}", err);
}
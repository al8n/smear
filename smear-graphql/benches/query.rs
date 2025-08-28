use chumsky::{error::Simple, extra, span::SimpleSpan};
use criterion::*;
use smear_parser::parse::ParseStr;

fn apollo_parser_parse_query(query: &str) {
  use apollo_parser::cst;

  let parser = apollo_parser::Parser::new(query);
  let tree = parser.parse();

  if tree.errors().len() != 0 {
    panic!("error parsing query: {:?}", tree.errors());
  }
  let document = tree.document();

  // Simulate a basic selection set traversal operation.
  for definition in document.definitions() {
    if let cst::Definition::OperationDefinition(operation) = definition {
      let selection_set = operation
        .selection_set()
        .expect("the node SelectionSet is not optional in the spec; qed");
      for selection in selection_set.selections() {
        if let cst::Selection::Field(field) = selection {
          std::hint::black_box(field.selection_set());
        }
      }
    }
  }
}

fn smear_parser_parse_query(query: &str) {
  use smear_graphql::cst;

  let operation =
    cst::ExecutableDefinition::<SimpleSpan>::parse_str_padded::<extra::Err<Simple<char>>>(query)
      .unwrap();
  let selection_set = operation
    .unwrap_operation_ref()
    .try_unwrap_named_ref()
    .expect("the node SelectionSet is not optional in the spec; qed")
    .selection_set();
  for selection in selection_set.selections() {
    if let cst::Selection::Field(field) = selection {
      std::hint::black_box(field.selection_set());
    }
  }
}

fn bench_apollo_query_parser(c: &mut Criterion) {
  let query = "query ExampleQuery($topProductsFirst: Int) {\n  me { \n    id\n  }\n  topProducts(first:  $topProductsFirst) {\n    name\n    price\n    inStock\n weight\n test test test test test test test test test test test test }\n}";

  c.bench_function("apollo-parser/query_parser", move |b| {
    b.iter(|| apollo_parser_parse_query(query))
  });
}

fn bench_apollo_parser_many_aliases(c: &mut Criterion) {
  let query = include_str!("testdata/alias.graphql");

  c.bench_function("apollo-parser/many_aliases", move |b| {
    b.iter(|| apollo_parser_parse_query(query))
  });
}

fn bench_smear_query_parser(c: &mut Criterion) {
  let query = "query ExampleQuery($topProductsFirst: Int) {\n  me { \n    id\n  }\n  topProducts(first:  $topProductsFirst) {\n    name\n    price\n    inStock\n weight\n test test test test test test test test test test test test }\n}";

  c.bench_function("smear-graphql/query_parser", move |b| {
    b.iter(|| smear_parser_parse_query(query))
  });
}

fn bench_smear_parser_many_aliases(c: &mut Criterion) {
  let query = include_str!("testdata/alias.graphql");

  c.bench_function("smear-graphql/many_aliases", move |b| {
    b.iter(|| smear_parser_parse_query(query))
  });
}

criterion_group!(
  benches,
  bench_apollo_parser_many_aliases,
  bench_apollo_query_parser,
  bench_smear_parser_many_aliases,
  bench_smear_query_parser
);
criterion_main!(benches);

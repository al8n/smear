extra! {
  span: Span;
}

concrete! {
  amp<S> = Amp<S>;
  bang<S> = Bang<S>;
  spread<S> = Spread<S>;
  pipe<S> = Pipe<S>;
  eq<S> = Equal<S>;

  scalar<S> = keyword::Scalar<S>;
  union<S> = keyword::Union<S>;
}

tokens! {
  amp;
  bang;
  spread;
  pipe;
  eq;
  colon;
}

keywords! {
  scalar;
  union;
}

VariableDefinition = Variable @amp Type DefaultValue? [Directive];

MemberType<@pipe?, Name>;

UnionMemberTypes<@eq, [MemberType] = std::vec::Vec<MemberType>>;

#[cfg(feature = "std")]
ScalarTypeDefinition<
  #[phantom]
  S,
  @scalar,
  Name => crate::name::Name<S>,
  [Directive] = std::vec::Vec<Directive>
>;

#[cfg(not(any(feature = "std", feature = "alloc"))]
ScalarTypeDefinition = @scalar Name [Directive*];

Described = Description? Node;

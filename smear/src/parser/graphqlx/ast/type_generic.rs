use core::marker::PhantomData;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenerics<S> {
  name: S,
  generics: Vec<Self>,
}


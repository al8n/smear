use crate::scaffold;

use super::*;

pub type TypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<Type<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, Type<S>, PathSegmentContainer, TypeContainer>,
>;
pub type RcTypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<RcType<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, RcType<S>, PathSegmentContainer, TypeContainer>,
>;
pub type ArcTypeCondition<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<ArcType<S>>,
> = scaffold::TypeCondition<
  scaffold::generic::TypePath<Ident<S>, ArcType<S>, PathSegmentContainer, TypeContainer>,
>;

pub type FragmentTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<Type<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, Type<S>, PathSegmentContainer, TypeContainer>;
pub type FragmentRcTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<RcType<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, RcType<S>, PathSegmentContainer, TypeContainer>;
pub type FragmentArcTypePath<
  S,
  PathSegmentContainer = DefaultVec<Ident<S>>,
  TypeContainer = DefaultVec<ArcType<S>>,
> = scaffold::generic::FragmentTypePath<Ident<S>, ArcType<S>, PathSegmentContainer, TypeContainer>;

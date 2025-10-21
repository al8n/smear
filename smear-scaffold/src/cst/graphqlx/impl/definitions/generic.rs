use core::fmt::Debug;

use crate::cst::{Path, generic::{
  Constrained, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam, DefinitionTypePath,
  ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, ExecutableDefinitionTypeParam,
  ExtensionName, ExtensionTypeGenerics, FragmentTypePath, TypeGenerics, TypePath,
  WhereClause, WherePredicate,
}, graphqlx::GraphQLxLanguage};

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypeParam<Ident, Type, GraphQLxLanguage> =>
    TypeParameter(DefinitionTypeParam::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> =>
    DefinitionTypeGenerics(DefinitionTypeGenerics::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Type> TypeGenerics<Type, GraphQLxLanguage> => TypeGenerics(TypeGenerics::new)
  where Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionTypeGenerics<Ident, Type, GraphQLxLanguage> =>
    ExtensionTypeGenerics(ExtensionTypeGenerics::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionName<Ident, Type, GraphQLxLanguage> => ExtensionName(ExtensionName::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionName<Ident, Type, GraphQLxLanguage> => DefinitionName(DefinitionName::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type>
    ExecutableDefinitionName<Ident, Type, GraphQLxLanguage> =>
      ExecutableDefinitionName(ExecutableDefinitionName::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident> ExecutableDefinitionTypeParam<Ident, GraphQLxLanguage> =>
    TypeParameter(ExecutableDefinitionTypeParam::new)
  where Ident: Debug
}

impl_graphqlx_node! {
  for<Ident, Type>
    ExecutableDefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> =>
      ExecutableDefinitionTypeGenerics(ExecutableDefinitionTypeGenerics::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident> Path<Ident, GraphQLxLanguage> => Path(Path::new)
  where Ident: Debug
}

impl_graphqlx_node! {
  for<PathT, Type> TypePath<PathT, Type, GraphQLxLanguage> => TypePath(TypePath::new)
  where
    PathT: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypePath<Ident, Type, GraphQLxLanguage> =>
    DefinitionTypePath(DefinitionTypePath::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> FragmentTypePath<Ident, Type, GraphQLxLanguage> =>
    FragmentTypePath(FragmentTypePath::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> WherePredicate<Ident, Type, GraphQLxLanguage> =>
    WherePredicate(WherePredicate::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type> WhereClause<Ident, Type, GraphQLxLanguage> =>
    WhereClause(WhereClause::new)
  where
    Ident: Debug,
    Type: Debug
}

impl_graphqlx_node! {
  for<Ident, Type, Target> Constrained<Ident, Type, Target, GraphQLxLanguage> =>
    TypeConstraint(Constrained::new)
  where
    Ident: Debug,
    Type: Debug,
    Target: Debug
}

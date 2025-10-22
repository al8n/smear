use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U1, U2, U3},
};
use rowan::SyntaxNode;

use crate::cst::{
  Path,
  generic::{
    Constrained, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam, DefinitionTypePath,
    ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, ExecutableDefinitionTypeParam,
    ExtensionName, ExtensionTypeGenerics, FragmentTypePath, TypeGenerics, TypePath, WhereClause,
    WherePredicate,
  },
  graphqlx::{GraphQLxLanguage, SyntaxKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DefinitionTypeParamSyntax {
  #[display("identifier")]
  Ident,
  #[display("default type")]
  DefaultType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum TypeGenericsSyntax {
  #[display("'<' ")]
  LAngle,
  #[display("parameters")]
  Parameters,
  #[display("'>' ")]
  RAngle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ExtensionNameSyntax {
  #[display("name")]
  Name,
  #[display("generics")]
  Generics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DefinitionNameSyntax {
  #[display("name")]
  Name,
  #[display("generics")]
  Generics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ExecutableDefinitionNameSyntax {
  #[display("name")]
  Name,
  #[display("generics")]
  Generics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum TypePathSyntax {
  #[display("path")]
  Path,
  #[display("generics")]
  Generics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum DefinitionTypePathSyntax {
  #[display("name")]
  Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum FragmentTypePathSyntax {
  #[display("path")]
  Path,
  #[display("generics")]
  Generics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum WherePredicateSyntax {
  #[display("bounded type")]
  BoundedType,
  #[display("bounds")]
  Bounds,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum WhereClauseSyntax {
  #[display("'where'")]
  WhereKeyword,
  #[display("predicates")]
  Predicates,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ConstrainedSyntax {
  #[display("where clause")]
  WhereClause,
  #[display("target")]
  Target,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypeParam<Ident, Type, GraphQLxLanguage> {
    type Component = DefinitionTypeParamSyntax;
    type COMPONENTS = U2;
  } => TypeParameter(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Ident>(&syntax).is_some() {
      Ok(DefinitionTypeParam::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DefinitionTypeParamSyntax::Ident);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => DefinitionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let langle_missing = token(&syntax, &SyntaxKind::LAngle).is_none();
    let has_params = children::<DefinitionTypeParam<Ident, Type, GraphQLxLanguage>>(&syntax)
      .next()
      .is_some();
    let rangle_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

    if langle_missing || !has_params || rangle_missing {
      let missing = [
        langle_missing.then_some(TypeGenericsSyntax::LAngle),
        (!has_params).then_some(TypeGenericsSyntax::Parameters),
        rangle_missing.then_some(TypeGenericsSyntax::RAngle),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(DefinitionTypeGenerics::new(syntax))
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Type> TypeGenerics<Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => TypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let langle_missing = token(&syntax, &SyntaxKind::LAngle).is_none();
    let has_params = children::<Type>(&syntax).next().is_some();
    let rangle_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

    if langle_missing || !has_params || rangle_missing {
      let missing = [
        langle_missing.then_some(TypeGenericsSyntax::LAngle),
        (!has_params).then_some(TypeGenericsSyntax::Parameters),
        rangle_missing.then_some(TypeGenericsSyntax::RAngle),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(TypeGenerics::new(syntax))
    }
  })
  where
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => ExtensionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let langle_missing = token(&syntax, &SyntaxKind::LAngle).is_none();
    let has_params = children::<DefinitionTypeParam<Ident, Type, GraphQLxLanguage>>(&syntax)
      .next()
      .is_some();
    let rangle_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

    if langle_missing || !has_params || rangle_missing {
      let missing = [
        langle_missing.then_some(TypeGenericsSyntax::LAngle),
        (!has_params).then_some(TypeGenericsSyntax::Parameters),
        rangle_missing.then_some(TypeGenericsSyntax::RAngle),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(ExtensionTypeGenerics::new(syntax))
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionName<Ident, Type, GraphQLxLanguage> {
    type Component = ExtensionNameSyntax;
    type COMPONENTS = U2;
  } => ExtensionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Ident>(&syntax).is_some() {
      Ok(ExtensionName::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(ExtensionNameSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionName<Ident, Type, GraphQLxLanguage> {
    type Component = DefinitionNameSyntax;
    type COMPONENTS = U2;
  } => DefinitionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Ident>(&syntax).is_some() {
      Ok(DefinitionName::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DefinitionNameSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExecutableDefinitionName<Ident, Type, GraphQLxLanguage> {
    type Component = ExecutableDefinitionNameSyntax;
    type COMPONENTS = U2;
  } => ExecutableDefinitionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Ident>(&syntax).is_some() {
      Ok(ExecutableDefinitionName::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(ExecutableDefinitionNameSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident> ExecutableDefinitionTypeParam<Ident, GraphQLxLanguage> {
    type Component = DefinitionTypeParamSyntax;
    type COMPONENTS = U2;
  } => TypeParameter(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Ident>(&syntax).is_some() {
      Ok(ExecutableDefinitionTypeParam::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DefinitionTypeParamSyntax::Ident);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExecutableDefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => ExecutableDefinitionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let langle_missing = token(&syntax, &SyntaxKind::LAngle).is_none();
    let has_params = children::<ExecutableDefinitionTypeParam<Ident, GraphQLxLanguage>>(&syntax)
      .next()
      .is_some();
    let rangle_missing = token(&syntax, &SyntaxKind::RAngle).is_none();

    if langle_missing || !has_params || rangle_missing {
      let missing = [
        langle_missing.then_some(TypeGenericsSyntax::LAngle),
        (!has_params).then_some(TypeGenericsSyntax::Parameters),
        rangle_missing.then_some(TypeGenericsSyntax::RAngle),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(ExecutableDefinitionTypeGenerics::new(syntax))
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident> Path<Ident, GraphQLxLanguage> {
    type Component = DefinitionTypePathSyntax;
    type COMPONENTS = U1;
  } => Path(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if children::<Ident>(&syntax).next().is_some() {
      Ok(Path::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DefinitionTypePathSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<PathT, Type> TypePath<PathT, Type, GraphQLxLanguage> {
    type Component = TypePathSyntax;
    type COMPONENTS = U2;
  } => TypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<PathT>(&syntax).is_some() {
      Ok(TypePath::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(TypePathSyntax::Path);
      Err(missing.into())
    }
  })
  where
    PathT: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypePath<Ident, Type, GraphQLxLanguage> {
    type Component = DefinitionTypePathSyntax;
    type COMPONENTS = U1;
  } => DefinitionTypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<DefinitionName<Ident, Type, GraphQLxLanguage>>(&syntax).is_some() {
      Ok(DefinitionTypePath::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(DefinitionTypePathSyntax::Name);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> FragmentTypePath<Ident, Type, GraphQLxLanguage> {
    type Component = FragmentTypePathSyntax;
    type COMPONENTS = U2;
  } => FragmentTypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Path<Ident, GraphQLxLanguage>>(&syntax).is_some() {
      Ok(FragmentTypePath::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(FragmentTypePathSyntax::Path);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> WherePredicate<Ident, Type, GraphQLxLanguage> {
    type Component = WherePredicateSyntax;
    type COMPONENTS = U2;
  } => WherePredicate(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let bounded_missing = children::<TypePath<Ident, Type, GraphQLxLanguage>>(&syntax)
      .next()
      .is_none();
    let bounds_missing = children::<TypePath<Ident, Type, GraphQLxLanguage>>(&syntax)
      .skip(1)
      .next()
      .is_none();

    if bounded_missing || bounds_missing {
      let missing = [
        bounded_missing.then_some(WherePredicateSyntax::BoundedType),
        bounds_missing.then_some(WherePredicateSyntax::Bounds),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(WherePredicate::new(syntax))
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> WhereClause<Ident, Type, GraphQLxLanguage> {
    type Component = WhereClauseSyntax;
    type COMPONENTS = U3;
  } => WhereClause(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let where_missing = token(&syntax, &SyntaxKind::where_KW).is_none();
    let predicates_missing = children::<WherePredicate<Ident, Type, GraphQLxLanguage>>(&syntax)
      .next()
      .is_none();

    if where_missing || predicates_missing {
      let missing = [
        where_missing.then_some(WhereClauseSyntax::WhereKeyword),
        predicates_missing.then_some(WhereClauseSyntax::Predicates),
      ];
      let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
      Err(missing.into())
    } else {
      Ok(WhereClause::new(syntax))
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type, Target> Constrained<Ident, Type, Target, GraphQLxLanguage> {
    type Component = ConstrainedSyntax;
    type COMPONENTS = U2;
  } => TypeConstraint(|syntax: SyntaxNode<GraphQLxLanguage>| {
    if child::<Target>(&syntax).is_some() {
      Ok(Constrained::new(syntax))
    } else {
      let missing = IncompleteSyntax::new(ConstrainedSyntax::Target);
      Err(missing.into())
    }
  })
  where
    Ident: Debug + CstNode<Language = GraphQLxLanguage>,
    Type: Debug + CstNode<Language = GraphQLxLanguage>,
    Target: Debug + CstNode<Language = GraphQLxLanguage>,
}

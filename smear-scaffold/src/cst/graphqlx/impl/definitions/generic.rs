use core::fmt::Debug;

use logosky::cst::{
  CstNode,
  cast::{child, children, token},
  error::IncompleteSyntax,
  typenum::{U1, U2, U3},
};
use rowan::SyntaxNode;
use smear_lexer::{
  keywords::Where,
  punctuator::{Ampersand, Colon, Equal, LAngle, PathSeparator, RAngle},
};

use crate::cst::{
  Path, PathSegment, generic::{
    Constrained, DefinitionName, DefinitionTypeGenerics, DefinitionTypeParam, DefinitionTypePath,
    ExecutableDefinitionName, ExecutableDefinitionTypeGenerics, ExecutableDefinitionTypeParam,
    ExtensionName, ExtensionTypeGenerics, FragmentTypePath, TypeGenerics, TypePath, WhereClause,
    WherePredicate,
  }, graphqlx::{GraphQLxLanguage, SyntaxKind}
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum PathSegmentSyntax {
  #[display("identifier")]
  Ident,
  #[display("separator")]
  Separator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::Display)]
pub enum PathSyntax {
  #[display("segments")]
  Segments,
}

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
    let ident = child::<Ident>(&syntax);
    let equal = token(&syntax, &SyntaxKind::Equal)
      .map(|t| Equal::with_content(t.text_range(), t));
    let default_ty = child::<Type>(&syntax);

    let ident_missing = ident.is_none();
    let mismatch_default = equal.is_some() ^ default_ty.is_some();

    match (ident, equal, default_ty) {
      (Some(ident), equal, default_ty) if !mismatch_default => {
        Ok(DefinitionTypeParam::new(syntax, ident, equal, default_ty))
      }
      _ => {
        let missing = [
          ident_missing.then_some(DefinitionTypeParamSyntax::Ident),
          mismatch_default.then_some(DefinitionTypeParamSyntax::DefaultType),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => DefinitionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_angle = token(&syntax, &SyntaxKind::LAngle)
      .map(|t| LAngle::with_content(t.text_range(), t));
    let params = children::<DefinitionTypeParam<Ident, Type, GraphQLxLanguage>>(&syntax);
    let has_params = params.clone().next().is_some();
    let r_angle = token(&syntax, &SyntaxKind::RAngle)
      .map(|t| RAngle::with_content(t.text_range(), t));

    match (l_angle, has_params, r_angle) {
      (Some(l_angle), true, Some(r_angle)) => Ok(DefinitionTypeGenerics::new(
        syntax,
        l_angle,
        params,
        r_angle,
      )),
      (l_angle, has_params, r_angle) => {
        let missing = [
          l_angle.is_none().then_some(TypeGenericsSyntax::LAngle),
          (!has_params).then_some(TypeGenericsSyntax::Parameters),
          r_angle.is_none().then_some(TypeGenericsSyntax::RAngle),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Type> TypeGenerics<Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => TypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_angle = token(&syntax, &SyntaxKind::LAngle)
      .map(|t| LAngle::with_content(t.text_range(), t));
    let params = children::<Type>(&syntax);
    let has_params = params.clone().next().is_some();
    let r_angle = token(&syntax, &SyntaxKind::RAngle)
      .map(|t| RAngle::with_content(t.text_range(), t));

    match (l_angle, has_params, r_angle) {
      (Some(l_angle), true, Some(r_angle)) => Ok(TypeGenerics::new(
        syntax,
        l_angle,
        params,
        r_angle,
      )),
      (l_angle, has_params, r_angle) => {
        let missing = [
          l_angle.is_none().then_some(TypeGenericsSyntax::LAngle),
          (!has_params).then_some(TypeGenericsSyntax::Parameters),
          r_angle.is_none().then_some(TypeGenericsSyntax::RAngle),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => ExtensionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_angle = token(&syntax, &SyntaxKind::LAngle)
      .map(|t| LAngle::with_content(t.text_range(), t));
    let params = children::<DefinitionTypeParam<Ident, Type, GraphQLxLanguage>>(&syntax);
    let has_params = params.clone().next().is_some();
    let r_angle = token(&syntax, &SyntaxKind::RAngle)
      .map(|t| RAngle::with_content(t.text_range(), t));

    match (l_angle, has_params, r_angle) {
      (Some(l_angle), true, Some(r_angle)) => Ok(ExtensionTypeGenerics::new(
        syntax,
        l_angle,
        params,
        r_angle,
      )),
      (l_angle, has_params, r_angle) => {
        let missing = [
          l_angle.is_none().then_some(TypeGenericsSyntax::LAngle),
          (!has_params).then_some(TypeGenericsSyntax::Parameters),
          r_angle.is_none().then_some(TypeGenericsSyntax::RAngle),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExtensionName<Ident, Type, GraphQLxLanguage> {
    type Component = ExtensionNameSyntax;
    type COMPONENTS = U2;
  } => ExtensionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Ident>(&syntax);
    let generics = child::<ExtensionTypeGenerics<Ident, Type, GraphQLxLanguage>>(&syntax);

    match name {
      Some(name) => Ok(ExtensionName::new(syntax, name, generics)),
      None => {
        let missing = IncompleteSyntax::new(ExtensionNameSyntax::Name);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionName<Ident, Type, GraphQLxLanguage> {
    type Component = DefinitionNameSyntax;
    type COMPONENTS = U2;
  } => DefinitionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Ident>(&syntax);
    let generics = child::<DefinitionTypeGenerics<Ident, Type, GraphQLxLanguage>>(&syntax);

    match name {
      Some(name) => Ok(DefinitionName::new(syntax, name, generics)),
      None => {
        let missing = IncompleteSyntax::new(DefinitionNameSyntax::Name);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExecutableDefinitionName<Ident, Type, GraphQLxLanguage> {
    type Component = ExecutableDefinitionNameSyntax;
    type COMPONENTS = U2;
  } => ExecutableDefinitionName(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let name = child::<Ident>(&syntax);
    let generics = child::<ExecutableDefinitionTypeGenerics<Ident, Type, GraphQLxLanguage>>(&syntax);

    match name {
      Some(name) => Ok(ExecutableDefinitionName::new(syntax, name, generics)),
      None => {
        let missing = IncompleteSyntax::new(ExecutableDefinitionNameSyntax::Name);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident> ExecutableDefinitionTypeParam<Ident, GraphQLxLanguage> {
    type Component = DefinitionTypeParamSyntax;
    type COMPONENTS = U2;
  } => TypeParameter(|syntax: SyntaxNode<GraphQLxLanguage>| {
    match child::<Ident>(&syntax) {
      Some(ident) => Ok(ExecutableDefinitionTypeParam::new(syntax, ident)),
      None => {
        let missing = IncompleteSyntax::new(DefinitionTypeParamSyntax::Ident);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> ExecutableDefinitionTypeGenerics<Ident, Type, GraphQLxLanguage> {
    type Component = TypeGenericsSyntax;
    type COMPONENTS = U3;
  } => ExecutableDefinitionTypeGenerics(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let l_angle = token(&syntax, &SyntaxKind::LAngle)
      .map(|t| LAngle::with_content(t.text_range(), t));
    let params = children::<ExecutableDefinitionTypeParam<Ident, GraphQLxLanguage>>(&syntax);
    let has_params = params.clone().next().is_some();
    let r_angle = token(&syntax, &SyntaxKind::RAngle)
      .map(|t| RAngle::with_content(t.text_range(), t));

    match (l_angle, has_params, r_angle) {
      (Some(l_angle), true, Some(r_angle)) => Ok(ExecutableDefinitionTypeGenerics::new(
        syntax,
        l_angle,
        params,
        r_angle,
      )),
      (l_angle, has_params, r_angle) => {
        let missing = [
          l_angle.is_none().then_some(TypeGenericsSyntax::LAngle),
          (!has_params).then_some(TypeGenericsSyntax::Parameters),
          r_angle.is_none().then_some(TypeGenericsSyntax::RAngle),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident> PathSegment<Ident, GraphQLxLanguage> {
    type Component = PathSegmentSyntax;
    type COMPONENTS = U2;
  } => PathSegment(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let sep = token(&syntax, &SyntaxKind::PathSeparator).map(|t| PathSeparator::with_content(t.text_range(), t));
    let ident = child::<Ident>(&syntax);

    match ident {
      Some(ident) => Ok(PathSegment::new(syntax, sep, ident)),
      None => Err(IncompleteSyntax::new(PathSegmentSyntax::Ident).into()),
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident> Path<Ident, GraphQLxLanguage> {
    type Component = PathSyntax;
    type COMPONENTS = U1;
  } => Path(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let segments = children::<PathSegment<Ident, _>>(&syntax);
    let leading: Option<PathSegment<Ident, _>> = segments.clone().next();

    match leading {
      Some(leading) => {
        Ok(Path::new(syntax, segments, leading.separator().is_some()))
      },
      None => Err(IncompleteSyntax::new(PathSyntax::Segments).into()),
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    PathSegment<Ident, GraphQLxLanguage>: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<PathT, Type> TypePath<PathT, Type, GraphQLxLanguage> {
    type Component = TypePathSyntax;
    type COMPONENTS = U2;
  } => TypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let path = child::<PathT>(&syntax);
    let generics = child::<TypeGenerics<Type, GraphQLxLanguage>>(&syntax);

    match path {
      Some(path) => Ok(TypePath::new(syntax, path, generics)),
      None => {
        let missing = IncompleteSyntax::new(TypePathSyntax::Path);
        Err(missing.into())
      }
    }
  })
  where
    PathT: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> DefinitionTypePath<Ident, Type, GraphQLxLanguage> {
    type Component = DefinitionTypePathSyntax;
    type COMPONENTS = U1;
  } => DefinitionTypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let path = child::<Path<Ident, GraphQLxLanguage>>(&syntax);
    let name = child::<DefinitionName<Ident, Type, GraphQLxLanguage>>(&syntax);

    match name {
      Some(name) => Ok(DefinitionTypePath::new(syntax, path, name)),
      None => {
        let missing = IncompleteSyntax::new(DefinitionTypePathSyntax::Name);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> FragmentTypePath<Ident, Type, GraphQLxLanguage> {
    type Component = FragmentTypePathSyntax;
    type COMPONENTS = U2;
  } => FragmentTypePath(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let path = child::<Path<Ident, GraphQLxLanguage>>(&syntax);
    let generics = child::<TypeGenerics<Type, GraphQLxLanguage>>(&syntax);

    match path {
      Some(path) => Ok(FragmentTypePath::new(syntax, path, generics)),
      None => {
        let missing = IncompleteSyntax::new(FragmentTypePathSyntax::Path);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> WherePredicate<Ident, Type, GraphQLxLanguage> {
    type Component = WherePredicateSyntax;
    type COMPONENTS = U2;
  } => WherePredicate(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let type_paths = children::<TypePath<Ident, Type, GraphQLxLanguage>>(&syntax);
    let bounded_type = child::<TypePath<Ident, Type, GraphQLxLanguage>>(&syntax);
    let colon = token(&syntax, &SyntaxKind::Colon)
      .map(|t| Colon::with_content(t.text_range(), t));
    let bounds_missing = type_paths.clone().skip(1).next().is_none();

    let separators = syntax
      .children_with_tokens()
      .filter_map(|element| match element {
        rowan::NodeOrToken::Token(token) if token.kind() == SyntaxKind::Ampersand => {
          Some(Ampersand::with_content(token.text_range(), token))
        }
        _ => None,
      })
      .collect::<Vec<_>>();

    match (bounded_type, colon, bounds_missing) {
      (Some(bounded_type), Some(colon), false) => Ok(WherePredicate::new(
        syntax,
        bounded_type,
        colon,
        type_paths,
        separators,
      )),
      (bounded_type, colon, bounds_missing) => {
        let missing = [
          bounded_type.is_none().then_some(WherePredicateSyntax::BoundedType),
          (bounds_missing || colon.is_none()).then_some(WherePredicateSyntax::Bounds),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type> WhereClause<Ident, Type, GraphQLxLanguage> {
    type Component = WhereClauseSyntax;
    type COMPONENTS = U3;
  } => WhereClause(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let where_token = token(&syntax, &SyntaxKind::where_KW)
      .map(|t| Where::with_content(t.text_range(), t));
    let predicates = children::<WherePredicate<Ident, Type, GraphQLxLanguage>>(&syntax);
    let has_predicates = predicates.clone().next().is_some();

    match (where_token, has_predicates) {
      (Some(where_token), true) => Ok(WhereClause::new(syntax, where_token, predicates)),
      (where_token, has_predicates) => {
        let missing = [
          where_token.is_none().then_some(WhereClauseSyntax::WhereKeyword),
          (!has_predicates).then_some(WhereClauseSyntax::Predicates),
        ];
        let missing = IncompleteSyntax::from_iter(missing.into_iter().flatten()).unwrap();
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
}

impl_graphqlx_node! {
  for<Ident, Type, Target> Constrained<Ident, Type, Target, GraphQLxLanguage> {
    type Component = ConstrainedSyntax;
    type COMPONENTS = U2;
  } => TypeConstraint(|syntax: SyntaxNode<GraphQLxLanguage>| {
    let where_clause = child::<WhereClause<Ident, Type, GraphQLxLanguage>>(&syntax);
    let target = child::<Target>(&syntax);

    match target {
      Some(target) => Ok(Constrained::new(syntax, where_clause, target)),
      None => {
        let missing = IncompleteSyntax::new(ConstrainedSyntax::Target);
        Err(missing.into())
      }
    }
  })
  where
    Ident: CstNode<Language = GraphQLxLanguage>,
    Type: CstNode<Language = GraphQLxLanguage>,
    Target: CstNode<Language = GraphQLxLanguage>,
}

use super::DefaultVec;

use crate::{parser::ident::Ident, scaffold};

pub type Path<S, Container = DefaultVec<Ident<S>>> = scaffold::Path<Ident<S>, Container>;

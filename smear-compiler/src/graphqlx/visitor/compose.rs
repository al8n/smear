//! Composition at compile time: the impls that make one visitor of several, a visitor switched at
//! run time, and a borrowed or boxed visitor.
//!
//! Each hook of a composition calls every element's, first to last. Rules do not see one another
//! and the diagnostics they report are sorted, so the order of composition never changes what a
//! walk answers.

use std::boxed::Box;

use crate::graphqlx::id::{
  ApplicationId, DefId, MemberId, ModuleId, PackageId, TypeExprId, ValueId,
};

use super::{Cx, Visitor};

/// The empty visitor: every hook does nothing.
impl Visitor for () {}

/// A visitor switched at run time: `Some` is its visitor, and `None` does no work.
impl<V: Visitor> Visitor for Option<V> {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    if let Some(visitor) = self {
      V::begin(visitor, cx);
    }
  }

  fn enter_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    if let Some(visitor) = self {
      V::enter_package(visitor, cx, package);
    }
  }

  fn exit_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    if let Some(visitor) = self {
      V::exit_package(visitor, cx, package);
    }
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    if let Some(visitor) = self {
      V::enter_module(visitor, cx, module);
    }
  }

  fn exit_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    if let Some(visitor) = self {
      V::exit_module(visitor, cx, module);
    }
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    if let Some(visitor) = self {
      V::enter_def(visitor, cx, def);
    }
  }

  fn exit_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    if let Some(visitor) = self {
      V::exit_def(visitor, cx, def);
    }
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    if let Some(visitor) = self {
      V::enter_member(visitor, cx, member);
    }
  }

  fn exit_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    if let Some(visitor) = self {
      V::exit_member(visitor, cx, member);
    }
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    if let Some(visitor) = self {
      V::type_expr(visitor, cx, ty);
    }
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    if let Some(visitor) = self {
      V::application(visitor, cx, application);
    }
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    if let Some(visitor) = self {
      V::value(visitor, cx, value);
    }
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    if let Some(visitor) = self {
      V::schema(visitor, cx);
    }
  }

  fn end(&mut self, cx: &mut Cx<'_>) {
    if let Some(visitor) = self {
      V::end(visitor, cx);
    }
  }
}

/// A borrowed visitor, `&mut dyn Visitor` included: each hook is the visitor's.
impl<V: Visitor + ?Sized> Visitor for &mut V {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    V::begin(self, cx);
  }

  fn enter_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    V::enter_package(self, cx, package);
  }

  fn exit_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    V::exit_package(self, cx, package);
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    V::enter_module(self, cx, module);
  }

  fn exit_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    V::exit_module(self, cx, module);
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    V::enter_def(self, cx, def);
  }

  fn exit_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    V::exit_def(self, cx, def);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    V::enter_member(self, cx, member);
  }

  fn exit_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    V::exit_member(self, cx, member);
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    V::type_expr(self, cx, ty);
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    V::application(self, cx, application);
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    V::value(self, cx, value);
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    V::schema(self, cx);
  }

  fn end(&mut self, cx: &mut Cx<'_>) {
    V::end(self, cx);
  }
}

/// A boxed visitor, `Box<dyn Visitor>` included: a rule unknown at compile time, as one element of
/// a composition. Each hook is the visitor's.
impl<V: Visitor + ?Sized> Visitor for Box<V> {
  fn begin(&mut self, cx: &mut Cx<'_>) {
    V::begin(self, cx);
  }

  fn enter_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    V::enter_package(self, cx, package);
  }

  fn exit_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
    V::exit_package(self, cx, package);
  }

  fn enter_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    V::enter_module(self, cx, module);
  }

  fn exit_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
    V::exit_module(self, cx, module);
  }

  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    V::enter_def(self, cx, def);
  }

  fn exit_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    V::exit_def(self, cx, def);
  }

  fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    V::enter_member(self, cx, member);
  }

  fn exit_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
    V::exit_member(self, cx, member);
  }

  fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
    V::type_expr(self, cx, ty);
  }

  fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
    V::application(self, cx, application);
  }

  fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
    V::value(self, cx, value);
  }

  fn schema(&mut self, cx: &mut Cx<'_>) {
    V::schema(self, cx);
  }

  fn end(&mut self, cx: &mut Cx<'_>) {
    V::end(self, cx);
  }
}

/// The impl for the tuple of the elements `$name`, each at the tuple index `$index`: each hook calls
/// every element's, first to last.
macro_rules! tuple {
  ($($index:tt $name:ident),+) => {
    /// Static composition: each hook calls every element's, first to last.
    impl<$($name: Visitor),+> Visitor for ($($name,)+) {
      fn begin(&mut self, cx: &mut Cx<'_>) {
        $( $name::begin(&mut self.$index, cx); )+
      }

      fn enter_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
        $( $name::enter_package(&mut self.$index, cx, package); )+
      }

      fn exit_package(&mut self, cx: &mut Cx<'_>, package: PackageId) {
        $( $name::exit_package(&mut self.$index, cx, package); )+
      }

      fn enter_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
        $( $name::enter_module(&mut self.$index, cx, module); )+
      }

      fn exit_module(&mut self, cx: &mut Cx<'_>, module: ModuleId) {
        $( $name::exit_module(&mut self.$index, cx, module); )+
      }

      fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
        $( $name::enter_def(&mut self.$index, cx, def); )+
      }

      fn exit_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
        $( $name::exit_def(&mut self.$index, cx, def); )+
      }

      fn enter_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
        $( $name::enter_member(&mut self.$index, cx, member); )+
      }

      fn exit_member(&mut self, cx: &mut Cx<'_>, member: MemberId) {
        $( $name::exit_member(&mut self.$index, cx, member); )+
      }

      fn type_expr(&mut self, cx: &mut Cx<'_>, ty: TypeExprId) {
        $( $name::type_expr(&mut self.$index, cx, ty); )+
      }

      fn application(&mut self, cx: &mut Cx<'_>, application: ApplicationId) {
        $( $name::application(&mut self.$index, cx, application); )+
      }

      fn value(&mut self, cx: &mut Cx<'_>, value: ValueId) {
        $( $name::value(&mut self.$index, cx, value); )+
      }

      fn schema(&mut self, cx: &mut Cx<'_>) {
        $( $name::schema(&mut self.$index, cx); )+
      }

      fn end(&mut self, cx: &mut Cx<'_>) {
        $( $name::end(&mut self.$index, cx); )+
      }
    }
  };
}

tuple!(0 A);
tuple!(0 A, 1 B);
tuple!(0 A, 1 B, 2 C);
tuple!(0 A, 1 B, 2 C, 3 D);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K);
tuple!(0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H, 8 I, 9 J, 10 K, 11 L);

//! Members: the fields, arguments, input fields and enum values of definitions.

use tokora::diagnostic::Location;

use crate::graphqlx::id::{ApplicationId, DefId, IdRange, MemberId, Sym, TypeExprId, ValueId};

use super::{Model, name::Description, table::MemberRow};

/// What kind of member a member is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum MemberKind {
  /// A field of an object or an interface.
  Field,
  /// An argument of a field, or of a directive definition.
  Argument,
  /// A field of an input object.
  InputField,
  /// A value of an enum.
  EnumValue,
}

impl Model {
  /// The member `id` names.
  ///
  /// # Panics
  ///
  /// If `id` is not one of this model's members: an id belongs to the analysis that issued it.
  #[inline]
  pub fn member(&self, id: MemberId) -> Member<'_> {
    assert!(
      (id.get() as usize) < self.members.len(),
      "member id {} is past the {} members of this model",
      id.get(),
      self.members.len()
    );
    Member { model: self, id }
  }
}

/// One member of the model: a field, an argument, an input field or an enum value.
///
/// `Copy`: it holds the model and an id, and everything it answers is borrowed from the model.
#[derive(Clone, Copy)]
pub struct Member<'a> {
  model: &'a Model,
  id: MemberId,
}

impl<'a> Member<'a> {
  /// Its id.
  #[inline]
  pub const fn id(&self) -> MemberId {
    self.id
  }

  /// What kind of member it is.
  #[inline]
  pub fn kind(&self) -> MemberKind {
    self.row().kind
  }

  /// Its name.
  #[inline]
  pub fn name(&self) -> &'a str {
    let model: &'a Model = self.model;
    model.name(self.row().name)
  }

  /// Its name's symbol.
  #[inline]
  pub fn sym(&self) -> Sym {
    self.row().name
  }

  /// The definition it belongs to; for a field's argument, the field's.
  #[inline]
  pub fn owner(&self) -> DefId {
    self.row().owner
  }

  /// The field it is an argument of, or `None` for every other member.
  #[inline]
  pub fn parent(&self) -> Option<MemberId> {
    self.row().parent
  }

  /// Where its name is written. Its source is its own, which for a member an extension
  /// contributed is the extension's.
  #[inline]
  pub fn location(&self) -> Location {
    self.row().location
  }

  /// Where it is written, from its description, or its first token when it has none, to its last
  /// token. Its source is its own, which for a member an extension contributed is the extension's.
  ///
  /// In an analysis made through [`analyze_with`](crate::graphqlx::analyze_with) with a layer that
  /// keeps a tree, such as `smear-parser`'s lossless layer, the smallest node of its file's tree
  /// that covers the extent is the node the member was written as, and
  /// [`Trees::node`](crate::graphqlx::Trees::node) answers it. A file of a built-in package has no
  /// tree, and `Trees::node` answers `None` for an extent in one.
  #[inline]
  pub fn extent(&self) -> Location {
    self.row().extent
  }

  /// Its description, if one is written.
  #[inline]
  pub fn description(&self) -> Option<Description<'a>> {
    Description::of(self.model, self.row().description)
  }

  /// Its type as written, or `None` for an enum value.
  #[inline]
  pub fn ty(&self) -> Option<TypeExprId> {
    self.row().ty
  }

  /// Its default value, or `None` when it has none. Only an argument or an input field has one.
  #[inline]
  pub fn default(&self) -> Option<ValueId> {
    self.row().default
  }

  /// A field's arguments, in order; empty for every other member.
  #[inline]
  pub fn arguments(&self) -> IdRange<MemberId> {
    self.row().arguments
  }

  /// Its directive applications, in order.
  #[inline]
  pub fn applications(&self) -> &'a [ApplicationId] {
    let model: &'a Model = self.model;
    self.row().applications.of(&model.member_applications)
  }

  /// Whether an error a phase or one of smear's rules reported lies within it, from its
  /// description, or its first token, to its last token.
  #[inline]
  pub fn has_errors(&self) -> bool {
    self.row().marked
  }

  /// The model's record of this member.
  fn row(&self) -> &'a MemberRow {
    let model: &'a Model = self.model;
    &model.members[self.id.get() as usize]
  }
}

impl core::fmt::Debug for Member<'_> {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    f.debug_struct("Member")
      .field("id", &self.id)
      .field("kind", &self.kind())
      .field("name", &self.name())
      .field("owner", &self.owner())
      .finish()
  }
}

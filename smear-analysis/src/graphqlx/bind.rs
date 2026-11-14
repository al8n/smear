//! Binding: the phase after coercion, which gives each scalar its host types, the externs
//! `Def::externs()` answers.
//!
//! `@extern` binds a scalar its own package defines to a host type of one language. A scalar's
//! externs are the `@extern`s its own package writes on it, in its definition or in an extension of
//! its own, in its applications' order: a `lang` written twice is bound twice, and the rule on
//! `@extern`'s use refuses the later. What binds:
//!
//! - a scalar whose origin is `User`. A built-in is bound to nothing, a GraphQL built-in the root
//!   package replaces among them: a built-in's host type is given by `@repr(for: …)` on the schema
//!   alone;
//! - each of its applications that resolved to `core`'s `@extern`, told by the definition it
//!   resolved to and never by how it is spelled, that the scalar's own package wrote, the package
//!   of the module its declaration is written in. That is the test instantiation asks before it
//!   gives a definition its built-in meaning: an `@extern` another package writes binds nothing,
//!   and the orphan rule refuses it;
//! - of those, each whose first `lang` and first `type` coerced to strings, and whose first `tier`
//!   is absent or `null`, which stands for the tier the scalar's own package is declared at, or a
//!   value of `core`'s `Tier`. Any other binds nothing: coercion refused it where it is written. An
//!   argument written twice is another rule's.
//!
//! `@extern`, `Tier` and their members are found once, each by its name in `core`'s root module or
//! among the library's own members: a program's spelling is never read.
//!
//! It writes no text and interns no name. An extern holds the runs coercion kept for its strings,
//! so coercion stays the last phase that writes the model's arenas, and the check of them it makes
//! when it ends covers every text the model keeps. It reports nothing, and nothing recurses: it
//! looks once at each application of each scalar a supplied package defines, and once at each of
//! that application's arguments, and appends one row for each binding, definition by definition
//! in id order, each definition's in its applications' order.

use crate::graphqlx::{
  builtin::{CORE, DIRECTIVE, TYPE},
  declare::Declarations,
  id::{ApplicationId, DefId, MemberId, ValueId},
  instantiate::{binding, own_directive, own_package},
  model::{DefKind, ModelBuilder, Origin, Stored, Text},
  package::Tier,
  source::SourceMap,
};

/// Gives each scalar of `builder`, whose declarations are `declarations` and whose sources
/// `sources` numbers, its externs: each `@extern` its own package writes on it whose values
/// coerced, with its effective tier.
pub(crate) fn bind(
  declarations: &Declarations<'_, '_>,
  sources: &SourceMap,
  builder: &mut ModelBuilder,
) {
  let Some(keys) = Keys::find(declarations, builder) else {
    return;
  };
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    if builder.def_kind(def) != Some(DefKind::Scalar)
      || builder.def_origin(def) != Some(Origin::User)
    {
      continue;
    }
    let Some(own) = own_package(declarations, def) else {
      continue;
    };
    let Some(declared) = builder.package_tier(own) else {
      continue;
    };
    let count = builder.def_applications(def).len();
    for at in 0..count {
      let Some(&application) = builder.def_applications(def).get(at) else {
        continue;
      };
      if own_directive(builder, sources, own, application) != Some(keys.directive) {
        continue;
      }
      if let Some((lang, ty, tier)) = keys.read(builder, application, declared) {
        builder.bind_extern(def, application, lang, ty, tier);
      }
    }
  }
}

/// `core`'s `@extern` and its three arguments, and the three values of `core`'s `Tier`, each with
/// the tier it names.
struct Keys {
  directive: DefId,
  lang: MemberId,
  ty: MemberId,
  tier: MemberId,
  tiers: [(MemberId, Tier); 3],
}

impl Keys {
  /// Finds each by its name: the directive and the enum in `core`'s root module, the arguments
  /// and the values among their own members. `None` when one is missing, which the library of an
  /// analysis never is.
  fn find(declarations: &Declarations<'_, '_>, builder: &ModelBuilder) -> Option<Self> {
    let directive = binding(declarations, builder, CORE, DIRECTIVE, b"extern")?;
    let enumeration = binding(declarations, builder, CORE, TYPE, b"Tier")?;
    Some(Self {
      directive,
      lang: member_named(builder, directive, b"lang")?,
      ty: member_named(builder, directive, b"type")?,
      tier: member_named(builder, directive, b"tier")?,
      tiers: [
        (member_named(builder, enumeration, b"CORE")?, Tier::Core),
        (member_named(builder, enumeration, b"ALLOC")?, Tier::Alloc),
        (member_named(builder, enumeration, b"STD")?, Tier::Std),
      ],
    })
  }

  /// What `application`, an `@extern`, binds: the kept text of its first `lang` and of its first
  /// `type`, each a string, and the tier its first `tier` names, `declared` when it is absent or
  /// `null`. `None` when one of them is not what it takes, which coercion reported.
  fn read(
    &self,
    builder: &ModelBuilder,
    application: ApplicationId,
    declared: Tier,
  ) -> Option<(Text, Text, Tier)> {
    let mut lang: Option<ValueId> = None;
    let mut ty: Option<ValueId> = None;
    let mut tier: Option<ValueId> = None;
    for argument in builder.application_arguments(application) {
      let first = if argument.argument() == self.lang {
        &mut lang
      } else if argument.argument() == self.ty {
        &mut ty
      } else if argument.argument() == self.tier {
        &mut tier
      } else {
        continue;
      };
      if first.is_none() {
        *first = Some(argument.value());
      }
    }
    let Stored::String(lang) = builder.value_stored(lang?) else {
      return None;
    };
    let Stored::String(ty) = builder.value_stored(ty?) else {
      return None;
    };
    let tier = match tier.map(|value: ValueId| builder.value_stored(value)) {
      None | Some(Stored::Null) => declared,
      Some(Stored::Enum(value)) => self.tier_of(value)?,
      Some(Stored::String(_) | Stored::Other) => return None,
    };
    Some((lang, ty, tier))
  }

  /// The tier the value `value` of `Tier` names: `None` for a value of another enum's.
  fn tier_of(&self, value: MemberId) -> Option<Tier> {
    self
      .tiers
      .iter()
      .find(|entry: &&(MemberId, Tier)| entry.0 == value)
      .map(|entry: &(MemberId, Tier)| entry.1)
  }
}

/// The first of `owner`'s direct members named `name`, a name of the built-in library's own.
fn member_named(builder: &ModelBuilder, owner: DefId, name: &[u8]) -> Option<MemberId> {
  let sym = builder.find(name)?;
  builder
    .def_members(owner)
    .into_iter()
    .find(|member: &MemberId| builder.member_name(*member) == Some(sym))
}

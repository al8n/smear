//! Instantiation: the phase after lowering, which gives each definition its built-in meaning.
//!
//! It reads no document: it reads the rows lowering wrote, through the builder, and writes onto
//! them.
//!
//! # Built-in meaning
//!
//! `core`'s `@marker`, `@set` and `@map` are found once, each by its name among the directives
//! `core`'s root module declares. An application means one of them when it resolved to that
//! definition, whatever path it is written with; a directive of the program's own means nothing
//! here, whatever its name. Each definition's applications, every contribution's, are then looked
//! at once: an interface that applies `@marker` is a marker, and a scalar that applies `@set` or
//! `@map` stands for that collection, the first of the two in application order when it applies
//! both. A definition of another kind gets neither: where a directive may stand is a rule's to
//! judge.

use super::{
  builtin::{CORE, DIRECTIVE},
  declare::{Binding, Declarations},
  id::{ApplicationId, DefId},
  model::{Collection, DefKind, ModelBuilder, Resolution},
};

/// Instantiates what lowering left in `builder`, whose declarations are `declarations`: each
/// definition's built-in meaning.
pub(crate) fn instantiate(declarations: &Declarations<'_, '_>, builder: &mut ModelBuilder) {
  meaning(declarations, builder);
}

/// Gives each definition its built-in meaning: an interface that applies `core`'s `@marker` is a
/// marker, and a scalar that applies `core`'s `@set` or `@map` stands for that collection.
fn meaning(declarations: &Declarations<'_, '_>, builder: &mut ModelBuilder) {
  let marker = directive(declarations, builder, b"marker");
  let set = directive(declarations, builder, b"set");
  let map = directive(declarations, builder, b"map");
  for index in 0..builder.def_count() {
    let def = DefId::new(index);
    match builder.def_kind(def) {
      Some(DefKind::Interface) => {
        if marker.is_some_and(|wanted: DefId| applies(builder, def, wanted)) {
          builder.set_marker(def);
        }
      }
      Some(DefKind::Scalar) => {
        let collection = builder
          .def_applications(def)
          .iter()
          .find_map(|application: &ApplicationId| stands_for(builder, *application, set, map));
        if let Some(collection) = collection {
          builder.set_collection(def, collection);
        }
      }
      _ => {}
    }
  }
}

/// The collection `application` makes the scalar that applies it stand for: a set when it applies
/// `set`, `core`'s `@set`, and a map when it applies `map`, `core`'s `@map`.
fn stands_for(
  builder: &ModelBuilder,
  application: ApplicationId,
  set: Option<DefId>,
  map: Option<DefId>,
) -> Option<Collection> {
  let Some((Resolution::Resolved(directive), _, _)) = builder.applied(application) else {
    return None;
  };
  if Some(directive) == set {
    Some(Collection::Set)
  } else if Some(directive) == map {
    Some(Collection::Map)
  } else {
    None
  }
}

/// Whether `def` applies `directive`, in any of its contributions.
fn applies(builder: &ModelBuilder, def: DefId, directive: DefId) -> bool {
  builder
    .def_applications(def)
    .iter()
    .any(|application: &ApplicationId| {
      matches!(
        builder.applied(*application),
        Some((Resolution::Resolved(found), _, _)) if found == directive
      )
    })
}

/// `core`'s directive named `name`: the one `core`'s root module declares, read off its binding
/// there. `None` when no name so spelled was ever interned, which an analysis's library rules out.
fn directive(
  declarations: &Declarations<'_, '_>,
  builder: &ModelBuilder,
  name: &[u8],
) -> Option<DefId> {
  let module = *declarations.roots.get(CORE)?;
  let sym = builder.find(name)?;
  let key = (module, DIRECTIVE, sym);
  let at = declarations
    .names
    .binary_search_by(|binding: &Binding| {
      (binding.module, binding.namespace, binding.name).cmp(&key)
    })
    .ok()?;
  declarations
    .names
    .get(at)
    .map(|binding: &Binding| binding.def)
}

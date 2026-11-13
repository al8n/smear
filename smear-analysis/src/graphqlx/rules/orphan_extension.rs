//! The orphan rule: a package extends its own definitions.
//!
//! A definition's canonical name is its identity in every program that holds its package. Were
//! another package to extend it, that one name would have a shape for each set of packages it is
//! analyzed beside. So an extension is judged by the package that wrote it, which is the package
//! of the source it is written in, against the package of the definition it extends:
//!
//! - the same package: nothing, from whichever of its modules the extension is written in;
//! - a supplied package's definition, the root's or a dependency's, extended by another package:
//!   refused. That is the root package extending a dependency's type or directive, a dependency
//!   extending another dependency's, and a dependency extending the root package's;
//! - a built-in package's definition, `core`'s, `alloc`'s or `std`'s, a primitive `core`
//!   materialises and a GraphQL built-in the root package replaces among them: allowed only when
//!   the root package wrote the extension and it adds directives and nothing else, which is what
//!   GraphQL's `extend scalar String @x` does. A dependency's extension of a built-in is refused
//!   whatever it adds, since what a built-in means in a program would otherwise depend on the
//!   program's dependencies; and so is one of the root package's that adds a member, an
//!   `implements` entry or a union member.
//!
//! Each refusal is `smear::graphqlx::orphan-extension`, at the extension's target path, with a
//! label at the definition's name unless the definition has no position, as a primitive `core`
//! materialises has none.
//!
//! Of the directives the root package may add to a built-in, three may not be among them: `core`'s
//! `@marker`, `@set` and `@map`, which say what a definition is. A definition's built-in meaning is
//! its own package's to say, and what a built-in is is the language's. Each application of one of
//! the three in the root package's extension of a built-in is refused too, with the same code and
//! label, at the application's path, whether or not the extension adds more than directives, which
//! is another mistake. The directive is told by the definition its application resolved to, never
//! by how it is spelled. The test is the one instantiation asks before it gives a definition its
//! meaning: whether the package that wrote the directive wrote the definition. So a GraphQL
//! built-in the root package replaces is the root package's own, and the root package's `@set` on
//! it is no orphan. An extension the rule refuses whole, a dependency's of a built-in or any
//! package's of another supplied package's definition, is refused once, and its applications are
//! not reported again.
//!
//! The rule judges a definition's extensions as the walk enters the definition. `analyze` walks
//! what the supplied packages wrote, and that is exactly what this judges: the walk enters a
//! definition when a supplied package wrote one of its contributions, and every extension is a
//! supplied package's, since no built-in package writes one. A built-in definition no supplied
//! package extends is not entered, and has no extension to judge.
//!
//! The rule reads the public model and nothing else.

use std::{format, string::String};

use crate::graphqlx::{
  Contribution, Cx, DefId, DefKind, Diagnostic, DiagnosticKind, PackageKind, Resolution, Visitor,
  diagnostic::Excerpt,
};

/// Reports each extension of a definition that the package that wrote the extension may not
/// extend, and each application of `core`'s `@marker`, `@set` or `@map` in the root package's
/// extension of a built-in it did not write, whether or not the directive could give that kind of
/// definition a meaning.
///
/// It reads a definition's contributions in the `enter_def` hook, and tells an extension from the
/// definition's own contribution by what the model says of each, never by its place among them.
///
/// It says nothing about an extension whose `has_errors()` is set: a phase already refused
/// something in it, and a second diagnostic within it would repeat the phase's.
///
/// It costs one visit of each contribution of each definition the walk enters, and, in the root
/// package's extension of a built-in, one look at each application that extension writes; it keeps
/// nothing between two walks. Its messages quote the definition's own name, the two packages'
/// names and an applied directive's own name, each as an excerpt, so each is bounded whatever the
/// names' lengths, and none quotes a path.
#[derive(Default)]
pub(super) struct OrphanExtension;

impl Visitor for OrphanExtension {
  fn enter_def(&mut self, cx: &mut Cx<'_>, def: DefId) {
    let model = cx.model();
    let sources = cx.sources();
    let definition = model.def(def);
    let home = model.module(definition.module()).package();
    let root = model.root_package();
    let kind = DiagnosticKind::OrphanExtension;
    // The package that wrote the definition, which its own contribution, the first, is written by:
    // the root package for a GraphQL built-in it replaces, though the definition is the built-in's
    // and belongs to the built-in's package.
    let definer = definition
      .contributions()
      .next()
      .filter(|own: &Contribution<'_>| !own.is_extension())
      .and_then(|own: Contribution<'_>| sources.written_by(own.location()));
    for part in definition.contributions() {
      if !part.is_extension() || part.has_errors() {
        continue;
      }
      let Some(writer) = sources.written_by(part.location()) else {
        continue;
      };
      if writer == home {
        continue;
      }
      let owner = model.package(home);
      let built_in = owner.kind() == PackageKind::BuiltIn;
      let by_root = writer == root;
      let directives_alone = part.members().is_empty()
        && part.implements().is_empty()
        && part.union_members().is_empty();
      let author = Excerpt(model.package(writer).name().as_bytes());
      let owner = Excerpt(owner.name().as_bytes());
      let name = Excerpt(definition.name().as_bytes());
      let (noun, sigil) = if definition.kind() == DefKind::Directive {
        ("directive", "@")
      } else {
        ("type", "")
      };
      if built_in && by_root {
        // The root package may add directives to a built-in, but none that says what a definition
        // is, unless it wrote the definition: a built-in it replaces is its own.
        if definer != Some(writer) {
          let builtins = model.builtins();
          let meanings = [builtins.marker(), builtins.set(), builtins.map()];
          for application in part.applications() {
            let applied = model.application(*application);
            let Resolution::Resolved(directive) = applied.directive() else {
              continue;
            };
            if !meanings.contains(&directive) {
              continue;
            }
            // Whether the directive could give this kind of definition a meaning or not: it is
            // refused on every built-in.
            let spelled = Excerpt(model.def(directive).name().as_bytes());
            let mut report = Diagnostic::builder(kind.code(), applied.path_location())
              .severity(kind.severity())
              .message(format!(
                "package `{author}` applies `@{spelled}` to the {noun} `{sigil}{name}` of the \
                 built-in package `{owner}`, and what a built-in is cannot be changed by an \
                 extension"
              ))
              .primary_label(kind.title())
              .help(
                "`@marker`, `@set` and `@map` say what a definition is, and what a built-in is is \
                 the language's to say: remove this directive",
              );
            // A primitive `core` materialises has no position of its own to point at.
            if !definition.location().is_entire() {
              report = report.label(definition.location(), "the definition it extends");
            }
            cx.report(report.build());
          }
        }
        // The one extension of another package's definition that is allowed: the root package's,
        // of a built-in, adding directives alone.
        if directives_alone {
          continue;
        }
      }

      let (message, help): (String, &'static str) = if !built_in {
        (
          format!(
            "package `{author}` extends the {noun} `{sigil}{name}`, which package `{owner}` \
             defines"
          ),
          "a package extends its own definitions alone, so that a definition is the same in \
           every program that holds it: define a type or a directive of this package's own, or \
           make the change in the package that defines it",
        )
      } else if !by_root {
        (
          format!(
            "package `{author}` extends the {noun} `{sigil}{name}` of the built-in package \
             `{owner}`, which only the root package extends"
          ),
          // The root package may write this extension in its place only when it adds directives
          // alone.
          if directives_alone {
            "only the root package extends a definition of `core`, `alloc` or `std`, and only to \
             add directives: remove this extension, or write it in the root package"
          } else {
            "only the root package extends a definition of `core`, `alloc` or `std`, and only to \
             add directives: remove this extension"
          },
        )
      } else {
        // A GraphQL built-in the root package replaces has the root package's definition for its
        // own contribution, and that definition may write what this extension adds.
        let replaced = definer == Some(root);
        (
          format!(
            "package `{author}` adds more than directives to the {noun} `{sigil}{name}` of the \
             built-in package `{owner}`"
          ),
          if replaced {
            "an extension of a definition of `core`, `alloc` or `std` adds directives and nothing \
             else, and this package's own definition replaces the built-in: write what this one \
             adds in the definition itself"
          } else {
            "an extension of a definition of `core`, `alloc` or `std` adds directives and nothing \
             else: remove what this one adds beside them"
          },
        )
      };
      let mut report = Diagnostic::builder(kind.code(), part.location())
        .severity(kind.severity())
        .message(message)
        .primary_label(kind.title())
        .help(help);
      // A primitive `core` materialises has no position of its own to point at.
      if !definition.location().is_entire() {
        report = report.label(definition.location(), "the definition it extends");
      }
      cx.report(report.build());
    }
  }
}

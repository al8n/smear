//! Every module of a GraphQLx lossless tree, held to the braces as written.
//!
//! While a brace is open, a closing brace is taken only by the production that owns it, and that
//! rule is what makes a module's node faithful to the braces: counting braces alone from the
//! node's own `{`, the count returns to zero at most once, at the node's own `}`, which is the
//! last token of the node that is no trivia; a node with no `}` of its own never returns to zero.
//! [`faithful`] checks every module of a tree in one walk, so a sweep can afford it on every tree
//! it builds, and answers how many modules it read and how many of them hold an `Error`: a sweep
//! that never met a broken module witnessed nothing.
//!
//! # The braces are told by count, never by parentage
//!
//! Being a direct child of a module's node does not make a brace the module's own. An item of a
//! module that fails leaves its node unbuilt, and every token it had taken becomes a direct child
//! of the module: a type whose fields failed leaves its fields' `{` there, beside the module's
//! own. A `use` tree that a refusal stops after its group leaves the group's `}` there as well. So
//! the module's own `{` is the first `{` anywhere in its node, which only the module's
//! description, its `mod` and its name can stand in front of, and its own `}` is the one at which
//! the count from that `{`, over every brace of the node at every depth, returns to zero.
//!
//! Shared by the sweeps that parse broken texts, `lossless_x_mutation.rs`,
//! `lossless_x_roundtrip.rs` and `lossless_x_parity.rs`, and by `recovery_graphqlx.rs`.

use rowan::{NodeOrToken, WalkEvent};
use smear::parser::graphqlx::{kinds::SyntaxKind as K, lossless::SyntaxNode};

/// What a walk read: how many modules, and how many of those hold an `Error` node.
#[derive(Clone, Copy, Debug, Default)]
pub struct Modules {
  pub read: usize,
  pub broken: usize,
}

/// A module the walk is inside.
struct Open {
  node: SyntaxNode,
  /// The count of braces in front of the module's own `{`, once that `{` is read.
  base: Option<i64>,
  /// Whether the count has come back to `base` since.
  closed: bool,
  /// Whether the module holds an `Error` node.
  broken: bool,
}

/// The six ignorable token images.
const fn is_trivia(kind: K) -> bool {
  matches!(
    kind,
    K::Space | K::Tab | K::Newline | K::Comma | K::Comment | K::Bom
  )
}

/// Holds every module of `tree` to the braces as written, and answers what it read. The error
/// names the first module that is not faithful, and how.
///
/// One walk, constant work at each step. The count is the braces of the whole tree, which a stray
/// `}` may take below zero; a module's count is that count less the one in front of its `{`.
/// While a module's count is above zero the one around it is above zero too, so only the
/// innermost open module can return to zero at a `}`.
pub fn faithful(tree: &SyntaxNode) -> Result<Modules, String> {
  let mut modules = Modules::default();
  let mut open: Vec<Open> = Vec::new();
  let mut count: i64 = 0;
  for event in tree.preorder_with_tokens() {
    match event {
      WalkEvent::Enter(NodeOrToken::Node(node)) => match node.kind() {
        K::ModDefinition => {
          if let Some(around) = open.last().filter(|around: &&Open| around.closed) {
            return Err(std::format!(
              "the module at {:?} holds a module after its braces have closed",
              around.node.text_range()
            ));
          }
          open.push(Open {
            node,
            base: None,
            closed: false,
            broken: false,
          });
        }
        K::Error => {
          if let Some(innermost) = open.last_mut() {
            innermost.broken = true;
          }
        }
        _ => {}
      },
      WalkEvent::Leave(NodeOrToken::Node(node)) => {
        if node.kind() == K::ModDefinition {
          let done: Open = open
            .pop()
            .expect("a module the walk leaves is one it entered");
          modules.read += 1;
          if done.broken {
            modules.broken += 1;
            if let Some(around) = open.last_mut() {
              around.broken = true;
            }
          }
        }
      }
      WalkEvent::Enter(NodeOrToken::Token(token)) => {
        let kind: K = token.kind();
        if is_trivia(kind) {
          continue;
        }
        let Some(innermost) = open.last_mut() else {
          match kind {
            K::LBrace => count += 1,
            K::RBrace => count -= 1,
            _ => {}
          }
          continue;
        };
        if innermost.closed {
          return Err(std::format!(
            "the module at {:?} holds `{}` at {:?}, after its braces have closed",
            innermost.node.text_range(),
            token.text(),
            token.text_range()
          ));
        }
        match kind {
          K::LBrace => {
            // The first `{` of the node is the module's own, wherever the tree has put it.
            if innermost.base.is_none() {
              innermost.base = Some(count);
            }
            count += 1;
          }
          K::RBrace => {
            let Some(base) = innermost.base else {
              return Err(std::format!(
                "the module at {:?} holds a `}}` at {:?}, in front of its `{{`",
                innermost.node.text_range(),
                token.text_range()
              ));
            };
            count -= 1;
            // The count from the module's `{` is back at zero: this is its own `}`, and nothing
            // but trivia may follow it in the node.
            if count == base {
              innermost.closed = true;
            }
          }
          _ => {}
        }
      }
      WalkEvent::Leave(NodeOrToken::Token(_)) => {}
    }
  }
  Ok(modules)
}

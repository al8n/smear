//! Every module of a GraphQLx lossless tree, held to the braces as written.
//!
//! While a brace is open, a closing brace is taken only by the production that owns it, and that
//! rule is what makes a module's node faithful to the braces: counting braces alone from the
//! node's own `{`, the count first returns to zero at the node's own `}`, which is the last token
//! of the node that is no trivia, and a node with no `}` of its own never returns to zero.
//! [`faithful`] checks every module of a tree in one walk, so a sweep can afford it on every tree
//! it builds, and answers how many modules it read and how many of them hold an `Error`: a sweep
//! that never met a broken module witnessed nothing.
//!
//! Shared by the sweeps that parse broken texts: `lossless_x_mutation.rs`,
//! `lossless_x_roundtrip.rs` and `lossless_x_parity.rs`.

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
              "the module at {:?} holds a module after its own `}}`",
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
        let at = innermost.node.text_range();
        if innermost.closed {
          return Err(std::format!(
            "the module at {at:?} holds `{}` after its own `}}`",
            token.text()
          ));
        }
        let own: bool = token.parent().as_ref() == Some(&innermost.node);
        match kind {
          K::LBrace => {
            match (own, innermost.base) {
              (true, None) => innermost.base = Some(count),
              (true, Some(_)) => {
                return Err(std::format!(
                  "the module at {at:?} holds a second `{{` of its own"
                ));
              }
              (false, None) => {
                return Err(std::format!(
                  "the module at {at:?} holds a `{{` in front of its own"
                ));
              }
              (false, Some(_)) => {}
            }
            count += 1;
          }
          K::RBrace => {
            count -= 1;
            match innermost.base {
              None => {
                return Err(std::format!(
                  "the module at {at:?} holds a `}}` in front of its `{{`"
                ));
              }
              Some(base) if count == base => {
                if !own {
                  return Err(std::format!(
                    "the braces of the module at {at:?} close at a `}}` that is not its own, at \
                     {:?}",
                    token.text_range()
                  ));
                }
                innermost.closed = true;
              }
              Some(_) if own => {
                return Err(std::format!(
                  "the module at {at:?} holds a `}}` of its own that closes none of its braces"
                ));
              }
              Some(_) => {}
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

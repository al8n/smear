use super::SyntaxErrorKind;
use tokora::diagnostic::Severity;

/// Whether `text` names a source position: a `[0-9]+..[0-9]+` range, or one of the words a
/// position is spelled with. The acceptance suites hold every rendered syntax message to this.
fn names_a_position(text: &str) -> bool {
  let bytes = text.as_bytes();
  let range = bytes.windows(2).enumerate().any(|(i, pair)| {
    pair == b".."
      && i > 0
      && bytes[i - 1].is_ascii_digit()
      && bytes.get(i + 2).is_some_and(u8::is_ascii_digit)
  });
  range
    || ["byte", "offset", "position", " at "]
      .iter()
      .any(|word| text.contains(word))
}

#[test]
fn the_inventory_lists_twenty_four_distinct_kinds() {
  assert_eq!(SyntaxErrorKind::ALL.len(), 24);
  for (i, a) in SyntaxErrorKind::ALL.iter().enumerate() {
    for b in &SyntaxErrorKind::ALL[i + 1..] {
      assert_ne!(a, b, "{a:?} is listed twice");
    }
  }
}

#[test]
fn codes_are_distinct_kebab_case_and_in_the_syntax_family() {
  for (i, kind) in SyntaxErrorKind::ALL.iter().enumerate() {
    let code = kind.code().as_str();
    let rule = code
      .strip_prefix("smear::syntax::")
      .unwrap_or_else(|| panic!("{kind:?} answers {code}, outside `smear::syntax`"));
    assert!(
      !rule.is_empty()
        && !rule.starts_with('-')
        && !rule.ends_with('-')
        && !rule.contains("--")
        && rule
          .bytes()
          .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'-'),
      "{code} is not kebab-case"
    );
    for other in &SyntaxErrorKind::ALL[i + 1..] {
      assert_ne!(
        code,
        other.code().as_str(),
        "{kind:?} and {other:?} share a code"
      );
    }
  }
}

#[test]
fn messages_are_lowercase_unpunctuated_and_positionless() {
  for kind in SyntaxErrorKind::ALL {
    let message = kind.message();
    assert!(
      message.starts_with(|c: char| c.is_ascii_lowercase()),
      "{kind:?}: {message:?} does not start lowercase"
    );
    assert!(
      !message.ends_with('.'),
      "{kind:?}: {message:?} ends in a period"
    );
    assert!(
      !names_a_position(message),
      "{kind:?}: {message:?} names a position"
    );
    let mut rendered = std::string::String::new();
    core::fmt::Write::write_fmt(&mut rendered, format_args!("{kind}")).expect("a string sink");
    assert_eq!(
      rendered, message,
      "{kind:?} displays as something other than its message"
    );
  }
}

#[test]
fn every_kind_is_an_error_and_every_help_says_something() {
  for kind in SyntaxErrorKind::ALL {
    assert_eq!(kind.severity(), Severity::Error, "{kind:?}");
    if let Some(help) = kind.help() {
      assert!(!help.is_empty(), "{kind:?} has an empty help line");
      assert!(!help.ends_with('.'), "{kind:?}: {help:?} ends in a period");
      assert!(
        !names_a_position(help),
        "{kind:?}: {help:?} names a position"
      );
    }
  }
}

#[test]
fn the_position_predicate_is_a_check() {
  assert!(names_a_position("unexpected token at 3..5"));
  assert!(names_a_position("refused 12..40"));
  assert!(names_a_position("the byte after it"));
  assert!(!names_a_position("more than 6 hex digits"));
  assert!(!names_a_position("expected `...`"));
}

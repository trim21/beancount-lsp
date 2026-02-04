use beancount_lsp::{Document, account_at_position, build_document};
use std::str::FromStr;
use tower_lsp_server::ls_types::{Position, Uri as Url};

fn build_doc(uri: &Url, content: &str) -> Document {
    build_document(content.to_string(), uri.as_str()).expect("build doc")
}

#[test]
fn finds_account_and_range() {
    let uri = Url::from_str("file:///account.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&uri, content);

    let cursor = Position::new(0, 20);
    let (account, range) = account_at_position(&doc, cursor).expect("expected account hit");

    assert_eq!(account, "Assets:Cash");
    assert_eq!(range.start, Position::new(0, 16));
    assert_eq!(range.end, Position::new(0, 27));
}

#[test]
fn finds_account_when_cursor_at_token_end() {
    let uri = Url::from_str("file:///account.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&uri, content);

    // Cursor immediately after the account token (no trailing whitespace)
    let cursor = Position::new(0, 27);
    let (account, range) = account_at_position(&doc, cursor).expect("expected account hit");

    assert_eq!(account, "Assets:Cash");
    assert_eq!(range.start, Position::new(0, 16));
    assert_eq!(range.end, Position::new(0, 27));
}

#[test]
fn returns_none_outside_account_nodes() {
    let uri = Url::from_str("file:///account.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n2023-01-02 * \"Payee\" \"Narration\"\n";
    let doc = build_doc(&uri, content);

    let cursor = Position::new(1, 14); // inside payee string
    assert!(account_at_position(&doc, cursor).is_none());
}

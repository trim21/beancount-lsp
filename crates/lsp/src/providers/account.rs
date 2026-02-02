use beancount_parser::ast::{self, CustomValueKind};
use tower_lsp::lsp_types::{Position, Range};

use crate::doc::Document;
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

fn text_account_at_position(doc: &Document, position: Position) -> Option<(String, Range)> {
    let byte_idx = lsp_position_to_byte(&doc.rope, position)?;

    let line = doc.rope.byte_to_line(byte_idx);
    let line_start = doc.rope.line_to_byte(line);
    let line_end = if line + 1 < doc.rope.len_lines() {
        doc.rope.line_to_byte(line + 1)
    } else {
        doc.rope.len_bytes()
    };

    if byte_idx < line_start || byte_idx > line_end {
        return None;
    }

    let rel = byte_idx - line_start;
    let line_slice = doc.rope.byte_slice(line_start..line_end).to_string();
    if rel > line_slice.len() {
        return None;
    }

    let mut start = 0;
    for (idx, ch) in line_slice.char_indices() {
        if idx >= rel {
            break;
        }
        if ch.is_whitespace() {
            start = idx + ch.len_utf8();
        }
    }

    let mut end = line_slice.len();
    for (idx, ch) in line_slice.char_indices() {
        if idx < rel {
            continue;
        }
        if ch.is_whitespace() {
            end = idx;
            break;
        }
    }

    let token = line_slice.get(start..end)?.trim();
    if token.is_empty() || !token.contains(':') {
        return None;
    }

    let start_byte = line_start + start;
    let end_byte = line_start + end;
    let start_pos = byte_to_lsp_position(&doc.rope, start_byte)?;
    let end_pos = byte_to_lsp_position(&doc.rope, end_byte)?;

    Some((
        token.to_owned(),
        Range {
            start: start_pos,
            end: end_pos,
        },
    ))
}

fn span_contains(span: ast::Span, byte_idx: usize) -> bool {
    span.start <= byte_idx && byte_idx <= span.end
}

fn account_from_span(
    doc: &Document,
    span: ast::Span,
    account: &str,
) -> Option<(String, Range)> {
    let start = byte_to_lsp_position(&doc.rope, span.start)?;
    let end = byte_to_lsp_position(&doc.rope, span.end)?;
    Some((account.to_owned(), Range { start, end }))
}

fn account_from_with_span(
    doc: &Document,
    byte_idx: usize,
    account: &ast::WithSpan<&str>,
) -> Option<(String, Range)> {
    if !span_contains(account.span, byte_idx) {
        return None;
    }

    account_from_span(doc, account.span, account.content)
}

/// Find the account node (if any) at the given position and return its text and range.
pub fn account_at_position(doc: &Document, position: Position) -> Option<(String, Range)> {
    let byte_idx = lsp_position_to_byte(&doc.rope, position)?;

    for directive in doc.ast() {
        match directive {
            ast::Directive::Open(open) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &open.account) {
                    return Some(hit);
                }
            }
            ast::Directive::Close(close) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &close.account) {
                    return Some(hit);
                }
            }
            ast::Directive::Balance(balance) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &balance.account) {
                    return Some(hit);
                }
            }
            ast::Directive::Pad(pad) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &pad.account) {
                    return Some(hit);
                }
                if let Some(hit) = account_from_with_span(doc, byte_idx, &pad.from_account) {
                    return Some(hit);
                }
            }
            ast::Directive::Note(note) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &note.account) {
                    return Some(hit);
                }
            }
            ast::Directive::Document(document) => {
                if let Some(hit) = account_from_with_span(doc, byte_idx, &document.account) {
                    return Some(hit);
                }
            }
            ast::Directive::Transaction(txn) => {
                for posting in &txn.postings {
                    if let Some(hit) =
                        account_from_with_span(doc, byte_idx, &posting.account)
                    {
                        return Some(hit);
                    }
                }
            }
            ast::Directive::Custom(custom) => {
                for value in &custom.values {
                    if value.kind == CustomValueKind::Account
                        && span_contains(value.raw.span, byte_idx)
                    {
                        if let Some(hit) =
                            account_from_span(doc, value.raw.span, value.raw.content)
                        {
                            return Some(hit);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Fallback to a plain-text heuristic when the parse tree does not yield an account node.
    text_account_at_position(doc, position)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::{Position, Url};

    fn build_doc(uri: &Url, content: &str) -> Document {
        crate::doc::build_document(content, uri.as_str()).expect("build document")
    }

    #[test]
    fn finds_account_and_range() {
        let uri = Url::parse("file:///account.bean").unwrap();
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
        let uri = Url::parse("file:///account.bean").unwrap();
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
        let uri = Url::parse("file:///account.bean").unwrap();
        let content = "2023-01-01 open Assets:Cash\n2023-01-02 * \"Payee\" \"Narration\"\n";
        let doc = build_doc(&uri, content);

        let cursor = Position::new(1, 14); // inside payee string
        assert!(account_at_position(&doc, cursor).is_none());
    }
}

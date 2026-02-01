use beancount_tree_sitter::NodeKind;
use tower_lsp::lsp_types::{Position, Range};

use crate::server::Document;
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

/// Find the account node (if any) at the given position and return its text and range.
pub fn account_at_position(doc: &Document, position: Position) -> Option<(String, Range)> {
    let byte_idx = lsp_position_to_byte(&doc.rope, position)?;

    let mut stack = vec![doc.tree.root_node()];
    while let Some(node) = stack.pop() {
        let start_byte = node.start_byte();
        let end_byte = node.end_byte();

        // Only descend into nodes that could contain the cursor.
        if !(start_byte <= byte_idx && byte_idx <= end_byte) {
            continue;
        }

        if NodeKind::from(node.kind()) == NodeKind::Account {
            let text = doc
                .rope
                .byte_slice(start_byte..end_byte)
                .to_string()
                .trim()
                .to_owned();

            if text.is_empty() {
                return None;
            }

            let start = byte_to_lsp_position(&doc.rope, start_byte)?;
            let end = byte_to_lsp_position(&doc.rope, end_byte)?;

            return Some((
                text,
                Range {
                    start,
                    end,
                },
            ));
        }

        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            stack.push(child);
        }
    }

    // Fallback to a plain-text heuristic when the parse tree does not yield an account node.
    text_account_at_position(doc, position)
}

#[cfg(test)]
mod tests {
    use super::*;
    use beancount_parser::{core, parse_str};
    use beancount_tree_sitter::{language, tree_sitter};
    use ropey::Rope;
    use tower_lsp::lsp_types::{Position, Url};

    fn build_doc(uri: &Url, content: &str) -> Document {
        let directives =
            core::normalize_directives(parse_str(content, uri.as_str()).unwrap()).unwrap();
        let rope = Rope::from_str(content);
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language()).unwrap();
        let tree = parser.parse(content, None).unwrap();
        Document {
            directives,
            content: content.to_owned(),
            rope,
            tree,
        }
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

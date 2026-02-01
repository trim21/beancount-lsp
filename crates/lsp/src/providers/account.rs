use beancount_tree_sitter::NodeKind;
use tower_lsp::lsp_types::{Position, Range};

use crate::server::Document;
use crate::text::{
    byte_to_lsp_position, lsp_position_to_byte, lsp_position_to_ts_point, ts_point_to_lsp_position,
};

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
    if token.is_empty() {
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
    let point = lsp_position_to_ts_point(&doc.rope, position)?;
    let node = doc
        .tree
        .root_node()
        .descendant_for_point_range(point, point)?;

    let mut current = node;
    let account_node = loop {
        match NodeKind::from(current.kind()) {
            NodeKind::Account => break current,
            _ => {
                if let Some(parent) = current.parent() {
                    current = parent;
                } else {
                    return None;
                }
            }
        }
    };

    let start = ts_point_to_lsp_position(&doc.rope, account_node.start_position())?;
    let end = ts_point_to_lsp_position(&doc.rope, account_node.end_position())?;

    let account_text = doc
        .rope
        .byte_slice(account_node.start_byte()..account_node.end_byte())
        .to_string();
    let account = account_text.trim().to_owned();
    if account.is_empty() {
        return text_account_at_position(doc, position);
    }

    Some((account, Range { start, end })).or_else(|| text_account_at_position(doc, position))
}

use beancount_parser::ast;
use tower_lsp_server::ls_types::{Position, Range};

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

    fn span_contains(span: ast::Span, byte_idx: usize) -> bool {
        span.start <= byte_idx && byte_idx < span.end
    }

    let to_range = |span: ast::Span| {
        let start = byte_to_lsp_position(&doc.rope, span.start)?;
        let end = byte_to_lsp_position(&doc.rope, span.end)?;
        Some(Range { start, end })
    };

    if let Some(directive) = doc.directive_at_offset(byte_idx) {
        match directive {
            ast::Directive::Open(open) => {
                if span_contains(open.account.span, byte_idx) {
                    let range = to_range(open.account.span)?;
                    return Some((open.account.content.to_string(), range));
                }
            }
            ast::Directive::Close(close) => {
                if span_contains(close.account.span, byte_idx) {
                    let range = to_range(close.account.span)?;
                    return Some((close.account.content.to_string(), range));
                }
            }
            ast::Directive::Balance(balance) => {
                if span_contains(balance.account.span, byte_idx) {
                    let range = to_range(balance.account.span)?;
                    return Some((balance.account.content.to_string(), range));
                }
            }
            ast::Directive::Pad(pad) => {
                if span_contains(pad.account.span, byte_idx) {
                    let range = to_range(pad.account.span)?;
                    return Some((pad.account.content.to_string(), range));
                }
                if span_contains(pad.from_account.span, byte_idx) {
                    let range = to_range(pad.from_account.span)?;
                    return Some((pad.from_account.content.to_string(), range));
                }
            }
            ast::Directive::Transaction(tx) => {
                for posting in &tx.postings {
                    if span_contains(posting.account.span, byte_idx) {
                        let range = to_range(posting.account.span)?;
                        return Some((posting.account.content.to_string(), range));
                    }
                }
            }
            ast::Directive::Note(note) => {
                if span_contains(note.account.span, byte_idx) {
                    let range = to_range(note.account.span)?;
                    return Some((note.account.content.to_string(), range));
                }
            }
            ast::Directive::Document(doc_directive) => {
                if span_contains(doc_directive.account.span, byte_idx) {
                    let range = to_range(doc_directive.account.span)?;
                    return Some((doc_directive.account.content.to_string(), range));
                }
            }
            _ => {}
        }
    }

    // Fallback to a plain-text heuristic when structured spans don't match.
    text_account_at_position(doc, position)
}

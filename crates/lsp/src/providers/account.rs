use beancount_parser::ast;
use tower_lsp_server::ls_types::{Position, Range};

use crate::server::Document;
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

fn text_account_in_span(
    doc: &Document,
    byte_idx: usize,
    span: std::ops::Range<usize>,
) -> Option<(String, Range)> {
    if byte_idx < span.start || byte_idx > span.end {
        return None;
    }

    let rel = byte_idx.checked_sub(span.start)?;
    let slice = doc.rope.byte_slice(span.clone()).to_string();
    if rel > slice.len() {
        return None;
    }

    let mut start = 0;
    for (idx, ch) in slice.char_indices() {
        if idx >= rel {
            break;
        }
        if ch.is_whitespace() {
            start = idx + ch.len_utf8();
        }
    }

    let mut end = slice.len();
    for (idx, ch) in slice.char_indices() {
        if idx < rel {
            continue;
        }
        if ch.is_whitespace() {
            end = idx;
            break;
        }
    }

    let token = slice.get(start..end)?.trim();
    if token.is_empty() || !token.contains(':') {
        return None;
    }

    let start_byte = span.start + start;
    let end_byte = span.start + end;
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
        span.start <= byte_idx && byte_idx <= span.end
    }

    let to_range = |span: ast::Span| {
        let start = byte_to_lsp_position(&doc.rope, span.start)?;
        let end = byte_to_lsp_position(&doc.rope, span.end)?;
        Some(Range { start, end })
    };

    let directive = doc
        .directive_at_offset(byte_idx)
        .or_else(|| byte_idx.checked_sub(1).and_then(|idx| doc.directive_at_offset(idx)))?;

    match directive {
        ast::Directive::Open(open) => span_contains(open.account.span, byte_idx)
            .then(|| to_range(open.account.span))
            .flatten()
            .map(|range| (open.account.content.to_string(), range)),
        ast::Directive::Close(close) => span_contains(close.account.span, byte_idx)
            .then(|| to_range(close.account.span))
            .flatten()
            .map(|range| (close.account.content.to_string(), range)),
        ast::Directive::Balance(balance) => span_contains(balance.account.span, byte_idx)
            .then(|| to_range(balance.account.span))
            .flatten()
            .map(|range| (balance.account.content.to_string(), range)),
        ast::Directive::Pad(pad) => {
            if span_contains(pad.account.span, byte_idx) {
                to_range(pad.account.span)
                    .map(|range| (pad.account.content.to_string(), range))
            } else if span_contains(pad.from_account.span, byte_idx) {
                to_range(pad.from_account.span)
                    .map(|range| (pad.from_account.content.to_string(), range))
            } else {
                None
            }
        }
        ast::Directive::Transaction(tx) => tx.postings.iter().find_map(|posting| {
            span_contains(posting.account.span, byte_idx).then(|| {
                to_range(posting.account.span)
                    .map(|range| (posting.account.content.to_string(), range))
            })?
        }),
        ast::Directive::Note(note) => span_contains(note.account.span, byte_idx)
            .then(|| to_range(note.account.span))
            .flatten()
            .map(|range| (note.account.content.to_string(), range)),
        ast::Directive::Document(doc_directive) => span_contains(doc_directive.account.span, byte_idx)
            .then(|| to_range(doc_directive.account.span))
            .flatten()
            .map(|range| (doc_directive.account.content.to_string(), range)),
        ast::Directive::Raw(raw) => text_account_in_span(doc, byte_idx, raw.span.start..raw.span.end),
        _ => None,
    }
}

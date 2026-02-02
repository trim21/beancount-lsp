use std::convert::TryFrom;

use beancount_tree_sitter::tree_sitter;
use ropey::Rope;
use tower_lsp_server::ls_types::{Position, Range};

/// Convert an LSP UTF-16 position to a UTF-8 byte offset in the document.
/// Returns None when the position is outside the current text (line or column too large).
pub fn lsp_position_to_byte(content: &Rope, position: Position) -> Option<usize> {
    let line = usize::try_from(position.line).ok()?;
    if line >= content.len_lines() {
        return None;
    }

    let line_start_char = content.line_to_char(line);
    let line_start_utf16 = content.char_to_utf16_cu(line_start_char);

    let column_utf16 = usize::try_from(position.character).ok()?;
    let target_utf16 = line_start_utf16 + column_utf16;

    let line_end_char = if line + 1 < content.len_lines() {
        content.line_to_char(line + 1)
    } else {
        content.len_chars()
    };
    let line_end_utf16 = content.char_to_utf16_cu(line_end_char);
    if target_utf16 > line_end_utf16 {
        return None;
    }

    let target_char = content.utf16_cu_to_char(target_utf16);
    Some(content.char_to_byte(target_char))
}

/// Convert a UTF-8 byte offset to an LSP UTF-16 position.
/// Returns None when the byte offset is past the end of the document.
pub fn byte_to_lsp_position(content: &Rope, byte_idx: usize) -> Option<Position> {
    if byte_idx > content.len_bytes() {
        return None;
    }

    let line = content.byte_to_line(byte_idx);
    let line_start_char = content.line_to_char(line);
    let line_start_utf16 = content.char_to_utf16_cu(line_start_char);

    let char_idx = content.byte_to_char(byte_idx);
    let utf16_at_char = content.char_to_utf16_cu(char_idx);
    let character_utf16 = utf16_at_char.checked_sub(line_start_utf16)?;

    Some(Position::new(
        u32::try_from(line).ok()?,
        u32::try_from(character_utf16).ok()?,
    ))
}

/// Convert a UTF-8 byte span to an LSP range (UTF-16). Returns None when either end is invalid
/// or when the end precedes the start.
#[allow(dead_code)]
pub fn byte_span_to_lsp_range(content: &Rope, start_byte: usize, end_byte: usize) -> Option<Range> {
    if end_byte < start_byte {
        return None;
    }
    let start = byte_to_lsp_position(content, start_byte)?;
    let end = byte_to_lsp_position(content, end_byte)?;
    Some(Range { start, end })
}

/// Convert an LSP UTF-16 position to a tree-sitter point (row, byte column).
pub fn lsp_position_to_ts_point(content: &Rope, position: Position) -> Option<tree_sitter::Point> {
    let byte_idx = lsp_position_to_byte(content, position)?;
    let row = content.byte_to_line(byte_idx);
    let row_start = content.line_to_byte(row);
    let column = byte_idx.checked_sub(row_start)?;
    Some(tree_sitter::Point { row, column })
}

/// Convert a tree-sitter point (row, byte column) to an LSP UTF-16 position.
pub fn ts_point_to_lsp_position(content: &Rope, point: tree_sitter::Point) -> Option<Position> {
    if point.row >= content.len_lines() {
        return None;
    }

    let line_start = content.line_to_byte(point.row);
    let byte_idx = line_start.checked_add(point.column)?;
    byte_to_lsp_position(content, byte_idx)
}

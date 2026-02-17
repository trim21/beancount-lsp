use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use beancount_core as core;
use beancount_parser::ast;
use tower_lsp_server::ls_types::{
  CompletionItem, CompletionItemKind, CompletionList, CompletionParams,
  CompletionTextEdit, Position, Range, TextEdit, Uri as Url,
};

use crate::providers::account::account_at_position;
use crate::server::{Document, documents_bfs, find_document};
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

const DATE_KEYWORDS: &[&str] =
  &["custom", "balance", "open", "close", "note", "price", "pad"];
const ROOT_KEYWORDS: &[&str] = &[
  "include", "option", "pushtag", "poptag", "pushmeta", "popmeta",
];

#[derive(Debug, Clone, PartialEq, Eq)]
enum CompletionMode {
  Account {
    prefix: String,
    range: Range,
  },
  Currency {
    prefix: String,
    range: Range,
  },
  Keyword {
    prefix: String,
    range: Range,
    variants: &'static [&'static str],
  },
  Tag {
    prefix: String,
    range: Range,
  },
  Link {
    prefix: String,
    range: Range,
  },
}

fn token_prefix_at_position(
  doc: &Document,
  position: Position,
  require_nonzero_start: bool,
  allow_empty_token: bool,
) -> Option<(String, Range)> {
  let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;

  let line = doc.rope.byte_to_line(cursor_byte);
  let line_start = doc.rope.line_to_byte(line);
  let line_end = if line + 1 < doc.rope.len_lines() {
    doc.rope.line_to_byte(line + 1)
  } else {
    doc.rope.len_bytes()
  };

  if cursor_byte < line_start || cursor_byte > line_end {
    return None;
  }

  let rel = cursor_byte - line_start;
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

  if start > end || rel > end {
    return None;
  }

  if require_nonzero_start && start == 0 {
    return None;
  }

  let token = line_slice.get(start..end)?;
  let prefix = line_slice.get(start..rel)?.to_owned();
  if token.is_empty() && !allow_empty_token {
    return None;
  }

  let start_byte = line_start + start;
  let end_byte = line_start + end;
  let start_pos = byte_to_lsp_position(&doc.rope, start_byte)?;
  let end_pos = byte_to_lsp_position(&doc.rope, end_byte)?;

  Some((
    prefix,
    Range {
      start: start_pos,
      end: end_pos,
    },
  ))
}

fn marker_prefix_at_position(
  doc: &Document,
  position: Position,
  marker: char,
) -> Option<(String, Range)> {
  let (_token_prefix, token_range) =
    token_prefix_at_position(doc, position, true, true)?;

  let token_start = lsp_position_to_byte(&doc.rope, token_range.start)?;
  let token_end = lsp_position_to_byte(&doc.rope, token_range.end)?;
  if token_start >= token_end {
    return None;
  }

  let token_text = doc.rope.byte_slice(token_start..token_end).to_string();
  if !token_text.starts_with(marker) {
    return None;
  }

  let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;
  let after_hash = token_start + 1;
  if cursor_byte < after_hash {
    return None;
  }

  let prefix = doc.rope.byte_slice(after_hash..cursor_byte).to_string();

  let start_pos = byte_to_lsp_position(&doc.rope, after_hash)?;
  Some((
    prefix,
    Range {
      start: start_pos,
      end: token_range.end,
    },
  ))
}

fn is_within_account_context(doc: &Document, byte_idx: usize) -> bool {
  let span_contains = |span: ast::Span| span.start <= byte_idx && byte_idx <= span.end;
  let directive = doc.directive_at_offset(byte_idx).or_else(|| {
    byte_idx
      .checked_sub(1)
      .and_then(|idx| doc.directive_at_offset(idx))
  });

  match directive {
    Some(ast::Directive::Open(open)) => span_contains(open.account.span),
    Some(ast::Directive::Balance(balance)) => span_contains(balance.account.span),
    Some(ast::Directive::Pad(pad)) => {
      span_contains(pad.account.span) || span_contains(pad.from_account.span)
    }
    Some(ast::Directive::Close(close)) => span_contains(close.account.span),
    Some(ast::Directive::Transaction(tx)) => {
      tx.postings.iter().any(|p| span_contains(p.account.span))
    }
    Some(ast::Directive::Raw(_raw)) => {
      let Some(position) = byte_to_lsp_position(&doc.rope, byte_idx) else {
        return false;
      };

      // Try the account finder, which includes a text fallback for partial raw content.
      if account_at_position(doc, position).is_some() {
        return true;
      }

      // When the parser falls back to Raw directives, the cursor may still be inside a
      // transaction body. In that case we still want to offer account completion.
      is_in_transaction_body_line(doc, byte_idx)
    }
    None => is_in_transaction_body_line(doc, byte_idx),
    _ => false,
  }
}

fn line_indent(doc: &Document, line: usize) -> Option<(usize, bool)> {
  if line >= doc.rope.len_lines() {
    return None;
  }

  let line_start = doc.rope.line_to_byte(line);
  let line_end = if line + 1 < doc.rope.len_lines() {
    doc.rope.line_to_byte(line + 1)
  } else {
    doc.rope.len_bytes()
  };

  let text = doc.rope.byte_slice(line_start..line_end).to_string();
  let trimmed = text.trim_start();
  let indent = text.len().saturating_sub(trimmed.len());
  let is_empty = trimmed.trim().is_empty();
  Some((indent, is_empty))
}

fn directive_starts_indented(doc: &Document, directive: &ast::Directive<'_>) -> bool {
  let span = Document::directive_span(directive);
  let Some(start_pos) = byte_to_lsp_position(&doc.rope, span.start) else {
    return false;
  };
  let Ok(line) = usize::try_from(start_pos.line) else {
    return false;
  };
  line_indent(doc, line)
    .map(|(indent, _)| indent > 0)
    .unwrap_or(false)
}

fn directive_index_before_line(doc: &Document, line: usize) -> Option<usize> {
  let mut last_idx = None;
  for (idx, directive) in doc.ast().iter().enumerate() {
    let span = Document::directive_span(directive);
    let Some(start_pos) = byte_to_lsp_position(&doc.rope, span.start) else {
      continue;
    };
    let Ok(start_line) = usize::try_from(start_pos.line) else {
      continue;
    };

    if start_line < line {
      last_idx = Some(idx);
    }
  }
  last_idx
}

fn is_in_transaction_body_line(doc: &Document, byte_idx: usize) -> bool {
  let Some(position) = byte_to_lsp_position(&doc.rope, byte_idx) else {
    return false;
  };

  let Ok(line) = usize::try_from(position.line) else {
    return false;
  };

  // Only for indented lines.
  let Some((indent, _is_empty)) = line_indent(doc, line) else {
    return false;
  };
  if indent == 0 {
    return false;
  }

  // The transaction header may be in a previous AST directive (Transaction), while the current
  // line is a broken/empty posting line that might not belong to any directive span.
  // Find the previous directive by line number and scan backwards.
  let Some(idx) = directive_index_before_line(doc, line) else {
    return false;
  };

  let directives = doc.ast();
  for prev in directives[..=idx].iter().rev() {
    match prev {
      ast::Directive::Transaction(_) => return true,
      // Indented raw/comment lines can be part of a transaction body; keep scanning.
      ast::Directive::Raw(_) | ast::Directive::Comment(_) => {
        if directive_starts_indented(doc, prev) {
          continue;
        }
        return false;
      }
      _ => return false,
    }
  }

  false
}

fn is_tag_or_link_capable_directive(
  doc: &Document,
  byte_idx: usize,
  line_slice: &str,
  indent: usize,
  position: Position,
) -> bool {
  let directive = doc.directive_at_offset(byte_idx).or_else(|| {
    byte_idx
      .checked_sub(1)
      .and_then(|idx| doc.directive_at_offset(idx))
  });

  match directive {
    Some(ast::Directive::Transaction(_)) | Some(ast::Directive::Document(_)) => true,
    Some(ast::Directive::Raw(_)) => {
      has_date_prefix(line_slice, indent)
        && (marker_prefix_at_position(doc, position, '#').is_some()
          || marker_prefix_at_position(doc, position, '^').is_some())
    }
    _ => false,
  }
}

fn is_probably_metadata_key(token: &str) -> bool {
  // Beancount transaction metadata keys are typically like `foo:` (lowercase + trailing colon).
  // We avoid treating those as account completion contexts.
  if !token.ends_with(':') {
    return false;
  }

  let key = &token[..token.len().saturating_sub(1)];
  if key.is_empty() {
    return false;
  }

  key
    .chars()
    .all(|ch| ch.is_ascii_lowercase() || ch.is_ascii_digit() || ch == '-' || ch == '_')
}

fn is_number_like_token(token: &str) -> bool {
  let trimmed = token.trim();
  if trimmed.is_empty() {
    return false;
  }

  let has_digit = trimmed.chars().any(|ch| ch.is_ascii_digit());
  if !has_digit {
    return false;
  }

  trimmed.chars().all(|ch| {
    ch.is_ascii_digit()
      || matches!(ch, '+' | '-' | '.' | ',' | '_' | '(' | ')' | '*' | '/')
  })
}

fn token_index_at_start(line: &str, token_start: usize) -> Option<usize> {
  let mut index = 0usize;
  let mut current_start: Option<usize> = None;

  for (idx, ch) in line.char_indices() {
    if ch.is_whitespace() {
      if let Some(start) = current_start.take() {
        if start == token_start {
          return Some(index);
        }
        index += 1;
      }
    } else if current_start.is_none() {
      current_start = Some(idx);
    }
  }

  if let Some(start) = current_start
    && start == token_start {
      return Some(index);
    }

  None
}

fn previous_token_before(line: &str, token_start: usize) -> Option<String> {
  let prefix = line.get(..token_start)?;
  let mut last = None;
  for token in prefix.split_whitespace() {
    last = Some(token.to_string());
  }
  last
}

fn is_within_currency_context(doc: &Document, byte_idx: usize) -> bool {
  let span_contains = |span: ast::Span| span.start <= byte_idx && byte_idx <= span.end;
  let directive = doc.directive_at_offset(byte_idx).or_else(|| {
    byte_idx
      .checked_sub(1)
      .and_then(|idx| doc.directive_at_offset(idx))
  });

  match directive {
    Some(ast::Directive::Balance(balance)) => balance
      .amount
      .currency
      .as_ref()
      .map(|currency| span_contains(currency.span))
      .unwrap_or(false),
    Some(ast::Directive::Commodity(commodity)) => {
      span_contains(commodity.currency.span)
    }
    Some(ast::Directive::Price(price)) => {
      span_contains(price.currency.span)
        || price
          .amount
          .currency
          .as_ref()
          .map(|currency| span_contains(currency.span))
          .unwrap_or(false)
    }
    Some(ast::Directive::Transaction(tx)) => tx.postings.iter().any(|posting| {
      posting
        .amount
        .as_ref()
        .and_then(|amount| amount.currency.as_ref())
        .map(|currency| span_contains(currency.span))
        .unwrap_or(false)
        || posting
          .cost_spec
          .as_ref()
          .and_then(|cost| cost.amount.as_ref())
          .and_then(|amount| amount.currency.as_ref())
          .map(|currency| span_contains(currency.span))
          .unwrap_or(false)
        || posting
          .price_annotation
          .as_ref()
          .and_then(|amount| amount.currency.as_ref())
          .map(|currency| span_contains(currency.span))
          .unwrap_or(false)
    }),
    _ => false,
  }
}

fn is_probably_currency_context(
  doc: &Document,
  position: Position,
  line_slice: &str,
  line_start: usize,
  indent: usize,
  has_date: bool,
) -> Option<(String, Range)> {
  let (prefix, range) = token_prefix_at_position(doc, position, true, true)?;
  let token_start = lsp_position_to_byte(&doc.rope, range.start)?;
  if token_start < line_start {
    return None;
  }

  let token_start_rel = token_start - line_start;
  if token_start_rel > line_slice.len() {
    return None;
  }

  let token_end = lsp_position_to_byte(&doc.rope, range.end)?;
  let token = doc.rope.byte_slice(token_start..token_end).to_string();

  if is_probably_metadata_key(token.trim()) {
    return None;
  }

  let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;
  if is_within_currency_context(doc, cursor_byte) {
    return Some((prefix, range));
  }

  if indent == 0 && has_date {
    let token_idx = token_index_at_start(line_slice, token_start_rel)?;
    let keyword = line_slice
      .split_whitespace()
      .nth(1)
      .map(|v| v.to_ascii_lowercase());

    if let Some(keyword) = keyword {
      if keyword == "commodity" && token_idx == 2 {
        return Some((prefix, range));
      }

      if keyword == "price" && (token_idx == 2 || token_idx >= 4) {
        return Some((prefix, range));
      }
    }
  }

  if let Some(previous_token) = previous_token_before(line_slice, token_start_rel)
    && is_number_like_token(&previous_token)
  {
    return Some((prefix, range));
  }

  None
}

fn has_date_prefix(line_slice: &str, indent: usize) -> bool {
  if line_slice.len() < indent + 10 {
    return false;
  }

  let bytes = line_slice.as_bytes();
  let window = &bytes[indent..indent + 10];
  let is_digit = |b: u8| b.is_ascii_digit();

  if !(is_digit(window[0])
    && is_digit(window[1])
    && is_digit(window[2])
    && is_digit(window[3])
    && window[4] == b'-'
    && is_digit(window[5])
    && is_digit(window[6])
    && window[7] == b'-'
    && is_digit(window[8])
    && is_digit(window[9]))
  {
    return false;
  }

  if line_slice.len() == indent + 10 {
    return true;
  }

  line_slice
    .get(indent + 10..)
    .and_then(|rest| rest.chars().next())
    .map(|c| c.is_whitespace())
    .unwrap_or(false)
}

fn determine_completion_context(
  doc: &Document,
  position: Position,
) -> Option<CompletionMode> {
  let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;

  let line = doc.rope.byte_to_line(cursor_byte);
  let line_start = doc.rope.line_to_byte(line);
  let line_end = if line + 1 < doc.rope.len_lines() {
    doc.rope.line_to_byte(line + 1)
  } else {
    doc.rope.len_bytes()
  };

  let rel = cursor_byte - line_start;
  let line_slice = doc.rope.byte_slice(line_start..line_end).to_string();
  let trimmed = line_slice.trim_start();
  let indent = line_slice.len().saturating_sub(trimmed.len());
  let has_date = has_date_prefix(&line_slice, indent);

  let in_account_context = is_within_account_context(doc, cursor_byte);
  let supports_tag_link =
    is_tag_or_link_capable_directive(doc, cursor_byte, &line_slice, indent, position);

  let account_mode = || {
    if in_account_context
      && let Some((prefix, range)) = token_prefix_at_position(doc, position, true, true)
    {
      // Avoid treating amount/price fields as accounts; require token to start alphabetic.
      let token_start = lsp_position_to_byte(&doc.rope, range.start)?;
      let token_end = lsp_position_to_byte(&doc.rope, range.end)?;
      let token = doc.rope.byte_slice(token_start..token_end).to_string();
      if indent > 0 && range.start.character != u32::try_from(indent).ok()? {
        return None;
      }
      if is_probably_metadata_key(token.trim()) {
        return None;
      }
      if token.is_empty() {
        return Some(CompletionMode::Account { prefix, range });
      }
      if token
        .chars()
        .next()
        .map(|c| c.is_ascii_alphabetic())
        .unwrap_or(false)
      {
        return Some(CompletionMode::Account { prefix, range });
      }
    }

    None
  };

  if indent == 0 {
    // For non-indented directive lines, prioritize marker-based completion first
    // (`#tag` / `^link`), then account completion, and finally directive keywords.
    // Only Transaction/Document directives are eligible for tag/link completion.
    if has_date
      && supports_tag_link
      && let Some((prefix, range)) = marker_prefix_at_position(doc, position, '#')
    {
      return Some(CompletionMode::Tag { prefix, range });
    }

    if has_date
      && supports_tag_link
      && let Some((prefix, range)) = marker_prefix_at_position(doc, position, '^')
    {
      return Some(CompletionMode::Link { prefix, range });
    }

    if let Some(mode) = account_mode() {
      return Some(mode);
    }

    if let Some((prefix, range)) = is_probably_currency_context(
      doc,
      position,
      &line_slice,
      line_start,
      indent,
      has_date,
    ) {
      return Some(CompletionMode::Currency { prefix, range });
    }

    if has_date
      && rel >= indent + 10
      && !in_account_context
      && let Some((prefix, range)) =
        token_prefix_at_position(doc, position, false, true)
      && DATE_KEYWORDS
        .iter()
        .any(|label| fuzzy_match(label, &prefix))
    {
      return Some(CompletionMode::Keyword {
        prefix,
        range,
        variants: DATE_KEYWORDS,
      });
    }

    if !in_account_context
      && let Some((prefix, range)) =
        token_prefix_at_position(doc, position, false, true)
      && ROOT_KEYWORDS
        .iter()
        .any(|label| fuzzy_match(label, &prefix))
    {
      return Some(CompletionMode::Keyword {
        prefix,
        range,
        variants: ROOT_KEYWORDS,
      });
    }

    return None;
  }

  if has_date && supports_tag_link {
    for marker in ['#', '^'] {
      if let Some((prefix, range)) = marker_prefix_at_position(doc, position, marker) {
        return Some(match marker {
          '#' => CompletionMode::Tag { prefix, range },
          '^' => CompletionMode::Link { prefix, range },
          _ => unreachable!(),
        });
      }
    }
  }

  if let Some(mode) = account_mode() {
    return Some(mode);
  }

  if let Some((prefix, range)) = is_probably_currency_context(
    doc,
    position,
    &line_slice,
    line_start,
    indent,
    has_date,
  ) {
    return Some(CompletionMode::Currency { prefix, range });
  }

  None
}

fn fuzzy_match(candidate: &str, prefix: &str) -> bool {
  if prefix.is_empty() {
    return true;
  }

  let mut iter = candidate.chars();
  for ch in prefix.chars() {
    match iter.position(|c| c.eq_ignore_ascii_case(&ch)) {
      Some(_) => continue,
      None => return false,
    }
  }
  true
}

fn starts_with_ignore_ascii_case(candidate: &str, prefix: &str) -> bool {
  if prefix.is_empty() {
    return true;
  }

  let mut candidate_chars = candidate.chars();
  for prefix_char in prefix.chars() {
    let Some(candidate_char) = candidate_chars.next() else {
      return false;
    };

    if !candidate_char.eq_ignore_ascii_case(&prefix_char) {
      return false;
    }
  }

  true
}

pub fn completion(
  documents: &HashMap<Url, Arc<Document>>,
  root_uri: &Url,
  params: &CompletionParams,
) -> Option<CompletionList> {
  let uri = &params.text_document_position.text_document.uri;
  let position = params.text_document_position.position;
  let doc = find_document(documents, uri)?;

  let ctx = match determine_completion_context(doc.as_ref(), position) {
    Some(ctx) => ctx,
    None => {
      return None;
    }
  };

  spdlog::debug!("completion for {:?}", ctx);

  let mut items: Vec<CompletionItem> = match ctx {
    CompletionMode::Account {
      prefix,
      range: replace_range,
    } => {
      let mut accounts = HashSet::new();
      for (_, doc) in documents_bfs(documents, root_uri) {
        accounts.extend(doc.accounts.iter().cloned());
      }

      accounts
        .into_iter()
        .filter(|label| fuzzy_match(label, &prefix))
        .map(|label| CompletionItem {
          label: label.clone(),
          kind: Some(CompletionItemKind::VALUE),
          text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range,
            new_text: label.clone(),
          })),
          ..CompletionItem::default()
        })
        .collect()
    }
    CompletionMode::Currency {
      prefix,
      range: replace_range,
    } => {
      let docs: Vec<_> = documents_bfs(documents, root_uri)
        .into_iter()
        .map(|(_, doc)| doc)
        .collect();

      let mut currencies: HashSet<&str> = HashSet::new();
      for doc in &docs {
        currencies.extend(doc.currencies.iter().map(String::as_str));
      }

      currencies
        .into_iter()
        .filter(|label| starts_with_ignore_ascii_case(label, &prefix))
        .map(|label| CompletionItem {
          label: label.to_owned(),
          kind: Some(CompletionItemKind::VALUE),
          text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range,
            new_text: label.to_owned(),
          })),
          ..CompletionItem::default()
        })
        .collect()
    }
    CompletionMode::Keyword {
      prefix,
      range: replace_range,
      variants,
    } => variants
      .iter()
      .copied()
      .filter(|label| fuzzy_match(label, &prefix))
      .map(|label| CompletionItem {
        label: label.to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit {
          range: replace_range,
          new_text: label.to_string(),
        })),
        ..CompletionItem::default()
      })
      .collect(),
    CompletionMode::Tag {
      prefix,
      range: replace_range,
    } => {
      let mut tags = HashSet::new();
      for (_, doc) in documents_bfs(documents, root_uri) {
        for dir in &doc.directives {
          if let core::Directive::Transaction(tx) = dir {
            for tag in &tx.tags {
              if !tag.is_empty() {
                tags.insert(tag.clone());
              }
            }
          }
        }
      }

      tags
        .into_iter()
        .filter(|label| fuzzy_match(label, &prefix))
        .map(|label| CompletionItem {
          label: label.clone(),
          kind: Some(CompletionItemKind::KEYWORD),
          text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range,
            new_text: label.clone(),
          })),
          ..CompletionItem::default()
        })
        .collect()
    }
    CompletionMode::Link {
      prefix,
      range: replace_range,
    } => {
      let mut links = HashSet::new();
      for (_, doc) in documents_bfs(documents, root_uri) {
        for dir in doc.ast() {
          match dir {
            ast::Directive::Transaction(tx) => {
              for link in &tx.links {
                let label = link.content.trim_start_matches('^');
                if !label.is_empty() {
                  links.insert(label.to_string());
                }
              }
            }
            ast::Directive::Document(document) => {
              for link in &document.links {
                let label = link.content.trim_start_matches('^');
                if !label.is_empty() {
                  links.insert(label.to_string());
                }
              }

              if let Some(tags_links) = &document.tags_links {
                for item in tags_links {
                  let label = item.content.trim_start_matches('^');
                  if item.content.starts_with('^') && !label.is_empty() {
                    links.insert(label.to_string());
                  }
                }
              }
            }
            _ => {}
          }
        }
      }

      links
        .into_iter()
        .filter(|label| fuzzy_match(label, &prefix))
        .map(|label| CompletionItem {
          label: label.clone(),
          kind: Some(CompletionItemKind::KEYWORD),
          text_edit: Some(CompletionTextEdit::Edit(TextEdit {
            range: replace_range,
            new_text: label.clone(),
          })),
          ..CompletionItem::default()
        })
        .collect()
    }
  };

  items.sort_by(|a, b| a.label.cmp(&b.label));

  if items.is_empty() {
    None
  } else {
    Some(CompletionList {
      is_incomplete: true,
      items,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::doc;
  use std::collections::HashSet;
  use std::str::FromStr;
  use tower_lsp_server::ls_types::{
    CompletionContext, CompletionTriggerKind, Position, TextDocumentIdentifier,
    TextDocumentPositionParams,
  };

  /// Build a document and cursor position from lines containing a single `|` marker.
  fn doc_with_cursor(lines: &[&str]) -> (Arc<Document>, Position) {
    let mut content_lines = Vec::new();
    let mut cursor: Option<Position> = None;

    for (row, line) in lines.iter().enumerate() {
      if let Some(col) = line.find('|') {
        let mut owned = line.to_string();
        owned.remove(col);
        content_lines.push(owned);

        let col_clamped = col.min(content_lines.last().unwrap().chars().count());
        let col_u32 = u32::try_from(col_clamped).expect("column fits in u32");
        let row_u32 = u32::try_from(row).expect("row fits in u32");
        cursor = Some(Position::new(row_u32, col_u32));
      } else {
        content_lines.push((*line).to_owned());
      }
    }

    let cursor = cursor.expect("cursor marker '|' not found");
    let content = content_lines.join("\n");

    let uri = Url::from_str("file:///completion-helper.bean").unwrap();
    let doc = doc::build_document(content, uri.as_str()).expect("build doc");

    (Arc::new(doc), cursor)
  }

  fn determine_context_helper(lines: &[&str]) -> Option<CompletionMode> {
    let (doc, pos) = doc_with_cursor(lines);
    determine_completion_context(doc.as_ref(), pos)
  }

  fn completion_items(response: Option<CompletionList>) -> Vec<CompletionItem> {
    response.expect("got completion").items
  }

  fn assert_account_range(lines: &[&str], start: Position, end: Position) {
    let ctx = determine_context_helper(lines).expect("expected account context");
    match ctx {
      CompletionMode::Account { range, .. } => {
        assert_eq!(range.start, start, "unexpected start position");
        assert_eq!(range.end, end, "unexpected end position");
      }
      other => panic!("unexpected completion mode: {other:?}"),
    }
  }

  fn assert_currency_range(lines: &[&str], start: Position, end: Position) {
    let ctx = determine_context_helper(lines).expect("expected currency context");
    match ctx {
      CompletionMode::Currency { range, .. } => {
        assert_eq!(range.start, start, "unexpected start position");
        assert_eq!(range.end, end, "unexpected end position");
      }
      other => panic!("unexpected completion mode: {other:?}"),
    }
  }

  fn build_doc(uri: &Url, content: &str) -> Arc<Document> {
    Arc::new(doc::build_document(content.to_string(), uri.as_str()).expect("build doc"))
  }

  #[test]
  fn completes_accounts_and_replaces_prefix() {
    let uri = Url::from_str("file:///main.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&uri, content);

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let position = Position::new(0, 25); // after "Assets:Ca"
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Cash")
      .expect("expected Assets:Cash item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 16));
    assert_eq!(edit.range.end, Position::new(0, 27));
    assert_eq!(edit.new_text, "Assets:Cash");
  }

  #[test]
  fn offers_completion_at_document_root() {
    let lines = [r#"a|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///completion-helper.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      !items.is_empty(),
      "expected root keyword completions at document root"
    );
  }

  #[test]
  fn context_allows_balance_account() {
    let lines = [r#"2025-10-10 balance Assets:Bank:CCB:C6485|"#];

    assert_account_range(&lines, Position::new(0, 19), Position::new(0, 40));
  }

  #[test]
  fn context_allows_open_account() {
    let lines = [r#"2025-10-10 open Assets:Cash|"#];

    assert_account_range(&lines, Position::new(0, 16), Position::new(0, 27));
  }

  #[test]
  fn context_allows_posting_account_when_indented() {
    let lines = [
      r#"2025-10-10 * "Payee" "Narration""#,
      r#"  Assets:Cash:Wallet|"#,
    ];

    assert_account_range(&lines, Position::new(1, 2), Position::new(1, 20));
  }

  #[test]
  fn context_rejects_top_level_word() {
    let lines = [r#"word|"#];

    let ctx = determine_context_helper(&lines);
    assert!(
      ctx.is_none(),
      "expected no account context at document root"
    );
  }

  #[test]
  fn allows_completion_on_balance_directive() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Bank:C1234\n";
    let open_doc = build_doc(&open_uri, open_content);

    let bal_uri = Url::from_str("file:///bal.bean").unwrap();
    let bal_with_cursor = "2020-10-01 balance Assets:Bank:C1234|           0.00 CNY\n";
    let cursor_col = bal_with_cursor.find('|').expect("cursor marker present");
    let bal_content = {
      let mut s = bal_with_cursor.to_string();
      s.remove(cursor_col);
      s
    };
    let bal_doc = build_doc(&bal_uri, &bal_content);

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(bal_uri.clone(), bal_doc);

    // Cursor at the marker position (after removal)
    let position = Position::new(0, u32::try_from(cursor_col).unwrap());
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: bal_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &open_uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Bank:C1234")
      .expect("expected account item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 19));
    assert_eq!(edit.range.end, Position::new(0, 36));
    assert_eq!(edit.new_text, "Assets:Bank:C1234");
  }

  #[test]
  fn completes_when_cursor_at_account_end() {
    let uri = Url::from_str("file:///main.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&uri, content);

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    // Cursor immediately after the account token
    let position = Position::new(0, 27);
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Cash")
      .expect("expected Assets:Cash item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 16));
    assert_eq!(edit.range.end, Position::new(0, 27));
    assert_eq!(edit.new_text, "Assets:Cash");
  }

  #[test]
  fn suppresses_completion_outside_account_nodes() {
    let uri = Url::from_str("file:///main.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n2023-01-02 * \"Payee\" \"Narration\"\n";
    let doc = build_doc(&uri, content);

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let position = Position::new(1, 14); // inside the payee string, not an account node
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &uri, &params);
    assert!(
      response.is_none(),
      "expected no completion outside account nodes"
    );
  }

  #[test]
  fn suppresses_account_completion_in_amount_field() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash\n";
    let open_doc = build_doc(&open_uri, open_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 1|00 CNY"#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &open_uri, &params);
    assert!(
      response.is_none(),
      "expected no account completion in amount field"
    );
  }

  #[test]
  fn completes_currency_in_amount_field() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let open_doc = build_doc(&open_uri, open_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 C|"#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &open_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected currency completion in amount field"
    );
  }

  #[test]
  fn context_allows_currency_in_posting_amount() {
    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 CN|"#,
    ];

    assert_currency_range(&lines, Position::new(2, 50), Position::new(2, 52));
  }

  #[test]
  fn completes_currency_on_commodity_directive() {
    let source_uri = Url::from_str("file:///source-currency.bean").unwrap();
    let source_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [r#"2022-01-01 commodity C|"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///commodity-cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &source_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected CNY completion on commodity directive"
    );
  }

  #[test]
  fn completes_currency_on_price_directive() {
    let source_uri = Url::from_str("file:///source-price.bean").unwrap();
    let source_content =
      "2020-01-01 * \"...\" \"...\"\n  Assets:Cash -10 USD\n  Expenses:Food\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [r#"2022-01-01 price U|"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///price-cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &source_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "USD"),
      "expected USD completion on price directive"
    );
  }

  #[test]
  fn dedupes_and_sorts_currency_suggestions() {
    let a_uri = Url::from_str("file:///a.bean").unwrap();
    let a_content = "2020-01-01 open Assets:Cash USD CNY\n";
    let a_doc = build_doc(&a_uri, a_content);

    let b_uri = Url::from_str("file:///b.bean").unwrap();
    let b_content =
      "2020-01-01 * \"...\" \"...\"\n  Assets:Cash -1 EUR\n  Expenses:Food\n";
    let b_doc = build_doc(&b_uri, b_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                           1 |"#,
    ];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(a_uri.clone(), a_doc);
    documents.insert(b_uri.clone(), b_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &a_uri, &params));
    let labels: Vec<String> = items.into_iter().map(|item| item.label).collect();

    assert!(labels.windows(2).all(|pair| pair[0] <= pair[1]));
    assert_eq!(
      labels
        .iter()
        .filter(|label| label.as_str() == "USD")
        .count(),
      1,
      "expected duplicate USD suggestions to be removed"
    );
  }

  #[test]
  fn completes_currency_with_utf16_position() {
    let lines = [
      r#"2021-12-31 open Assets:Cash CNY"#,
      r#"2022-01-01 * "🍣" "...""#,
      r#"  Assets:Cash -1 CN|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///currency-utf16.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected currency completion with utf16-aware position"
    );
  }

  #[test]
  fn currency_completion_uses_prefix_not_subsequence() {
    let source_uri = Url::from_str("file:///source-prefix.bean").unwrap();
    let source_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 ny|"#,
    ];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor-prefix.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &source_uri, &params);
    assert!(
      response.is_none(),
      "expected no currency completion for non-prefix input"
    );
  }

  #[test]
  fn suppresses_account_completion_after_posting_account() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash\n";
    let open_doc = build_doc(&open_uri, open_content);

    assert!(matches!(open_doc.ast()[0], ast::Directive::Open(_)));

    let lines = [r#"2022-01-02 * "..." "...""#, r#"  Expenses:Food a|"#];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn2.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &open_uri, &params);
    assert!(
      response.is_none(),
      "expected no account completion after posting account"
    );
  }

  #[test]
  fn completes_account_after_unicode_payee() {
    let lines = [
      r#"2026-01-01 open Expenses:Rent USD   "#,
      r#"2026-01-05 *                        "#,
      r#"  Expenses:R|                       "#,
      r#"  Assets:Cash           -1.00 CNY   "#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn-unicode-payee.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(tx_uri.clone(), tx_doc);

    let ctx =
      determine_completion_context(documents.get(&tx_uri).unwrap().as_ref(), position);
    assert!(
      matches!(ctx, Some(CompletionMode::Account { .. })),
      "expected account completion context",
    );

    let mut accounts = HashSet::new();
    let doc = documents.get(&tx_uri).unwrap();
    accounts.extend(doc.accounts.iter().cloned());
    assert!(
      accounts.contains("Expenses:Rent"),
      "expected account set to include Expenses:Rent",
    );

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &tx_uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Rent"),
      "expected account completion after unicode payee",
    );
  }

  #[test]
  fn completes_account_on_empty_posting_line_in_raw_transaction() {
    let lines = [
      r#"2026-01-16 * "" """#,
      r#"  Expenses:Food                          -1 CNY"#,
      r#"  |"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///raw-tx-empty-posting.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let doc = documents.get(&uri).unwrap();
    assert!(
      doc
        .ast()
        .iter()
        .any(|d| matches!(d, ast::Directive::Transaction(_))),
      "expected the transaction header to parse as Transaction",
    );

    let ctx = determine_completion_context(doc.as_ref(), position);
    assert!(
      matches!(ctx, Some(CompletionMode::Account { .. })),
      "expected account completion context on empty posting line",
    );

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Food"),
      "expected account completion to include Expenses:Food",
    );
  }

  #[test]
  fn completes_in_transaction_body() {
    let lines = [
      r#"2026-01-16 open Expenses:Food CNY"#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Liabilities:C1                        -1.00 CNY"#,
      r#"  Expenses:Food"#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Liabilities:C1                          -1.00 CNY"#,
      r#"  e|                       "#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Expenses:Food"#,
      r#"  Liabilities:C1"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tx-body.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let doc = documents.get(&uri).unwrap();
    let ctx = determine_completion_context(doc.as_ref(), position);

    match ctx {
      Some(CompletionMode::Account { prefix, .. }) => {
        assert_eq!(prefix, "e");
      }
      _ => panic!("expected account completion in transaction body",),
    };

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Food"),
      "expected completion to suggest Expenses:Food",
    );
  }

  #[test]
  fn completes_tags_after_hash_on_transaction() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn completes_tags_with_existing_prefix() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #fo|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags-prefix.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion with prefix");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col + 2));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn replaces_whole_tag_when_cursor_is_in_middle() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #fo|od-old"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags-middle.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion with middle cursor");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_start = u32::try_from(hash_col + 1).unwrap();
    let expected_end = expected_start + u32::try_from("food-old".len()).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_start));
    assert_eq!(edit.range.end, Position::new(3, expected_end));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn completes_tags_from_parsed_directives() {
    let tag_uri = Url::from_str("file:///source-tags.bean").unwrap();
    let tag_content = "2024-01-01 * \"p\" \"n\" #groceries #fun\n  Assets:Cash -10 USD\n  Expenses:Food\n";
    let tag_doc = build_doc(&tag_uri, tag_content);

    let lines = [r#"2025-01-01 * "p" "n" #|"#, r#"  Expenses:Food"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor-tags.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(tag_uri.clone(), tag_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &tag_uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "groceries")
      .expect("expected tag completion from directives");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[0].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(0, expected_col));
    assert_eq!(edit.range.end, Position::new(0, expected_col));
    assert_eq!(edit.new_text, "groceries");
  }

  #[test]
  fn completes_links_after_caret_on_transaction() {
    let lines = [
      r#"2022-01-01 * "..." "..." ^invoice-2022"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." ^|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///links.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let link = items
      .iter()
      .find(|i| i.label == "invoice-2022")
      .expect("expected link completion");

    let edit = match link.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let caret_col = lines[3].find('^').expect("caret present");
    let expected_col = u32::try_from(caret_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col));
    assert_eq!(edit.new_text, "invoice-2022");
  }

  #[test]
  fn replaces_whole_link_when_cursor_is_in_middle() {
    let lines = [
      r#"2022-01-01 * "..." "..." ^invoice-2022"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." ^in|voice-old"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///links-middle.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let link = items
      .iter()
      .find(|i| i.label == "invoice-2022")
      .expect("expected link completion with middle cursor");

    let edit = match link.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let caret_col = lines[3].find('^').expect("caret present");
    let expected_start = u32::try_from(caret_col + 1).unwrap();
    let expected_end = expected_start + u32::try_from("invoice-old".len()).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_start));
    assert_eq!(edit.range.end, Position::new(3, expected_end));
    assert_eq!(edit.new_text, "invoice-2022");
  }

  #[test]
  fn does_not_complete_tags_or_links_on_unsupported_directive() {
    let lines = [r#"2022-01-02 open Assets:Cash #|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///unsupported-marker.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: Some("#".to_string()),
      }),
    };

    let response = completion(&documents, &uri, &params);
    assert!(
      response.is_none(),
      "expected no tag/link completion on unsupported directive"
    );
  }

  #[test]
  fn completes_tag_on_incomplete_date_directive_line() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 unknown-directive #|"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///raw-tx-tag.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: Some("#".to_string()),
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "food"),
      "expected tag completion while current line is still being typed"
    );
  }

  #[test]
  fn completes_keywords_after_date() {
    let lines = [r#"2026-02-02 |"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///kw.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let labels: HashSet<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in ["custom", "balance", "open", "close", "note"] {
      assert!(labels.contains(expected), "missing keyword {expected}");
    }

    let custom = items
      .iter()
      .find(|i| i.label == "custom")
      .expect("custom item present");

    let edit = match custom.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 11));
    assert_eq!(edit.range.end, Position::new(0, 11));
  }

  #[test]
  fn completes_root_keywords() {
    let lines = ["|"];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///root.kw.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));

    let labels: HashSet<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in ["include", "option"] {
      assert!(labels.contains(expected), "missing keyword {expected}");
    }

    let include = items
      .iter()
      .find(|i| i.label == "include")
      .expect("include item present");

    let edit = match include.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 0));
    assert_eq!(edit.range.end, Position::new(0, 0));
  }

  #[cfg(windows)]
  #[test]
  fn matches_documents_case_insensitively_on_windows() {
    let stored_uri =
      Url::from_str("file:///C:/Users/Trim21/proj/count/logs/2026/01.bean").unwrap();
    let requested_uri =
      Url::from_str("file:///c%3A/Users/Trim21/proj/count/logs/2026/01.bean").unwrap();

    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&stored_uri, content);

    let mut documents = HashMap::new();
    documents.insert(stored_uri.clone(), doc);

    let position = Position::new(0, 25);
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: requested_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &stored_uri, &params);
    assert!(
      response.is_some(),
      "expected completion despite URI casing differences"
    );
  }
}

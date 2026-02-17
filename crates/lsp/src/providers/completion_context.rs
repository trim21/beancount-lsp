use chumsky::prelude::*;
use beancount_parser::ast;
use tower_lsp_server::ls_types::{Position, Range};

use crate::providers::account::account_at_position;
use crate::server::Document;
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MarkerKind {
  Tag,
  Link,
}

#[derive(Debug, Clone)]
pub(crate) enum CompletionMode<'a> {
  Account {
    prefix: &'a str,
    range: Range,
  },
  Currency {
    prefix: &'a str,
    range: Range,
  },
  Keyword {
    prefix: &'a str,
    range: Range,
    variants: &'static [&'static str],
  },
  Tag {
    prefix: &'a str,
    range: Range,
  },
  Link {
    prefix: &'a str,
    range: Range,
  },
}

#[derive(Debug, Clone)]
pub(crate) struct CursorToken<'a> {
  pub prefix: &'a str,
  pub token: &'a str,
  pub range: Range,
  pub start_byte: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct MarkerToken<'a> {
  pub kind: MarkerKind,
  pub prefix: &'a str,
  pub range: Range,
}

#[derive(Debug, Clone)]
pub(crate) struct CompletionLineContext<'a> {
  pub cursor_byte: usize,
  pub line_start: usize,
  pub line_slice: &'a str,
  is_blank: bool,
  pub indent: usize,
  pub has_date_prefix: bool,
  parsed_tokens: Vec<ParsedToken>,
  rel: usize,
}

#[derive(Debug, Clone)]
struct ParsedToken {
  start: usize,
  end: usize,
}

fn span_slice<'a>(text: &'a str, start: usize, end: usize) -> Option<&'a str> {
  if start > end || end > text.len() {
    return None;
  }

  let bytes = &text.as_bytes()[start..end];
  Some(unsafe { std::str::from_utf8_unchecked(bytes) })
}

impl<'a> CompletionLineContext<'a> {
  pub(crate) fn token_at_cursor(
    &self,
    doc: &Document,
    require_nonzero_start: bool,
    allow_empty_token: bool,
  ) -> Option<CursorToken<'a>> {
    let (start, end) = if let Some(token) = self
      .parsed_tokens
      .iter()
      .find(|token| token.start <= self.rel && self.rel <= token.end)
    {
      (token.start, token.end)
    } else {
      if !allow_empty_token {
        return None;
      }
      (self.rel, self.rel)
    };

    if require_nonzero_start && start == 0 {
      return None;
    }

    let token_text = span_slice(self.line_slice, start, end)?;
    let prefix = span_slice(self.line_slice, start, self.rel)?;
    if token_text.is_empty() && !allow_empty_token {
      return None;
    }

    let start_byte = self.line_start + start;
    let end_byte = self.line_start + end;
    let start_pos = byte_to_lsp_position(&doc.rope, start_byte)?;
    let end_pos = byte_to_lsp_position(&doc.rope, end_byte)?;

    Some(CursorToken {
      prefix,
      token: token_text,
      range: Range {
        start: start_pos,
        end: end_pos,
      },
      start_byte,
    })
  }

  pub(crate) fn marker_at_cursor(
    &self,
    doc: &Document,
    kind: MarkerKind,
  ) -> Option<MarkerToken<'a>> {
    let marker = match kind {
      MarkerKind::Tag => '#',
      MarkerKind::Link => '^',
    };

    let token = self
      .parsed_tokens
      .iter()
      .find(|token| token.start <= self.rel && self.rel <= token.end)?;

    if token.start >= token.end {
      return None;
    }

    let token_text = span_slice(self.line_slice, token.start, token.end)?;
    if !token_text.starts_with(marker) {
      return None;
    }

    let token_start = self.line_start + token.start;
    let token_end = self.line_start + token.end;
    let after_marker = token_start + marker.len_utf8();
    if self.cursor_byte < after_marker {
      return None;
    }

    let after_marker_rel = token.start + marker.len_utf8();
    let prefix = span_slice(self.line_slice, after_marker_rel, self.rel)?;
    let start = byte_to_lsp_position(&doc.rope, after_marker)?;
    let end = byte_to_lsp_position(&doc.rope, token_end)?;

    Some(MarkerToken {
      kind,
      prefix,
      range: Range {
        start,
        end,
      },
    })
  }

  fn token_index_at_start(&self, token_start: usize) -> Option<usize> {
    self
      .parsed_tokens
      .iter()
      .position(|token| token.start == token_start)
  }

  fn previous_token_before_text(
    &self,
    token_start: usize,
  ) -> Option<&'a str> {
    let token = self
      .parsed_tokens
      .iter()
      .rev()
      .find(|token| token.end <= token_start)?;
    span_slice(self.line_slice, token.start, token.end)
  }
}

pub(crate) fn analyze_completion_line<'a>(
  doc: &'a Document,
  position: Position,
) -> Option<CompletionLineContext<'a>> {
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
  let line_slice = doc.content().get(line_start..line_end)?;
  if rel > line_slice.len() {
    return None;
  }

  let trimmed = line_slice.trim_start();
  let indent = line_slice.len().saturating_sub(trimmed.len());
  let is_blank = line_slice.trim().is_empty();
  let parsed_tokens = parse_line_tokens(line_slice);
  let has_date_prefix = has_date_prefix_tolerant(line_slice, indent);

  Some(CompletionLineContext {
    cursor_byte,
    line_start,
    line_slice,
    is_blank,
    indent,
    has_date_prefix,
    parsed_tokens,
    rel,
  })
}

fn parse_line_tokens(line_slice: &str) -> Vec<ParsedToken> {
  let token = any::<_, extra::Err<Rich<char>>>()
    .filter(|ch: &char| !ch.is_whitespace())
    .repeated()
    .at_least(1)
    .to_span()
    .map(|span: SimpleSpan<usize>| ParsedToken {
      start: span.start,
      end: span.end,
    })
    .padded();

  let parser = token.repeated().collect::<Vec<_>>().then_ignore(end());
  parser
    .parse(line_slice)
    .output()
    .cloned()
    .unwrap_or_default()
}

fn has_date_prefix_tolerant(line_slice: &str, indent: usize) -> bool {
  let Some(rest) = line_slice.get(indent..) else {
    return false;
  };

  let head = rest.split_whitespace().next().unwrap_or(rest);
  if head.is_empty() {
    return false;
  }

  let year = text::digits::<_, extra::Err<Rich<char>>>(10).exactly(4);
  let month = text::digits::<_, extra::Err<Rich<char>>>(10)
    .at_least(1)
    .at_most(2);
  let day = text::digits::<_, extra::Err<Rich<char>>>(10).at_most(2);

  let parser = year
    .then_ignore(just('-'))
    .then(month)
    .then(just('-').then(day).or_not())
    .then_ignore(end());

  parser.parse(head).has_output()
}

pub(crate) fn is_probably_metadata_key(token: &str) -> bool {
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

pub(crate) fn is_probably_currency_context<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  token: &CursorToken<'a>,
) -> Option<(&'a str, Range)> {
  let token_start_rel = token.start_byte.saturating_sub(line_ctx.line_start);
  if token_start_rel > line_ctx.line_slice.len() {
    return None;
  }

  if is_probably_metadata_key(token.token.trim()) {
    return None;
  }

  if is_within_currency_context(doc, line_ctx.cursor_byte) {
    return Some((token.prefix, token.range));
  }

  if line_ctx.indent == 0 && line_ctx.has_date_prefix {
    let token_idx = line_ctx.token_index_at_start(token_start_rel)?;
    let keyword = line_ctx.line_slice.split_whitespace().nth(1);

    if let Some(keyword) = keyword {
      if keyword.eq_ignore_ascii_case("commodity") && token_idx == 2 {
        return Some((token.prefix, token.range));
      }

      if keyword.eq_ignore_ascii_case("price")
        && (token_idx == 2 || token_idx >= 4)
      {
        return Some((token.prefix, token.range));
      }
    }
  }

  if let Some(previous_token) =
    line_ctx.previous_token_before_text(token_start_rel)
    && is_number_like_token(&previous_token)
  {
    return Some((token.prefix, token.range));
  }

  None
}

fn directive_at_cursor<'a>(
  doc: &'a Document,
  byte_idx: usize,
) -> Option<&'a ast::Directive<'a>> {
  doc.directive_at_offset(byte_idx).or_else(|| {
    byte_idx
      .checked_sub(1)
      .and_then(|idx| doc.directive_at_offset(idx))
  })
}

fn is_within_account_context_parsed_directive(
  directive: &ast::Directive<'_>,
  byte_idx: usize,
) -> bool {
  let span_contains = |span: ast::Span| span.start <= byte_idx && byte_idx <= span.end;

  match directive {
    ast::Directive::Open(open) => span_contains(open.account.span),
    ast::Directive::Balance(balance) => span_contains(balance.account.span),
    ast::Directive::Pad(pad) => {
      span_contains(pad.account.span) || span_contains(pad.from_account.span)
    }
    ast::Directive::Close(close) => span_contains(close.account.span),
    ast::Directive::Transaction(tx) => {
      tx.postings.iter().any(|p| span_contains(p.account.span))
    }
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

  let text = doc.content().get(line_start..line_end)?;
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

  let Some((indent, _is_empty)) = line_indent(doc, line) else {
    return false;
  };
  if indent == 0 {
    return false;
  }

  let Some(idx) = directive_index_before_line(doc, line) else {
    return false;
  };

  let directives = doc.ast();
  for prev in directives[..=idx].iter().rev() {
    match prev {
      ast::Directive::Transaction(_) => return true,
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

fn is_within_account_context_tolerant(
  doc: &Document,
  position: Position,
  byte_idx: usize,
) -> bool {
  account_at_position(doc, position).is_some() || is_in_transaction_body_line(doc, byte_idx)
}

fn marker_completion_mode<'a>(marker: &MarkerToken<'a>) -> CompletionMode<'a> {
  match marker.kind {
    MarkerKind::Tag => CompletionMode::Tag {
      prefix: marker.prefix,
      range: marker.range,
    },
    MarkerKind::Link => CompletionMode::Link {
      prefix: marker.prefix,
      range: marker.range,
    },
  }
}

fn account_completion_mode<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  in_account_context: bool,
) -> Option<CompletionMode<'a>> {
  if !in_account_context {
    return None;
  }

  let token = line_ctx.token_at_cursor(doc, true, true)?;
  if line_ctx.indent > 0
    && token.range.start.character != u32::try_from(line_ctx.indent).ok()?
  {
    return None;
  }
  if is_probably_metadata_key(token.token.trim()) {
    return None;
  }

  if token.token.is_empty()
    || token
      .token
      .chars()
      .next()
      .map(|c| c.is_ascii_alphabetic())
      .unwrap_or(false)
  {
    return Some(CompletionMode::Account {
      prefix: token.prefix,
      range: token.range,
    });
  }

  None
}

fn currency_completion_mode<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
) -> Option<CompletionMode<'a>> {
  let token = line_ctx.token_at_cursor(doc, true, true)?;
  let (prefix, range) = is_probably_currency_context(doc, line_ctx, &token)?;

  Some(CompletionMode::Currency { prefix, range })
}

fn date_keyword_completion_mode<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  in_account_context: bool,
  date_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  if line_ctx.indent != 0
    || !line_ctx.has_date_prefix
    || in_account_context
    || line_ctx.cursor_byte.saturating_sub(line_ctx.line_start) < line_ctx.indent + 10
  {
    return None;
  }

  let token = line_ctx.token_at_cursor(doc, false, true)?;
  if !date_keywords
    .iter()
    .any(|label| fuzzy_match(label, &token.prefix))
  {
    return None;
  }

  Some(CompletionMode::Keyword {
    prefix: token.prefix,
    range: token.range,
    variants: date_keywords,
  })
}

fn root_keyword_completion_mode<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  in_account_context: bool,
  root_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  if line_ctx.indent != 0 || in_account_context {
    return None;
  }

  let token = line_ctx.token_at_cursor(doc, false, true)?;
  if !root_keywords
    .iter()
    .any(|label| fuzzy_match(label, &token.prefix))
  {
    return None;
  }

  Some(CompletionMode::Keyword {
    prefix: token.prefix,
    range: token.range,
    variants: root_keywords,
  })
}

fn determine_from_parsed_directive<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  directive: &ast::Directive<'_>,
  marker_ctx: Option<&MarkerToken<'a>>,
  date_keywords: &'static [&'static str],
  root_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  let in_account_context = is_within_account_context_parsed_directive(
    directive,
    line_ctx.cursor_byte,
  );
  let supports_tag_link =
    matches!(directive, ast::Directive::Transaction(_) | ast::Directive::Document(_));

  if line_ctx.has_date_prefix && supports_tag_link && let Some(marker) = marker_ctx {
    return Some(marker_completion_mode(marker));
  }

  if let Some(mode) = account_completion_mode(doc, line_ctx, in_account_context) {
    return Some(mode);
  }

  if let Some(mode) = currency_completion_mode(doc, line_ctx) {
    return Some(mode);
  }

  if let Some(mode) =
    date_keyword_completion_mode(doc, line_ctx, in_account_context, date_keywords)
  {
    return Some(mode);
  }

  if let Some(mode) =
    root_keyword_completion_mode(doc, line_ctx, in_account_context, root_keywords)
  {
    return Some(mode);
  }

  None
}

fn determine_from_tolerant_context<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  position: Position,
  marker_ctx: Option<&MarkerToken<'a>>,
  date_keywords: &'static [&'static str],
  root_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  let in_account_context =
    is_within_account_context_tolerant(doc, position, line_ctx.cursor_byte);

  if line_ctx.has_date_prefix && marker_ctx.is_some() && let Some(marker) = marker_ctx {
    return Some(marker_completion_mode(marker));
  }

  if let Some(mode) = account_completion_mode(doc, line_ctx, in_account_context) {
    return Some(mode);
  }

  if let Some(mode) = currency_completion_mode(doc, line_ctx) {
    return Some(mode);
  }

  if let Some(mode) =
    date_keyword_completion_mode(doc, line_ctx, in_account_context, date_keywords)
  {
    return Some(mode);
  }

  if let Some(mode) =
    root_keyword_completion_mode(doc, line_ctx, in_account_context, root_keywords)
  {
    return Some(mode);
  }

  None
}

fn determine_from_outside_directive<'a>(
  doc: &Document,
  line_ctx: &CompletionLineContext<'a>,
  root_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  root_keyword_completion_mode(doc, line_ctx, false, root_keywords)
}

pub(crate) fn determine_completion_context<'a>(
  doc: &'a Document,
  position: Position,
  date_keywords: &'static [&'static str],
  root_keywords: &'static [&'static str],
) -> Option<CompletionMode<'a>> {
  let line_ctx = analyze_completion_line(doc, position)?;
  let marker_ctx = line_ctx
    .marker_at_cursor(doc, MarkerKind::Tag)
    .or_else(|| line_ctx.marker_at_cursor(doc, MarkerKind::Link));
  let marker_ctx = marker_ctx.as_ref();

  let directive = directive_at_cursor(doc, line_ctx.cursor_byte);
  let is_outside_directive = directive.is_none()
    && line_ctx.indent == 0
    && line_ctx.is_blank;

  if let Some(directive) = directive {
    if matches!(directive, ast::Directive::Raw(_)) {
      return determine_from_tolerant_context(
        doc,
        &line_ctx,
        position,
        marker_ctx,
        date_keywords,
        root_keywords,
      );
    }

    return determine_from_parsed_directive(
      doc,
      &line_ctx,
      directive,
      marker_ctx,
      date_keywords,
      root_keywords,
    );
  }

  if is_outside_directive {
    return determine_from_outside_directive(doc, &line_ctx, root_keywords);
  }

  determine_from_tolerant_context(
    doc,
    &line_ctx,
    position,
    marker_ctx,
    date_keywords,
    root_keywords,
  )
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::doc;
  use std::str::FromStr;
  use tower_lsp_server::ls_types::Uri as Url;

  const DATE_KEYWORDS: &[&str] =
    &["custom", "balance", "open", "close", "note", "price", "pad"];
  const ROOT_KEYWORDS: &[&str] = &[
    "include", "option", "pushtag", "poptag", "pushmeta", "popmeta",
  ];

  fn doc_with_cursor(lines: &[&str]) -> (Document, Position) {
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

    let uri = Url::from_str("file:///completion-context-helper.bean").unwrap();
    let doc = doc::build_document(content, uri.as_str()).expect("build doc");

    (doc, cursor)
  }

  fn assert_account_range(lines: &[&str], start: Position, end: Position) {
    let (doc, pos) = doc_with_cursor(lines);
    let ctx = determine_completion_context(&doc, pos, DATE_KEYWORDS, ROOT_KEYWORDS)
      .expect("expected account context");
    match ctx {
      CompletionMode::Account { range, .. } => {
        assert_eq!(range.start, start, "unexpected start position");
        assert_eq!(range.end, end, "unexpected end position");
      }
      other => panic!("unexpected completion hint: {other:?}"),
    }
  }

  fn assert_currency_range(lines: &[&str], start: Position, end: Position) {
    let (doc, pos) = doc_with_cursor(lines);
    let ctx = determine_completion_context(&doc, pos, DATE_KEYWORDS, ROOT_KEYWORDS)
      .expect("expected currency context");
    match ctx {
      CompletionMode::Currency { range, .. } => {
        assert_eq!(range.start, start, "unexpected start position");
        assert_eq!(range.end, end, "unexpected end position");
      }
      other => panic!("unexpected completion hint: {other:?}"),
    }
  }

  #[test]
  fn parsed_context_allows_balance_account() {
    let lines = [r#"2025-10-10 balance Assets:Bank:CCB:C6485|"#];

    assert_account_range(&lines, Position::new(0, 19), Position::new(0, 40));
  }

  #[test]
  fn parsed_context_allows_open_account() {
    let lines = [r#"2025-10-10 open Assets:Cash|"#];

    assert_account_range(&lines, Position::new(0, 16), Position::new(0, 27));
  }

  #[test]
  fn parsed_context_allows_posting_account_when_indented() {
    let lines = [
      r#"2025-10-10 * "Payee" "Narration""#,
      r#"  Assets:Cash:Wallet|"#,
    ];

    assert_account_range(&lines, Position::new(1, 2), Position::new(1, 20));
  }

  #[test]
  fn outside_context_rejects_top_level_word() {
    let lines = [r#"word|"#];
    let (doc, pos) = doc_with_cursor(&lines);

    let ctx = determine_completion_context(&doc, pos, DATE_KEYWORDS, ROOT_KEYWORDS);
    assert!(ctx.is_none(), "expected no account context at document root");
  }

  #[test]
  fn parsed_context_allows_currency_in_posting_amount() {
    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 CN|"#,
    ];

    assert_currency_range(&lines, Position::new(2, 50), Position::new(2, 52));
  }

  #[test]
  fn parsed_dispatch_branch() {
    let lines = [r#"2025-10-10 open Assets:Cash|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let hint = determine_completion_context(&doc, position, DATE_KEYWORDS, ROOT_KEYWORDS);

    assert!(
      matches!(hint, Some(CompletionMode::Account { .. })),
      "expected account completion from parsed directive branch"
    );
  }

  #[test]
  fn tolerant_dispatch_branch_for_nonparsed_line() {
    let lines = [r#"2022-01-02 unknown-directive #|"#];
    let (doc, position) = doc_with_cursor(&lines);

    let hint = determine_completion_context(&doc, position, DATE_KEYWORDS, ROOT_KEYWORDS);
    assert!(
      matches!(hint, Some(CompletionMode::Tag { .. })),
      "expected marker completion from tolerant branch"
    );
  }

  #[test]
  fn outside_dispatch_branch() {
    let lines = [r#"2022-01-01 open Assets:Cash"#, r#""#, r#"|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let hint = determine_completion_context(&doc, position, DATE_KEYWORDS, ROOT_KEYWORDS);

    assert!(
      matches!(
        hint,
        Some(CompletionMode::Keyword {
          variants: ROOT_KEYWORDS,
          ..
        })
      ),
      "expected root keyword completion from outside-directive branch"
    );
  }
}

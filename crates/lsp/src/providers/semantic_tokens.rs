use std::cmp::Ordering;

use crate::text::byte_to_lsp_position;
use beancount_parser::{ast, parse_lossy};
use ropey::Rope;
use tower_lsp_server::ls_types::{
  SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
  SemanticTokensLegend, SemanticTokensResult,
};

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum TokenKind {
  Keyword,
  Comment,
  String,
  Number,
  Type,
  Macro,
  Operator,
  Parameter,
  Property,
  Class,
  Function,
}

const TOKEN_KINDS: [TokenKind; 11] = [
  TokenKind::Keyword,
  TokenKind::Comment,
  TokenKind::String,
  TokenKind::Number,
  TokenKind::Type,
  TokenKind::Macro,
  TokenKind::Operator,
  TokenKind::Parameter,
  TokenKind::Property,
  TokenKind::Class,
  TokenKind::Function,
];

fn token_types() -> Vec<SemanticTokenType> {
  TOKEN_KINDS.iter().copied().map(token_type).collect()
}

fn token_index(kind: TokenKind) -> u32 {
  kind as u32
}

fn token_type(kind: TokenKind) -> SemanticTokenType {
  match kind {
    TokenKind::Keyword => SemanticTokenType::KEYWORD,
    TokenKind::Comment => SemanticTokenType::COMMENT,
    TokenKind::String => SemanticTokenType::STRING,
    TokenKind::Number => SemanticTokenType::NUMBER,
    TokenKind::Type => SemanticTokenType::TYPE,
    TokenKind::Macro => SemanticTokenType::MACRO,
    TokenKind::Operator => SemanticTokenType::OPERATOR,
    TokenKind::Parameter => SemanticTokenType::PARAMETER,
    TokenKind::Property => SemanticTokenType::PROPERTY,
    TokenKind::Class => SemanticTokenType::CLASS,
    TokenKind::Function => SemanticTokenType::FUNCTION,
  }
}

const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[];

#[derive(Debug)]
struct RawToken {
  line: u32,
  start: u32,
  length: u32,
  token_type: u32,
  modifiers_bitset: u32,
}

/// Shared legend between capability advertisement and responses.
pub fn legend() -> SemanticTokensLegend {
  SemanticTokensLegend {
    token_types: token_types(),
    token_modifiers: TOKEN_MODIFIERS.to_vec(),
  }
}

/// Compute semantic tokens from the parsed AST.
pub fn semantic_tokens_full(
  doc: &crate::doc::Document,
) -> Option<SemanticTokensResult> {
  semantic_tokens_from_parsed(doc.ast(), &doc.rope, doc.content().len())
}

fn semantic_tokens_from_parsed(
  directives: &[ast::Directive<'_>],
  rope: &Rope,
  content_len: usize,
) -> Option<SemanticTokensResult> {
  let mut raw_tokens = Vec::with_capacity(content_len / 5);

  for directive in directives {
    collect_directive_tokens(directive, rope, &mut raw_tokens);
  }

  if raw_tokens.is_empty() {
    return Some(SemanticTokensResult::Tokens(SemanticTokens {
      result_id: None,
      data: vec![],
    }));
  }

  raw_tokens.sort_by(|a, b| match a.line.cmp(&b.line) {
    Ordering::Equal => a.start.cmp(&b.start),
    other => other,
  });

  let mut data = Vec::with_capacity(raw_tokens.len());
  let mut prev_line = 0u32;
  let mut prev_start = 0u32;

  for token in raw_tokens {
    let delta_line = token.line.saturating_sub(prev_line);
    let delta_start = if delta_line == 0 {
      token.start.saturating_sub(prev_start)
    } else {
      token.start
    };

    data.push(SemanticToken {
      delta_line,
      delta_start,
      length: token.length,
      token_type: token.token_type,
      token_modifiers_bitset: token.modifiers_bitset,
    });

    prev_line = token.line;
    prev_start = token.start;
  }

  Some(SemanticTokensResult::Tokens(SemanticTokens {
    result_id: None,
    data,
  }))
}

/// Compute semantic tokens directly from source text.
///
/// This is intended for the "highlight while typing" path: it avoids depending on the indexer
/// snapshot (which can lag behind rapid edits) and doesn't require building a full `Document`.
pub fn semantic_tokens_full_from_text(text: &str) -> Option<SemanticTokensResult> {
  let rope = Rope::from_str(text);
  let directives = parse_lossy(text);
  semantic_tokens_from_parsed(&directives, &rope, text.len())
}

fn collect_directive_tokens(
  directive: &ast::Directive<'_>,
  content: &Rope,
  out: &mut Vec<RawToken>,
) {
  match directive {
    ast::Directive::Open(open) => {
      push_span(content, open.keyword, TokenKind::Function, out);
      push_with_span(content, &open.date, TokenKind::Number, out);

      for currency in &open.currencies {
        push_with_span(content, currency, TokenKind::Class, out);
      }

      push_opt_with_span(content, &open.comment, TokenKind::Comment, out);
      collect_key_values(content, &open.key_values, out);
    }
    ast::Directive::Close(close) => {
      push_with_span(content, &close.date, TokenKind::Number, out);
      push_opt_with_span(content, &close.comment, TokenKind::Comment, out);
      collect_key_values(content, &close.key_values, out);
    }
    ast::Directive::Balance(balance) => {
      push_span(content, balance.keyword, TokenKind::Function, out);
      push_with_span(content, &balance.date, TokenKind::Number, out);

      collect_amount(content, &balance.amount, out);
      if let Some(tolerance) = &balance.tolerance {
        collect_tolerance(content, tolerance, out);
      }

      push_opt_with_span(content, &balance.comment, TokenKind::Comment, out);
      collect_key_values(content, &balance.key_values, out);
    }
    ast::Directive::Pad(pad) => {
      push_span(content, pad.keyword, TokenKind::Function, out);
      push_with_span(content, &pad.date, TokenKind::Number, out);
      push_opt_with_span(content, &pad.comment, TokenKind::Comment, out);
      collect_key_values(content, &pad.key_values, out);
    }
    ast::Directive::Transaction(tx) => {
      push_with_span(content, &tx.date, TokenKind::Number, out);

      if let Some(flag) = &tx.txn {
        push_with_span(content, flag, TokenKind::Function, out);

        // Keep the "!" flag highlighted as a keyword to mirror the snapshot.
        if !flag.content.is_empty() && flag.content != "*" {
          push_with_span(content, flag, TokenKind::Keyword, out);
        }
      }

      push_opt_with_span(content, &tx.payee, TokenKind::String, out);
      push_opt_with_span(content, &tx.narration, TokenKind::String, out);

      push_tags(content, &tx.tags, out);
      push_tags(content, &tx.links, out);

      push_opt_with_span(content, &tx.comment, TokenKind::Comment, out);

      collect_key_values(content, &tx.key_values, out);

      for posting in &tx.postings {
        collect_posting(content, posting, out);
      }
    }
    ast::Directive::Commodity(commodity) => {
      push_with_span(content, &commodity.date, TokenKind::Number, out);
      push_with_span(content, &commodity.currency, TokenKind::Class, out);
      push_opt_with_span(content, &commodity.comment, TokenKind::Comment, out);
      collect_key_values(content, &commodity.key_values, out);
    }
    ast::Directive::Price(price) => {
      push_with_span(content, &price.date, TokenKind::Number, out);
      push_with_span(content, &price.currency, TokenKind::Class, out);
      collect_amount(content, &price.amount, out);
      push_opt_with_span(content, &price.comment, TokenKind::Comment, out);
      collect_key_values(content, &price.key_values, out);
    }
    ast::Directive::Event(event) => {
      push_with_span(content, &event.date, TokenKind::Number, out);
      push_with_span(content, &event.event_type, TokenKind::String, out);
      push_with_span(content, &event.desc, TokenKind::String, out);
      push_opt_with_span(content, &event.comment, TokenKind::Comment, out);
      collect_key_values(content, &event.key_values, out);
    }
    ast::Directive::Query(query) => {
      push_with_span(content, &query.date, TokenKind::Number, out);
      push_with_span(content, &query.name, TokenKind::String, out);
      push_with_span(content, &query.query, TokenKind::String, out);
      push_opt_with_span(content, &query.comment, TokenKind::Comment, out);
      collect_key_values(content, &query.key_values, out);
    }
    ast::Directive::Note(note) => {
      push_span(content, note.keyword, TokenKind::Function, out);
      push_with_span(content, &note.date, TokenKind::Number, out);
      push_with_span(content, &note.note, TokenKind::String, out);
      push_opt_with_span(content, &note.comment, TokenKind::Comment, out);
      collect_key_values(content, &note.key_values, out);
    }
    ast::Directive::Document(doc_directive) => {
      push_with_span(content, &doc_directive.date, TokenKind::Number, out);
      push_with_span(content, &doc_directive.filename, TokenKind::String, out);

      if let Some(tags_links) = &doc_directive.tags_links {
        push_tags(content, tags_links, out);
      }

      push_tags(content, &doc_directive.tags, out);
      push_tags(content, &doc_directive.links, out);

      push_opt_with_span(content, &doc_directive.comment, TokenKind::Comment, out);
      collect_key_values(content, &doc_directive.key_values, out);
    }
    ast::Directive::Custom(custom) => {
      push_span(content, custom.keyword, TokenKind::Function, out);
      push_with_span(content, &custom.date, TokenKind::Number, out);
      push_with_span(content, &custom.name, TokenKind::String, out);

      for value in &custom.values {
        collect_custom_value_tokens(content, value, out);
      }

      push_opt_with_span(content, &custom.comment, TokenKind::Comment, out);
      collect_key_values(content, &custom.key_values, out);
    }
    ast::Directive::Option(option) => {
      push_span(content, option.keyword, TokenKind::Function, out);
      push_with_span(content, &option.key, TokenKind::String, out);
      push_with_span(content, &option.value, TokenKind::String, out);
      push_opt_with_span(content, &option.comment, TokenKind::Comment, out);
    }
    ast::Directive::Include(include) => {
      push_span(content, include.keyword, TokenKind::Function, out);
      push_with_span(content, &include.filename, TokenKind::String, out);
      push_opt_with_span(content, &include.comment, TokenKind::Comment, out);
    }
    ast::Directive::Plugin(plugin) => {
      push_span(content, plugin.keyword, TokenKind::Function, out);
      push_with_span(content, &plugin.name, TokenKind::String, out);
      push_opt_with_span(content, &plugin.config, TokenKind::String, out);
      push_opt_with_span(content, &plugin.comment, TokenKind::Comment, out);
    }
    ast::Directive::PushTag(tag) => {
      push_span(content, tag.keyword, TokenKind::Function, out);
      push_with_span(content, &tag.tag, TokenKind::Parameter, out);
      push_opt_with_span(content, &tag.comment, TokenKind::Comment, out);
    }
    ast::Directive::PopTag(tag) => {
      push_span(content, tag.keyword, TokenKind::Function, out);
      push_with_span(content, &tag.tag, TokenKind::Parameter, out);
      push_opt_with_span(content, &tag.comment, TokenKind::Comment, out);
    }
    ast::Directive::PushMeta(pm) => {
      push_span(content, pm.keyword, TokenKind::Function, out);
      push_with_span(content, &pm.key, TokenKind::Keyword, out);

      if let Some(value) = &pm.value {
        collect_key_value_value_tokens(content, value, out);
      }

      push_opt_with_span(content, &pm.comment, TokenKind::Comment, out);
    }
    ast::Directive::PopMeta(pm) => {
      push_span(content, pm.keyword, TokenKind::Function, out);
      push_with_span(content, &pm.key, TokenKind::Keyword, out);
      push_opt_with_span(content, &pm.comment, TokenKind::Comment, out);
    }
    ast::Directive::Comment(comment) => {
      push_with_span(content, &comment.text, TokenKind::Comment, out);
    }
    ast::Directive::Headline(headline) => {
      push_with_span(content, &headline.text, TokenKind::Keyword, out);
    }
    ast::Directive::Raw(_) => {}
  }
}

fn collect_posting(
  content: &Rope,
  posting: &ast::Posting<'_>,
  out: &mut Vec<RawToken>,
) {
  push_opt_with_span(content, &posting.opt_flag, TokenKind::Keyword, out);

  if let Some(amount) = &posting.amount {
    collect_amount(content, amount, out);
  }

  if let Some(cost_spec) = &posting.cost_spec {
    collect_cost_spec(content, cost_spec, out);
  }

  push_opt_with_span(content, &posting.price_operator, TokenKind::Operator, out);

  if let Some(price_annotation) = &posting.price_annotation {
    collect_amount(content, price_annotation, out);
  }

  push_opt_with_span(content, &posting.comment, TokenKind::Comment, out);
  collect_key_values(content, &posting.key_values, out);
}

fn collect_cost_spec(
  content: &Rope,
  cost_spec: &ast::CostSpec<'_>,
  out: &mut Vec<RawToken>,
) {
  if let Some(amount) = &cost_spec.amount {
    if let Some(per) = &amount.per {
      collect_number_expr(content, per, out);
    }
    if let Some(total) = &amount.total {
      collect_number_expr(content, total, out);
    }
    push_opt_with_span(content, &amount.currency, TokenKind::Class, out);
  }

  push_opt_with_span(content, &cost_spec.date, TokenKind::Number, out);
  push_opt_with_span(content, &cost_spec.label, TokenKind::String, out);
  push_opt_with_span(content, &cost_spec.merge, TokenKind::Keyword, out);
}

fn collect_amount(content: &Rope, amount: &ast::Amount<'_>, out: &mut Vec<RawToken>) {
  collect_number_expr(content, &amount.number, out);
  push_opt_with_span(content, &amount.currency, TokenKind::Class, out);
}

fn collect_custom_value_tokens(
  content: &Rope,
  value: &ast::CustomValue<'_>,
  out: &mut Vec<RawToken>,
) {
  match value.kind {
    ast::CustomValueKind::String => {
      push_with_span(content, &value.raw, TokenKind::String, out)
    }
    ast::CustomValueKind::Date => {
      push_with_span(content, &value.raw, TokenKind::Number, out)
    }
    ast::CustomValueKind::Bool => {
      push_with_span(content, &value.raw, TokenKind::Keyword, out)
    }
    ast::CustomValueKind::Amount => {
      if let Some(amount) = &value.amount {
        collect_amount(content, amount, out);
      }
    }
    ast::CustomValueKind::Number => {
      if let Some(expr) = &value.number {
        collect_number_expr(content, expr, out);
      }
    }
    ast::CustomValueKind::Account => {}
  }
}

fn collect_key_values(
  content: &Rope,
  key_values: &[ast::KeyValue<'_>],
  out: &mut Vec<RawToken>,
) {
  for kv in key_values {
    push_with_span(content, &kv.key, TokenKind::Keyword, out);

    if let Some(value) = &kv.value {
      collect_key_value_value_tokens(content, value, out);
    }
  }
}

fn collect_key_value_value_tokens(
  content: &Rope,
  value: &ast::WithSpan<ast::KeyValueValue<'_>>,
  out: &mut Vec<RawToken>,
) {
  match &value.content {
    ast::KeyValueValue::String(_)
    | ast::KeyValueValue::UnquotedString(_)
    | ast::KeyValueValue::Raw(_) => {
      push_with_span(content, value, TokenKind::String, out)
    }
    ast::KeyValueValue::Date(_) => {
      push_with_span(content, value, TokenKind::Number, out)
    }
    ast::KeyValueValue::Bool(_) => {
      push_with_span(content, value, TokenKind::Keyword, out)
    }
  }
}

fn collect_number_expr(
  content: &Rope,
  expr: &ast::NumberExpr<'_>,
  out: &mut Vec<RawToken>,
) {
  match expr {
    ast::NumberExpr::Missing { .. } => {}
    ast::NumberExpr::Literal(value) => push_number_literal(content, value, out),
    ast::NumberExpr::Binary { span, .. } => {
      push_span(content, *span, TokenKind::Number, out);
    }
  }
}

fn push_number_literal(
  content: &Rope,
  literal: &ast::WithSpan<&str>,
  out: &mut Vec<RawToken>,
) {
  let text = literal.content;

  if let Some(first) = text.chars().next()
    && (first == '-' || first == '+')
    && text.len() > first.len_utf8()
  {
    let op_start = literal.span.start;
    let op_end = op_start + first.len_utf8();
    push_span_range(content, op_start, op_end, TokenKind::Operator, out);

    let num_start = op_end;
    if num_start < literal.span.end {
      push_span_range(content, num_start, literal.span.end, TokenKind::Number, out);
    }

    return;
  }

  push_with_span(content, literal, TokenKind::Number, out);
}

fn collect_tolerance(
  content: &Rope,
  tolerance: &ast::WithSpan<&str>,
  out: &mut Vec<RawToken>,
) {
  let raw = tolerance.content;
  let base = tolerance.span.start;

  let mut token_start: Option<usize> = None;

  for (idx, ch) in raw.char_indices() {
    if ch.is_whitespace() {
      if let Some(start) = token_start.take() {
        push_tolerance_token(content, raw, base, start, idx, out);
      }
      continue;
    }

    if token_start.is_none() {
      token_start = Some(idx);
    }
  }

  if let Some(start) = token_start {
    push_tolerance_token(content, raw, base, start, raw.len(), out);
  }
}

fn push_tolerance_token(
  content: &Rope,
  raw: &str,
  base: usize,
  start: usize,
  end: usize,
  out: &mut Vec<RawToken>,
) {
  if start >= end || end > raw.len() {
    return;
  }

  let token = &raw[start..end];
  let token_start_in_raw = token.char_indices().next().unwrap_or((0, '\0'));

  let (trim_offset, trimmed) = match token_start_in_raw {
    (idx, ch) if ch == '~' || ch == '-' || ch == '+' => {
      let off = idx + ch.len_utf8();
      (off, token.get(off..).unwrap_or(""))
    }
    _ => (0, token),
  };

  if trimmed.is_empty() {
    return;
  }

  let mut push_token = |kind: TokenKind| {
    let global_start = base + start + trim_offset;
    let global_end = base + end;
    push_span_range(content, global_start, global_end, kind, out);
  };

  if trimmed.chars().all(|c| c.is_ascii_digit() || c == '.') {
    push_token(TokenKind::Number);
  } else if trimmed.chars().all(|c| c.is_ascii_uppercase()) {
    push_token(TokenKind::Class);
  }
}

fn push_tags(content: &Rope, tags: &[ast::WithSpan<&str>], out: &mut Vec<RawToken>) {
  for tag in tags {
    push_with_span(content, tag, TokenKind::Parameter, out);
  }
}

fn push_span(
  content: &Rope,
  span: ast::Span,
  kind: TokenKind,
  out: &mut Vec<RawToken>,
) {
  if let Some(token) = to_semantic_token(content, span.start, span.end, kind) {
    out.push(token);
  }
}

fn push_span_range(
  content: &Rope,
  start: usize,
  end: usize,
  kind: TokenKind,
  out: &mut Vec<RawToken>,
) {
  if let Some(token) = to_semantic_token(content, start, end, kind) {
    out.push(token);
  }
}

fn push_with_span<T>(
  content: &Rope,
  value: &ast::WithSpan<T>,
  kind: TokenKind,
  out: &mut Vec<RawToken>,
) {
  push_span(content, value.span, kind, out);
}

fn push_opt_with_span<T>(
  content: &Rope,
  value: &Option<ast::WithSpan<T>>,
  kind: TokenKind,
  out: &mut Vec<RawToken>,
) {
  if let Some(value) = value {
    push_span(content, value.span, kind, out);
  }
}

fn to_semantic_token(
  content: &Rope,
  start_byte: usize,
  end_byte: usize,
  kind: TokenKind,
) -> Option<RawToken> {
  if start_byte >= end_byte {
    return None;
  }

  let start_pos = byte_to_lsp_position(content, start_byte)?;
  let end_pos = byte_to_lsp_position(content, end_byte)?;

  if start_pos.line != end_pos.line {
    // Emit only single-line tokens to keep deltas simple.
    return None;
  }

  let length_utf16 = end_pos.character.checked_sub(start_pos.character)?;
  if length_utf16 == 0 {
    return None;
  }

  Some(RawToken {
    line: start_pos.line,
    start: start_pos.character,
    length: length_utf16,
    token_type: token_index(kind),
    modifiers_bitset: 0,
  })
}
#[cfg(test)]
mod tests {
  use super::*;

  const SAMPLE: &str = r#"; top level comment
2026-02-01 open Assets:Cash USD ; open comment
2026-02-02 close Assets:Cash
2026-02-03 balance Assets:Cash 0 USD ~1 USD ; tolerance
2026-02-04 pad Assets:Cash Equity:Opening-Balances
2026-02-05 note Assets:Cash "note text"
2026-02-06 commodity USD
2026-02-07 price USD 1 EUR
2026-02-08 event "type" "desc"
2026-02-09 query "q" "select 1"
2026-02-10 document Assets:Cash "file.pdf" #doc ^link
2026-02-11 custom "name" "val" TRUE 1*2+3/4 USD
option "title" "value"
include "other.bean"
plugin "mod" "config"
pushtag #tag2
poptag #tag3
pushmeta foo: TRUE
popmeta foo:
2026-02-12 * "Payee" "Narration" #tag ^link
  Assets:Cash -10 USD { 1 # 2 USD } @ 5 EUR ; line comment
  Expenses:Food 10 USD @@ 20 USD
  key: FALSE
2026-02-13 ! "Payee2" "Narr2"
* Headline
"#;

  fn type_name(idx: u32) -> &'static str {
    match TOKEN_KINDS[idx as usize] {
      TokenKind::Keyword => "keyword",
      TokenKind::Comment => "comment",
      TokenKind::String => "string",
      TokenKind::Number => "number",
      TokenKind::Type => "type",
      TokenKind::Macro => "macro",
      TokenKind::Operator => "operator",
      TokenKind::Parameter => "parameter",
      TokenKind::Property => "property",
      TokenKind::Class => "class",
      TokenKind::Function => "function",
    }
  }

  fn render_tokens(data: &[SemanticToken]) -> String {
    let mut line = 0u32;
    let mut start = 0u32;
    let mut items = Vec::with_capacity(data.len());

    for (idx, token) in data.iter().enumerate() {
      line = line.saturating_add(token.delta_line);
      start = if token.delta_line == 0 {
        start.saturating_add(token.delta_start)
      } else {
        token.delta_start
      };

      items.push((line, start, token.length, token.token_type, idx));
    }

    items.sort_by(|a, b| match a.0.cmp(&b.0) {
      std::cmp::Ordering::Equal => match a.1.cmp(&b.1) {
        std::cmp::Ordering::Equal => match a.2.cmp(&b.2) {
          std::cmp::Ordering::Equal => a.4.cmp(&b.4),
          other => other,
        },
        other => other,
      },
      other => other,
    });

    let mut out = String::new();
    for (line, start, len, ty, _) in items {
      let _ = std::fmt::Write::write_fmt(
        &mut out,
        format_args!("{line}:{start} len={len} type={}\n", type_name(ty)),
      );
    }

    out
  }

  #[test]
  fn semantic_tokens_snapshot() {
    let doc = crate::doc::build_document(SAMPLE.to_owned(), "snapshot.bean").unwrap();

    let result = semantic_tokens_full(&doc).expect("tokens");

    let SemanticTokensResult::Tokens(tokens) = result else {
      panic!("expected token result");
    };

    let actual = render_tokens(&tokens.data);
    insta::assert_snapshot!("semantic_tokens_snapshot", actual);
  }
}

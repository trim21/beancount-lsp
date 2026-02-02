use std::cmp::Ordering;

use beancount_parser::parse_str;
use beancount_parser::ast;
use ropey::Rope;
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensResult,
};

use crate::text::byte_to_lsp_position;

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

/// Compute semantic tokens for a document using the tree-sitter grammar.
pub fn semantic_tokens_full(text: &str) -> Option<SemanticTokensResult> {
    let content = Rope::from_str(text);

    let directives = match parse_str(text, "<memory>") {
        Ok(directives) => directives,
        Err(_) => {
            return Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: vec![],
            }))
        }
    };

    let mut raw_tokens = Vec::new();
    for directive in directives.iter() {
        collect_tokens_for_directive(directive, &content, &mut raw_tokens);
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

fn collect_tokens_for_directive(
    directive: &ast::Directive<'_>,
    content: &Rope,
    out: &mut Vec<RawToken>,
) {
    match directive {
        ast::Directive::Open(open) => {
            push_keyword_span(out, content, open.keyword);
            push_with_span(out, content, &open.date, TokenKind::Number);
            for currency in open.currencies.iter() {
                push_with_span(out, content, currency, TokenKind::Class);
            }
            if let Some(booking) = &open.opt_booking {
                push_with_span(out, content, booking, TokenKind::Keyword);
            }
            if let Some(comment) = &open.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in open.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Close(close) => {
            push_keyword_span(out, content, close.keyword);
            push_with_span(out, content, &close.date, TokenKind::Number);
            if let Some(comment) = &close.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in close.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Balance(balance) => {
            push_keyword_span(out, content, balance.keyword);
            push_with_span(out, content, &balance.date, TokenKind::Number);
            push_amount(out, content, &balance.amount);
            if let Some(tolerance) = &balance.tolerance {
                push_with_span(out, content, tolerance, TokenKind::Number);
            }
            if let Some(comment) = &balance.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in balance.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Pad(pad) => {
            push_keyword_span(out, content, pad.keyword);
            push_with_span(out, content, &pad.date, TokenKind::Number);
            if let Some(comment) = &pad.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in pad.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Transaction(txn) => {
            push_with_span(out, content, &txn.date, TokenKind::Number);
            if let Some(flag) = &txn.txn {
                push_with_span(out, content, flag, TokenKind::Operator);
            }
            if let Some(payee) = &txn.payee {
                push_with_span(out, content, payee, TokenKind::String);
            }
            if let Some(narration) = &txn.narration {
                push_with_span(out, content, narration, TokenKind::String);
            }
            for tag in txn.tags.iter() {
                push_with_span(out, content, tag, TokenKind::Parameter);
            }
            for link in txn.links.iter() {
                push_with_span(out, content, link, TokenKind::Parameter);
            }
            if let Some(comment) = &txn.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for comment in txn.comments.iter() {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in txn.key_values.iter() {
                push_key_value(out, content, kv);
            }
            for posting in txn.postings.iter() {
                collect_tokens_for_posting(posting, content, out);
            }
        }
        ast::Directive::Commodity(commodity) => {
            push_keyword_span(out, content, commodity.keyword);
            push_with_span(out, content, &commodity.date, TokenKind::Number);
            push_with_span(out, content, &commodity.currency, TokenKind::Class);
            if let Some(comment) = &commodity.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in commodity.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Price(price) => {
            push_keyword_span(out, content, price.keyword);
            push_with_span(out, content, &price.date, TokenKind::Number);
            push_with_span(out, content, &price.currency, TokenKind::Class);
            push_amount(out, content, &price.amount);
            if let Some(comment) = &price.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in price.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Event(event) => {
            push_keyword_span(out, content, event.keyword);
            push_with_span(out, content, &event.date, TokenKind::Number);
            push_with_span(out, content, &event.event_type, TokenKind::String);
            push_with_span(out, content, &event.desc, TokenKind::String);
            if let Some(comment) = &event.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in event.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Query(query) => {
            push_keyword_span(out, content, query.keyword);
            push_with_span(out, content, &query.date, TokenKind::Number);
            push_with_span(out, content, &query.name, TokenKind::String);
            push_with_span(out, content, &query.query, TokenKind::String);
            if let Some(comment) = &query.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in query.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Note(note) => {
            push_keyword_span(out, content, note.keyword);
            push_with_span(out, content, &note.date, TokenKind::Number);
            push_with_span(out, content, &note.note, TokenKind::String);
            if let Some(comment) = &note.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in note.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Document(document) => {
            push_keyword_span(out, content, document.keyword);
            push_with_span(out, content, &document.date, TokenKind::Number);
            push_with_span(out, content, &document.filename, TokenKind::String);
            for tag in document.tags.iter() {
                push_with_span(out, content, tag, TokenKind::Parameter);
            }
            for link in document.links.iter() {
                push_with_span(out, content, link, TokenKind::Parameter);
            }
            if let Some(comment) = &document.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in document.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Custom(custom) => {
            push_keyword_span(out, content, custom.keyword);
            push_with_span(out, content, &custom.date, TokenKind::Number);
            push_with_span(out, content, &custom.name, TokenKind::Function);
            for value in custom.values.iter() {
                push_custom_value(out, content, value);
            }
            if let Some(comment) = &custom.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
            for kv in custom.key_values.iter() {
                push_key_value(out, content, kv);
            }
        }
        ast::Directive::Option(option) => {
            push_keyword_span(out, content, option.keyword);
            push_with_span(out, content, &option.key, TokenKind::Property);
            push_with_span(out, content, &option.value, TokenKind::String);
            if let Some(comment) = &option.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::Include(include) => {
            push_keyword_span(out, content, include.keyword);
            push_with_span(out, content, &include.filename, TokenKind::String);
            if let Some(comment) = &include.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::Plugin(plugin) => {
            push_keyword_span(out, content, plugin.keyword);
            push_with_span(out, content, &plugin.name, TokenKind::String);
            if let Some(config) = &plugin.config {
                push_with_span(out, content, config, TokenKind::String);
            }
            if let Some(comment) = &plugin.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::PushTag(tag) => {
            push_keyword_span(out, content, tag.keyword);
            push_with_span(out, content, &tag.tag, TokenKind::Parameter);
            if let Some(comment) = &tag.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::PopTag(tag) => {
            push_keyword_span(out, content, tag.keyword);
            push_with_span(out, content, &tag.tag, TokenKind::Parameter);
            if let Some(comment) = &tag.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::PushMeta(meta) => {
            push_keyword_span(out, content, meta.keyword);
            push_with_span(out, content, &meta.key, TokenKind::Property);
            if let Some(value) = &meta.value {
                push_key_value_value(out, content, value);
            }
            if let Some(comment) = &meta.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::PopMeta(meta) => {
            push_keyword_span(out, content, meta.keyword);
            push_with_span(out, content, &meta.key, TokenKind::Property);
            if let Some(comment) = &meta.comment {
                push_with_span(out, content, comment, TokenKind::Comment);
            }
        }
        ast::Directive::Comment(comment) => {
            push_with_span(out, content, &comment.text, TokenKind::Comment);
        }
        ast::Directive::Headline(headline) => {
            push_with_span(out, content, &headline.text, TokenKind::String);
        }
        ast::Directive::Raw(_) => {}
    }
}

fn collect_tokens_for_posting(
    posting: &ast::Posting<'_>,
    content: &Rope,
    out: &mut Vec<RawToken>,
) {
    if let Some(flag) = &posting.opt_flag {
        push_with_span(out, content, flag, TokenKind::Operator);
    }
    if let Some(amount) = &posting.amount {
        push_amount(out, content, amount);
    }
    if let Some(cost_spec) = &posting.cost_spec {
        push_cost_spec(out, content, cost_spec);
    }
    if let Some(price_op) = &posting.price_operator {
        push_with_span(out, content, price_op, TokenKind::Operator);
    }
    if let Some(price) = &posting.price_annotation {
        push_amount(out, content, price);
    }
    if let Some(comment) = &posting.comment {
        push_with_span(out, content, comment, TokenKind::Comment);
    }
    for kv in posting.key_values.iter() {
        push_key_value(out, content, kv);
    }
}

fn push_cost_spec(out: &mut Vec<RawToken>, content: &Rope, cost_spec: &ast::CostSpec<'_>) {
    if let Some(amount) = &cost_spec.amount {
        if let Some(per) = &amount.per {
            push_number_expr(out, content, per);
        }
        if let Some(total) = &amount.total {
            push_number_expr(out, content, total);
        }
        if let Some(currency) = &amount.currency {
            push_with_span(out, content, currency, TokenKind::Class);
        }
    }
    if let Some(date) = &cost_spec.date {
        push_with_span(out, content, date, TokenKind::Number);
    }
    if let Some(label) = &cost_spec.label {
        push_with_span(out, content, label, TokenKind::String);
    }
    if let Some(merge) = &cost_spec.merge {
        push_with_span(out, content, merge, TokenKind::Keyword);
    }
    push_with_span(out, content, &cost_spec.is_total, TokenKind::Operator);
}

fn push_amount(out: &mut Vec<RawToken>, content: &Rope, amount: &ast::Amount<'_>) {
    push_number_expr(out, content, &amount.number);
    if let Some(currency) = &amount.currency {
        push_with_span(out, content, currency, TokenKind::Class);
    }
}

fn push_custom_value(out: &mut Vec<RawToken>, content: &Rope, value: &ast::CustomValue<'_>) {
    match value.kind {
        ast::CustomValueKind::String => {
            push_with_span(out, content, &value.raw, TokenKind::String);
        }
        ast::CustomValueKind::Date => {
            push_with_span(out, content, &value.raw, TokenKind::Number);
        }
        ast::CustomValueKind::Bool => {
            push_with_span(out, content, &value.raw, TokenKind::Keyword);
        }
        ast::CustomValueKind::Number => {
            if let Some(expr) = &value.number {
                push_number_expr(out, content, expr);
            } else {
                push_with_span(out, content, &value.raw, TokenKind::Number);
            }
        }
        ast::CustomValueKind::Amount => {
            if let Some(amount) = &value.amount {
                push_amount(out, content, amount);
            } else {
                push_with_span(out, content, &value.raw, TokenKind::Number);
            }
        }
        ast::CustomValueKind::Account => {}
    }
}

fn push_key_value(out: &mut Vec<RawToken>, content: &Rope, kv: &ast::KeyValue<'_>) {
    push_with_span(out, content, &kv.key, TokenKind::Property);
    if let Some(value) = &kv.value {
        push_key_value_value(out, content, value);
    }
}

fn push_key_value_value(
    out: &mut Vec<RawToken>,
    content: &Rope,
    value: &ast::WithSpan<ast::KeyValueValue<'_>>,
) {
    match value.content {
        ast::KeyValueValue::String(_) | ast::KeyValueValue::UnquotedString(_) => {
            push_with_span(out, content, value, TokenKind::String);
        }
        ast::KeyValueValue::Date(_) => {
            push_with_span(out, content, value, TokenKind::Number);
        }
        ast::KeyValueValue::Bool(_) => {
            push_with_span(out, content, value, TokenKind::Keyword);
        }
        ast::KeyValueValue::Raw(_) => {
            push_with_span(out, content, value, TokenKind::String);
        }
    }
}

fn push_number_expr(out: &mut Vec<RawToken>, content: &Rope, expr: &ast::NumberExpr<'_>) {
    match expr {
        ast::NumberExpr::Missing { .. } => {}
        ast::NumberExpr::Literal(literal) => {
            push_with_span(out, content, literal, TokenKind::Number);
        }
        ast::NumberExpr::Binary { left, op, right, .. } => {
            push_number_expr(out, content, left);
            push_with_span(out, content, op, TokenKind::Operator);
            push_number_expr(out, content, right);
        }
    }
}

fn push_with_span<T>(
    out: &mut Vec<RawToken>,
    content: &Rope,
    value: &ast::WithSpan<T>,
    kind: TokenKind,
) {
    if let Some(token) = to_semantic_token_span(content, value.span, kind) {
        out.push(token);
    }
}

fn push_keyword_span(out: &mut Vec<RawToken>, content: &Rope, span: ast::Span) {
    if let Some(token) = to_semantic_token_span(content, span, TokenKind::Function) {
        out.push(token);
    }
}

fn to_semantic_token_span(
    content: &Rope,
    span: ast::Span,
    kind: TokenKind,
) -> Option<RawToken> {
    let start_pos = byte_to_lsp_position(content, span.start)?;
    let end_pos = byte_to_lsp_position(content, span.end)?;

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

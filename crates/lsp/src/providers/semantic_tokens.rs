use std::cmp::Ordering;

use crate::text::byte_to_lsp_position;
use beancount_parser::ast;
use ropey::Rope;
use tower_lsp_server::ls_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensResult,
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

/// Compute semantic tokens for a document using parsed AST spans.
pub fn semantic_tokens_full(doc: &crate::doc::Document) -> Option<SemanticTokensResult> {
    let mut raw_tokens = Vec::with_capacity(doc.content().len() / 5);
    collect_tokens(doc, &mut raw_tokens);

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

fn push_token_from_span(content: &Rope, span: ast::Span, kind: TokenKind, out: &mut Vec<RawToken>) {
    let Some(start_pos) = byte_to_lsp_position(content, span.start) else {
        return;
    };
    let Some(end_pos) = byte_to_lsp_position(content, span.end) else {
        return;
    };

    if start_pos.line != end_pos.line {
        return;
    }

    let Some(length_utf16) = end_pos.character.checked_sub(start_pos.character) else {
        return;
    };

    if length_utf16 == 0 {
        return;
    }

    out.push(RawToken {
        line: start_pos.line,
        start: start_pos.character,
        length: length_utf16,
        token_type: token_index(kind),
        modifiers_bitset: 0,
    });
}

fn push_with_span<T>(
    content: &Rope,
    value: &ast::WithSpan<T>,
    kind: TokenKind,
    out: &mut Vec<RawToken>,
) {
    push_token_from_span(content, value.span, kind, out);
}

fn push_opt_with_span<T>(
    content: &Rope,
    value: &Option<ast::WithSpan<T>>,
    kind: TokenKind,
    out: &mut Vec<RawToken>,
) {
    if let Some(span) = value {
        push_with_span(content, span, kind, out);
    }
}

fn push_tags(content: &Rope, tags: &[ast::WithSpan<&str>], out: &mut Vec<RawToken>) {
    for tag in tags {
        push_with_span(content, tag, TokenKind::Parameter, out);
    }
}

fn push_comments(content: &Rope, comments: &[ast::WithSpan<&str>], out: &mut Vec<RawToken>) {
    for comment in comments {
        push_with_span(content, comment, TokenKind::Comment, out);
    }
}

fn collect_number_expr_tokens(content: &Rope, expr: &ast::NumberExpr<'_>, out: &mut Vec<RawToken>) {
    match expr {
        ast::NumberExpr::Missing { .. } => {}
        ast::NumberExpr::Literal(value) => push_with_span(content, value, TokenKind::Number, out),
        ast::NumberExpr::Binary {
            left, op, right, ..
        } => {
            collect_number_expr_tokens(content, left, out);
            push_with_span(content, op, TokenKind::Operator, out);
            collect_number_expr_tokens(content, right, out);
        }
    }
}

fn collect_amount_tokens(content: &Rope, amount: &ast::Amount<'_>, out: &mut Vec<RawToken>) {
    collect_number_expr_tokens(content, &amount.number, out);

    if let Some(currency) = &amount.currency {
        push_with_span(content, currency, TokenKind::Class, out);
    }
}

fn collect_cost_amount_tokens(content: &Rope, cost: &ast::CostAmount<'_>, out: &mut Vec<RawToken>) {
    if let Some(per) = &cost.per {
        collect_number_expr_tokens(content, per, out);
    }

    if let Some(total) = &cost.total {
        collect_number_expr_tokens(content, total, out);
    }

    if let Some(currency) = &cost.currency {
        push_with_span(content, currency, TokenKind::Class, out);
    }
}

fn collect_cost_spec_tokens(
    content: &Rope,
    cost_spec: &ast::CostSpec<'_>,
    out: &mut Vec<RawToken>,
) {
    if let Some(amount) = &cost_spec.amount {
        collect_cost_amount_tokens(content, amount, out);
    }

    if let Some(date) = &cost_spec.date {
        push_with_span(content, date, TokenKind::Number, out);
    }

    if let Some(label) = &cost_spec.label {
        push_with_span(content, label, TokenKind::String, out);
    }

    if let Some(merge) = &cost_spec.merge {
        push_with_span(content, merge, TokenKind::Keyword, out);
    }

    push_with_span(content, &cost_spec.is_total, TokenKind::Keyword, out);
}

fn collect_key_value_value_tokens(
    content: &Rope,
    value: &ast::WithSpan<ast::KeyValueValue<'_>>,
    out: &mut Vec<RawToken>,
) {
    let kind = match value.content {
        ast::KeyValueValue::String(_)
        | ast::KeyValueValue::UnquotedString(_)
        | ast::KeyValueValue::Raw(_) => TokenKind::String,
        ast::KeyValueValue::Date(_) => TokenKind::Number,
        ast::KeyValueValue::Bool(_) => TokenKind::Keyword,
    };

    push_token_from_span(content, value.span, kind, out);
}

fn collect_key_values(content: &Rope, key_values: &[ast::KeyValue<'_>], out: &mut Vec<RawToken>) {
    for key_value in key_values {
        push_with_span(content, &key_value.key, TokenKind::Keyword, out);

        if let Some(value) = &key_value.value {
            collect_key_value_value_tokens(content, value, out);
        }
    }
}

fn collect_custom_value_tokens(
    content: &Rope,
    value: &ast::CustomValue<'_>,
    out: &mut Vec<RawToken>,
) {
    match value.kind {
        ast::CustomValueKind::String => push_with_span(content, &value.raw, TokenKind::String, out),
        ast::CustomValueKind::Date => push_with_span(content, &value.raw, TokenKind::Number, out),
        ast::CustomValueKind::Bool => push_with_span(content, &value.raw, TokenKind::Keyword, out),
        ast::CustomValueKind::Amount => {
            if let Some(amount) = &value.amount {
                collect_amount_tokens(content, amount, out);
            } else {
                push_with_span(content, &value.raw, TokenKind::Number, out);
            }
        }
        ast::CustomValueKind::Number => {
            if let Some(number) = &value.number {
                collect_number_expr_tokens(content, number, out);
            } else {
                push_with_span(content, &value.raw, TokenKind::Number, out);
            }
        }
        ast::CustomValueKind::Account => {}
    }
}

fn collect_posting_tokens(content: &Rope, posting: &ast::Posting<'_>, out: &mut Vec<RawToken>) {
    push_opt_with_span(content, &posting.opt_flag, TokenKind::Keyword, out);

    if let Some(amount) = &posting.amount {
        collect_amount_tokens(content, amount, out);
    }

    if let Some(cost_spec) = &posting.cost_spec {
        collect_cost_spec_tokens(content, cost_spec, out);
    }

    if let Some(price_operator) = &posting.price_operator {
        push_with_span(content, price_operator, TokenKind::Operator, out);
    }

    if let Some(price_annotation) = &posting.price_annotation {
        collect_amount_tokens(content, price_annotation, out);
    }

    push_opt_with_span(content, &posting.comment, TokenKind::Comment, out);
    collect_key_values(content, &posting.key_values, out);
}

fn collect_transaction_tokens(content: &Rope, tx: &ast::Transaction<'_>, out: &mut Vec<RawToken>) {
    push_with_span(content, &tx.date, TokenKind::Number, out);
    push_opt_with_span(content, &tx.txn, TokenKind::Keyword, out);
    push_opt_with_span(content, &tx.payee, TokenKind::String, out);
    push_opt_with_span(content, &tx.narration, TokenKind::String, out);

    if let Some(tags_links) = &tx.tags_links {
        push_tags(content, tags_links, out);
    }

    push_tags(content, &tx.tags_links_lines, out);

    push_opt_with_span(content, &tx.comment, TokenKind::Comment, out);
    push_comments(content, &tx.comments, out);

    collect_key_values(content, &tx.key_values, out);

    for posting in &tx.postings {
        collect_posting_tokens(content, posting, out);
    }
}

fn collect_tokens(doc: &crate::doc::Document, out: &mut Vec<RawToken>) {
    let content = &doc.rope;

    for directive in doc.ast() {
        match directive {
            ast::Directive::Open(open) => {
                push_with_span(content, &open.date, TokenKind::Number, out);

                for currency in &open.currencies {
                    push_with_span(content, currency, TokenKind::Class, out);
                }

                push_opt_with_span(content, &open.opt_booking, TokenKind::String, out);
                push_opt_with_span(content, &open.comment, TokenKind::Comment, out);
                collect_key_values(content, &open.key_values, out);
            }
            ast::Directive::Close(close) => {
                push_with_span(content, &close.date, TokenKind::Number, out);
                push_opt_with_span(content, &close.comment, TokenKind::Comment, out);
                collect_key_values(content, &close.key_values, out);
            }
            ast::Directive::Balance(balance) => {
                push_with_span(content, &balance.date, TokenKind::Number, out);
                collect_amount_tokens(content, &balance.amount, out);

                if let Some(tolerance) = &balance.tolerance {
                    push_with_span(content, tolerance, TokenKind::Number, out);
                }

                push_opt_with_span(content, &balance.comment, TokenKind::Comment, out);
                collect_key_values(content, &balance.key_values, out);
            }
            ast::Directive::Pad(pad) => {
                push_with_span(content, &pad.date, TokenKind::Number, out);
                push_opt_with_span(content, &pad.comment, TokenKind::Comment, out);
                collect_key_values(content, &pad.key_values, out);
            }
            ast::Directive::Transaction(tx) => {
                collect_transaction_tokens(content, tx, out);
            }
            ast::Directive::Commodity(cmdty) => {
                push_with_span(content, &cmdty.date, TokenKind::Number, out);
                push_with_span(content, &cmdty.currency, TokenKind::Class, out);
                push_opt_with_span(content, &cmdty.comment, TokenKind::Comment, out);
                collect_key_values(content, &cmdty.key_values, out);
            }
            ast::Directive::Price(price) => {
                push_with_span(content, &price.date, TokenKind::Number, out);
                push_with_span(content, &price.currency, TokenKind::Class, out);
                collect_amount_tokens(content, &price.amount, out);
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
                push_with_span(content, &custom.date, TokenKind::Number, out);
                push_with_span(content, &custom.name, TokenKind::String, out);

                for value in &custom.values {
                    collect_custom_value_tokens(content, value, out);
                }

                push_opt_with_span(content, &custom.comment, TokenKind::Comment, out);
                collect_key_values(content, &custom.key_values, out);
            }
            ast::Directive::Option(option) => {
                push_with_span(content, &option.key, TokenKind::Keyword, out);
                push_with_span(content, &option.value, TokenKind::String, out);
                push_opt_with_span(content, &option.comment, TokenKind::Comment, out);
            }
            ast::Directive::Include(include) => {
                push_with_span(content, &include.filename, TokenKind::String, out);
                push_opt_with_span(content, &include.comment, TokenKind::Comment, out);
            }
            ast::Directive::Plugin(plugin) => {
                push_with_span(content, &plugin.name, TokenKind::String, out);
                push_opt_with_span(content, &plugin.config, TokenKind::String, out);
                push_opt_with_span(content, &plugin.comment, TokenKind::Comment, out);
            }
            ast::Directive::PushTag(tag) => {
                push_with_span(content, &tag.tag, TokenKind::Parameter, out);
                push_opt_with_span(content, &tag.comment, TokenKind::Comment, out);
            }
            ast::Directive::PopTag(tag) => {
                push_with_span(content, &tag.tag, TokenKind::Parameter, out);
                push_opt_with_span(content, &tag.comment, TokenKind::Comment, out);
            }
            ast::Directive::PushMeta(pm) => {
                push_with_span(content, &pm.key, TokenKind::Keyword, out);

                if let Some(value) = &pm.value {
                    collect_key_value_value_tokens(content, value, out);
                }

                push_opt_with_span(content, &pm.comment, TokenKind::Comment, out);
            }
            ast::Directive::PopMeta(pm) => {
                push_with_span(content, &pm.key, TokenKind::Keyword, out);
                push_opt_with_span(content, &pm.comment, TokenKind::Comment, out);
            }
            ast::Directive::Comment(comment) => {
                push_with_span(content, &comment.text, TokenKind::Comment, out);
            }
            ast::Directive::Headline(headline) => {
                push_with_span(content, &headline.text, TokenKind::Keyword, out);
            }
            ast::Directive::Raw(raw) => {
                let _ = raw;
            }
        }
    }
}

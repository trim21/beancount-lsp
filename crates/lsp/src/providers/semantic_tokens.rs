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
    let mut raw_tokens = Vec::new();
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

fn collect_tokens(doc: &crate::doc::Document, out: &mut Vec<RawToken>) {
    let content = &doc.rope;

    for directive in doc.ast() {
        match directive {
            ast::Directive::Open(open) => {
                push_token_from_span(content, open.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, open.account.span, TokenKind::Parameter, out);
                for currency in &open.currencies {
                    push_token_from_span(content, currency.span, TokenKind::Class, out);
                }
            }
            ast::Directive::Close(close) => {
                push_token_from_span(content, close.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, close.account.span, TokenKind::Parameter, out);
            }
            ast::Directive::Balance(balance) => {
                push_token_from_span(content, balance.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, balance.account.span, TokenKind::Parameter, out);
            }
            ast::Directive::Pad(pad) => {
                push_token_from_span(content, pad.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, pad.account.span, TokenKind::Parameter, out);
                push_token_from_span(content, pad.from_account.span, TokenKind::Parameter, out);
            }
            ast::Directive::Transaction(tx) => {
                push_token_from_span(content, tx.date.span, TokenKind::Number, out);
                for posting in &tx.postings {
                    push_token_from_span(content, posting.account.span, TokenKind::Parameter, out);
                    if let Some(amount) = &posting.amount {
                        push_token_from_span(content, amount.raw.span, TokenKind::Number, out);
                        if let Some(currency) = &amount.currency {
                            push_token_from_span(content, currency.span, TokenKind::Class, out);
                        }
                    }
                }
            }
            ast::Directive::Commodity(cmdty) => {
                push_token_from_span(content, cmdty.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, cmdty.currency.span, TokenKind::Class, out);
            }
            ast::Directive::Price(price) => {
                push_token_from_span(content, price.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, price.currency.span, TokenKind::Class, out);
            }
            ast::Directive::Event(event) => {
                push_token_from_span(content, event.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Query(query) => {
                push_token_from_span(content, query.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Note(note) => {
                push_token_from_span(content, note.keyword, TokenKind::Keyword, out);
                push_token_from_span(content, note.account.span, TokenKind::Parameter, out);
            }
            ast::Directive::Document(doc_directive) => {
                push_token_from_span(content, doc_directive.keyword, TokenKind::Keyword, out);
                push_token_from_span(
                    content,
                    doc_directive.account.span,
                    TokenKind::Parameter,
                    out,
                );
            }
            ast::Directive::Custom(custom) => {
                push_token_from_span(content, custom.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Option(option) => {
                push_token_from_span(content, option.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Include(include) => {
                push_token_from_span(content, include.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Plugin(plugin) => {
                push_token_from_span(content, plugin.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::PushTag(tag) => {
                push_token_from_span(content, tag.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::PopTag(tag) => {
                push_token_from_span(content, tag.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::PushMeta(pm) => {
                push_token_from_span(content, pm.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::PopMeta(pm) => {
                push_token_from_span(content, pm.keyword, TokenKind::Keyword, out);
            }
            ast::Directive::Comment(comment) => {
                push_token_from_span(content, comment.span, TokenKind::Comment, out);
            }
            ast::Directive::Headline(headline) => {
                push_token_from_span(content, headline.span, TokenKind::Keyword, out);
            }
            ast::Directive::Raw(raw) => {
                push_token_from_span(content, raw.span, TokenKind::Keyword, out);
            }
        }
    }
}

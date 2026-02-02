use std::cmp::Ordering;

use crate::text::byte_to_lsp_position;
use beancount_tree_sitter::{NodeKind, language, tree_sitter};
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

/// Compute semantic tokens for a document using the tree-sitter grammar.
pub fn semantic_tokens_full(text: &str) -> Option<SemanticTokensResult> {
    let mut parser = tree_sitter::Parser::new();
    parser.set_language(&language()).ok()?;

    let tree = parser.parse(text, None)?;
    let content = Rope::from_str(text);

    let mut raw_tokens = Vec::new();
    collect_tokens(&tree.root_node(), &content, &mut raw_tokens);

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

fn collect_tokens(node: &tree_sitter::Node, content: &Rope, out: &mut Vec<RawToken>) {
    let child = match NodeKind::from(node.kind()) {
        NodeKind::Include
        | NodeKind::Pushtag
        | NodeKind::Poptag
        | NodeKind::Pushmeta
        | NodeKind::Popmeta
        | NodeKind::Plugin
        | NodeKind::Option => Some((0, TokenKind::Function)),

        NodeKind::Open
        | NodeKind::Pad
        | NodeKind::Note
        | NodeKind::Balance
        | NodeKind::Transaction
        | NodeKind::Custom => Some((1, TokenKind::Function)),

        _ => None,
    };

    if let Some((index, kind)) = child
        && let Some(child) = node.child(index)
        && let Some(token) = to_semantic_token(&child, content, kind)
    {
        out.push(token);
    }

    if let Some(kind) = classify_node(node.kind().into())
        && let Some(tok) = to_semantic_token(node, content, kind)
    {
        out.push(tok);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_tokens(&child, content, out);
    }
}

fn classify_node(kind: NodeKind) -> Option<TokenKind> {
    match kind {
        NodeKind::Account => None,

        NodeKind::Asterisk
        | NodeKind::At
        | NodeKind::Atat
        | NodeKind::Plus
        | NodeKind::Minus
        | NodeKind::Slash => Some(TokenKind::Operator),

        NodeKind::Flag | NodeKind::Bool | NodeKind::Item | NodeKind::Key => {
            Some(TokenKind::Keyword)
        }

        NodeKind::Comment => Some(TokenKind::Comment),

        NodeKind::Currency => Some(TokenKind::Class),

        NodeKind::Date | NodeKind::Number => Some(TokenKind::Number),

        NodeKind::Link | NodeKind::Tag => Some(TokenKind::Parameter),

        NodeKind::Narration | NodeKind::Payee | NodeKind::String | NodeKind::UnquotedString => {
            Some(TokenKind::String)
        }

        NodeKind::Unknown => None,

        _ => None,
    }
}

fn to_semantic_token(
    node: &tree_sitter::Node,
    content: &Rope,
    kind: TokenKind,
) -> Option<RawToken> {
    let start_byte = node.start_byte();
    let end_byte = node.end_byte();

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

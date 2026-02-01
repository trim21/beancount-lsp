use std::collections::{HashMap, HashSet};

use beancount_parser::core;
use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, CompletionTextEdit,
    Position, Range, TextEdit, Url,
};

use crate::server::Document;
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

fn current_prefix(doc: &Document, position: Position) -> Option<(String, Range)> {
    let byte_idx = lsp_position_to_byte(&doc.rope, position)?;

    let line = doc.rope.byte_to_line(byte_idx);
    let line_start = doc.rope.line_to_byte(line);
    let line_slice = doc.rope.byte_slice(line_start..byte_idx).to_string();

    let token_start = line_slice
        .rfind(char::is_whitespace)
        .map(|idx| idx + 1)
        .unwrap_or(0);

    let prefix = line_slice[token_start..].trim();
    let start_byte = line_start + token_start;
    let start = byte_to_lsp_position(&doc.rope, start_byte)?;

    Some((
        prefix.to_owned(),
        Range {
            start,
            end: position,
        },
    ))
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

pub fn completion(
    documents: &HashMap<Url, Document>,
    params: &CompletionParams,
) -> Option<CompletionResponse> {
    let mut accounts = HashSet::new();

    for doc in documents.values() {
        for dir in &doc.directives {
            if let core::CoreDirective::Open(o) = dir {
                accounts.insert(o.account.clone());
            }
        }
    }

    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let (prefix, replace_range) = documents
        .get(uri)
        .and_then(|doc| current_prefix(doc, position))
        .unwrap_or_else(|| (String::new(), Range::new(position, position)));

    let mut items: Vec<CompletionItem> = accounts
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
        .collect();

    items.sort_by(|a, b| a.label.cmp(&b.label));

    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use beancount_parser::{core, parse_str};
    use beancount_tree_sitter::{language, tree_sitter};
    use tower_lsp::lsp_types::{
        CompletionContext, CompletionTriggerKind, TextDocumentIdentifier,
        TextDocumentPositionParams,
    };

    fn build_doc(uri: &Url, content: &str) -> Document {
        let leaked: &'static str = Box::leak(content.to_owned().into_boxed_str());
        let directives =
            core::normalize_directives(parse_str(leaked, uri.as_str()).unwrap()).unwrap();
        let rope = ropey::Rope::from_str(content);
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language()).unwrap();
        let tree = parser.parse(content, None).unwrap();
        Document {
            directives,
            content: content.to_owned(),
            rope,
            tree,
        }
    }

    #[test]
    fn completes_accounts_and_replaces_prefix() {
        let uri = Url::parse("file:///main.bean").unwrap();
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

        let response = completion(&documents, &params).expect("got completion");
        let items = match response {
            CompletionResponse::Array(items) => items,
            _ => panic!("unexpected completion response"),
        };

        let item = items
            .iter()
            .find(|i| i.label == "Assets:Cash")
            .expect("expected Assets:Cash item");

        let edit = match item.text_edit.as_ref().expect("text edit") {
            CompletionTextEdit::Edit(e) => e,
            _ => panic!("unexpected insert replace edit"),
        };

        assert_eq!(edit.range.start, Position::new(0, 16));
        assert_eq!(edit.range.end, Position::new(0, 25));
        assert_eq!(edit.new_text, "Assets:Cash");
    }
}

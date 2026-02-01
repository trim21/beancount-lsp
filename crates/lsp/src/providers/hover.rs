use std::collections::HashMap;

use beancount_parser::core;
use tower_lsp::lsp_types::{Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Url};

use crate::providers::account::account_at_position;
use crate::server::{find_document, Document};

fn notes_for_account(documents: &HashMap<Url, Document>, account: &str) -> Vec<String> {
    let mut notes = Vec::new();
    for doc in documents.values() {
        for dir in &doc.directives {
            if let core::CoreDirective::Note(n) = dir
                && n.account == account
                && !n.note.is_empty()
            {
                let note = n.note.clone();
                notes.push(note);
            }
        }
    }
    notes
}

pub fn hover(documents: &HashMap<Url, Document>, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let (account, account_range) = find_document(documents, uri)
        .and_then(|doc| account_at_position(doc, position))?;

    let notes = notes_for_account(documents, &account);
    if notes.is_empty() {
        return None;
    }

    let bullet_list = notes
        .into_iter()
        .map(|note| format!("- {}", note))
        .collect::<Vec<_>>()
        .join("\n");

    let contents = HoverContents::Markup(MarkupContent {
        kind: MarkupKind::Markdown,
        value: format!("**Notes for {}**\n\n{}", account, bullet_list),
    });

    Some(Hover {
        contents,
        range: Some(account_range),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::account::account_at_position;
    use beancount_parser::{core, parse_str};
    use beancount_tree_sitter::{language, tree_sitter};
    use tower_lsp::lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams};

    fn build_doc(uri: &Url, content: &str) -> Document {
        let directives =
            core::normalize_directives(parse_str(content, uri.as_str()).unwrap()).unwrap();
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
    fn hover_shows_account_notes() {
        let uri = Url::parse("file:///hover.bean").unwrap();
        let content = "2023-01-01 open Assets:Cash\n2023-02-01 note Assets:Cash \"First note\"\n";
        let doc = build_doc(&uri, content);

        let mut docs = HashMap::new();
        docs.insert(uri.clone(), doc);

        let cursor = Position::new(1, 20);

        let hit = account_at_position(docs.get(&uri).unwrap(), cursor);
        assert!(hit.is_some(), "expected account under cursor");

        let notes = notes_for_account(&docs, "Assets:Cash");
        assert!(!notes.is_empty(), "expected notes for account");

        let params = HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: cursor,
            },
            work_done_progress_params: Default::default(),
        };

        let hover = hover(&docs, &params).expect("hover present");
        let contents = match hover.contents {
            HoverContents::Markup(markup) => markup.value,
            _ => panic!("expected markup hover"),
        };

        assert!(contents.contains("First note"));
        assert!(contents.contains("Notes for Assets:Cash"));
    }
}

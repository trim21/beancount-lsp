use std::collections::HashMap;
use std::sync::Arc;

use beancount_parser::ast;
use tower_lsp_server::ls_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Uri as Url,
};

use crate::providers::account::account_at_position;
use crate::server::{Document, documents_bfs, find_document};
use crate::text::byte_span_to_lsp_range;

fn collect_open_definitions(
    doc: &Document,
    uri: &Url,
    account: &str,
    locations: &mut Vec<Location>,
) {
    for directive in doc.ast() {
        if let ast::Directive::Open(open) = directive
            && open.account.content == account
            && let Some(range) =
                byte_span_to_lsp_range(&doc.rope, open.account.span.start, open.account.span.end)
        {
            locations.push(Location {
                uri: uri.clone(),
                range,
            });
        }
    }
}

pub fn goto_definition(
    documents: &HashMap<Url, Arc<Document>>,
    root_uri: &Url,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let (account, _) = find_document(documents, uri)
        .and_then(|doc| account_at_position(doc.as_ref(), position))?;

    let mut locations = Vec::new();
    for (doc_uri, doc) in documents_bfs(documents, root_uri) {
        collect_open_definitions(doc.as_ref(), &doc_uri, &account, &mut locations);
    }

    if locations.is_empty() {
        None
    } else {
        Some(GotoDefinitionResponse::Array(locations))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc;
    use crate::test_utils::lines;
    use std::str::FromStr;
    use tower_lsp_server::ls_types::{
        Position, TextDocumentIdentifier, TextDocumentPositionParams, Uri as Url,
    };

    fn build_doc(uri: &Url, content: &str) -> Arc<Document> {
        Arc::new(doc::build_document(content.to_string(), uri.as_str()).expect("build doc"))
    }

    #[test]
    fn goto_definition_returns_open_locations() {
        let open_uri = Url::from_str("file:///open.bean").unwrap();
        let open_content = lines(&[r#"2023-01-01 open Assets:Cash"#]);
        let open_doc = build_doc(&open_uri, &open_content);

        let txn_uri = Url::from_str("file:///txn.bean").unwrap();
        let txn_content = lines(&["2023-02-01 txn \"\" \"\"", "  Assets:Cash 1 USD"]);
        let txn_doc = build_doc(&txn_uri, &txn_content);

        let mut docs = HashMap::new();
        docs.insert(open_uri.clone(), open_doc);
        docs.insert(txn_uri.clone(), txn_doc);

        let params = GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: txn_uri.clone(),
                },
                position: Position::new(1, 4), // inside Assets:Cash posting
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        };

        let resp = goto_definition(&docs, &open_uri, &params).expect("definition response");
        let locations = match resp {
            GotoDefinitionResponse::Array(locs) => locs,
            _ => panic!("unexpected goto definition response"),
        };

        assert_eq!(locations.len(), 1);
        let loc = &locations[0];
        assert_eq!(loc.uri, open_uri);
        assert_eq!(loc.range.start, Position::new(0, 16));
        assert_eq!(loc.range.end, Position::new(0, 27));
    }
}

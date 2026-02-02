use std::collections::HashMap;

use beancount_tree_sitter::{NodeKind, tree_sitter};
use ropey::Rope;
use tower_lsp_server::ls_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, Uri as Url,
};

use crate::providers::account::account_at_position;
use crate::server::{Document, documents_bfs, find_document};
use crate::text::ts_point_to_lsp_position;

fn collect_open_definitions(
    doc: &Document,
    uri: &Url,
    account: &str,
    locations: &mut Vec<Location>,
) {
    let rope = &doc.rope;
    collect_nodes(doc.tree.root_node(), rope, uri, account, locations);
}

fn collect_nodes(
    node: tree_sitter::Node,
    rope: &Rope,
    uri: &Url,
    account: &str,
    out: &mut Vec<Location>,
) {
    if NodeKind::from(node.kind()) == NodeKind::Account
        && let Some(parent) = node.parent()
        && NodeKind::from(parent.kind()) == NodeKind::Open
    {
        let text = rope
            .byte_slice(node.start_byte()..node.end_byte())
            .to_string()
            .trim()
            .to_owned();

        if text == account
            && let (Some(start), Some(end)) = (
                ts_point_to_lsp_position(rope, node.start_position()),
                ts_point_to_lsp_position(rope, node.end_position()),
            )
        {
            out.push(Location {
                uri: uri.clone(),
                range: Range { start, end },
            });
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_nodes(child, rope, uri, account, out);
    }
}

pub fn goto_definition(
    documents: &HashMap<Url, Document>,
    root_uri: &Url,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let (account, _) =
        find_document(documents, uri).and_then(|doc| account_at_position(doc, position))?;

    let mut locations = Vec::new();
    for (doc_uri, doc) in documents_bfs(documents, root_uri) {
        collect_open_definitions(doc, &doc_uri, &account, &mut locations);
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
    use beancount_parser::{core, parse_str};
    use beancount_tree_sitter::{language, tree_sitter};
    use std::str::FromStr;
    use tower_lsp_server::ls_types::{
        Position, TextDocumentIdentifier, TextDocumentPositionParams, Uri as Url,
    };

    fn build_doc(uri: &Url, content: &str) -> Document {
        let directives =
            core::normalize_directives(parse_str(content, uri.as_str()).unwrap()).unwrap();
        let includes = directives
            .iter()
            .filter_map(|directive| match directive {
                core::CoreDirective::Include(include) => Some(include.filename.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let rope = ropey::Rope::from_str(content);
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language()).unwrap();
        let tree = parser.parse(content, None).unwrap();
        Document {
            directives,
            includes,
            content: content.to_owned(),
            rope,
            tree,
        }
    }

    #[test]
    fn goto_definition_returns_open_locations() {
        let open_uri = Url::from_str("file:///open.bean").unwrap();
        let open_content = "2023-01-01 open Assets:Cash\n";
        let open_doc = build_doc(&open_uri, open_content);

        let txn_uri = Url::from_str("file:///txn.bean").unwrap();
        let txn_content = "2023-02-01 txn \"\" \"\"\n  Assets:Cash 1 USD\n";
        let txn_doc = build_doc(&txn_uri, txn_content);

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

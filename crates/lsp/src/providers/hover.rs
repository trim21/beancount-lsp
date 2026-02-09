use std::collections::HashMap;
use std::sync::Arc;

use beancount_core as core;
use tower_lsp_server::ls_types::{
  Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Uri as Url,
};

use crate::providers::account::account_at_position;
use crate::server::{Document, documents_bfs, find_document};

fn notes_for_account(
  documents: &HashMap<Url, Arc<Document>>,
  root_uri: &Url,
  account: &str,
) -> Vec<String> {
  let mut notes = Vec::new();
  for (_, doc) in documents_bfs(documents, root_uri) {
    for dir in &doc.directives {
      if let core::Directive::Note(n) = dir
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

pub fn hover(
  documents: &HashMap<Url, Arc<Document>>,
  root_uri: &Url,
  params: &HoverParams,
) -> Option<Hover> {
  let uri = &params.text_document_position_params.text_document.uri;
  let position = params.text_document_position_params.position;
  let (account, account_range) = find_document(documents, uri)
    .and_then(|doc| account_at_position(doc.as_ref(), position))?;

  tracing::debug!("hover requested for account: {}", account);

  let notes = notes_for_account(documents, root_uri, &account);
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
  use crate::doc;
  use crate::providers::account::account_at_position;
  use crate::test_utils::lines;
  use std::str::FromStr;
  use tower_lsp_server::ls_types::{
    Position, TextDocumentIdentifier, TextDocumentPositionParams,
  };

  fn build_doc(uri: &Url, content: &str) -> Arc<Document> {
    Arc::new(doc::build_document(content.to_string(), uri.as_str()).expect("build doc"))
  }

  #[test]
  fn hover_shows_account_notes() {
    let uri = Url::from_str("file:///hover.bean").unwrap();
    let content = lines(&[
      r#"2023-01-01 open Assets:Cash"#,
      r#"2023-02-01 note Assets:Cash "First note""#,
    ]);
    let doc = build_doc(&uri, &content);

    let mut docs = HashMap::new();
    docs.insert(uri.clone(), doc);

    let cursor = Position::new(1, 20);

    let hit = account_at_position(docs.get(&uri).unwrap().as_ref(), cursor);
    assert!(hit.is_some(), "expected account under cursor");

    let notes = notes_for_account(&docs, &uri, "Assets:Cash");
    assert!(!notes.is_empty(), "expected notes for account");

    let params = HoverParams {
      text_document_position_params: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position: cursor,
      },
      work_done_progress_params: Default::default(),
    };

    let hover = hover(&docs, &uri, &params).expect("hover present");
    let contents = match hover.contents {
      HoverContents::Markup(markup) => markup.value,
      _ => panic!("expected markup hover"),
    };

    assert!(contents.contains("First note"));
    assert!(contents.contains("Notes for Assets:Cash"));
  }
}

use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;

use beancount_core as core;
use beancount_parser::ast;
use tower_lsp_server::ls_types::{
  Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Position, Uri as Url,
};

use crate::providers::account::account_at_position;
use crate::server::{Document, documents_bfs, find_document};
use crate::text::lsp_position_to_byte;

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

struct PostingHoverTarget {
  transaction_index: usize,
  posting_index: usize,
}

fn posting_at_position(
  doc: &Document,
  position: Position,
) -> Option<PostingHoverTarget> {
  let byte_idx = lsp_position_to_byte(&doc.rope, position)?;
  let directives = doc.ast();

  let directive_index_at = |offset: usize| {
    directives
      .binary_search_by(|directive| {
        let span = Document::directive_span(directive);
        if offset < span.start {
          Ordering::Greater
        } else if offset >= span.end {
          Ordering::Less
        } else {
          Ordering::Equal
        }
      })
      .ok()
  };

  let directive_idx = directive_index_at(byte_idx)
    .or_else(|| byte_idx.checked_sub(1).and_then(directive_index_at))?;

  let directive = directives.get(directive_idx)?;
  let ast::Directive::Transaction(tx) = directive else {
    return None;
  };

  let span_contains = |span: ast::Span| span.start <= byte_idx && byte_idx <= span.end;

  let posting_index = tx
    .postings
    .iter()
    .position(|posting| span_contains(posting.account.span))?;
  let transaction_index = directives[..=directive_idx]
    .iter()
    .filter(|dir| matches!(dir, ast::Directive::Transaction(_)))
    .count()
    .saturating_sub(1);

  Some(PostingHoverTarget {
    transaction_index,
    posting_index,
  })
}

fn format_inferred_amount(amount: &core::inference::InferredAmount) -> String {
  if amount.currency.is_empty() {
    amount.raw.clone()
  } else {
    format!("{} {}", amount.raw, amount.currency)
  }
}

fn posting_amount_from_inferred(
  doc: &Document,
  transaction_index: usize,
  posting_index: usize,
) -> Option<String> {
  let inferred_tx = doc
    .inferred
    .iter()
    .filter_map(|dir| match dir {
      core::InferredDirective::Transaction(tx) => Some(tx),
      _ => None,
    })
    .nth(transaction_index)?;

  let posting = inferred_tx.postings.get(posting_index)?;
  Some(format_inferred_amount(&posting.amount))
}

fn posting_amount_hover(doc: &Document, target: &PostingHoverTarget) -> Option<String> {
  posting_amount_from_inferred(doc, target.transaction_index, target.posting_index)
}

pub fn hover(
  documents: &HashMap<Url, Arc<Document>>,
  root_uri: &Url,
  params: &HoverParams,
) -> Option<Hover> {
  let uri = &params.text_document_position_params.text_document.uri;
  let position = params.text_document_position_params.position;
  let doc = find_document(documents, uri)?;
  let (account, account_range) = account_at_position(doc.as_ref(), position)?;
  let posting_target = posting_at_position(doc.as_ref(), position);

  spdlog::debug!("hover requested for account: {}", account);

  let amount_hover = posting_target
    .as_ref()
    .and_then(|target| posting_amount_hover(doc.as_ref(), target));
  let notes = notes_for_account(documents, root_uri, &account);
  if notes.is_empty() && amount_hover.is_none() {
    return None;
  }

  let mut sections = Vec::new();
  if let Some(amount) = amount_hover {
    sections.push(format!("**Amount**\n\n{}", amount));
  }

  if !notes.is_empty() {
    let bullet_list = notes
      .into_iter()
      .map(|note| format!("- {}", note))
      .collect::<Vec<_>>()
      .join("\n");
    sections.push(format!("**Notes for {}**\n\n{}", account, bullet_list));
  }

  let hover_value = sections.join("\n\n");

  let contents = HoverContents::Markup(MarkupContent {
    kind: MarkupKind::Markdown,
    value: hover_value,
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

  #[test]
  fn hover_shows_inferred_amount_for_missing_posting() {
    let uri = Url::from_str("file:///hover-amount.bean").unwrap();
    let content = lines(&[
      r#"2023-01-01 * "Payee" "Memo""#,
      r#"  Assets:Cash                          -1 USD"#,
      r#"  Expenses:Food"#,
    ]);
    let doc = build_doc(&uri, &content);

    let mut docs = HashMap::new();
    docs.insert(uri.clone(), doc);

    let params = HoverParams {
      text_document_position_params: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position: Position::new(2, 4),
      },
      work_done_progress_params: Default::default(),
    };

    let hover = hover(&docs, &uri, &params).expect("hover present");
    let contents = match hover.contents {
      HoverContents::Markup(markup) => markup.value,
      _ => panic!("expected markup hover"),
    };

    assert!(contents.contains("Amount"));
    assert!(contents.contains("USD"));
  }

  #[test]
  fn hover_shows_amount_for_expression_posting() {
    let uri = Url::from_str("file:///hover-amount-expr.bean").unwrap();
    let content = lines(&[
      r#"2023-01-01 * "Payee" "Memo""#,
      r#"  Assets:Cash                          -3 USD"#,
      r#"  Expenses:Food                         1 + 2 USD"#,
    ]);
    let doc = build_doc(&uri, &content);

    let mut docs = HashMap::new();
    docs.insert(uri.clone(), doc);

    let params = HoverParams {
      text_document_position_params: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position: Position::new(2, 4),
      },
      work_done_progress_params: Default::default(),
    };

    let hover = hover(&docs, &uri, &params).expect("hover present");
    let contents = match hover.contents {
      HoverContents::Markup(markup) => markup.value,
      _ => panic!("expected markup hover"),
    };

    assert!(contents.contains("Amount"));
    assert!(contents.contains("USD"));
  }
}

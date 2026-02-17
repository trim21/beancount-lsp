use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use beancount_parser::ast;
use tower_lsp_server::ls_types::{
  CompletionItem, CompletionItemKind, CompletionList, CompletionParams,
  CompletionTextEdit, Position, TextEdit, Uri as Url,
};

use crate::providers::completion_context::{
  CompletionMode, determine_completion_context as determine_completion_context_inner,
};
use crate::server::{Document, documents_bfs, find_document};
use crate::text::lsp_position_to_byte;

const DATE_KEYWORDS: &[&str] =
  &["custom", "balance", "open", "close", "note", "price", "pad"];
const ROOT_KEYWORDS: &[&str] = &[
  "include", "option", "pushtag", "poptag", "pushmeta", "popmeta",
];

fn determine_completion_context<'a>(
  doc: &'a Document,
  position: Position,
) -> Option<CompletionMode<'a>> {
  determine_completion_context_inner(doc, position, DATE_KEYWORDS, ROOT_KEYWORDS)
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

fn starts_with_ignore_ascii_case(candidate: &str, prefix: &str) -> bool {
  if prefix.is_empty() {
    return true;
  }

  let mut candidate_chars = candidate.chars();
  for prefix_char in prefix.chars() {
    let Some(candidate_char) = candidate_chars.next() else {
      return false;
    };

    if !candidate_char.eq_ignore_ascii_case(&prefix_char) {
      return false;
    }
  }

  true
}

fn current_marker_label_span(
  doc: &Document,
  range: tower_lsp_server::ls_types::Range,
) -> Option<(usize, usize)> {
  let start = lsp_position_to_byte(&doc.rope, range.start)?;
  let end = lsp_position_to_byte(&doc.rope, range.end)?;
  (start <= end).then_some((start, end))
}

fn marker_label_span(
  content: &str,
  span: ast::Span,
  marker: char,
) -> Option<(usize, usize)> {
  let token = content.get(span.start..span.end)?;
  let marker_len = marker.len_utf8();
  let start = if token.starts_with(marker) {
    span.start.checked_add(marker_len)?
  } else {
    span.start
  };

  (start <= span.end).then_some((start, span.end))
}

fn should_skip_current_marker(
  is_current_doc: bool,
  current_span: Option<(usize, usize)>,
  content: &str,
  token_span: ast::Span,
  marker: char,
) -> bool {
  if !is_current_doc {
    return false;
  }

  let Some(current_span) = current_span else {
    return false;
  };

  marker_label_span(content, token_span, marker) == Some(current_span)
}

fn completion_item_with_edit(
  label: String,
  kind: CompletionItemKind,
  range: tower_lsp_server::ls_types::Range,
  append_whitespace: bool,
) -> CompletionItem {
  let mut new_text = label.clone();
  if append_whitespace {
    new_text.push(' ');
  }

  CompletionItem {
    label,
    kind: Some(kind),
    text_edit: Some(CompletionTextEdit::Edit(TextEdit { range, new_text })),
    ..CompletionItem::default()
  }
}

fn should_append_keyword_whitespace(
  doc: &Document,
  position: Position,
  replace_range: tower_lsp_server::ls_types::Range,
) -> bool {
  if position != replace_range.end {
    return false;
  }

  let Some(cursor_byte) = lsp_position_to_byte(&doc.rope, position) else {
    return false;
  };

  let Some(after_cursor) = doc.content().get(cursor_byte..) else {
    return false;
  };

  match after_cursor.chars().next() {
    None => true,
    Some('\n' | '\r') => true,
    _ => false,
  }
}

fn insert_label<'a>(labels: &mut HashSet<&'a str>, label: &'a str) {
  if !label.is_empty() {
    labels.insert(label);
  }
}

fn insert_link_label<'a>(
  labels: &mut HashSet<&'a str>,
  content: &'a str,
  require_caret_prefix: bool,
) {
  if require_caret_prefix && !content.starts_with('^') {
    return;
  }

  let label = content.trim_start_matches('^');
  insert_label(labels, label);
}

pub fn completion(
  documents: &HashMap<Url, Arc<Document>>,
  root_uri: &Url,
  params: &CompletionParams,
) -> Option<CompletionList> {
  let uri = &params.text_document_position.text_document.uri;
  let position = params.text_document_position.position;
  let doc = find_document(documents, uri)?;

  let ctx = match determine_completion_context(doc.as_ref(), position) {
    Some(ctx) => ctx,
    None => {
      return None;
    }
  };

  spdlog::debug!("completion for {:?}", ctx);

  let mut items: Vec<CompletionItem> = match ctx {
    CompletionMode::Account {
      prefix,
      range: replace_range,
    } => {
      let docs = documents_bfs(documents, root_uri);

      let mut accounts = HashSet::new();
      for (_, doc) in &docs {
        accounts.extend(doc.accounts.iter().map(String::as_str));
      }

      accounts
        .into_iter()
        .filter(|label| fuzzy_match(label, prefix))
        .map(|label| {
          completion_item_with_edit(
            label.to_string(),
            CompletionItemKind::VALUE,
            replace_range,
            false,
          )
        })
        .collect()
    }
    CompletionMode::Currency {
      prefix,
      range: replace_range,
    } => {
      let docs = documents_bfs(documents, root_uri);

      let mut currencies = HashSet::new();
      for (_, doc) in &docs {
        currencies.extend(doc.currencies.iter().map(String::as_str));
      }

      currencies
        .into_iter()
        .filter(|label| starts_with_ignore_ascii_case(label, prefix))
        .map(|label| {
          completion_item_with_edit(
            label.to_string(),
            CompletionItemKind::VALUE,
            replace_range,
            false,
          )
        })
        .collect()
    }
    CompletionMode::Keyword {
      prefix,
      range: replace_range,
      variants,
    } => {
      let append_whitespace =
        should_append_keyword_whitespace(doc.as_ref(), position, replace_range);

      variants
        .iter()
        .copied()
        .filter(|label| fuzzy_match(label, prefix))
        .map(|label| {
          completion_item_with_edit(
            label.to_string(),
            CompletionItemKind::KEYWORD,
            replace_range,
            append_whitespace,
          )
        })
        .collect()
    }
    CompletionMode::Tag {
      prefix,
      range: replace_range,
    } => {
      let docs = documents_bfs(documents, root_uri);
      let current_span = current_marker_label_span(doc.as_ref(), replace_range);

      let mut tags = HashSet::new();
      for (_, candidate_doc) in &docs {
        let is_current_doc = Arc::ptr_eq(candidate_doc, &doc);
        for dir in candidate_doc.ast() {
          if let ast::Directive::Transaction(tx) = dir {
            for tag in &tx.tags {
              if should_skip_current_marker(
                is_current_doc,
                current_span,
                candidate_doc.content(),
                tag.span,
                '#',
              ) {
                continue;
              }

              let label = tag.content.trim_start_matches('#');
              insert_label(&mut tags, label);
            }
          }
        }
      }

      tags
        .into_iter()
        .filter(|label| fuzzy_match(label, prefix))
        .map(|label| {
          completion_item_with_edit(
            label.to_string(),
            CompletionItemKind::KEYWORD,
            replace_range,
            false,
          )
        })
        .collect()
    }
    CompletionMode::Link {
      prefix,
      range: replace_range,
    } => {
      let docs = documents_bfs(documents, root_uri);
      let current_span = current_marker_label_span(doc.as_ref(), replace_range);

      let mut links = HashSet::new();
      for (_, candidate_doc) in &docs {
        let is_current_doc = Arc::ptr_eq(candidate_doc, &doc);
        for dir in candidate_doc.ast() {
          match dir {
            ast::Directive::Transaction(tx) => {
              for link in &tx.links {
                if should_skip_current_marker(
                  is_current_doc,
                  current_span,
                  candidate_doc.content(),
                  link.span,
                  '^',
                ) {
                  continue;
                }

                insert_link_label(&mut links, link.content, false);
              }
            }
            ast::Directive::Document(document) => {
              for link in &document.links {
                if should_skip_current_marker(
                  is_current_doc,
                  current_span,
                  candidate_doc.content(),
                  link.span,
                  '^',
                ) {
                  continue;
                }

                insert_link_label(&mut links, link.content, false);
              }

              if let Some(tags_links) = &document.tags_links {
                for item in tags_links {
                  if should_skip_current_marker(
                    is_current_doc,
                    current_span,
                    candidate_doc.content(),
                    item.span,
                    '^',
                  ) {
                    continue;
                  }

                  insert_link_label(&mut links, item.content, true);
                }
              }
            }
            _ => {}
          }
        }
      }

      links
        .into_iter()
        .filter(|label| fuzzy_match(label, prefix))
        .map(|label| {
          completion_item_with_edit(
            label.to_string(),
            CompletionItemKind::KEYWORD,
            replace_range,
            false,
          )
        })
        .collect()
    }
  };

  items.sort_by(|a, b| a.label.cmp(&b.label));

  if items.is_empty() {
    None
  } else {
    Some(CompletionList {
      is_incomplete: true,
      items,
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::doc;
  use std::collections::HashSet;
  use std::str::FromStr;
  use tower_lsp_server::ls_types::{
    CompletionContext, CompletionTriggerKind, Position, TextDocumentIdentifier,
    TextDocumentPositionParams,
  };

  /// Build a document and cursor position from lines containing a single `|` marker.
  fn doc_with_cursor(lines: &[&str]) -> (Arc<Document>, Position) {
    let mut content_lines = Vec::new();
    let mut cursor: Option<Position> = None;

    for (row, line) in lines.iter().enumerate() {
      if let Some(col) = line.find('|') {
        let mut owned = line.to_string();
        owned.remove(col);
        content_lines.push(owned);

        let col_clamped = col.min(content_lines.last().unwrap().chars().count());
        let col_u32 = u32::try_from(col_clamped).expect("column fits in u32");
        let row_u32 = u32::try_from(row).expect("row fits in u32");
        cursor = Some(Position::new(row_u32, col_u32));
      } else {
        content_lines.push((*line).to_owned());
      }
    }

    let cursor = cursor.expect("cursor marker '|' not found");
    let content = content_lines.join("\n");

    let uri = Url::from_str("file:///completion-helper.bean").unwrap();
    let doc = doc::build_document(content, uri.as_str()).expect("build doc");

    (Arc::new(doc), cursor)
  }

  fn completion_items(response: Option<CompletionList>) -> Vec<CompletionItem> {
    response.expect("got completion").items
  }

  fn build_doc(uri: &Url, content: &str) -> Arc<Document> {
    Arc::new(doc::build_document(content.to_string(), uri.as_str()).expect("build doc"))
  }

  #[test]
  fn completes_accounts_and_replaces_prefix() {
    let uri = Url::from_str("file:///main.bean").unwrap();
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

    let items = completion_items(completion(&documents, &uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Cash")
      .expect("expected Assets:Cash item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 16));
    assert_eq!(edit.range.end, Position::new(0, 27));
    assert_eq!(edit.new_text, "Assets:Cash");
  }

  #[test]
  fn offers_completion_at_document_root() {
    let lines = [r#"a|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///completion-helper.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      !items.is_empty(),
      "expected root keyword completions at document root"
    );
  }

  #[test]
  fn allows_completion_on_balance_directive() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Bank:C1234\n";
    let open_doc = build_doc(&open_uri, open_content);

    let bal_uri = Url::from_str("file:///bal.bean").unwrap();
    let bal_with_cursor = "2020-10-01 balance Assets:Bank:C1234|           0.00 CNY\n";
    let cursor_col = bal_with_cursor.find('|').expect("cursor marker present");
    let bal_content = {
      let mut s = bal_with_cursor.to_string();
      s.remove(cursor_col);
      s
    };
    let bal_doc = build_doc(&bal_uri, &bal_content);

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(bal_uri.clone(), bal_doc);

    // Cursor at the marker position (after removal)
    let position = Position::new(0, u32::try_from(cursor_col).unwrap());
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: bal_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &open_uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Bank:C1234")
      .expect("expected account item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 19));
    assert_eq!(edit.range.end, Position::new(0, 36));
    assert_eq!(edit.new_text, "Assets:Bank:C1234");
  }

  #[test]
  fn completes_when_cursor_at_account_end() {
    let uri = Url::from_str("file:///main.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&uri, content);

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    // Cursor immediately after the account token
    let position = Position::new(0, 27);
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

    let items = completion_items(completion(&documents, &uri, &params));

    let item = items
      .iter()
      .find(|i| i.label == "Assets:Cash")
      .expect("expected Assets:Cash item");

    let edit = match item.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 16));
    assert_eq!(edit.range.end, Position::new(0, 27));
    assert_eq!(edit.new_text, "Assets:Cash");
  }

  #[test]
  fn suppresses_completion_outside_account_nodes() {
    let uri = Url::from_str("file:///main.bean").unwrap();
    let content = "2023-01-01 open Assets:Cash\n2023-01-02 * \"Payee\" \"Narration\"\n";
    let doc = build_doc(&uri, content);

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let position = Position::new(1, 14); // inside the payee string, not an account node
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

    let response = completion(&documents, &uri, &params);
    assert!(
      response.is_none(),
      "expected no completion outside account nodes"
    );
  }

  #[test]
  fn suppresses_account_completion_in_amount_field() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash\n";
    let open_doc = build_doc(&open_uri, open_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 1|00 CNY"#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &open_uri, &params);
    assert!(
      response.is_none(),
      "expected no account completion in amount field"
    );
  }

  #[test]
  fn completes_currency_in_amount_field() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let open_doc = build_doc(&open_uri, open_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 C|"#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &open_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected currency completion in amount field"
    );
  }

  #[test]
  fn completes_currency_on_commodity_directive() {
    let source_uri = Url::from_str("file:///source-currency.bean").unwrap();
    let source_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [r#"2022-01-01 commodity C|"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///commodity-cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &source_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected CNY completion on commodity directive"
    );
  }

  #[test]
  fn completes_currency_on_price_directive() {
    let source_uri = Url::from_str("file:///source-price.bean").unwrap();
    let source_content =
      "2020-01-01 * \"...\" \"...\"\n  Assets:Cash -10 USD\n  Expenses:Food\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [r#"2022-01-01 price U|"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///price-cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &source_uri, &params));
    assert!(
      items.iter().any(|item| item.label == "USD"),
      "expected USD completion on price directive"
    );
  }

  #[test]
  fn dedupes_and_sorts_currency_suggestions() {
    let a_uri = Url::from_str("file:///a.bean").unwrap();
    let a_content = "2020-01-01 open Assets:Cash USD CNY\n";
    let a_doc = build_doc(&a_uri, a_content);

    let b_uri = Url::from_str("file:///b.bean").unwrap();
    let b_content =
      "2020-01-01 * \"...\" \"...\"\n  Assets:Cash -1 EUR\n  Expenses:Food\n";
    let b_doc = build_doc(&b_uri, b_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                           1 |"#,
    ];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(a_uri.clone(), a_doc);
    documents.insert(b_uri.clone(), b_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &a_uri, &params));
    let labels: Vec<String> = items.into_iter().map(|item| item.label).collect();

    assert!(labels.windows(2).all(|pair| pair[0] <= pair[1]));
    assert_eq!(
      labels
        .iter()
        .filter(|label| label.as_str() == "USD")
        .count(),
      1,
      "expected duplicate USD suggestions to be removed"
    );
  }

  #[test]
  fn completes_currency_with_utf16_position() {
    let lines = [
      r#"2021-12-31 open Assets:Cash CNY"#,
      r#"2022-01-01 * "🍣" "...""#,
      r#"  Assets:Cash -1 CN|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///currency-utf16.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|item| item.label == "CNY"),
      "expected currency completion with utf16-aware position"
    );
  }

  #[test]
  fn currency_completion_uses_prefix_not_subsequence() {
    let source_uri = Url::from_str("file:///source-prefix.bean").unwrap();
    let source_content = "2020-01-01 open Assets:Cash CNY USD\n";
    let source_doc = build_doc(&source_uri, source_content);

    let lines = [
      r#"2022-01-02 * "..." "...""#,
      r#"  Expenses:Food"#,
      r#"  Assets:Cash                                 100 ny|"#,
    ];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor-prefix.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(source_uri.clone(), source_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &source_uri, &params);
    assert!(
      response.is_none(),
      "expected no currency completion for non-prefix input"
    );
  }

  #[test]
  fn suppresses_account_completion_after_posting_account() {
    let open_uri = Url::from_str("file:///open.bean").unwrap();
    let open_content = "2020-01-01 open Assets:Cash\n";
    let open_doc = build_doc(&open_uri, open_content);

    assert!(matches!(open_doc.ast()[0], ast::Directive::Open(_)));

    let lines = [r#"2022-01-02 * "..." "...""#, r#"  Expenses:Food a|"#];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn2.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(open_uri.clone(), open_doc);
    documents.insert(tx_uri.clone(), tx_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &open_uri, &params);
    assert!(
      response.is_none(),
      "expected no account completion after posting account"
    );
  }

  #[test]
  fn completes_account_after_unicode_payee() {
    let lines = [
      r#"2026-01-01 open Expenses:Rent USD   "#,
      r#"2026-01-05 *                        "#,
      r#"  Expenses:R|                       "#,
      r#"  Assets:Cash           -1.00 CNY   "#,
    ];
    let (tx_doc, position) = doc_with_cursor(&lines);
    let tx_uri = Url::from_str("file:///txn-unicode-payee.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(tx_uri.clone(), tx_doc);

    let ctx =
      determine_completion_context(documents.get(&tx_uri).unwrap().as_ref(), position);
    assert!(
      matches!(ctx, Some(CompletionMode::Account { .. })),
      "expected account completion context",
    );

    let mut accounts = HashSet::new();
    let doc = documents.get(&tx_uri).unwrap();
    accounts.extend(doc.accounts.iter().cloned());
    assert!(
      accounts.contains("Expenses:Rent"),
      "expected account set to include Expenses:Rent",
    );

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: tx_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &tx_uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Rent"),
      "expected account completion after unicode payee",
    );
  }

  #[test]
  fn completes_account_on_empty_posting_line_in_raw_transaction() {
    let lines = [
      r#"2026-01-16 * "" """#,
      r#"  Expenses:Food                          -1 CNY"#,
      r#"  |"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///raw-tx-empty-posting.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let doc = documents.get(&uri).unwrap();
    assert!(
      doc
        .ast()
        .iter()
        .any(|d| matches!(d, ast::Directive::Transaction(_))),
      "expected the transaction header to parse as Transaction",
    );

    let ctx = determine_completion_context(doc.as_ref(), position);
    assert!(
      matches!(ctx, Some(CompletionMode::Account { .. })),
      "expected account completion context on empty posting line",
    );

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Food"),
      "expected account completion to include Expenses:Food",
    );
  }

  #[test]
  fn completes_in_transaction_body() {
    let lines = [
      r#"2026-01-16 open Expenses:Food CNY"#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Liabilities:C1                        -1.00 CNY"#,
      r#"  Expenses:Food"#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Liabilities:C1                          -1.00 CNY"#,
      r#"  e|                       "#,
      r#""#,
      r#"2026-01-16 *"#,
      r#"  Expenses:Food"#,
      r#"  Liabilities:C1"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tx-body.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let doc = documents.get(&uri).unwrap();
    let ctx = determine_completion_context(doc.as_ref(), position);

    match ctx {
      Some(CompletionMode::Account { prefix, .. }) => {
        assert_eq!(prefix, "e");
      }
      _ => panic!("expected account completion in transaction body",),
    };

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "Expenses:Food"),
      "expected completion to suggest Expenses:Food",
    );
  }

  #[test]
  fn completes_tags_after_hash_on_transaction() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn completes_tags_with_existing_prefix() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #fo|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags-prefix.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion with prefix");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col + 2));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn keeps_same_tag_from_other_directives_when_current_tag_is_fully_typed() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #food|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags-full.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "food"),
      "expected same tag from other directives to remain in suggestions"
    );
  }

  #[test]
  fn replaces_whole_tag_when_cursor_is_in_middle() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." #fo|od-old"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///tags-middle.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "food")
      .expect("expected tag completion with middle cursor");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[3].find('#').expect("hash present");
    let expected_start = u32::try_from(hash_col + 1).unwrap();
    let expected_end = expected_start + u32::try_from("food-old".len()).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_start));
    assert_eq!(edit.range.end, Position::new(3, expected_end));
    assert_eq!(edit.new_text, "food");
  }

  #[test]
  fn completes_tags_from_parsed_directives() {
    let tag_uri = Url::from_str("file:///source-tags.bean").unwrap();
    let tag_content = "2024-01-01 * \"p\" \"n\" #groceries #fun\n  Assets:Cash -10 USD\n  Expenses:Food\n";
    let tag_doc = build_doc(&tag_uri, tag_content);

    let lines = [r#"2025-01-01 * "p" "n" #|"#, r#"  Expenses:Food"#];
    let (cursor_doc, position) = doc_with_cursor(&lines);
    let cursor_uri = Url::from_str("file:///cursor-tags.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(tag_uri.clone(), tag_doc);
    documents.insert(cursor_uri.clone(), cursor_doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: cursor_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let items = completion_items(completion(&documents, &tag_uri, &params));

    let tag = items
      .iter()
      .find(|i| i.label == "groceries")
      .expect("expected tag completion from directives");

    let edit = match tag.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let hash_col = lines[0].find('#').expect("hash present");
    let expected_col = u32::try_from(hash_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(0, expected_col));
    assert_eq!(edit.range.end, Position::new(0, expected_col));
    assert_eq!(edit.new_text, "groceries");
  }

  #[test]
  fn completes_links_after_caret_on_transaction() {
    let lines = [
      r#"2022-01-01 * "..." "..." ^invoice-2022"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." ^|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///links.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let link = items
      .iter()
      .find(|i| i.label == "invoice-2022")
      .expect("expected link completion");

    let edit = match link.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let caret_col = lines[3].find('^').expect("caret present");
    let expected_col = u32::try_from(caret_col + 1).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_col));
    assert_eq!(edit.range.end, Position::new(3, expected_col));
    assert_eq!(edit.new_text, "invoice-2022");
  }

  #[test]
  fn replaces_whole_link_when_cursor_is_in_middle() {
    let lines = [
      r#"2022-01-01 * "..." "..." ^invoice-2022"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." ^in|voice-old"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///links-middle.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let link = items
      .iter()
      .find(|i| i.label == "invoice-2022")
      .expect("expected link completion with middle cursor");

    let edit = match link.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    let caret_col = lines[3].find('^').expect("caret present");
    let expected_start = u32::try_from(caret_col + 1).unwrap();
    let expected_end = expected_start + u32::try_from("invoice-old".len()).unwrap();
    assert_eq!(edit.range.start, Position::new(3, expected_start));
    assert_eq!(edit.range.end, Position::new(3, expected_end));
    assert_eq!(edit.new_text, "invoice-2022");
  }

  #[test]
  fn keeps_same_link_from_other_directives_when_current_link_is_fully_typed() {
    let lines = [
      r#"2022-01-01 * "..." "..." ^invoice-2022"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 * "..." "..." ^invoice-2022|"#,
      r#"  Expenses:Food"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///links-full.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "invoice-2022"),
      "expected same link from other directives to remain in suggestions"
    );
  }

  #[test]
  fn does_not_complete_tags_or_links_on_unsupported_directive() {
    let lines = [r#"2022-01-02 open Assets:Cash #|"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///unsupported-marker.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: Some("#".to_string()),
      }),
    };

    let response = completion(&documents, &uri, &params);
    assert!(
      response.is_none(),
      "expected no tag/link completion on unsupported directive"
    );
  }

  #[test]
  fn completes_tag_on_incomplete_date_directive_line() {
    let lines = [
      r#"2022-01-01 * "..." "..." #food"#,
      r#"  Assets:Cash -10 USD"#,
      r#"  Expenses:Food"#,
      r#"2022-01-02 unknown-directive #|"#,
    ];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///raw-tx-tag.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: Some("#".to_string()),
      }),
    };

    let items = completion_items(completion(&documents, &uri, &params));
    assert!(
      items.iter().any(|i| i.label == "food"),
      "expected tag completion while current line is still being typed"
    );
  }

  #[test]
  fn completes_keywords_after_date() {
    let lines = [r#"2026-02-02 |"#];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///kw.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let labels: HashSet<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in ["custom", "balance", "open", "close", "note"] {
      assert!(labels.contains(expected), "missing keyword {expected}");
    }

    let custom = items
      .iter()
      .find(|i| i.label == "custom")
      .expect("custom item present");

    let edit = match custom.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 11));
    assert_eq!(edit.range.end, Position::new(0, 11));
    assert_eq!(edit.new_text, "custom ");
  }

  #[test]
  fn completes_root_keywords() {
    let lines = ["|"];
    let (doc, position) = doc_with_cursor(&lines);
    let uri = Url::from_str("file:///root.kw.bean").unwrap();

    let mut documents = HashMap::new();
    documents.insert(uri.clone(), doc);

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

    let items = completion_items(completion(&documents, &uri, &params));

    let labels: HashSet<&str> = items.iter().map(|i| i.label.as_str()).collect();
    for expected in ["include", "option"] {
      assert!(labels.contains(expected), "missing keyword {expected}");
    }

    let include = items
      .iter()
      .find(|i| i.label == "include")
      .expect("include item present");

    let edit = match include.text_edit.as_ref().expect("text edit") {
      CompletionTextEdit::Edit(e) => e,
      _ => panic!("unexpected insert replace edit"),
    };

    assert_eq!(edit.range.start, Position::new(0, 0));
    assert_eq!(edit.range.end, Position::new(0, 0));
    assert_eq!(edit.new_text, "include ");
  }

  #[test]
  fn does_not_append_whitespace_when_position_is_not_range_end() {
    let lines = [r#"2026-02-02 custom|"#];
    let (doc, position) = doc_with_cursor(&lines);

    let range = tower_lsp_server::ls_types::Range {
      start: Position::new(position.line, position.character.saturating_sub(1)),
      end: Position::new(position.line, position.character.saturating_add(1)),
    };

    assert!(
      !should_append_keyword_whitespace(doc.as_ref(), position, range),
      "expected no trailing whitespace when cursor is not at replacement end"
    );
  }

  #[test]
  fn does_not_append_whitespace_when_non_newline_follows_cursor() {
    let lines = [r#"2026-02-02 |x"#];
    let (doc, position) = doc_with_cursor(&lines);

    let range = tower_lsp_server::ls_types::Range {
      start: position,
      end: position,
    };

    assert!(
      !should_append_keyword_whitespace(doc.as_ref(), position, range),
      "expected no trailing whitespace when non-newline follows cursor"
    );
  }

  #[cfg(windows)]
  #[test]
  fn matches_documents_case_insensitively_on_windows() {
    let stored_uri =
      Url::from_str("file:///C:/Users/Trim21/proj/count/logs/2026/01.bean").unwrap();
    let requested_uri =
      Url::from_str("file:///c%3A/Users/Trim21/proj/count/logs/2026/01.bean").unwrap();

    let content = "2023-01-01 open Assets:Cash\n";
    let doc = build_doc(&stored_uri, content);

    let mut documents = HashMap::new();
    documents.insert(stored_uri.clone(), doc);

    let position = Position::new(0, 25);
    let params = CompletionParams {
      text_document_position: TextDocumentPositionParams {
        text_document: TextDocumentIdentifier {
          uri: requested_uri.clone(),
        },
        position,
      },
      work_done_progress_params: Default::default(),
      partial_result_params: Default::default(),
      context: Some(CompletionContext {
        trigger_kind: CompletionTriggerKind::INVOKED,
        trigger_character: None,
      }),
    };

    let response = completion(&documents, &stored_uri, &params);
    assert!(
      response.is_some(),
      "expected completion despite URI casing differences"
    );
  }
}

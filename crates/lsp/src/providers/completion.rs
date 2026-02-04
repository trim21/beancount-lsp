use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use beancount_parser::{ast, core};
use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionParams, CompletionTextEdit,
    Position, Range, TextEdit, Uri as Url,
};

use crate::server::{Document, documents_bfs, find_document};
use crate::text::{byte_to_lsp_position, lsp_position_to_byte};

const DATE_KEYWORDS: &[&str] = &["custom", "balance", "open", "close", "note", "price"];
const ROOT_KEYWORDS: &[&str] = &[
    "include", "option", "pushtag", "poptag", "pushmeta", "popmeta",
];

#[derive(Debug, Clone, PartialEq, Eq)]
enum CompletionMode {
    Account {
        prefix: String,
        range: Range,
    },
    Keyword {
        prefix: String,
        range: Range,
        variants: &'static [&'static str],
    },
    Tag {
        prefix: String,
        range: Range,
    },
}

fn token_prefix_at_position(
    doc: &Document,
    position: Position,
    require_nonzero_start: bool,
    allow_empty_token: bool,
) -> Option<(String, Range)> {
    let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;

    let line = doc.rope.byte_to_line(cursor_byte);
    let line_start = doc.rope.line_to_byte(line);
    let line_end = if line + 1 < doc.rope.len_lines() {
        doc.rope.line_to_byte(line + 1)
    } else {
        doc.rope.len_bytes()
    };

    if cursor_byte < line_start || cursor_byte > line_end {
        return None;
    }

    let rel = cursor_byte - line_start;
    let line_slice = doc.rope.byte_slice(line_start..line_end).to_string();
    if rel > line_slice.len() {
        return None;
    }

    let mut start = 0;
    for (idx, ch) in line_slice.char_indices() {
        if idx >= rel {
            break;
        }
        if ch.is_whitespace() {
            start = idx + ch.len_utf8();
        }
    }

    let mut end = line_slice.len();
    for (idx, ch) in line_slice.char_indices() {
        if idx < rel {
            continue;
        }
        if ch.is_whitespace() {
            end = idx;
            break;
        }
    }

    if start > end || rel > end {
        return None;
    }

    if require_nonzero_start && start == 0 {
        return None;
    }

    let token = line_slice.get(start..end)?;
    let prefix = line_slice.get(start..rel)?.to_owned();
    if token.is_empty() && !allow_empty_token {
        return None;
    }

    let start_byte = line_start + start;
    let end_byte = line_start + end;
    let start_pos = byte_to_lsp_position(&doc.rope, start_byte)?;
    let end_pos = byte_to_lsp_position(&doc.rope, end_byte)?;

    Some((
        prefix,
        Range {
            start: start_pos,
            end: end_pos,
        },
    ))
}

fn tag_prefix_at_position(doc: &Document, position: Position) -> Option<(String, Range)> {
    let (_token_prefix, token_range) = token_prefix_at_position(doc, position, true, true)?;

    let token_start = lsp_position_to_byte(&doc.rope, token_range.start)?;
    let token_end = lsp_position_to_byte(&doc.rope, token_range.end)?;
    if token_start >= token_end {
        return None;
    }

    let token_text = doc.rope.byte_slice(token_start..token_end).to_string();
    if !token_text.starts_with('#') {
        return None;
    }

    let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;
    let after_hash = token_start + 1;
    if cursor_byte < after_hash {
        return None;
    }

    let prefix = doc.rope.byte_slice(after_hash..cursor_byte).to_string();

    let start_pos = byte_to_lsp_position(&doc.rope, after_hash)?;
    Some((
        prefix,
        Range {
            start: start_pos,
            end: token_range.end,
        },
    ))
}

fn is_within_account_context(doc: &Document, byte_idx: usize) -> bool {
    let directive = match doc.directive_at_offset(byte_idx) {
        Some(d) => d,
        None => return false,
    };

    match directive {
        ast::Directive::Open(_)
        | ast::Directive::Balance(_)
        | ast::Directive::Pad(_)
        | ast::Directive::Close(_) => true,
        ast::Directive::Transaction(tx) => tx
            .postings
            .iter()
            .any(|p| p.span.start <= byte_idx && byte_idx < p.span.end),
        _ => false,
    }
}

fn has_date_prefix(line_slice: &str, indent: usize) -> bool {
    if line_slice.len() < indent + 10 {
        return false;
    }

    let bytes = line_slice.as_bytes();
    let window = &bytes[indent..indent + 10];
    let is_digit = |b: u8| b.is_ascii_digit();

    if !(is_digit(window[0])
        && is_digit(window[1])
        && is_digit(window[2])
        && is_digit(window[3])
        && window[4] == b'-'
        && is_digit(window[5])
        && is_digit(window[6])
        && window[7] == b'-'
        && is_digit(window[8])
        && is_digit(window[9]))
    {
        return false;
    }

    if line_slice.len() == indent + 10 {
        return true;
    }

    line_slice
        .get(indent + 10..)
        .and_then(|rest| rest.chars().next())
        .map(|c| c.is_whitespace())
        .unwrap_or(false)
}

fn determine_completion_context(doc: &Document, position: Position) -> Option<CompletionMode> {
    let cursor_byte = lsp_position_to_byte(&doc.rope, position)?;

    let line = doc.rope.byte_to_line(cursor_byte);
    let line_start = doc.rope.line_to_byte(line);
    let line_end = if line + 1 < doc.rope.len_lines() {
        doc.rope.line_to_byte(line + 1)
    } else {
        doc.rope.len_bytes()
    };

    let rel = cursor_byte - line_start;
    let line_slice = doc.rope.byte_slice(line_start..line_end).to_string();
    let trimmed = line_slice.trim_start();
    let indent = line_slice.len().saturating_sub(trimmed.len());

    let in_account_context = is_within_account_context(doc, cursor_byte);

    if indent == 0
        && has_date_prefix(&line_slice, indent)
        && !in_account_context
        && let Some((prefix, range)) = tag_prefix_at_position(doc, position)
    {
        return Some(CompletionMode::Tag { prefix, range });
    }

    let mut allow_account = in_account_context;

    // Fallback heuristic: indented posting line or account-taking directives (open/balance).
    if !allow_account {
        allow_account = if indent > 0 {
            true
        } else {
            let mut parts = trimmed.split_whitespace();
            let first = parts.next();
            let second = parts.next();
            first.is_some() && matches!(second, Some("open") | Some("balance"))
        };
    }

    if allow_account
        && let Some((prefix, range)) = token_prefix_at_position(doc, position, true, false)
    {
        // Avoid treating amount/price fields as accounts; require token to start alphabetic.
        let token_start = lsp_position_to_byte(&doc.rope, range.start)?;
        let token_end = lsp_position_to_byte(&doc.rope, range.end)?;
        let token = doc.rope.byte_slice(token_start..token_end).to_string();
        if indent > 0 && range.start.character != u32::try_from(indent).ok()? {
            return None;
        }
        if token
            .chars()
            .next()
            .map(|c| c.is_ascii_alphabetic())
            .unwrap_or(false)
        {
            return Some(CompletionMode::Account { prefix, range });
        }
    }

    if indent == 0
        && has_date_prefix(&line_slice, indent)
        && rel >= indent + 10
        && !in_account_context
        && let Some((prefix, range)) = token_prefix_at_position(doc, position, false, true)
        && DATE_KEYWORDS
            .iter()
            .any(|label| fuzzy_match(label, &prefix))
    {
        return Some(CompletionMode::Keyword {
            prefix,
            range,
            variants: DATE_KEYWORDS,
        });
    }

    if indent == 0
        && !in_account_context
        && let Some((prefix, range)) = token_prefix_at_position(doc, position, false, true)
        && ROOT_KEYWORDS
            .iter()
            .any(|label| fuzzy_match(label, &prefix))
    {
        return Some(CompletionMode::Keyword {
            prefix,
            range,
            variants: ROOT_KEYWORDS,
        });
    }

    None
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

    tracing::debug!("completion for {:?}", ctx);

    let mut items: Vec<CompletionItem> = match ctx {
        CompletionMode::Account {
            prefix,
            range: replace_range,
        } => {
            let mut accounts = HashSet::new();
            for (_, doc) in documents_bfs(documents, root_uri) {
                for dir in &doc.directives {
                    if let core::CoreDirective::Open(o) = dir {
                        accounts.insert(o.account.clone());
                    }
                }
            }

            accounts
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
                .collect()
        }
        CompletionMode::Keyword {
            prefix,
            range: replace_range,
            variants,
        } => variants
            .iter()
            .copied()
            .filter(|label| fuzzy_match(label, &prefix))
            .map(|label| CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                    range: replace_range,
                    new_text: label.to_string(),
                })),
                ..CompletionItem::default()
            })
            .collect(),
        CompletionMode::Tag {
            prefix,
            range: replace_range,
        } => {
            let mut tags = HashSet::new();
            for (_, doc) in documents_bfs(documents, root_uri) {
                for dir in &doc.directives {
                    if let core::CoreDirective::Transaction(tx) = dir {
                        for tag in &tx.tags {
                            if !tag.is_empty() {
                                tags.insert(tag.clone());
                            }
                        }
                    }
                }
            }

            tags.into_iter()
                .filter(|label| fuzzy_match(label, &prefix))
                .map(|label| CompletionItem {
                    label: label.clone(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                        range: replace_range,
                        new_text: label.clone(),
                    })),
                    ..CompletionItem::default()
                })
                .collect()
        }
    };

    items.sort_by(|a, b| a.label.cmp(&b.label));

    if items.is_empty() {
        None
    } else {
        Some(CompletionList {
            is_incomplete: false,
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

    fn determine_context_helper(lines: &[&str]) -> Option<CompletionMode> {
        let (doc, pos) = doc_with_cursor(lines);
        determine_completion_context(doc.as_ref(), pos)
    }

    fn completion_items(response: Option<CompletionList>) -> Vec<CompletionItem> {
        response.expect("got completion").items
    }

    fn assert_account_range(lines: &[&str], start: Position, end: Position) {
        let ctx = determine_context_helper(lines).expect("expected account context");
        match ctx {
            CompletionMode::Account { range, .. } => {
                assert_eq!(range.start, start, "unexpected start position");
                assert_eq!(range.end, end, "unexpected end position");
            }
            other => panic!("unexpected completion mode: {other:?}"),
        }
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
    fn context_allows_balance_account() {
        let lines = [r#"2025-10-10 balance Assets:Bank:CCB:C6485|"#];

        assert_account_range(&lines, Position::new(0, 19), Position::new(0, 40));
    }

    #[test]
    fn context_allows_open_account() {
        let lines = [r#"2025-10-10 open Assets:Cash|"#];

        assert_account_range(&lines, Position::new(0, 16), Position::new(0, 27));
    }

    #[test]
    fn context_allows_posting_account_when_indented() {
        let lines = [
            r#"2025-10-10 * "Payee" "Narration""#,
            r#"  Assets:Cash:Wallet|"#,
        ];

        assert_account_range(&lines, Position::new(1, 2), Position::new(1, 20));
    }

    #[test]
    fn context_rejects_top_level_word() {
        let lines = [r#"word|"#];

        let ctx = determine_context_helper(&lines);
        assert!(
            ctx.is_none(),
            "expected no account context at document root"
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
    fn suppresses_account_completion_after_posting_account() {
        let open_uri = Url::from_str("file:///open.bean").unwrap();
        let open_content = "2020-01-01 open Assets:Cash\n";
        let open_doc = build_doc(&open_uri, open_content);

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
    fn completes_tags_from_parsed_directives() {
        let tag_uri = Url::from_str("file:///source-tags.bean").unwrap();
        let tag_content =
            "2024-01-01 * \"p\" \"n\" #groceries #fun\n  Assets:Cash -10 USD\n  Expenses:Food\n";
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

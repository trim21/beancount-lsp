use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use beancount_core as core;
use beancount_core::inference::InferenceError;
use beancount_parser::{ast, parse_lossy};
use ropey::Rope;
use tower_lsp_server::ls_types::Uri as Url;

use crate::indexer::Indexer;

/// Find a document by URI, tolerating platform-specific path casing/format differences.
pub(crate) fn find_document(
  documents: &HashMap<Url, Arc<Document>>,
  uri: &Url,
) -> Option<Arc<Document>> {
  if let Some(doc) = documents.get(uri) {
    return Some(Arc::clone(doc));
  }

  let target = uri
    .to_file_path()
    .map(|path| Indexer::normalized_path_key(path.as_ref()))?;

  for (key, doc) in documents.iter() {
    if let Some(candidate_path) = key
      .to_file_path()
      .map(|path| Indexer::normalized_path_key(path.as_ref()))
      && candidate_path == target
    {
      return Some(Arc::clone(doc));
    }
  }

  None
}

pub struct Document {
  content: String,
  ast: Vec<ast::Directive<'static>>,
  pub directives: Vec<core::Directive>,
  pub inferred: Vec<core::InferredDirective>,
  pub inferrence_errors: Vec<InferenceError>,
  pub errors: Vec<core::ParseError>,
  pub includes: Vec<String>,
  pub accounts: HashSet<String>,
  pub currencies: HashSet<String>,
  pub rope: Rope,
}

impl Document {
  pub fn content(&self) -> &str {
    &self.content
  }

  pub fn ast(&self) -> &[ast::Directive<'_>] {
    unsafe {
      std::mem::transmute::<&[ast::Directive<'static>], &[ast::Directive<'_>]>(
        self.ast.as_slice(),
      )
    }
  }

  pub fn directive_span(directive: &ast::Directive<'_>) -> ast::Span {
    match directive {
      ast::Directive::Open(v) => v.span,
      ast::Directive::Close(v) => v.span,
      ast::Directive::Balance(v) => v.span,
      ast::Directive::Pad(v) => v.span,
      ast::Directive::Transaction(v) => v.span,
      ast::Directive::Commodity(v) => v.span,
      ast::Directive::Price(v) => v.span,
      ast::Directive::Event(v) => v.span,
      ast::Directive::Query(v) => v.span,
      ast::Directive::Note(v) => v.span,
      ast::Directive::Document(v) => v.span,
      ast::Directive::Custom(v) => v.span,
      ast::Directive::Option(v) => v.span,
      ast::Directive::Include(v) => v.span,
      ast::Directive::Plugin(v) => v.span,
      ast::Directive::PushTag(v) => v.span,
      ast::Directive::PopTag(v) => v.span,
      ast::Directive::PushMeta(v) => v.span,
      ast::Directive::PopMeta(v) => v.span,
      ast::Directive::Comment(v) => v.span,
      ast::Directive::Headline(v) => v.span,
      ast::Directive::Raw(v) => v.span,
    }
  }

  /// Return the directive whose span contains the given byte offset using binary search.
  /// AST directives are expected to be sorted and non-overlapping.
  pub fn directive_at_offset(&self, byte_idx: usize) -> Option<&ast::Directive<'_>> {
    let directives = self.ast();
    let idx = directives
      .binary_search_by(|directive| {
        let span = Self::directive_span(directive);
        if byte_idx < span.start {
          Ordering::Greater
        } else if byte_idx >= span.end {
          Ordering::Less
        } else {
          Ordering::Equal
        }
      })
      .ok()?;

    directives.get(idx)
  }
}

/// Build a document from input text and filename.
///
/// This keeps the backing string pinned and stores an AST that borrows from it.
pub fn build_document(text: String, filename: &str) -> Option<Document> {
  let rope = Rope::from_str(text.as_str());
  let ast = parse_lossy(&text);

  let mut errors = Vec::new();

  let directives = match core::normalize_directives_with_rope(&ast, filename, &rope) {
    Ok(directives) => directives,
    Err(err) => {
      errors.push(err);
      Vec::new()
    }
  };

  let (inferred, inferrence_errors) = core::infer_directives(directives.clone());

  let includes = directives
    .iter()
    .filter_map(|directive| match directive {
      core::Directive::Include(include) => Some(include.filename.clone()),
      _ => None,
    })
    .collect();

  // Prefer normalized directives (more accurate).
  //
  // Note: completion may choose to apply additional heuristics for partially-edited/broken
  // documents; other LSP features can assume the document snapshot is valid.
  let accounts = collect_accounts(&directives);
  let currencies = collect_currencies(&ast, &inferred);

  let ast = unsafe {
    std::mem::transmute::<Vec<ast::Directive<'_>>, Vec<ast::Directive<'static>>>(ast)
  };

  Some(Document {
    content: text,
    ast,
    directives,
    inferred,
    errors,
    inferrence_errors,
    includes,
    accounts,
    currencies,
    rope,
  })
}

fn collect_currencies(
  directives: &[ast::Directive<'_>],
  inferred: &[core::InferredDirective],
) -> HashSet<String> {
  let mut currencies = HashSet::new();

  let mut insert_currency = |value: &str| {
    let label = value.trim();
    if !label.is_empty() {
      currencies.insert(label.to_owned());
    }
  };

  for directive in directives {
    match directive {
      ast::Directive::Open(open) => {
        for currency in &open.currencies {
          insert_currency(currency.content);
        }
      }
      ast::Directive::Balance(balance) => {
        if let Some(currency) = &balance.amount.currency {
          insert_currency(currency.content);
        }
      }
      ast::Directive::Commodity(commodity) => {
        insert_currency(commodity.currency.content)
      }
      ast::Directive::Price(price) => {
        insert_currency(price.currency.content);
        if let Some(currency) = &price.amount.currency {
          insert_currency(currency.content);
        }
      }
      ast::Directive::Transaction(tx) => {
        for posting in &tx.postings {
          if let Some(amount) = &posting.amount
            && let Some(currency) = &amount.currency
          {
            insert_currency(currency.content);
          }

          if let Some(cost_spec) = &posting.cost_spec
            && let Some(amount) = &cost_spec.amount
            && let Some(currency) = &amount.currency
          {
            insert_currency(currency.content);
          }

          if let Some(price_annotation) = &posting.price_annotation
            && let Some(currency) = &price_annotation.currency
          {
            insert_currency(currency.content);
          }
        }
      }
      _ => {}
    }
  }

  for directive in inferred {
    if let core::InferredDirective::Transaction(tx) = directive {
      for posting in &tx.postings {
        insert_currency(posting.amount.currency.as_str());
      }
    }
  }

  currencies
}

fn collect_accounts(directives: &[core::Directive]) -> HashSet<String> {
  let mut accounts = HashSet::new();

  for directive in directives {
    if let core::Directive::Open(open) = directive {
      insert_account(&mut accounts, &open.account);
    }
  }

  accounts
}

fn insert_account(accounts: &mut HashSet<String>, account: &str) {
  if !account.is_empty() {
    accounts.insert(account.to_owned());
  }
}

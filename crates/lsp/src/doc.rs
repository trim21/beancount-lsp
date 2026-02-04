use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::Arc;

use beancount_parser::{ast, core, parse_str_with_rope};
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
    pub directives: Vec<core::CoreDirective>,
    pub includes: Vec<String>,
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
    let (ast, rope) = parse_str_with_rope(&text);

    let directives = core::normalize_directives_with_rope(&ast, filename, &rope)
        .ok()
        .unwrap_or_default();

    let includes = directives
        .iter()
        .filter_map(|directive| match directive {
            core::CoreDirective::Include(include) => Some(include.filename.clone()),
            _ => None,
        })
        .collect();

    let ast = unsafe {
        std::mem::transmute::<Vec<ast::Directive<'_>>, Vec<ast::Directive<'static>>>(ast)
    };

    Some(Document {
        content: text,
        ast,
        directives,
        includes,
        rope,
    })
}

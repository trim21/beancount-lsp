use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;

use beancount_parser::{ast, core, parse_str};
use ropey::Rope;
use tower_lsp::lsp_types::Url;

use crate::server::normalized_path_key;

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
        .ok()
        .map(|path| normalized_path_key(&path))?;

    documents.iter().find_map(|(key, doc)| {
        key.to_file_path()
            .ok()
            .map(|path| normalized_path_key(&path))
            .filter(|candidate| candidate == &target)
            .map(|_| Arc::clone(doc))
    })
}

pub struct Document {
    content: Pin<String>,
    ast: Vec<ast::Directive<'static>>,
    pub directives: Vec<core::CoreDirective>,
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
}

/// Build a document from input text and filename.
///
/// This keeps the backing string pinned and stores an AST that borrows from it.
pub(crate) fn build_document(text: &str, filename: &str) -> Option<Document> {
    let content = Pin::new(text.to_owned());

    let ast = parse_str(&content, filename).unwrap_or_default();

    let directives = core::normalize_directives(&ast).ok().unwrap_or_default();

    let ast = unsafe {
        std::mem::transmute::<Vec<ast::Directive<'_>>, Vec<ast::Directive<'static>>>(ast)
    };

    let rope = Rope::from_str(text);
    Some(Document {
        content,
        ast,
        directives,
        rope,
    })
}

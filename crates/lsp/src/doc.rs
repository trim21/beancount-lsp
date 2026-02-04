use std::pin::Pin;

use beancount_parser::ast;
use beancount_parser::core;
use beancount_parser::parse_str;
use ropey::Rope;

///
/// ```compile_fail
/// use beancount_lsp::doc::DocumentWithAst;
/// use beancount_parser::parse_str;
/// use beancount_parser::core;
/// use ropey::Rope;
///
/// let content = "2023-01-01 open Assets:Cash\n".to_owned();
/// let directives = core::normalize_directives(parse_str(&content, "mem.bean").unwrap()).unwrap();
/// let includes = Vec::new();
/// let rope = Rope::from_str(&content);
/// let doc = DocumentWithAst::new(content, directives, includes, rope);
///
/// let borrowed_tree = doc.tree();
/// drop(doc);
/// let _ = borrowed_tree.len();
/// ```
pub struct DocumentWithAst {
    content: Pin<String>,
    tree: Vec<ast::Directive<'static>>,
    directives: Vec<core::CoreDirective>,
    includes: Vec<String>,
    rope: Rope,
}

impl DocumentWithAst {
    /// Build a document with a self-referential AST derived from `content`.
    pub fn new(
        content: String,
        directives: Vec<core::CoreDirective>,
        includes: Vec<String>,
        rope: Rope,
    ) -> Self {
        let content = Pin::new(content);
        let tree = parse_str(&content, "").unwrap();

        let tree = unsafe {
            std::mem::transmute::<Vec<ast::Directive<'_>>, Vec<ast::Directive<'static>>>(tree)
        };

        Self {
            content,
            tree,
            directives,
            includes,
            rope,
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn tree(&self) -> &[ast::Directive<'_>] {
        unsafe {
            std::mem::transmute::<&[ast::Directive<'static>], &[ast::Directive<'_>]>(
                self.tree.as_slice(),
            )
        }
    }

    pub fn directives(&self) -> &[core::CoreDirective] {
        &self.directives
    }

    pub fn includes(&self) -> &[String] {
        &self.includes
    }

    pub fn rope(&self) -> &Rope {
        &self.rope
    }
}

impl Clone for DocumentWithAst {
    fn clone(&self) -> Self {
        let content = self.content.as_ref().get_ref().clone();
        let content = Pin::new(content);
        let tree = parse_str(&content, "").unwrap();
        let tree = unsafe {
            std::mem::transmute::<Vec<ast::Directive<'_>>, Vec<ast::Directive<'static>>>(tree)
        };

        let rope = Rope::from_str(&content);

        Self {
            content,
            tree,
            directives: self.directives.clone(),
            includes: self.includes.clone(),
            rope,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tree_access_is_scoped_to_document() {
        use beancount_parser::core;
        use beancount_parser::parse_str;
        use ropey::Rope;
        let content = "".to_owned();
        let directives = core::normalize_directives(parse_str(&content, "").unwrap()).unwrap();
        let includes = Vec::new();
        let rope = Rope::from_str(&content);
        let doc = DocumentWithAst::new(content.clone(), directives, includes, rope);
        let borrowed_tree = doc.tree();
        let _ = borrowed_tree.len();
        drop(doc);
    }
}

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::{Result as AnyResult, anyhow};
use beancount_parser::{core, parse_str};
use beancount_tree_sitter::{language, tree_sitter};
use glob::glob;
use ropey::Rope;
use serde::Deserialize;
use tokio::sync::{Mutex, RwLock, mpsc};
use tokio::task;
use tower_lsp::async_trait;
use tower_lsp::jsonrpc::{Error, Result};
use tower_lsp::lsp_types::{
    CompletionOptions, CompletionParams, CompletionResponse, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, MessageType, Position, Range,
    SaveOptions, SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url,
};
use tower_lsp::{Client, LanguageServer};
use tracing::info;

use crate::checkers::{self, Checker, CheckerDiagnostic};
use crate::providers::definition;
use crate::providers::{completion, hover, semantic_tokens};

#[derive(Clone)]
pub struct Document {
    pub directives: Vec<core::CoreDirective>,
    pub content: String,
    pub rope: Rope,
    pub tree: tree_sitter::Tree,
}

#[derive(Clone)]
pub struct Backend {
    client: Client,
    inner: Arc<RwLock<Option<InnerBackend>>>, // initialized state
    checker: Option<Arc<dyn Checker>>,
    checker_queued: Arc<AtomicBool>,
    checker_tx: mpsc::UnboundedSender<()>,
    checker_rx: Arc<Mutex<Option<mpsc::UnboundedReceiver<()>>>>,
}

struct InnerBackend {
    documents: HashMap<Url, Document>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InitializeConfig {
    pub root_file: PathBuf,
}

fn canonical_or_original(path: &Path) -> PathBuf {
    fn strip_windows_verbatim_prefix(path: &Path) -> PathBuf {
        if !cfg!(windows) {
            return path.to_path_buf();
        }

        let raw = path.to_string_lossy();
        if let Some(rest) = raw.strip_prefix("\\\\?\\UNC\\") {
            // Convert verbatim UNC (\\?\UNC\server\share\...) into normal UNC (\\server\share\...)
            return PathBuf::from(format!("\\\\{rest}"));
        }

        if let Some(rest) = raw.strip_prefix("\\\\?\\") {
            // Convert verbatim drive paths (\\?\C:\...) into normal drive paths (C:\...)
            return PathBuf::from(rest);
        }

        path.to_path_buf()
    }

    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    strip_windows_verbatim_prefix(&canonical)
}

fn normalized_path_key(path: &Path) -> String {
    let raw = path.to_string_lossy();

    // Normalize Windows-specific prefixes and separators so we can reliably dedupe
    // queued/visited entries (Windows paths are generally case-insensitive).
    let normalized = raw.strip_prefix("\\\\?\\").unwrap_or(&raw);
    let normalized = normalized
        .strip_prefix("UNC\\")
        .map(|rest| format!("\\\\{rest}"))
        .unwrap_or_else(|| normalized.to_owned());
    let normalized = normalized.replace('\\', "/");

    if cfg!(windows) {
        normalized.to_lowercase()
    } else {
        normalized
    }
}

fn glob_pattern_if_any(path: &str) -> Option<String> {
    if !(path.contains('*') || path.contains('?') || path.contains('[')) {
        return None;
    }

    // Normalize Windows-specific path prefixes and separators so the `glob` crate
    // can expand patterns reliably.
    let pattern = path.strip_prefix("\\\\?\\").unwrap_or(path);
    let pattern = pattern
        .strip_prefix("UNC\\")
        .map(|rest| format!("\\\\{rest}"))
        .unwrap_or_else(|| pattern.to_owned());
    Some(pattern.replace('\\', "/"))
}

fn checker_diagnostic_to_lsp(diag: CheckerDiagnostic, source: &str) -> Diagnostic {
    let line = diag.lineno.unwrap_or(0);
    let message = match diag.filename.filter(|name| !name.is_empty()) {
        Some(name) => format!("{name}: {}", diag.message),
        None => diag.message,
    };

    Diagnostic {
        range: Range {
            start: Position::new(line, 0),
            end: Position::new(line, u32::MAX),
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(format!("beancount ({source})")),
        message,
        ..Diagnostic::default()
    }
}

pub fn parse_initialize_config(params: &InitializeParams) -> Result<InitializeConfig> {
    let value = params
        .initialization_options
        .as_ref()
        .ok_or_else(|| Error::invalid_params("missing initializationOptions"))?;

    tracing::debug!("init opts: {}", value.clone());

    serde_json::from_value(value.clone())
        .map_err(|err| Error::invalid_params(format!("invalid initializationOptions: {err}")))
}

impl Backend {
    pub fn new(client: Client) -> Self {
        let (checker_tx, checker_rx) = mpsc::unbounded_channel();

        Self {
            client,
            inner: Arc::new(RwLock::new(None)),
            checker: checkers::create().map(Arc::<dyn Checker>::from),
            checker_queued: Arc::new(AtomicBool::new(false)),
            checker_tx,
            checker_rx: Arc::new(Mutex::new(Some(checker_rx))),
        }
    }

    fn enqueue_checker_run(&self) {
        if self.checker_queued.swap(true, Ordering::SeqCst) {
            return;
        }

        let _ = self.checker_tx.send(());
    }

    fn spawn_checker_worker(&self, root_file: PathBuf, mut rx: mpsc::UnboundedReceiver<()>) {
        let this = self.clone();

        let root_uri: Url = Url::from_file_path(&root_file)
            .map_err(|_| {
                anyhow!(
                    "failed to convert root file to URI: {}",
                    root_file.display()
                )
            })
            .unwrap();

        tokio::spawn(async move {
            let mut published: HashSet<Url> = HashSet::new();
            while rx.recv().await.is_some() {
                published = this
                    .run_checker_once(&published, &root_uri, &root_file)
                    .await;

                this.checker_queued.store(false, Ordering::SeqCst);
            }
        });
    }

    async fn run_checker_once(
        &self,
        published: &HashSet<Url>,
        root_uri: &Url,
        root_file: &Path,
    ) -> HashSet<Url> {
        let root_path = canonical_or_original(root_file);
        let root_file_str = root_path.display().to_string();
        let mut diags_by_uri: HashMap<Url, Vec<Diagnostic>> = HashMap::new();

        if let Some(checker) = self.checker.as_ref().map(Arc::clone) {
            let result = task::spawn_blocking(move || {
                let name = checker.name();
                tracing::info!("run checker: {}", name);

                match checker.run(&root_file_str) {
                    Ok(diags) => {
                        let mapped = diags
                            .into_iter()
                            .map(|diag| {
                                let filename = diag.filename.clone();
                                let lsp = checker_diagnostic_to_lsp(diag, name);
                                (filename, lsp)
                            })
                            .collect::<Vec<_>>();
                        (mapped, Vec::new())
                    }
                    Err(err) => {
                        tracing::warn!("failed to run checker: {}", err);
                        (Vec::new(), vec![(name, err)])
                    }
                }
            })
            .await;

            match result {
                Ok((extra_diags, errors)) => {
                    for (filename, diag) in extra_diags {
                        let target_uri = filename
                            .and_then(|name| {
                                Url::from_file_path(canonical_or_original(Path::new(&name))).ok()
                            })
                            .unwrap_or_else(|| root_uri.clone());

                        diags_by_uri.entry(target_uri).or_default().push(diag);
                    }

                    for (name, err) in errors {
                        self.client
                            .log_message(
                                MessageType::ERROR,
                                format!("{name} checker failed: {err}"),
                            )
                            .await;
                    }
                }
                Err(err) => {
                    self.client
                        .log_message(MessageType::ERROR, format!("checker task panicked: {err}"))
                        .await;
                }
            }
        }

        let new_keys: HashSet<Url> = diags_by_uri.keys().cloned().collect();

        // Clear diagnostics that disappeared in this run so the client drops them.
        let stale = published.difference(&new_keys).cloned().collect::<Vec<_>>();

        for uri in stale {
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
        }

        for (target_uri, diags) in diags_by_uri {
            self.client
                .publish_diagnostics(target_uri.clone(), diags, None)
                .await;
        }

        new_keys
    }

    fn parse_document(text: &str, filename: &str) -> Option<Document> {
        let content = text.to_owned();

        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&language()).ok()?;
        let tree = parser.parse(&content, None)?;

        // Tree-sitter can still produce a tree for partially invalid input; keep that so
        // providers and the checker see the latest text even when the beancount parser fails.
        let directives = parse_str(&content, filename)
            .ok()
            .and_then(|ast| core::normalize_directives(ast).ok())
            .unwrap_or_default();

        let rope = Rope::from_str(&content);
        Some(Document {
            directives,
            content,
            rope,
            tree,
        })
    }

    fn load_journal_tree(journal_file: &Path) -> AnyResult<HashMap<Url, Document>> {
        fn to_url(path: &Path) -> Option<Url> {
            Url::from_file_path(path).ok()
        }

        fn resolve_include_path(base_file: &Path, raw: &str) -> PathBuf {
            let raw_path = PathBuf::from(raw);
            if raw_path.is_relative() {
                if let Some(parent) = base_file.parent() {
                    parent.join(raw_path)
                } else {
                    raw_path
                }
            } else {
                raw_path
            }
        }

        fn enqueue_includes(
            base_file: &Path,
            include_filename: &str,
            queue: &mut VecDeque<PathBuf>,
            queued: &mut HashSet<String>,
        ) {
            let resolved = resolve_include_path(base_file, include_filename);
            let resolved_str = resolved.to_string_lossy();

            if let Some(pattern) = glob_pattern_if_any(&resolved_str) {
                match glob(&pattern) {
                    Ok(entries) => {
                        for entry in entries {
                            match entry {
                                Ok(entry) => {
                                    if entry.is_file() {
                                        let key = normalized_path_key(&entry);
                                        if queued.insert(key) {
                                            queue.push_back(entry);
                                        }
                                    }
                                }
                                Err(err) => {
                                    tracing::warn!(
                                        base_file = %base_file.display(),
                                        include = %include_filename,
                                        pattern = %pattern,
                                        error = %err,
                                        "failed to expand include glob entry"
                                    );
                                }
                            }
                        }
                    }
                    Err(err) => {
                        tracing::warn!(
                            base_file = %base_file.display(),
                            include = %include_filename,
                            pattern = %pattern,
                            error = %err,
                            "failed to expand include glob"
                        );
                    }
                }
                return;
            }

            let key = normalized_path_key(&resolved);
            if queued.insert(key) {
                queue.push_back(resolved);
            }
        }

        let root = canonical_or_original(journal_file);
        if !root.is_file() {
            return Err(anyhow!(
                "journal_file does not exist or is not a file: {}",
                root.display()
            ));
        }

        let mut documents: HashMap<Url, Document> = HashMap::new();

        let mut queue: VecDeque<PathBuf> = VecDeque::new();
        let mut queued: HashSet<String> = HashSet::new();
        let mut visited: HashSet<String> = HashSet::new();

        queued.insert(normalized_path_key(&root));
        queue.push_back(root);

        while let Some(path) = queue.pop_front() {
            let canonical = canonical_or_original(&path);
            let visit_key = normalized_path_key(&canonical);
            if !visited.insert(visit_key) {
                continue;
            }

            if !canonical.is_file() {
                tracing::warn!(file = %canonical.display(), "skipping non-file path from includes");
                continue;
            }

            let uri = match to_url(&canonical) {
                Some(uri) => uri,
                None => {
                    tracing::warn!(file = %canonical.display(), "failed to convert path to URI");
                    continue;
                }
            };

            let content = match fs::read_to_string(&canonical) {
                Ok(content) => content,
                Err(err) => {
                    tracing::warn!(file = %canonical.display(), error = %err, "failed to read beancount file");
                    continue;
                }
            };

            let filename_for_parser = canonical.display().to_string();
            let doc = match Self::parse_document(&content, &filename_for_parser) {
                Some(doc) => doc,
                None => {
                    tracing::warn!(file = %canonical.display(), "failed to parse beancount file");
                    continue;
                }
            };

            for directive in &doc.directives {
                if let core::CoreDirective::Include(include) = directive {
                    enqueue_includes(&canonical, &include.filename, &mut queue, &mut queued);
                }
            }

            documents.insert(uri, doc);

            tracing::info!(file = %canonical.display(), "loaded beancount file");
        }

        Ok(documents)
    }

    async fn with_inner<R>(&self, f: impl FnOnce(&InnerBackend) -> R) -> Result<R> {
        let guard = self.inner.read().await;
        let inner = guard.as_ref().ok_or_else(Error::invalid_request)?;
        Ok(f(inner))
    }

    async fn with_inner_mut<R>(&self, f: impl FnOnce(&mut InnerBackend) -> R) -> Result<R> {
        let mut guard = self.inner.write().await;
        let inner = guard.as_mut().ok_or_else(Error::invalid_request)?;
        Ok(f(inner))
    }

    async fn update_document(&self, uri: Url, text: String) {
        if let Some(doc) = Self::parse_document(&text, uri.as_str())
            && let Ok(()) = self
                .with_inner_mut(|inner| {
                    inner.documents.insert(uri.clone(), doc);
                })
                .await
        {
            self.enqueue_checker_run();
        }
    }

    async fn reparse(&self, _uri: &Url) {
        self.enqueue_checker_run();
    }
}

#[async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        let config = parse_initialize_config(&params)?;

        info!(root_file = %config.root_file.display(), "received initialization config");

        let root_path = canonical_or_original(&config.root_file);
        {
            let mut guard = self.inner.write().await;
            if guard.is_some() {
                return Err(Error::invalid_params("initializationOptions already set"));
            }

            let documents = Self::load_journal_tree(&root_path).unwrap_or_else(|e| {
                tracing::warn!("failed to load journal tree: {e}");
                HashMap::new()
            });

            *guard = Some(InnerBackend { documents });
        }

        // Start checker worker after initialization so the root file path is known.
        if let Some(rx) = self.checker_rx.lock().await.take() {
            self.spawn_checker_worker(root_path, rx);
        }

        let text_document_sync = TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::FULL),
            save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                include_text: Some(true),
            })),
            ..TextDocumentSyncOptions::default()
        });

        let semantic_tokens =
            SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: tower_lsp::lsp_types::WorkDoneProgressOptions::default(
                ),
                legend: semantic_tokens::legend(),
                range: None,
                full: Some(SemanticTokensFullOptions::Bool(true)),
            });

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(text_document_sync),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![":".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(tower_lsp::lsp_types::OneOf::Left(true)),
                semantic_tokens_provider: Some(semantic_tokens),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "beancount-lsp".to_owned(),
                version: None,
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "beancount-lsp initialized")
            .await;

        // Run checker once after initialization without blocking this handler.
        self.enqueue_checker_run();
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.update_document(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.update_document(params.text_document.uri, change.text)
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.update_document(params.text_document.uri, text).await;
        } else {
            self.reparse(&params.text_document.uri).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.with_inner(|inner| completion::completion(&inner.documents, &params))
            .await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.with_inner(|inner| hover::hover(&inner.documents, &params))
            .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        self.with_inner(|inner| definition::goto_definition(&inner.documents, &params))
            .await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let text = self
            .with_inner(|inner| inner.documents.get(&uri).cloned())
            .await?;
        Ok(text.and_then(|doc| semantic_tokens::semantic_tokens_full(&doc.content)))
    }
}

#[cfg(test)]
mod tests {
    use super::Backend;
    use std::fs;
    use std::path::Path;
    use tower_lsp::lsp_types::Url;

    fn write_file(path: &Path, content: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, content).unwrap();
    }

    #[test]
    fn load_journal_tree_expands_includes_transitively() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().join("main.bean");

        // main -> accounts/*.bean + common.bean
        write_file(
            &root,
            r#"include "accounts/*.bean"
include "common.bean"
"#,
        );

        let account_one = dir.path().join("accounts/one.bean");
        let account_two = dir.path().join("accounts/two.bean");
        write_file(
            &account_one,
            r#"include "../nested/n1.bean"
"#,
        );
        write_file(&account_two, "\n");

        let common = dir.path().join("common.bean");
        write_file(
            &common,
            r#"include "nested/*.bean"
"#,
        );

        let nested1 = dir.path().join("nested/n1.bean");
        let nested2 = dir.path().join("nested/n2.bean");
        write_file(&nested1, "\n");
        write_file(&nested2, "\n");

        let docs = Backend::load_journal_tree(&root).unwrap();

        let expected_paths = [
            &root,
            &account_one,
            &account_two,
            &common,
            &nested1,
            &nested2,
        ];
        for p in expected_paths {
            let p = p.canonicalize().unwrap_or_else(|_| p.to_path_buf());
            let uri = Url::from_file_path(&p).unwrap();
            assert!(docs.contains_key(&uri), "missing document: {}", p.display());
        }

        assert_eq!(docs.len(), 6);
    }
}

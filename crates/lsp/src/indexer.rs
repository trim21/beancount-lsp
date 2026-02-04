use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

use anyhow::{Result as AnyResult, anyhow};
use glob::glob;
use lru::LruCache;
use tower_lsp_server::ls_types::Uri as Url;

use crate::doc::{self, Document};

pub(crate) fn canonical_or_original(path: &Path) -> PathBuf {
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

pub struct Indexer {
    documents: HashMap<Url, Arc<Document>>,
    include_edges: HashMap<Url, HashSet<Url>>,
    ref_counts: HashMap<Url, usize>,
    root: Option<Url>,
    file_cache: LruCache<String, CacheEntry>,
    last_gc: Instant,
}

#[derive(Clone)]
struct CacheEntry {
    size: u64,
    mtime: Option<SystemTime>,
    doc: Arc<Document>,
}

impl Default for Indexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Indexer {
    const FILE_CACHE_CAPACITY: usize = 512;

    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            include_edges: HashMap::new(),
            ref_counts: HashMap::new(),
            root: None,
            file_cache: LruCache::new(
                NonZeroUsize::new(Self::FILE_CACHE_CAPACITY).expect("file cache capacity"),
            ),
            last_gc: Instant::now(),
        }
    }

    pub fn from_journal(journal_file: &Path) -> AnyResult<Self> {
        let mut indexer = Self::new();
        indexer.load_journal_tree(journal_file)?;
        Ok(indexer)
    }

    pub fn documents(&self) -> &HashMap<Url, Arc<Document>> {
        &self.documents
    }

    pub fn normalized_path_key(path: &Path) -> String {
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

    pub fn glob_pattern_if_any(path: &str) -> Option<String> {
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

    pub fn resolve_include_path(base_dir: &Path, raw: &str) -> PathBuf {
        let raw_path = PathBuf::from(raw);
        if raw_path.is_relative() {
            base_dir.join(raw_path)
        } else {
            raw_path
        }
    }

    fn enqueue_includes(
        base_dir: &Path,
        include_filename: &str,
        queue: &mut VecDeque<PathBuf>,
        queued: &mut HashSet<String>,
    ) {
        let resolved = Self::resolve_include_path(base_dir, include_filename);
        let key = Self::normalized_path_key(&resolved);
        if queued.insert(key) {
            queue.push_back(resolved);
        }
    }

    pub fn load_journal_tree(&mut self, journal_file: &Path) -> AnyResult<()> {
        fn to_url(path: &Path) -> Option<Url> {
            Url::from_file_path(path)
        }
        let root = canonical_or_original(journal_file);
        if !root.is_file() {
            return Err(anyhow!(
                "journal_file does not exist or is not a file: {}",
                root.display()
            ));
        }

        let mut queue: VecDeque<PathBuf> = VecDeque::new();
        let mut queued: HashSet<String> = HashSet::new();
        let mut visited: HashSet<String> = HashSet::new();

        queued.insert(Self::normalized_path_key(&root));
        queue.push_back(root);

        while let Some(path) = queue.pop_front() {
            let canonical = canonical_or_original(&path);
            let visit_key = Self::normalized_path_key(&canonical);
            if !visited.insert(visit_key) {
                continue;
            }

            let uri = match to_url(&canonical) {
                Some(uri) => uri,
                None => {
                    tracing::warn!(file = %canonical.display(), "failed to convert path to URI");
                    continue;
                }
            };

            if !canonical.is_file() {
                tracing::warn!(file = %canonical.display(), "skipping non-file path from includes");
                continue;
            }

            let doc = match self.load_document_from_path(&canonical) {
                Some(doc) => doc,
                None => continue,
            };

            let base_dir = match canonical.parent() {
                Some(parent) => parent,
                None => {
                    tracing::warn!(file = %canonical.display(), "missing parent directory for include resolution");
                    &canonical
                }
            };
            for include in &doc.includes {
                Self::enqueue_includes(base_dir, include, &mut queue, &mut queued);
            }

            if self.root.is_none() {
                self.root = Some(uri.clone());
                self.ref_counts.insert(uri.clone(), 1);
            }

            self.insert_document(uri, doc);

            tracing::info!(file = %canonical.display(), "loaded beancount file");
        }

        Ok(())
    }

    pub fn update_document(&mut self, uri: Url, text: String) -> bool {
        let Some(base_path) = uri.to_file_path() else {
            return false;
        };

        let Some(doc) = Self::parse_document(&text, &base_path) else {
            return false;
        };
        let doc = Arc::new(doc);

        let target_key = Self::url_normalized_key(&uri);
        let mut previous: Option<Vec<String>> = None;
        let mut previous_includes: HashSet<Url> = HashSet::new();
        if let Some(target_key) = &target_key {
            let dupes: Vec<Url> = self
                .documents
                .keys()
                .filter(|existing| Self::url_normalized_key(existing).as_ref() == Some(target_key))
                .cloned()
                .collect();
            for k in dupes {
                if previous.is_none() {
                    previous = self.documents.get(&k).map(|doc| doc.includes.clone());
                }
                if previous_includes.is_empty() {
                    previous_includes = self.include_edges.get(&k).cloned().unwrap_or_default();
                }
                self.documents.remove(&k);
                self.include_edges.remove(&k);
            }
        }

        let new_includes = self.collect_include_uris(doc.as_ref());
        self.documents.insert(uri.clone(), Arc::clone(&doc));
        self.include_edges.insert(uri.clone(), new_includes.clone());

        let removed = previous_includes
            .difference(&new_includes)
            .cloned()
            .collect::<Vec<_>>();
        let added = new_includes
            .difference(&previous_includes)
            .cloned()
            .collect::<Vec<_>>();

        for child in removed {
            self.decrement_ref(&child);
        }

        let mut missing_paths = Vec::new();
        for child in &added {
            self.increment_ref(child);
            if !self.documents.contains_key(child)
                && let Some(path) = child.to_file_path()
            {
                missing_paths.push(path.into_owned());
            }
        }

        if !missing_paths.is_empty() {
            let new_docs = self.load_missing_from_paths(&missing_paths);
            for (uri, doc) in new_docs {
                self.insert_document(uri, doc);
            }
        }

        self.maybe_run_gc();

        true
    }

    fn insert_document(&mut self, uri: Url, doc: Arc<Document>) {
        let includes = self.collect_include_uris(doc.as_ref());
        self.documents.insert(uri.clone(), Arc::clone(&doc));
        self.include_edges.insert(uri.clone(), includes.clone());

        for child in includes {
            self.increment_ref(&child);
        }
    }

    fn collect_include_uris(&self, doc: &Document) -> HashSet<Url> {
        let mut includes = HashSet::new();
        for raw in &doc.includes {
            let path = Path::new(raw);
            let canonical = canonical_or_original(path);
            match Url::from_file_path(&canonical) {
                Some(uri) => {
                    includes.insert(uri);
                }
                None => {
                    tracing::warn!(file = %canonical.display(), "failed to convert include path to URI");
                }
            }
        }
        includes
    }

    fn increment_ref(&mut self, uri: &Url) {
        let entry = self.ref_counts.entry(uri.clone()).or_insert(0);
        *entry = entry.saturating_add(1);
    }

    fn decrement_ref(&mut self, uri: &Url) {
        let Some(count) = self.ref_counts.get_mut(uri) else {
            return;
        };

        if *count == 0 {
            return;
        }

        *count -= 1;
        if *count > 0 {
            return;
        }

        if self.root.as_ref() == Some(uri) {
            return;
        }

        self.ref_counts.remove(uri);
        let children = self.include_edges.remove(uri).unwrap_or_default();
        self.documents.remove(uri);

        for child in children {
            self.decrement_ref(&child);
        }
    }

    fn load_missing_from_paths(&mut self, paths: &[PathBuf]) -> Vec<(Url, Arc<Document>)> {
        let mut queue: VecDeque<PathBuf> = paths.iter().cloned().collect();
        let mut queued: HashSet<String> = HashSet::new();
        let mut visited: HashSet<String> = HashSet::new();
        let mut new_docs: Vec<(Url, Arc<Document>)> = Vec::new();

        for path in paths {
            queued.insert(Self::normalized_path_key(path));
        }

        while let Some(path) = queue.pop_front() {
            let canonical = canonical_or_original(&path);
            let visit_key = Self::normalized_path_key(&canonical);
            if !visited.insert(visit_key.clone()) {
                continue;
            }

            let uri = match Url::from_file_path(&canonical) {
                Some(uri) => uri,
                None => {
                    tracing::warn!(file = %canonical.display(), "failed to convert path to URI");
                    continue;
                }
            };

            if self.documents.contains_key(&uri) {
                continue;
            }

            if !canonical.is_file() {
                tracing::warn!(file = %canonical.display(), "skipping non-file path from includes");
                continue;
            }

            let parsed = match self.load_document_from_path(&canonical) {
                Some(doc) => doc,
                None => continue,
            };

            let base_dir = match canonical.parent() {
                Some(parent) => parent,
                None => {
                    tracing::warn!(file = %canonical.display(), "missing parent directory for include resolution");
                    &canonical
                }
            };
            for include in &parsed.includes {
                let resolved = Self::resolve_include_path(base_dir, include);
                let key = Self::normalized_path_key(&resolved);
                if queued.insert(key) {
                    queue.push_back(resolved);
                }
            }

            new_docs.push((uri, parsed));
        }

        new_docs
    }

    fn load_document_from_path(&mut self, path: &Path) -> Option<Arc<Document>> {
        let canonical = canonical_or_original(path);
        let key = Self::normalized_path_key(&canonical);

        let metadata = match fs::metadata(&canonical) {
            Ok(metadata) => metadata,
            Err(err) => {
                tracing::warn!(file = %canonical.display(), error = %err, "failed to read file metadata");
                return None;
            }
        };

        let size = metadata.len();
        let mtime = metadata.modified().ok();

        if let Some(entry) = self.file_cache.get(&key) {
            if entry.size == size && entry.mtime == mtime {
                return Some(Arc::clone(&entry.doc));
            }
            self.file_cache.pop(&key);
        }

        let content = match fs::read_to_string(&canonical) {
            Ok(content) => content,
            Err(err) => {
                tracing::warn!(file = %canonical.display(), error = %err, "failed to read beancount file");
                return None;
            }
        };

        let doc = match Self::parse_document(&content, &canonical) {
            Some(doc) => Arc::new(doc),
            None => {
                tracing::warn!(file = %canonical.display(), "failed to parse beancount file");
                return None;
            }
        };

        self.file_cache.put(
            key,
            CacheEntry {
                size,
                mtime,
                doc: Arc::clone(&doc),
            },
        );

        Some(doc)
    }

    fn run_gc(&mut self) -> bool {
        let Some(root) = self.root.clone() else {
            return false;
        };

        let mut reachable: HashSet<Url> = HashSet::new();
        let mut queue: VecDeque<Url> = VecDeque::new();
        reachable.insert(root.clone());
        queue.push_back(root.clone());

        while let Some(current) = queue.pop_front() {
            if let Some(children) = self.include_edges.get(&current) {
                for child in children {
                    if reachable.insert(child.clone()) {
                        queue.push_back(child.clone());
                    }
                }
            }
        }

        let mut new_ref_counts: HashMap<Url, usize> = HashMap::new();
        new_ref_counts.insert(root.clone(), 1);
        for (parent, children) in &self.include_edges {
            if !reachable.contains(parent) {
                continue;
            }
            for child in children {
                let entry = new_ref_counts.entry(child.clone()).or_insert(0);
                *entry = entry.saturating_add(1);
            }
        }

        let mut changed = false;

        let unreachable_docs = self
            .documents
            .keys()
            .filter(|uri| !reachable.contains(*uri))
            .cloned()
            .collect::<Vec<_>>();
        for uri in unreachable_docs {
            self.documents.remove(&uri);
            self.include_edges.remove(&uri);
            changed = true;
        }

        if self.ref_counts != new_ref_counts {
            self.ref_counts = new_ref_counts;
            changed = true;
        }

        changed
    }

    fn maybe_run_gc(&mut self) {
        let elapsed = self.last_gc.elapsed();
        if elapsed < Duration::from_secs(600) {
            return;
        }

        self.run_gc();
        self.last_gc = Instant::now();
    }

    fn url_normalized_key(url: &Url) -> Option<String> {
        let path = url.to_file_path()?;
        Some(Self::normalized_path_key(path.as_ref()))
    }

    pub fn parse_document(text: &str, filename: &Path) -> Option<Document> {
        let base_path = match filename.parent() {
            Some(parent) => parent,
            None => {
                tracing::warn!(file = %filename.display(), "missing parent directory for include resolution");
                filename
            }
        };

        let mut doc = doc::build_document(text.to_owned(), &filename.to_string_lossy())?;

        let mut expanded_includes = Vec::new();
        for include in &doc.includes {
            let resolved = Self::resolve_include_path(base_path, include);
            let resolved_str = resolved.to_string_lossy();
            if let Some(pattern) = Self::glob_pattern_if_any(&resolved_str) {
                match glob(&pattern) {
                    Ok(entries) => {
                        for entry in entries {
                            match entry {
                                Ok(entry) => {
                                    if entry.is_file() {
                                        expanded_includes.push(entry.to_string_lossy().to_string());
                                    }
                                }
                                Err(err) => {
                                    tracing::warn!(
                                        base_file = %base_path.display(),
                                        include = %include,
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
                            base_file = %base_path.display(),
                            include = %include,
                            pattern = %pattern,
                            error = %err,
                            "failed to expand include glob"
                        );
                    }
                }
                continue;
            }

            expanded_includes.push(resolved_str.to_string());
        }

        doc.includes = expanded_includes;

        Some(doc)
    }

    pub(crate) fn parse_document_for_bench(text: &str, filename: &Path) {
        let _ = Self::parse_document(text, filename);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn ref_counts_follow_include_graph() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root_path = dir.path().join("root.bean");
        let a_path = dir.path().join("a.bean");
        let b_path = dir.path().join("b.bean");
        let c_path = dir.path().join("c.bean");

        fs::write(&root_path, "include \"a.bean\"\ninclude \"b.bean\"\n").expect("write root");
        fs::write(&a_path, "include \"c.bean\"\n").expect("write a");
        fs::write(&b_path, "").expect("write b");
        fs::write(&c_path, "").expect("write c");

        let mut indexer = Indexer::from_journal(&root_path).expect("load journal");

        let root_uri = Url::from_file_path(canonical_or_original(&root_path)).expect("root uri");
        let a_uri = Url::from_file_path(canonical_or_original(&a_path)).expect("a uri");
        let b_uri = Url::from_file_path(canonical_or_original(&b_path)).expect("b uri");
        let c_uri = Url::from_file_path(canonical_or_original(&c_path)).expect("c uri");

        assert_eq!(indexer.ref_counts.get(&root_uri), Some(&1));
        assert_eq!(indexer.ref_counts.get(&a_uri), Some(&1));
        assert_eq!(indexer.ref_counts.get(&b_uri), Some(&1));
        assert_eq!(indexer.ref_counts.get(&c_uri), Some(&1));

        let updated = indexer.update_document(root_uri.clone(), "include \"a.bean\"\n".to_owned());
        assert!(updated);

        assert!(indexer.documents.contains_key(&root_uri));
        assert!(indexer.documents.contains_key(&a_uri));
        assert!(indexer.documents.contains_key(&c_uri));
        assert!(!indexer.documents.contains_key(&b_uri));
        assert!(!indexer.ref_counts.contains_key(&b_uri));
    }
}

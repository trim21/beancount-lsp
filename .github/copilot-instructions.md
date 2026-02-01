# Beancount LSP – AI Guide

## Architecture
- The stdio LSP entrypoint is tiny; it simply forwards to the library main in [crates/lsp/src/main.rs](crates/lsp/src/main.rs).
- Core server logic lives in [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs); `Backend` holds the `tower_lsp` client plus an in-memory `RwLock<HashMap<Url, String>>` cache of document texts.
- Diagnostics are produced by `beancount_parser::parse_str`, keyed by the URI-derived filename; failures emit a single `Diagnostic` at the document origin with the parser message [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).
- Document highlights delegate to providers under [crates/lsp/src/providers](crates/lsp/src/providers); the only provider today highlights the entire line containing the cursor [crates/lsp/src/providers/highlight.rs](crates/lsp/src/providers/highlight.rs).
- LSP capabilities are minimal: text sync, save handling, and document highlights only [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).

## LSP behavior
- Text sync uses full document changes with `include_text` on save; the server re-parses on open, change (last content change only), and save [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).
- Highlights return at most one `DocumentHighlight`, covering the whole line (kind `TEXT`) [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).
- Filename resolution prefers local file paths from the URI; fall back to the URI string for diagnostics [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).

## Extension patterns
- Add new LSP features by creating modules under `crates/lsp/src/providers/` and wiring them through `Backend` plus the capability flags in `initialize()` [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).
- When reading document text, use the cached map in `Backend.documents` to avoid blocking IO; mutations should reuse `update_document()` so diagnostics stay in sync [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).
- Keep diagnostic ranges meaningful; current behavior pins them to the start of the file—update `publish_diagnostics_for()` if the parser yields span data [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs).

## Python wrapper
- The Python package `beancount-lsp` exposes the same server via a pyo3 extension built as a `cdylib` [crates/lsp-py/Cargo.toml](crates/lsp-py/Cargo.toml).
- Runtime wrapper spins up a multi-threaded Tokio runtime and blocks on the Rust `main()` [crates/lsp-py/src/lib.rs](crates/lsp-py/src/lib.rs).
- CLI entrypoint `beancount-lsp` calls `console_endpoint()` from the extension module [crates/lsp-py/py-src/beancount_lsp/cli.py](crates/lsp-py/py-src/beancount_lsp/cli.py).

## Dev workflows
- Rust server: `cargo run -p beancount-lsp` starts the stdio LSP; `cargo test -p beancount-lsp` runs crate tests (none today).
- Python package builds with maturin; from the repo root use the project-local virtualenv and run `uv run --project crates/lsp-py maturin develop` to build and install the extension in editable mode.
- Workspace dependencies share versions via `[workspace.dependencies]` in [Cargo.toml](Cargo.toml); `beancount-parser` is pinned to a specific git revision—avoid bumping without testing parser output.

## Conventions and cautions
- Stick to async patterns already in use (Tokio multi-thread runtime; `tower_lsp` async_trait handlers).
- The server assumes full document sync; if you add incremental sync, adjust the capability and cache update logic together.
- Keep file paths ASCII-friendly; diagnostics and parser calls derive filenames from URIs.
- Avoid global installs; use the repo-local `.venv` and `uv` for Python tooling.

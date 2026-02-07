# Beancount Language Server

This is a **beancount language server** implementation written in Rust that provides LSP (Language Server Protocol) support for Beancount files (.bean, .beancount). Beancount is a double-entry bookkeeping system that uses plain text files.

## Architecture
- The stdio LSP entrypoint is tiny; it simply forwards to the library main in [crates/lsp/src/main.rs](crates/lsp/src/main.rs).
- Core server logic lives in [crates/lsp/src/lib.rs](crates/lsp/src/lib.rs); `Backend` holds the `tower_lsp_server` client plus an in-memory `RwLock<HashMap<Url, Document>>` cache of document texts.
- Diagnostics are produced by `beancount_parser::parse_str`, keyed by the URI-derived filename; failures emit a single `Diagnostic` at the document origin with the parser message [crates/lsp/src/server.rs](crates/lsp/src/server.rs).
- Providers live under [crates/lsp/src/providers](crates/lsp/src/providers) (completion, hover, definition, semantic tokens).
- LSP capabilities include full document sync, completion, hover, definition, and semantic tokens in [crates/lsp/src/server.rs](crates/lsp/src/server.rs).

## LSP behavior
- Text sync uses full document changes with `include_text` on save; the server re-parses on open, change (last content change only), and save [crates/lsp/src/server.rs](crates/lsp/src/server.rs).
- Filename resolution prefers local file paths from the URI; fall back to the URI string for diagnostics [crates/lsp/src/server.rs](crates/lsp/src/server.rs).

## Extension patterns
- Add new LSP features by creating modules under [crates/lsp/src/providers](crates/lsp/src/providers) and wiring them through `Backend` plus the capability flags in `initialize()` [crates/lsp/src/server.rs](crates/lsp/src/server.rs).
- When reading document text, use the cached map in `Backend.documents` to avoid blocking IO; mutations should reuse `update_document()` so diagnostics stay in sync [crates/lsp/src/server.rs](crates/lsp/src/server.rs).
- Keep diagnostic ranges meaningful; current behavior pins them to the start of the file—update `checker_diagnostic_to_lsp()` if the parser yields span data [crates/lsp/src/server.rs](crates/lsp/src/server.rs).

## Python wrapper
- The Python package `beancount-lsp` exposes the same server via a pyo3 extension built as a `cdylib` [crates/lsp-py/Cargo.toml](crates/lsp-py/Cargo.toml).
- Runtime wrapper spins up a multi-threaded Tokio runtime and blocks on the Rust `main()` [crates/lsp-py/src/lib.rs](crates/lsp-py/src/lib.rs).
- CLI entrypoint `beancount-lsp` calls `console_endpoint()` from the extension module [crates/lsp-py/py-src/beancount_lsp/cli.py](crates/lsp-py/py-src/beancount_lsp/cli.py).

## Dev workflows
- Rust server: `cargo run -p beancount-lsp` starts the stdio LSP; `cargo test -p beancount-lsp` runs crate test.
- Python package builds with maturin; from the repo root use the project-local virtualenv and run `uv run maturin develop` to build and install the extension in editable mode.
- Workspace dependencies share versions via `[workspace.dependencies]` in [Cargo.toml](Cargo.toml); `beancount-parser` is pinned to a specific git revision—avoid bumping without testing parser output.

- When you touch rust code, you must run `cargo check -p beancount-lsp` to check if your code is correct. If it doesn't, you should fix the errors.

## Conventions and cautions
- Stick to async patterns already in use (Tokio multi-thread runtime; `tower_lsp_server` async handlers).
- The server assumes full document sync; if you add incremental sync, adjust the capability and cache update logic together.
- Keep file paths ASCII-friendly; diagnostics and parser calls derive filenames from URIs.
- Avoid global installs; use the repo-local `.venv` and `uv` for Python tooling. but if there is already some tools in global PATH, for example, user installed `matruin`, you should avoid install it again in local env.

## Code style

when you are add beancount testing case , you should use raw string and join to make it more readable. for example:

```
let example: String = lines((
    r#"2022-01-01 * "..." "..." #food  "#,
    r#"  Assets:Cash -10 USD           "#,
    r#"  Expenses:Food                 "#,
    r#"2022-01-02 * "..." "..." #      "#,
    r#"  Expenses:Food                 "#,
));

```

And in rust, the `"` inside a raw string (`r#"..."#`) doesn't need to be escaped.

## Gotcha

The lsp specifies text positions (line, character) using UTF-16 code units, but beancount source files are UTF-8. so when converting position, you should always do a multiple bytes aware converting, with some utils.

Beancount LSP

Cross-platform Language Server for Beancount, distributed as the `beancount-lsp` Python package.
The server is implemented in Rust and shipped as a PyO3 extension,
so you get native performance with a familiar Python installation and CLI.

## Requirements
- Python 3.10+
- `beancount` 3.x (installed automatically as a dependency)
- An editor that can speak the Language Server Protocol over stdio

## Install
```bash
pip install beancount-lsp
# or
uv pip install beancount-lsp
```

The package installs two console scripts: `beancount-lsp` (preferred) and `beancount-langauge-server` (compat alias).

## Quick start
1) Pick your main journal file (the one that includes the rest). Call it `/path/to/main.bean`.
2) Configure your LSP client to start the server via stdio:
	 - Command: `beancount-lsp`
	 - Initialization options (required):
		 ```json
		 {
			 "root_file": "/path/to/main.bean"
		 }
		 ```
3) Open any included Beancount file; the server will parse the full include graph starting from `root_file`.

### VS Code
Install the Marketplace extension: [Beancount LSP](https://marketplace.visualstudio.com/items?itemName=trim21.beancount-lsp). See [vscode/README.md](vscode/README.md) for extension configuration details (root file setting, commands, and troubleshooting).


## Capabilities
- Diagnostics: runs the Beancount loader on the root file (and imports) to report parser errors and other validation issues. Diagnostics are published per file with line/column data when available.
- Account completion: offers account names gathered from all `open` directives in the loaded files.
- Hover: shows notes attached to the account under the cursor (`note ACCOUNT "..."`).
- Go to definition: jumps to the `open` directive for the account under the cursor, searching across included files.
- Semantic tokens: tree-sitter-based semantic highlighting for Beancount syntax.
- Include expansion: `include` directives (and glob patterns) are loaded transitively from the `root_file` so completion/hover/definitions work across your whole journal.

## Logging and troubleshooting
- Increase verbosity: `beancount-lsp --log-level debug`
- Write logs to a file: `beancount-lsp --log-file beancount-lsp.log`
- If the server exits immediately, verify that `root_file` exists and that the process can import `beancount` in the same Python environment.
- The server requires full-document sync; incremental sync clients are not supported.

## Updating
To upgrade to the latest release:
```bash
pip install --upgrade beancount-lsp
```

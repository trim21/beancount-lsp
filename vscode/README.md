# Beancount LSP for VS Code

Use this extension to connect VS Code to the Beancount language server (`beancount-lsp`) and get Beancount language features.

## What you need

- Python 3.10+ with `pip` (or `uv`)
- The `beancount-lsp` package installed from PyPI
- This VS Code extension installed

## Install the extension

install extension from vscode market: https://marketplace.visualstudio.com/items?itemName=trim21.beancount-lsp

## Install the language server (PyPI)

Install into your preferred Python environment (a workspace `.venv` is recommended so the extension can auto-detect it):

```bash
# Standard pip
pip install beancount-lsp

# Or with uv
uv pip install beancount-lsp
```

Verify it is available:

```bash
beancount-lsp --version
```

## Configure the extension

Set these in VS Code Settings or `settings.json`:

- `beancount.root_file` (required): path to your main Beancount file, e.g.

```json
"beancount.root_file": "ledger.beancount"
```

- `beancount.server_path` (optional): full path to the `beancount-lsp` executable. If omitted, the extension searches in order: `beancount.server_path`, your workspace `.venv` (`.venv/bin/beancount-lsp`), then your system `PATH`.

```json
"beancount.server_path": "/home/user/.local/bin/beancount-lsp"
```

- `beancount.log_level` (optional): `trace`, `debug`, `info`, `warn`, `error`, `off` (default: `info`).

## Commands

Commands (Command Palette → type “Beancount”):
- Beancount: Start Language Server
- Beancount: Stop Language Server
- Beancount: Restart Language Server

## Troubleshooting

- View logs in VS Code: `View` → `Output`, then pick **beancount-language-server (server)** from the dropdown.
- If the server does not start, check that `beancount-lsp` runs in a terminal (`beancount-lsp --version`).
- If you installed into a virtualenv, either open the workspace containing that `.venv` (auto-detected) or set `beancount.server_path` explicitly.
- Ensure `beancount.root_file` is set; the server will not start without it.
- Increase verbosity with `beancount.log_level` set to `debug` or `trace`.

## Link

- Repository: https://github.com/trim21/beancount-lsp

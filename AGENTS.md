# Beancount Language Server

This is a **beancount language server** implementation written in Rust that provides LSP (Language Server Protocol) support for Beancount files (.bean, .beancount). Beancount is a double-entry bookkeeping system that uses plain text files.

## Gotcha

The lsp specifies text positions (line, character) using UTF-16 code units, but beancount source files are UTF-8. so when converting position, you should always do a multiple bytes aware converting, with some utils.

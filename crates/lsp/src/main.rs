use std::env;

use tower_lsp_server::jsonrpc::Result;

fn main() -> Result<()> {
  let argv: Vec<String> = env::args().skip(1).collect();
  beancount_lsp::run_server_blocking(argv)
}

use std::hint::black_box;
use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;

use criterion::{Criterion, criterion_group, criterion_main};

fn generate_sample() -> &'static str {
  static SAMPLE: OnceLock<String> = OnceLock::new();
  SAMPLE.get_or_init(|| match Command::new("bean-example").output() {
    Ok(output) => {
      if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("bean-example failed, using fallback sample: {stderr}");
        return fallback_sample();
      }
      String::from_utf8(output.stdout).unwrap_or_else(|_| fallback_sample())
    }
    Err(err) => {
      eprintln!("bean-example not available ({err}), using fallback sample");
      fallback_sample()
    }
  })
}

fn fallback_sample() -> String {
  "2026-02-02 open Assets:Cash\n2026-02-02 open Expenses:Food\n\
2026-02-02 * \"Payee\" \"Narration\"\n  Assets:Cash -10 USD\n  Expenses:Food 10 USD\n"
    .to_owned()
}

fn bench_semantic_tokens_full(c: &mut Criterion) {
  static PATH: OnceLock<PathBuf> = OnceLock::new();
  let sample = generate_sample();
  let path = PATH.get_or_init(|| PathBuf::from("semantic_tokens.bean"));
  let doc = beancount_lsp::build_semantic_tokens_bench_doc(sample, path)
    .expect("parse sample doc for semantic tokens bench");

  c.bench_function("semantic_tokens_full", |b| {
    b.iter(|| {
      let tokens = doc.semantic_tokens_full();
      black_box(tokens)
    })
  });
}

criterion_group!(benches, bench_semantic_tokens_full);
criterion_main!(benches);

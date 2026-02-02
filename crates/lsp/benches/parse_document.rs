use std::process::Command;
use std::path::PathBuf;
use std::sync::OnceLock;

use criterion::{criterion_group, criterion_main, Criterion, BatchSize, black_box};
use tower_lsp_server::ls_types::Uri as Url;

fn generate_sample() -> &'static str {
    static SAMPLE: OnceLock<String> = OnceLock::new();
    SAMPLE.get_or_init(|| {
        match Command::new("bean-example").output() {
            Ok(output) => {
                if !output.status.success() {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    eprintln!("bean-example failed, using fallback sample: {stderr}");
                    return fallback_sample();
                }
                String::from_utf8(output.stdout)
                    .unwrap_or_else(|_| fallback_sample())
            }
            Err(err) => {
                eprintln!("bean-example not available ({err}), using fallback sample");
                fallback_sample()
            }
        }
    })
}

fn fallback_sample() -> String {
    "2026-02-02 open Assets:Cash\n2026-02-02 open Expenses:Food\n\
2026-02-02 * \"Payee\" \"Narration\"\n  Assets:Cash -10 USD\n  Expenses:Food 10 USD\n"
        .to_owned()
}

fn generate_sample_tree() -> &'static BenchTree {
    static TREE: OnceLock<BenchTree> = OnceLock::new();
    TREE.get_or_init(|| build_tree(generate_sample()))
}

struct BenchTree {
    _dir: tempfile::TempDir,
    root: PathBuf,
    update_uri: Url,
    update_content: String,
}

fn build_tree(sample: &str) -> BenchTree {
    let dir = tempfile::tempdir().expect("create temp dir");
    let root = dir.path().join("main.bean");
    let nested_dir = dir.path().join("nested");
    std::fs::create_dir_all(&nested_dir).expect("create nested dir");

    let mut include_lines = Vec::new();
    let mut update_uri = None;

    for i in 0..25 {
        let filename = dir.path().join(format!("inc_{i}.bean"));
        std::fs::write(&filename, &sample).expect("write include file");
        include_lines.push(format!("include \"{}\"", filename.file_name().unwrap().to_string_lossy()));
        if i == 0 {
            update_uri = Url::from_file_path(&filename);
        }
    }

    let nested = nested_dir.join("nested.bean");
    std::fs::write(&nested, &sample).expect("write nested file");
    include_lines.push("include \"nested/nested.bean\"".to_string());

    let root_content = format!("{}\n{}", include_lines.join("\n"), sample);
    std::fs::write(&root, root_content).expect("write root file");

    let update_content = format!("{sample}\n; updated\n");
    let update_uri = update_uri.expect("update uri");

    BenchTree {
        _dir: dir,
        root,
        update_uri,
        update_content,
    }
}

fn bench_parse_document(c: &mut Criterion) {
    let sample = generate_sample();
    let temp_dir = tempfile::tempdir().expect("create temp dir");
    let filename = temp_dir.path().join("sample.bean");

    c.bench_function("parse_document", |b| {
        b.iter(|| {
            beancount_lsp::parse_document_for_bench(black_box(&sample), &filename);
        })
    });
}

fn bench_load_tree(c: &mut Criterion) {
    let tree = generate_sample_tree();
    c.bench_function("load_document_tree", |b| {
        b.iter(|| {
            let _indexer = beancount_lsp::Indexer::from_journal(&tree.root).unwrap();
        })
    });
}

fn bench_update_with_refcount(c: &mut Criterion) {
    let tree = generate_sample_tree();
    c.bench_function("update_document_with_refcount", |b| {
        b.iter_batched(
            || beancount_lsp::Indexer::from_journal(&tree.root).unwrap(),
            |mut indexer| {
                indexer.update_document(tree.update_uri.clone(), tree.update_content.clone());
            },
            BatchSize::SmallInput,
        )
    });
}

fn bench_clone_documents(c: &mut Criterion) {
    let tree = generate_sample_tree();
    let indexer = beancount_lsp::Indexer::from_journal(&tree.root).unwrap();
    assert!(!indexer.documents().is_empty(), "expected non-empty documents");
    c.bench_function("clone_documents", |b| {
        b.iter(|| {
            black_box(indexer.documents().clone());
        })
    });
}

criterion_group!(
    benches,
    bench_parse_document,
    bench_load_tree,
    bench_update_with_refcount,
    bench_clone_documents
);
criterion_main!(benches);

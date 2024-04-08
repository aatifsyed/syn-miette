#!/usr/bin/env bash
set -euxo pipefail

cargo test --doc
cargo check-all-features
cargo build-all-features
cargo test-all-features --lib --bins --tests # not doctests
cargo msrv verify -- cargo test

RUSTDOCFLAGS="--cfg do_doc_cfg" cargo +nightly doc --all-features
lychee target/doc/syn_miette/index.html

# cargo rdme --check

EXECUTABLE = run-rust.sh

.PHONY: build
build:
	export CARGO_TARGET_DIR="$$(pwd)" && cd ../../src/rust && cargo build && cargo build --release
	date > compile-stamp

include ../../benchmark.mk

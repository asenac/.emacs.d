#!/bin/sh

export CARGO_TARGET_DIR="$(cargo metadata --format-version=1 | jq -r .target_directory)/rust-analyzer"
exec rust-analyzer "$@"

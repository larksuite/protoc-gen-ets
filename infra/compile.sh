#!/usr/bin/env bash

set -o pipefail -o errexit

wasm-pack build --out-dir js/plugin/dist --target nodejs --no-pack --out-name plugin

# npm install -g @napi-rs/cli

# cd js/plugin

# mkdir -p dist
# cd dist
# napi build --release --platform --cargo-cwd ../..
# cd ..
# npm publish

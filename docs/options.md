# Options

protoc-gen-ts supports various options to the control output.

## Recognized options

* With `--ets_opt=namespace_as_file=true`, the generated code will split to one main file per message on root and structured in tree directory as its namespace.

* With `--ets_opt=bigint_as_long=true`, all `int64`, `uint64` in generated code will be type `Long` in [long](https://www.npmjs.com/package/long). For now pre-defined interface in `@lark/protobuf-types` only support `Long` and configure `@ohos/protobufjs` as it.

* With `--ets_opt=export_indices=true`, all level of namespace-tree directory will export its declared message in `index.ets`.

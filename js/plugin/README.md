# Protoc Gen ArkTS

> !NOTE
> Forked from [protoc-gen-ts](https://github.com/thesayyn/protoc-gen-ts)

Compile `.proto` files to plain `ArkTS`. Supports concurrent with `@Sendable` on codec classes.

Run with [`@ohos/protobufjs`](https://ohpm.openharmony.cn/#/cn/detail/@ohos%2Fprotobufjs)

## Features

- Passes all required conformance tests
- **Supports ArkTS(ets)** with `@Sendable` for concurrent
- Supports well-known types
- ~~Supports json encoding (`toJson`, `fromJson`)~~
- Supports binary encoding (`toBinary`, `fromBinary`)
- Optimized for [de]serialization speed.

## Usage

```sh
npm install -g @larkoh/protoc-gen-ets
```

### Protoc

```properties
protoc -I=sourcedir --ets_out=dist myproto.proto --ets_opt="namespace_as_file=true,export_indcies=true,bigint_as_long=true"
```

## Development

```sh
./infra/test.sh
```

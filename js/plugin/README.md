# Protoc Gen Typescript

Compile `.proto` files to plain TypeScript. Supports gRPC Node and gRPC Web.

> As of 2024, this project has adopted Rust as its primary programming language, replacing JavaScript.
> See [the issue](https://github.com/thesayyn/protoc-gen-ts/issues/255) for the details.

## Contributing

I have limited availability to consistently maintain this project, as my time is primarily allocated to cutting new releases and implementing fixes on an ad hoc basis.

[See issues that needs help](https://github.com/thesayyn/protoc-gen-ts/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22)

[See issues for newcomers](https://github.com/thesayyn/protoc-gen-ts/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)

Become a maintainer? Send an [email](mailto:thesayyn@gmail.com?subject=Become%20a%20maintainer)

## Features

- Passes all required conformance tests
- Supports well-known types
- Supports [gRPC](docs/rpc.md) (`@grpc/grpc-js`)
- Supports [gRPC Web](docs/rpc.md) (`grpc-web`)
- Supports json encoding (`toJson`, `fromJson`)
- Supports binary encoding (`toBinary`, `fromBinary`)
- Supports ArkTS(ets), for more: [Lark Harmony Pb Custom]()
- Optimized for [de]serialization speed.

## Usage

```sh
npm install -g @larkoh/protoc-gen-ets
```

### Protoc

```properties
protoc -I=sourcedir --ts_out=dist myproto.proto
protoc --ts_out=./ohos/ --proto_path=$(PB_INPUT_DIR) \
--ts_opt="runtime_package=@ohos/protobufjs,import_style=named,generate_type=ets,namespace_as_file=true,bigint_as_long=true"
```

### Buf

```yaml
version: v1
plugins:
  - name: ts
    path: ./node_modules/.bin/protoc-gen-ts
    out: ./dist
```

### Example

```proto
syntax = "proto3";

enum Role {
    ADMIN = 0;
    MOD = 1;
}

message Author {
    Role role = 2;
    oneof id_or_name {
        string id = 4;
        string name = 5;
    }
}
```

```typescript
const author = Author.fromJson({
    role: Kind.ADMIN,
    name: "mary poppins",
});

// Serialize to binary
const bytes: Uint8Array = author.toBinary();

// Deserialize from binary
const received: Change = Change.fromBinary(bytes);

console.log(received.toJson())
```

## Development

```sh
./infra/test.sh
```

## Contributors

![GitHub Contributors Image](https://contrib.rocks/image?repo=thesayyn/protoc-gen-ts)

## Support

We need your constant support to keep protoc-gen-ts well maintained and add new features.

If your corporate has a OSS funding scheme, please consider supporting us monthly through open collective.

<a href="https://opencollective.com/protoc-gen-ts">
<img height="100px" src="https://opencollective.com/protoc-gen-ts/tiers/backer.svg?avatarHeight=36">
</a>

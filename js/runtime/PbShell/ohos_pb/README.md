# @lark/ohos_pb

## 简介

[protoc-gen-ets](https://github.com/larksuite/protoc-gen-ets) 生成代码的基础库，仅具有以下几个接口声明

- `Encoder`：编码器
- `Decoder`：解码器
- `Creator`：构造器
- `Meta`：元信息，包括以上三个类型的实例

更多使用信息，参看 [protoc-gen-ets](https://github.com/larksuite/protoc-gen-ets)

## 安装

在你的工程中使用

```shell
ohpm install @lark/ohos_pb
```

来安装此依赖

## 依赖，当前工程依赖

- `@ohos/protobufjs`: 2.0.0
- `long`: 5
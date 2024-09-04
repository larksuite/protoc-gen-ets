/**
 * Runtime declarations for protobuf generated code
 * Copyright (2024) Bytedance
 **/
import * as $protobuf from "@ohos/protobufjs";
import Long from 'long';

$protobuf.util.Long = Long;
$protobuf.configure();

export interface Encoder<T> {
  encode($req: T, $w?: $protobuf.Writer): $protobuf.Writer
}

export interface Decoder<T> {
  decode($data: ($protobuf.Reader | Uint8Array), $l?: number): T
}

export interface Creator<I, T extends I> {
  create(obj?: I): T;
}

export interface Meta<I, T extends I> {
  encoder: Encoder<I>;
  decoder: Decoder<I>;
  creator: Creator<I, T>;
}

use std::io::prelude::*;
use std::io::*;

pub mod runtime;

use crate::compile::compile;
use protoc_gen_ets::*;

fn main() {
    let mut buffer: Vec<u8> = Vec::new();
    stdin()
        .read_to_end(&mut buffer)
        .expect("expected data in stdin");

    let bytes = compile(buffer);

    stdout().write_all(&bytes).unwrap();
}

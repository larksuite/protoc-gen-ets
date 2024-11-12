include!(concat!(env!("OUT_DIR"), "/protogen/mod.rs"));

pub mod collector;
pub mod common;
pub mod compile;
pub mod context;
pub mod emit;
pub mod export;
pub mod options;
pub mod print;
pub mod runtime;
#[macro_use]
pub mod macros;

#[cfg(feature = "wasm-build")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "wasm-build")]
#[wasm_bindgen]
pub fn run(bytes: Vec<u8>) -> Vec<u8> {
    console_error_panic_hook::set_once();
    compile::compile(bytes)
}

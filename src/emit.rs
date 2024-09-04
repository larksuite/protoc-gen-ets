use swc_common::FilePathMapping;
use swc_common::{source_map::SourceMap, sync::Lrc, DUMMY_SP};
use swc_ecma_ast::*;
use swc_ecma_codegen::Config;
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};

pub fn emit(body: Vec<ModuleItem>) -> String {
    let module = Module {
        span: DUMMY_SP,
        body,
        shebang: None,
    };

    let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
    let mut buf = vec![];
    let mut emitter = Emitter {
        cm: cm.clone(),
        comments: None,
        wr: JsWriter::new(cm, "\n", &mut buf, None),
        cfg: Config::default()
            .with_target(EsVersion::Es2022)
            .with_omit_last_semi(true),
    };

    emitter.emit_module(&module).unwrap();

    String::from_utf8_lossy(&buf).to_string()
}

use std::{
    str::FromStr,
    sync::{Arc, Mutex},
    thread,
};

use protobuf::Message;

use crate::{
    collector::Collector,
    context::{Context, Syntax},
    emit::emit,
    options::Options,
    plugin::{code_generator_response::File, CodeGeneratorRequest, CodeGeneratorResponse},
    print::Print,
    runtime::{lark::protobuf_js::LarkPbRuntime, Runtime},
};

pub fn compile(buffer: Vec<u8>) -> Vec<u8> {
    let request = CodeGeneratorRequest::parse_from_bytes(&buffer).unwrap();
    let options = Options::parse(request.parameter());
    let mut ctx = Context::new(&options, &crate::context::Syntax::Unspecified);
    let response = compile_impl(request, &mut ctx, LarkPbRuntime::new());

    response.write_to_bytes().unwrap()
}

pub(crate) fn compile_impl<R: Runtime + Sized + Clone + Send + Sync>(
    request: CodeGeneratorRequest,
    ctx: &mut Context,
    runtime: R,
) -> CodeGeneratorResponse {
    request.collect_to(ctx);

    let outputs = Arc::new(Mutex::new(vec![]));
    let ext = ctx.get_generated_extension();

    thread::scope(|_s| {
        for file_descripor in request.proto_file {
            if !request
                .file_to_generate
                .contains(&file_descripor.name().to_string())
            {
                continue;
            }

            let ctx = ctx.clone();
            let runtime = runtime.clone();
            let outputs = outputs.clone();

            let closure = move || {
                let syntax = Syntax::from_str(file_descripor.syntax()).expect("unknown syntax");

                let namespace = file_descripor.package().replace('.', "/");
                let package = file_descripor.package();
                for r#enum in &file_descripor.enum_type {
                    let path_name = format!("{namespace}/{}.{ext}", r#enum.name());
                    let mut ty_ctx = ctx.fork(path_name.clone(), &syntax, false);
                    let body = r#enum.print(&mut ty_ctx, &runtime);
                    let ts = emit(ty_ctx.drain_imports().into_iter().chain(body).collect());
                    outputs.lock().unwrap().push(new_file(path_name, ts));
                }

                for message in &file_descripor.message_type {
                    let path_name = format!("{namespace}/{}.{ext}", message.name());
                    let mut ty_ctx = ctx
                        .fork(path_name.clone(), &syntax, false)
                        .descend(package.to_string())
                        .descend(message.name().to_string());
                    // import seperated codec file
                    let _ = ty_ctx.get_import(
                        &format!("./{}+codec", message.name()),
                        Some(&format!("${}", message.name())),
                    );
                    let body = message.print(&mut ty_ctx, &runtime, &mut vec![]);
                    let ts = emit(ty_ctx.drain_imports().into_iter().chain(body).collect());
                    outputs.lock().unwrap().push(new_file(path_name, ts));

                    let codec_path_name = format!("{namespace}/{}+codec.{ext}", message.name());
                    let mut codec_ctx = ctx
                        .fork(codec_path_name.clone(), &syntax, true)
                        .descend(package.to_string())
                        .descend(message.name().to_string());
                    let _ = codec_ctx.get_import_from(
                        &format!("./{}", message.name()),
                        &format!("I{}", message.name()),
                    );
                    let _ =
                        codec_ctx.get_import_from(&format!("./{}", message.name()), message.name());
                    let mut ns = package.split('.').map(ToString::to_string).collect();
                    let body = message.print_codec(&mut codec_ctx, &runtime, &mut ns);
                    let ts = emit(codec_ctx.drain_imports().into_iter().chain(body).collect());
                    outputs.lock().unwrap().push(new_file(codec_path_name, ts));
                }
            };
            #[cfg(not(target_family = "wasm"))]
            _s.spawn(closure);

            #[cfg(target_family = "wasm")]
            closure();
        }

        if ctx.options.export_indices {
            let indices = ctx.get_exported_indices();
            for index in indices {
                let mut ctx = ctx.clone();
                let ts = emit(runtime.print_index(&mut ctx, &index));
                let mut file = File::new();
                file.set_name(format!("{}/index.{ext}", index.path.replace('.', "/")));
                file.set_content(ts);
                outputs.lock().unwrap().push(file);
            }
        }
    });

    let mut resp = CodeGeneratorResponse::new();
    resp.file = outputs.lock().unwrap().to_vec();
    resp
}

pub(crate) fn new_file(path_name: String, body: String) -> File {
    let mut file = File::new();
    file.set_name(path_name);
    file.set_content(body);
    file
}

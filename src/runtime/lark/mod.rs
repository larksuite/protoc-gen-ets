pub mod protobuf_js;

#[cfg(test)]
mod test {
    use crate::plugin::CodeGeneratorRequest;

    use protobuf::Message;
    use protobuf::SpecialFields;
    use protobuf_parse::{pure::parse_and_typecheck::parse_and_typecheck, Parser};

    #[test]
    fn generate_lark_example() {
        let options_str = "runtime_package=@ohos/protobufjs,import_style=named,generate_type=ets,namespace_as_file=true,bigint_as_long=true";
        let files: Vec<String> = vec!["../examples/protobufjs/example.proto".to_string()];
        let proto_file: Vec<_> = files
            .iter()
            .flat_map(|f: &String| {
                parse_and_typecheck(&Parser::default())
                    .unwrap()
                    .file_descriptors
            })
            .map(|desc| desc.write_to_bytes().unwrap())
            .map(|bytes| crate::descriptor::FileDescriptorProto::parse_from_bytes(&bytes).unwrap())
            .collect();
        let request = CodeGeneratorRequest {
            file_to_generate: files,
            parameter: Some(options_str.to_string()),
            proto_file,
            compiler_version: protobuf::MessageField(None),
            special_fields: SpecialFields::default(),
        };

        crate::compile::compile(request.write_to_bytes().unwrap());
    }
}

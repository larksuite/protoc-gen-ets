use core::panic;
use std::string::String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportStyle {
    Counter,
    Named,
}

#[derive(Clone, Debug)]
pub struct Options {
    pub runtime_package: String,
    pub base64_package: String,
    pub namespace_as_file: bool,
    pub import_style: ImportStyle,
    pub bigint_as_long: bool,
    pub export_indices: bool,
}

impl Options {
    pub fn parse(raw: &str) -> Options {
        let mut runtime_package = "@ohos/protobufjs";
        let mut base64_package = "js-base64";
        let mut import_style = ImportStyle::Named;
        let mut bigint_as_long: Option<bool> = Some(true);
        let mut namespace_as_file: Option<bool> = None;
        let mut export_indices = true;

        let parts = raw.split(",");

        for part in parts {
            let mut kv = part.trim().split("=");
            let key = kv.next();
            if key.is_none() {
                panic!("option key can not be empty.")
            }
            match key.unwrap() {
                "runtime_package" => {
                    runtime_package = kv.next().expect("expected a value for runtime_package")
                }
                "base64_package" => {
                    base64_package = kv.next().expect("expected a value for base64_package")
                }
                "import_style" => {
                    let ty = kv.next().expect("expected a value for import_style");
                    import_style = match ty.to_lowercase().as_str() {
                        "counter" => ImportStyle::Counter,
                        "named" | "name" => ImportStyle::Named,
                        _ => panic!("ERROR: unknown import_style: {}", ty),
                    }
                }
                "bigint_as_long" => {
                    let value = kv.next().expect("expected a value for bigint_as_long");
                    bigint_as_long = match value {
                        "true" => Some(true),
                        "false" => Some(false),
                        _ => {
                            panic!("ERROR: unknown bigint_as_long: {}", value);
                        }
                    }
                }
                "namespace_as_file" => {
                    namespace_as_file =
                        Some(kv.next().expect("expected a value for namespace_as_file") == "true")
                }
                "export_indices" => {
                    let value = kv
                        .next()
                        .expect("expected a value for export_indices")
                        .to_lowercase();
                    export_indices = match value.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => {
                            panic!("ERROR: unknown export_indices: {}", value);
                        }
                    }
                }
                // just silently ignore
                option => {
                    eprintln!("WARNING: unknown option {}", option)
                }
            };
        }

        Options {
            runtime_package: runtime_package.to_string(),
            base64_package: base64_package.to_string(),
            namespace_as_file: namespace_as_file.unwrap_or_default(),
            import_style,
            bigint_as_long: bigint_as_long.unwrap_or_default(),
            export_indices,
        }
    }
}

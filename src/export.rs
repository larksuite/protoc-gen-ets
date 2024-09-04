use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    ExportAll, ExportNamespaceSpecifier, ExportSpecifier, ModuleDecl, ModuleExportName, ModuleItem,
    NamedExport,
};
use swc_ecma_utils::quote_ident;

#[derive(Clone, Debug)]
pub enum ExportItem {
    File(String),
    Module(String),
}

#[derive(Debug)]
pub struct ExportedIndex {
    pub path: String,
    pub export_items: Vec<ExportItem>,
}

impl ExportedIndex {
    pub fn print(&self) -> Vec<ModuleItem> {
        self.export_items
            .iter()
            .map(|item| match item {
                ExportItem::File(file_name) => {
                    ModuleItem::ModuleDecl(ModuleDecl::ExportAll(ExportAll {
                        span: DUMMY_SP,
                        src: Box::new(swc_ecma_ast::Str {
                            span: DUMMY_SP,
                            value: format!("./{file_name}").into(),
                            raw: None,
                        }),
                        type_only: false,
                        with: None,
                    }))
                }

                ExportItem::Module(name) => {
                    ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(NamedExport {
                        span: DUMMY_SP,
                        specifiers: vec![ExportSpecifier::Namespace(ExportNamespaceSpecifier {
                            span: DUMMY_SP,
                            name: ModuleExportName::Ident(quote_ident!(name.clone())),
                        })],
                        src: Some(Box::new(swc_ecma_ast::Str {
                            span: DUMMY_SP,
                            value: format!("./{name}").into(),
                            raw: None,
                        })),
                        type_only: false,
                        with: None,
                    }))
                }
            })
            .collect()
    }
}

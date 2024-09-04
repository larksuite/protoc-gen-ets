use std::cmp::Ordering;

use swc_ecma_ast::{ImportDecl, ImportSpecifier, ModuleDecl, ModuleItem};

pub fn module_item_comparator(a: &ModuleItem, b: &ModuleItem) -> Ordering {
    match (a, b) {
        (ModuleItem::ModuleDecl(a_decl), ModuleItem::ModuleDecl(b_decl)) => {
            match (a_decl, b_decl) {
                (ModuleDecl::Import(a_import), ModuleDecl::Import(b_import)) => {
                    import_decl_comparator(a_import, b_import)
                }
                (ModuleDecl::Import(_), _) => std::cmp::Ordering::Greater,
                (_, ModuleDecl::Import(_)) => std::cmp::Ordering::Less,
                _ => std::cmp::Ordering::Equal,
            }
        }
        (ModuleItem::ModuleDecl(_), ModuleItem::Stmt(_)) => std::cmp::Ordering::Greater,
        (ModuleItem::Stmt(_), ModuleItem::ModuleDecl(_)) => std::cmp::Ordering::Less,
        (ModuleItem::Stmt(_), ModuleItem::Stmt(_)) => std::cmp::Ordering::Equal,
    }
    .reverse()
}

pub fn import_decl_comparator(a: &ImportDecl, b: &ImportDecl) -> Ordering {
    a.is_default()
        .cmp(&b.is_default())
        .then_with(|| a.is_namespace().cmp(&b.is_namespace()))
        .then_with(|| a.src.as_ref().value.cmp(&b.src.as_ref().value))
}

trait ImportDeclExt {
    fn is_namespace(&self) -> bool;

    fn is_default(&self) -> bool;

    fn is_named(&self) -> bool;
}

impl ImportDeclExt for ImportDecl {
    fn is_namespace(&self) -> bool {
        self.specifiers
            .iter()
            .any(|s| matches!(s, ImportSpecifier::Namespace(_)))
    }

    fn is_default(&self) -> bool {
        self.specifiers
            .iter()
            .any(|s| matches!(s, ImportSpecifier::Default(_)))
    }

    fn is_named(&self) -> bool {
        self.specifiers
            .iter()
            .any(|s| matches!(s, ImportSpecifier::Named(_)))
    }
}

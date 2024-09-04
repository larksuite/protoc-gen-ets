use crate::{
    common, descriptor,
    export::{ExportItem, ExportedIndex},
    options::Options,
};
use dashmap::DashMap;
use pathdiff::diff_paths;
use std::{
    collections::{HashMap, HashSet},
    iter::once,
    path::PathBuf,
    str::FromStr,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, Mutex,
    },
};
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    Ident, ImportDecl, ImportDefaultSpecifier, ImportPhase, ImportSpecifier, ImportStarAsSpecifier,
    ModuleDecl, ModuleItem, Str,
};
use swc_ecma_utils::quote_ident;

macro_rules! descend_if_necessary {
    ($ctx:ident, $self:ident) => {
        match $self.has_package() {
            true => $ctx.descend($self.package().to_string()),
            false => $ctx.clone(),
        }
    };
}

pub(crate) use descend_if_necessary;

use self::util::module_item_comparator;

mod util;

#[derive(PartialEq, Debug)]
pub enum Syntax {
    Proto3,
    Proto2,
    Unspecified,
}

impl FromStr for Syntax {
    type Err = ();
    fn from_str(input: &str) -> Result<Syntax, Self::Err> {
        match input {
            "proto3" => Ok(Syntax::Proto3),
            "proto2" | "" => Ok(Syntax::Proto2),
            _ => Err(()),
        }
    }
}

fn resolve_relative(from: PathBuf, to: PathBuf) -> PathBuf {
    let common_root = diff_paths(
        from.parent().unwrap_or(PathBuf::from("").as_path()),
        to.parent().unwrap_or(PathBuf::from("").as_path()),
    );

    let root: PathBuf = match common_root {
        None => PathBuf::from("./"),
        Some(cr) if cr.as_os_str().is_empty() => PathBuf::from("./"),
        Some(cr) if !cr.starts_with(".") => PathBuf::from(format!("./{}", cr.display())),
        Some(cr) => cr,
    };

    root.join(from.file_name().expect("expected path to have filename"))
}

#[derive(Clone, Copy)]
pub enum PropPos {
    Definition,
    Interface,
}

pub struct Context<'a> {
    pub options: &'a Options,
    pub syntax: &'a Syntax,
    namespace: Vec<String>,
    name: String,
    counter: Arc<AtomicU64>,
    imports: Arc<Mutex<Vec<ImportDecl>>>,
    import_identifier_map: Arc<DashMap<String, u64>>,
    type_reg: Arc<DashMap<String, String>>,
    map_type_reg: Arc<DashMap<String, descriptor::DescriptorProto>>,
    leading_enum_member_reg: Arc<DashMap<String, i32>>,
    export_reg: Arc<DashMap<String, Vec<ExportItem>>>,
    codec: bool,
}

impl<'a> Clone for Context<'a> {
    fn clone(&self) -> Self {
        Self {
            options: self.options,
            syntax: self.syntax,
            namespace: self.namespace.clone(),
            name: self.name.clone(),
            counter: self.counter.clone(),
            imports: Arc::new(Mutex::new(Vec::new())),
            import_identifier_map: Arc::new(DashMap::new()),
            type_reg: Arc::clone(&self.type_reg),
            map_type_reg: Arc::clone(&self.map_type_reg),
            leading_enum_member_reg: Arc::clone(&self.leading_enum_member_reg),
            export_reg: Arc::clone(&self.export_reg),
            codec: self.codec,
        }
    }
}

macro_rules! impl_name {
    ($name: expr, $source: expr, $counter: expr, $ctx: ident) => {
        if matches!(
            $ctx.options.import_style,
            $crate::options::ImportStyle::Named
        ) {
            let name = $name.map(|s| s.to_string()).unwrap_or_else(|| {
                $source
                    .split("/")
                    .filter_map(|s| if s == "." || s == ".." { None } else { Some(s) })
                    .collect::<Vec<_>>()
                    .join("_")
            });
            format!("{}", name)
        } else {
            format!("$imp_{}", $counter)
        }
    };
}

impl<'a> Context<'a> {
    pub fn new(options: &'a Options, syntax: &'a Syntax) -> Self {
        Self {
            counter: Arc::new(AtomicU64::new(0)),
            options,
            syntax,
            namespace: vec![],
            name: String::new(),
            imports: Arc::new(Mutex::new(Vec::new())),
            import_identifier_map: Arc::new(DashMap::new()),
            type_reg: Arc::new(DashMap::new()),
            map_type_reg: Arc::new(DashMap::new()),
            leading_enum_member_reg: Arc::new(DashMap::new()),
            export_reg: Arc::new(DashMap::new()),
            codec: false,
        }
    }

    pub fn fork(&self, name: String, syntax: &'a Syntax, codec: bool) -> Self {
        let mut copy = self.clone();
        copy.name = name;
        copy.syntax = syntax;
        copy.codec = codec;
        copy
    }

    pub fn is_codec(&self) -> bool {
        self.codec
    }

    pub fn descend(&self, ns: String) -> Self {
        let mut namespace = self.namespace.clone();
        namespace.push(ns);

        Self {
            options: self.options,
            syntax: self.syntax,
            namespace,
            name: self.name.clone(),
            counter: self.counter.clone(),
            import_identifier_map: self.import_identifier_map.clone(),
            imports: self.imports.clone(),
            type_reg: self.type_reg.clone(),
            map_type_reg: self.map_type_reg.clone(),
            leading_enum_member_reg: self.leading_enum_member_reg.clone(),
            export_reg: self.export_reg.clone(),
            codec: self.codec,
        }
    }

    pub fn get_namespace(&self) -> String {
        self.namespace.clone().join(".")
    }

    pub fn drain_imports(&mut self) -> Vec<ModuleItem> {
        let mut imps = vec![];
        let mut imports = self.imports.lock().unwrap();
        let mut merged_imports: HashMap<_, HashSet<_>> = HashMap::new();
        for import in imports.to_vec() {
            for spec in import.specifiers.clone() {
                match spec {
                    ImportSpecifier::Named(_) => {
                        let source = merged_imports.entry(import.src.clone()).or_default();
                        source.insert(spec);
                    }
                    t => imps.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                        span: DUMMY_SP,
                        specifiers: vec![t],
                        src: import.src.clone(),
                        type_only: false,
                        with: None,
                        phase: ImportPhase::Evaluation,
                    }))),
                }
            }
        }
        for (merged_import, spec) in merged_imports.into_iter() {
            imps.push(ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
                span: DUMMY_SP,
                specifiers: spec.into_iter().collect(),
                src: merged_import.clone(),
                type_only: false,
                with: None,
                phase: ImportPhase::Evaluation,
            })));
        }
        imports.clear();
        imps.sort_by(module_item_comparator);
        imps
    }

    pub fn get_import(&self, source: &str, name: Option<&str>) -> Ident {
        let cached_counter = self.import_identifier_map.get(&String::from(source));

        if let Some(counter) = cached_counter {
            return quote_ident!(impl_name!(name, source, *counter, self));
        }

        let counter = self.counter.fetch_add(1, Ordering::Relaxed);
        let name = quote_ident!(impl_name!(name, source, counter, self));

        let decl = ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Namespace(ImportStarAsSpecifier {
                local: name.clone(),
                span: DUMMY_SP,
            })],
            src: Box::new(Str {
                span: DUMMY_SP,
                raw: None,
                value: source.into(),
            }),
            type_only: false,
            with: None,
            phase: ImportPhase::Evaluation,
        };
        self.imports.lock().unwrap().push(decl);
        self.import_identifier_map
            .insert(String::from(source), counter);

        name
    }

    pub fn get_import_from(&self, source: &str, item: &str) -> Ident {
        let key = format!("{source}_{item}");
        let cached_counter = self.import_identifier_map.get(&key);

        if cached_counter.is_some() {
            return quote_ident!(item);
        }
        let counter = self.counter.fetch_add(1, Ordering::Relaxed);

        let decl = ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Named(swc_ecma_ast::ImportNamedSpecifier {
                span: DUMMY_SP,
                local: quote_ident!(item),
                imported: None,
                is_type_only: false,
            })],
            src: Box::new(source.into()),
            type_only: false,
            with: None,
            phase: ImportPhase::Evaluation,
        };
        self.imports.lock().unwrap().push(decl);
        self.import_identifier_map.insert(key, counter);
        quote_ident!(item)
    }

    pub fn get_default_import(&self, source: &str, default_item: &str) -> Ident {
        let cached_counter = self.import_identifier_map.get(source);

        if cached_counter.is_some() {
            return quote_ident!(default_item);
        }
        let counter = self.counter.fetch_add(1, Ordering::Relaxed);

        let decl = ImportDecl {
            span: DUMMY_SP,
            specifiers: vec![ImportSpecifier::Default(ImportDefaultSpecifier {
                span: DUMMY_SP,
                local: quote_ident!(default_item),
            })],
            src: Box::new(source.into()),
            type_only: false,
            with: None,
            phase: ImportPhase::Evaluation,
        };
        self.imports.lock().unwrap().push(decl);
        self.import_identifier_map
            .insert(source.to_string(), counter);
        quote_ident!(default_item)
    }

    pub fn wrap_if_needed(&mut self, modules: Vec<ModuleItem>) -> Vec<ModuleItem> {
        if self.options.namespace_as_file || self.namespace.is_empty() {
            return modules;
        }
        let last_item = self.namespace.last().unwrap().clone();
        let name = last_item.as_str();
        vec![common::util::wrap(name, modules)]
    }

    pub fn normalize_type_name(&self, name: &str) -> String {
        let name = name.strip_prefix(".").unwrap_or(name);
        name.to_string()
    }

    pub fn normalize_name(&self, name: &str) -> String {
        let mut ns = vec![];
        ns.extend(self.namespace.clone());
        ns.push(name.to_string());
        ns.join(".")
    }

    pub fn find_type_provider(&self, type_name: &String) -> Option<String> {
        if let Some(val) = self.type_reg.get(type_name) {
            return Some(val.clone());
        }
        None
    }

    pub fn find_common_namespace(&self, left: &str, right: &str) -> Vec<String> {
        if left == right {
            return left.split('.').map(ToString::to_string).collect();
        }
        let left_segments = left.split('.');
        let right_segments = right.split('.');
        left_segments
            .zip(right_segments)
            .map_while(|(l, r)| if l == r { Some(r) } else { None })
            .map(ToString::to_string)
            .collect::<Vec<_>>()
    }

    pub fn lazy_type_ref(&self, type_name: &str) -> Ident {
        self.lazy_decl_type_ref(type_name, PropPos::Definition)
    }

    fn decl_type_name(&self, target_names: &[&str], pos: PropPos) -> String {
        match pos {
            PropPos::Definition => target_names.join("."),
            PropPos::Interface => {
                let (name, ns) = target_names.split_last().unwrap();
                ns.iter()
                    .map(ToString::to_string)
                    .chain(once(format!("I{}", name)))
                    .collect::<Vec<_>>()
                    .join(".")
            }
        }
    }

    pub fn lazy_decl_type_ref(&self, type_name: &str, import_pos: PropPos) -> Ident {
        if self.options.namespace_as_file {
            // 1. find the LCA of `type_name` and ctx
            let type_name = type_name.strip_prefix('.').unwrap();
            let namespace: Vec<_> = self
                .get_namespace()
                .split('.')
                .map(ToString::to_string)
                .collect();
            let lca = self.find_common_namespace(&self.get_namespace(), type_name);

            // 2. compute reset part of target `type_name` between LCA
            let target_type_name: Vec<_> = type_name.split('.').collect();
            assert!(
                lca.len() <= target_type_name.len(),
                "LCA({}) >= target_type_name({}), current: {}",
                lca.join("."),
                type_name,
                namespace.join(".")
            );
            if lca.len() == namespace.len() {
                // target is nested or current space
                quote_ident!(self.decl_type_name(&target_type_name[(lca.len() - 1)..], import_pos))
            } else if lca.len() == target_type_name.len()
                && target_type_name.len() < namespace.len()
            {
                quote_ident!(
                    self.decl_type_name(&target_type_name[(lca.len() - 1)..lca.len()], import_pos)
                )
            } else if lca.len() == namespace.len() - 1 && namespace.len() <= target_type_name.len()
            {
                // import target is sibling
                let mut target_import_path = lca.clone();
                while self.is_path_package(&target_import_path) {
                    if target_import_path.len() < target_type_name.len() {
                        target_import_path
                            .push(target_type_name[target_import_path.len()].to_string());
                    } else {
                        break;
                    }
                }
                let import_name = target_import_path.last().unwrap();
                let relative_import_path = &target_import_path[lca.len()..];
                let import_from = format!("./{}", relative_import_path.join("/"));
                let import_name = match import_pos {
                    PropPos::Definition => import_name.to_string(),
                    PropPos::Interface => format!("I{}", import_name),
                };
                let _ = self.get_import_from(&import_from, &import_name);
                let type_name = &target_type_name[target_import_path.len() - 1..];
                quote_ident!(self.decl_type_name(type_name, import_pos))
            } else {
                // target is parent
                let target = if lca.len() < namespace.len() - 1 {
                    // in other namespace or parent, need to import
                    let lca_diff_level = namespace.len() - lca.len() - 1;

                    let mut target_import_path = lca.clone();
                    while self.is_path_package(&target_import_path) || target_import_path.is_empty()
                    {
                        if target_import_path.len() < target_type_name.len() {
                            target_import_path
                                .push(target_type_name[target_import_path.len()].to_string());
                        } else {
                            break;
                        }
                    }
                    let relative_import_path = &target_import_path[lca.len()..];
                    if relative_import_path.is_empty() {
                        dbg!(namespace.join("."), target_type_name.join("."));
                    }
                    let import_name = relative_import_path.last().unwrap();
                    let import_from = std::iter::repeat("..")
                        .take(lca_diff_level)
                        .chain(relative_import_path.iter().map(|s| s.as_str()))
                        .collect::<Vec<_>>()
                        .join("/");
                    let import_name = match import_pos {
                        PropPos::Definition => import_name.to_string(),
                        PropPos::Interface => format!("I{}", import_name),
                    };
                    let _ = self.get_import_from(&import_from, &import_name);
                    &target_type_name[target_import_path.len() - 1..]
                } else {
                    &target_type_name[lca.len()..]
                };
                let target = self.decl_type_name(target, import_pos);
                quote_ident!(target)
            }
        } else {
            let provided_by = self.find_type_provider(&type_name.to_string());
            if let Some(provided_by) = provided_by {
                if self.name == provided_by {
                    let type_name = type_name
                        .strip_prefix('.')
                        .expect("expected type to have leading dot");
                    let common =
                        self.find_common_namespace(self.get_namespace().as_str(), type_name);
                    let type_name = if common.is_empty() {
                        type_name
                    } else {
                        type_name
                            .strip_prefix(&common.join("."))
                            .expect("expected type with same package previx")
                    };
                    let type_name = type_name.strip_prefix('.').unwrap_or(type_name);
                    quote_ident!(type_name)
                } else {
                    let import_from = resolve_relative(
                        provided_by.into(),
                        PathBuf::from_str(&self.name).unwrap(),
                    );
                    let import_from = import_from
                        .to_str()
                        .expect("invalid path conversion")
                        .strip_suffix(".proto")
                        .expect("expected path to have .proto suffix.")
                        .to_string();

                    // import_from.push_str(self.options.import_suffix.as_str());

                    let import_id = self.get_import(import_from.as_str(), None);
                    let type_name = self.normalize_type_name(
                        type_name
                            .strip_prefix(".")
                            .expect("expected type name to have leading dot"),
                    );
                    quote_ident!(format!("{}.{}", import_id.sym.to_string(), type_name))
                }
            } else {
                panic!("no proto provides {}", &type_name)
            }
        }
    }

    pub fn calculate_type_name(&self, type_name: &str) -> String {
        let mut fns = String::from(".");
        if !self.namespace.is_empty() {
            fns.push_str(self.namespace.join(".").as_str());
            fns.push('.');
        }
        fns.push_str(type_name);
        fns
    }

    pub fn register_type_name(&mut self, type_name: &str) {
        let fns = self.calculate_type_name(type_name);
        self.type_reg.insert(fns, self.name.clone());
    }

    pub fn register_map_type(&mut self, descriptor: &descriptor::DescriptorProto) {
        let fns = self.calculate_type_name(descriptor.name());
        self.map_type_reg.insert(fns, descriptor.clone());
    }

    pub fn get_map_type(&self, type_name: &str) -> Option<descriptor::DescriptorProto> {
        let res = self.map_type_reg.get(type_name);
        if let Some(descriptor) = res {
            return Some(descriptor.clone());
        }
        None
    }

    pub fn register_leading_enum_member(&mut self, descriptor: &descriptor::EnumDescriptorProto) {
        let fns = self.calculate_type_name(descriptor.name());
        self.leading_enum_member_reg
            .insert(fns, descriptor.value.first().unwrap().number());
    }

    pub fn get_leading_enum_member(&self, type_name: &str) -> i32 {
        let res = self.leading_enum_member_reg.get(type_name);
        if let Some(num) = res {
            return *num;
        }
        panic!("no proto provides enum {}", &type_name)
    }

    pub fn get_generated_extension(&self) -> &str {
        "ets"
    }

    pub fn register_export(&mut self, descriptor: &descriptor::FileDescriptorProto) {
        let fns = descriptor.package().to_string();
        let mut exports = self.export_reg.entry(fns.clone()).or_default();
        for r#enum in &descriptor.enum_type {
            exports.push(ExportItem::File(r#enum.name().to_string()));
        }

        for message in &descriptor.message_type {
            exports.push(ExportItem::File(message.name().to_string()));
        }
    }

    pub fn get_file_name(&self, descriptor: &descriptor::FileDescriptorProto) -> String {
        if self.options.namespace_as_file {
            // using namespace/package as file path
            let desc_file_name = descriptor.file_name();
            format!(
                "{}/{}",
                descriptor.package().replace('.', "/"),
                desc_file_name
            )
        } else {
            // using original file relative path as namespace
            std::path::Path::new(&descriptor.name().replace('.', "/"))
                .file_stem()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap()
        }
    }

    pub fn is_path_package(&self, path: &[String]) -> bool {
        self.export_reg.contains_key(&path.join("."))
    }

    pub fn populate_exported_indices(&mut self) {
        if !self.options.namespace_as_file {
            return;
        }

        let indices = self.export_reg.iter().map(|e| e.key().to_string()).fold(
            HashMap::new(),
            |mut ns, package| {
                if !package.starts_with("google") && !package.starts_with("annotation") {
                    for (path, name) in package
                        .split('.')
                        .fold((vec![], String::new()), |(mut paths, last_path), seg| {
                            paths.push((last_path.clone(), seg.to_string()));
                            let last_path = if last_path.is_empty() {
                                seg.to_string()
                            } else {
                                format!("{last_path}.{seg}")
                            };
                            (paths, last_path)
                        })
                        .0
                    {
                        let entry: &mut HashSet<String> = ns.entry(path).or_default();
                        entry.insert(name);
                    }
                }
                ns
            },
        );

        // merge all level package/namespace index
        for (path, items) in indices {
            let mut entry = self.export_reg.entry(path).or_default();
            entry.extend(items.into_iter().map(ExportItem::Module))
        }
    }

    pub fn get_exported_indices(&self) -> Vec<ExportedIndex> {
        if !self.options.namespace_as_file {
            println!("no indices due to no namespace_as_fisle");
            return vec![];
        }
        self.export_reg
            .iter()
            .map(|e| ExportedIndex {
                path: e.key().clone(),
                export_items: e.value().clone(),
            })
            .collect()
    }
}

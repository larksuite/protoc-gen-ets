use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    Accessibility, BinaryOp, BindingIdent, BlockStmt, Class, ClassDecl, ClassMember, ClassMethod,
    Constructor, Decl, ExportDecl, Expr, ExprStmt, ModuleDecl, ModuleItem, Param, Pat, ReturnStmt,
    Stmt, TsEnumDecl, TsEnumMember, TsEnumMemberId, TsKeywordType, TsKeywordTypeKind,
    TsModuleBlock, TsModuleDecl, TsModuleName, TsNamespaceBody, TsType, TsTypeAnn,
    TsTypeParamInstantiation, VarDecl, VarDeclKind, VarDeclarator,
};
use swc_ecma_utils::quote_ident;

use crate::{context::Context, descriptor, export::ExportedIndex, print::Print, runtime::Runtime};

use self::codec::serialize_field_accessor;

#[derive(Clone)]
pub struct LarkPbRuntime;

impl LarkPbRuntime {
    pub const RUNTIME_PACKAGE: &'static str = "@ohos/protobufjs";
    pub const RUNTIME_PACKAGE_NAME: &'static str = "$protobuf";
    pub const LARK_PACKAGE: &'static str = "@lark/lark_pb";
    pub const LARK_PACKAGE_NAME: &'static str = "$lark";

    pub const PB_WRITER_NAME: &'static str = "Writer";
    pub const PB_READER_NAME: &'static str = "Reader";

    pub fn new() -> Self {
        Self {}
    }
}

pub mod codec;
pub mod common;
pub mod ctor;
pub mod deserialize;
pub mod serialize;

impl Runtime for LarkPbRuntime {
    fn from_binary(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
    ) -> Vec<Stmt> {
        vec![]
        // self.deserialize_setup_inner(ctx, descriptor)
    }

    fn to_binary(&self, ctx: &mut Context, descriptor: &descriptor::DescriptorProto) -> Vec<Stmt> {
        self.serialize_setup_inner(ctx, descriptor, serialize_field_accessor)
    }

    fn from_json(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
    ) -> Option<ClassMember> {
        None
    }

    fn to_json(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
    ) -> Option<ClassMember> {
        None
    }

    fn print_message(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        ns: &mut Vec<String>,
    ) -> Vec<ModuleItem> {
        if descriptor.options.map_entry() {
            return vec![];
        }
        let ident = quote_ident!(descriptor.name());

        let interface_decl = self.print_decl_interface(ctx, descriptor);
        let def_decl = self.print_decl_class(ctx, descriptor, ns);

        let mut module_items = vec![];
        ns.push(descriptor.name().to_string());
        if !descriptor.nested_type.is_empty() || !descriptor.enum_type.is_empty() {
            let mut nested_modules = vec![];

            for nested in &descriptor.nested_type {
                nested_modules.extend(self.print_message(ctx, nested, ns));
            }

            for r#enum in &descriptor.enum_type {
                nested_modules.extend(self.print_enum(ctx, r#enum));
            }

            module_items.extend(ctx.wrap_if_needed(nested_modules));
        }
        ns.pop();

        module_items.extend(self.print_oneof_decls(ctx, descriptor));
        if module_items.is_empty() {
            vec![interface_decl, def_decl]
        } else {
            vec![
                interface_decl,
                def_decl,
                ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                    span: DUMMY_SP,
                    decl: Decl::TsModule(Box::new(TsModuleDecl {
                        span: DUMMY_SP,
                        declare: false,
                        global: false,
                        id: TsModuleName::Ident(ident.clone()),
                        body: Some(TsNamespaceBody::TsModuleBlock(TsModuleBlock {
                            span: DUMMY_SP,
                            body: module_items,
                        })),
                    })),
                })),
            ]
        }
    }

    fn print_enum(
        &self,
        _ctx: &mut Context,
        descriptor: &descriptor::EnumDescriptorProto,
    ) -> Vec<ModuleItem> {
        let mut members = vec![];

        for member in descriptor.value.iter() {
            members.push(TsEnumMember {
                span: DUMMY_SP,
                id: TsEnumMemberId::Ident(quote_ident!(member.name())),
                init: Some(Box::new(crate::lit_num!(member.number()).into())),
            })
        }

        let r#enum = Decl::TsEnum(Box::new(TsEnumDecl {
            span: DUMMY_SP,
            declare: false,
            is_const: false,
            id: quote_ident!(descriptor.name()),
            members,
        }));
        vec![ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: r#enum,
        }))]
    }

    fn print_codec(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        ns: &mut Vec<String>,
    ) -> Vec<ModuleItem> {
        if descriptor.options.map_entry() {
            return vec![];
        }
        let mut codec_items = vec![];

        codec_items.push(self.print_decoder(ctx, descriptor, ns));
        codec_items.push(self.print_encoder(ctx, descriptor, ns));
        codec_items.push(self.print_creator(ctx, descriptor, ns));

        ns.push(descriptor.name().to_string());
        for message in &descriptor.nested_type {
            if message.options.map_entry() {
                continue;
            }
            let nested_codec_items = self.print_codec(ctx, message, ns);
            codec_items.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                span: DUMMY_SP,
                decl: Decl::TsModule(Box::new(TsModuleDecl {
                    span: DUMMY_SP,
                    declare: false,
                    global: false,
                    id: TsModuleName::Ident(quote_ident!(format!("${}", message.name()))),
                    body: Some(TsNamespaceBody::TsModuleBlock(TsModuleBlock {
                        span: DUMMY_SP,
                        body: nested_codec_items,
                    })),
                })),
            })));
        }
        ns.pop();

        codec_items
    }

    fn print_index(&self, ctx: &mut Context, index: &ExportedIndex) -> Vec<ModuleItem> {
        let mut items = index.print();

        ctx.drain_imports().into_iter().chain(items).collect()
    }
}

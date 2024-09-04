use crate::{
    context::Context,
    descriptor::{
        DescriptorProto, EnumDescriptorProto, MethodDescriptorProto, ServiceDescriptorProto,
    },
    export::ExportedIndex,
};
use swc_ecma_ast::{ClassMember, ModuleItem, Stmt};

pub trait Runtime {
    // binary
    fn from_binary(&self, ctx: &mut Context, descriptor: &DescriptorProto) -> Vec<Stmt>;
    fn to_binary(&self, ctx: &mut Context, descriptor: &DescriptorProto) -> Vec<Stmt>;

    // json for well known types
    fn from_json(&self, ctx: &mut Context, descriptor: &DescriptorProto) -> Option<ClassMember>;
    fn to_json(&self, ctx: &mut Context, descriptor: &DescriptorProto) -> Option<ClassMember>;

    // message
    fn print_message(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &mut Vec<String>,
    ) -> Vec<ModuleItem>;
    fn print_enum(&self, ctx: &mut Context, descriptor: &EnumDescriptorProto) -> Vec<ModuleItem>;

    // codec
    fn print_codec(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &mut Vec<String>,
    ) -> Vec<ModuleItem>;

    // index
    fn print_index(&self, ctx: &mut Context, index: &ExportedIndex) -> Vec<ModuleItem>;
}

pub trait GrpcRuntime {
    fn print_setup(&self, ctx: &mut Context) -> Vec<ClassMember>;

    // json for well known types
    fn print_method(
        &self,
        ctx: &mut Context,
        method: &MethodDescriptorProto,
        svc: &ServiceDescriptorProto,
    ) -> Vec<ClassMember>;
}

pub mod lark;
// pub mod protobuf_js;

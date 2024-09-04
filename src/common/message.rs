use std::vec;

use crate::context::Context;
use crate::descriptor::DescriptorProto;
use crate::runtime::Runtime;

use swc_ecma_ast::ModuleItem;

impl DescriptorProto
{
    pub fn print<T>(&self, ctx: &mut Context, runtime: &T, ns: &mut Vec<String>) -> Vec<ModuleItem>
    where
        T: Runtime + Sized,
{
        if self.options.map_entry() {
            return vec![];
        }
        runtime.print_message(ctx, self, ns)
    }
}

impl DescriptorProto {
    pub fn print_codec<T: Runtime + Sized>(
        &self,
        ctx: &mut Context,
        runtime: &T,
        ns: &mut Vec<String>,
    ) -> Vec<ModuleItem> {
        if self.options.map_entry() {
            return vec![];
        }
        runtime.print_codec(ctx, self, ns)
    }
}

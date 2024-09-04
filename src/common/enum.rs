use crate::{context::Context, descriptor::EnumDescriptorProto, print::Print, runtime::Runtime};
use swc_ecma_ast::ModuleItem;

impl<T> Print<T> for EnumDescriptorProto
where
    T: Runtime + Sized,
{
    fn print(&self, ctx: &mut Context, runtime: &T) -> Vec<ModuleItem> {
        runtime.print_enum(ctx, self)
    }
}

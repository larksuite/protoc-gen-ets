use crate::{
    context::{self, Context, Syntax},
    descriptor::{DescriptorProto, EnumDescriptorProto, FileDescriptorProto},
    plugin::CodeGeneratorRequest,
};

pub trait Collector {
    fn collect_to(&self, ctx: &mut Context);
}

impl Collector for CodeGeneratorRequest {
    fn collect_to(&self, ctx: &mut Context) {
        for file in &self.proto_file {
            let mut ctx = ctx.fork(file.name().to_string(), &Syntax::Unspecified, false);
            file.collect_to(&mut ctx)
        }
        // compute to generate every indices
        ctx.populate_exported_indices();
    }
}

impl Collector for FileDescriptorProto {
    fn collect_to(&self, ctx: &mut Context) {
        let mut ctx = context::descend_if_necessary!(ctx, self);
        ctx.register_export(self);

        for r#enum in &self.enum_type {
            r#enum.collect_to(&mut ctx)
        }

        for message in &self.message_type {
            message.collect_to(&mut ctx)
        }
    }
}

impl Collector for EnumDescriptorProto {
    fn collect_to(&self, ctx: &mut Context) {
        ctx.register_type_name(self.name());
        ctx.register_leading_enum_member(self)
    }
}

impl Collector for DescriptorProto {
    fn collect_to(&self, ctx: &mut Context) {
        ctx.register_type_name(self.name());

        if self.options.map_entry() {
            ctx.register_map_type(self);
        }

        if self.nested_type.is_empty() && self.enum_type.is_empty() {
            return;
        }
        
        let mut ctx = ctx.descend(self.name().to_string());

        for nested in &self.nested_type {
            ctx.register_type_name(nested.name());
            nested.collect_to(&mut ctx)
        }

        for r#enum in &self.enum_type {
            r#enum.collect_to(&mut ctx)
        }
    }
}

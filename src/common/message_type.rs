use crate::{
    context::Context,
    descriptor::{DescriptorProto, FieldDescriptorProto, OneofDescriptorProto},
};

impl DescriptorProto {
    pub fn get_oneof_fields(&self, current: &FieldDescriptorProto) -> Vec<FieldDescriptorProto> {
        let mut fields = vec![];
        for field in self.field.clone() {
            if field.has_oneof_index()
                && field.oneof_index() == current.oneof_index()
                && field.number() != current.number()
            {
                fields.push(field)
            }
        }
        fields
    }

    pub fn get_oneof_fields_by_index(&self, oneof_index: i32) -> Vec<&FieldDescriptorProto> {
        self.field
            .iter()
            .filter(|f| f.has_oneof_index() && f.oneof_index() == oneof_index)
            .collect()
    }

    pub fn is_well_known(&self, ctx: &Context) -> bool {
        let type_name = ctx.calculate_type_name(self.name());
        type_name.starts_with(".google.protobuf.")
    }
}

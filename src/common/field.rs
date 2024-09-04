use crate::{
    common::field_type::long_expr,
    context::{Context, Syntax},
    descriptor::FieldDescriptorProto,
};
use protobuf::Enum;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    ArrayLit, BinaryOp, Expr, ObjectLit, TsArrayType, TsEntityName, TsKeywordType,
    TsKeywordTypeKind, TsType, TsTypeAnn, TsTypeParamInstantiation, TsTypeRef,
};
use swc_ecma_utils::{quote_ident, quote_str};

use super::{field_type::long_ref_type, wire::{Tag, WireType}};

pub type AccessNormalizerFn = fn(expr: &Expr) -> Expr;

pub fn map_to_string_normalizer(expr: &Expr) -> Expr {
    crate::call_expr!(
        crate::member_expr_bare!(expr.clone(), "map"),
        vec![crate::expr_or_spread!(crate::arrow_func_short!(
            crate::call_expr!(crate::member_expr!("v", "toString")),
            vec![crate::pat_ident!(quote_ident!("v"))]
        ))]
    )
}

pub fn to_string_normalizer(expr: &Expr) -> Expr {
    crate::call_expr!(crate::member_expr_bare!(expr.clone(), "toString"))
}

pub type FieldAccessorFn = fn(field: &FieldDescriptorProto) -> Expr;

pub fn this_field_member(field: &FieldDescriptorProto) -> Expr {
    crate::member_expr!("this", field.name())
}

pub fn bare_field_member(field: &FieldDescriptorProto) -> Expr {
    Expr::Ident(quote_ident!(field.name()))
}

pub fn static_field_member(_field: &FieldDescriptorProto) -> Expr {
    Expr::Ident(quote_ident!("r"))
}

impl FieldDescriptorProto {
    pub fn into_accessor(&self, ctx: &Context) -> FieldAccessorFn {
        if (self.is_repeated() && self.is_map(ctx)) || (self.is_repeated() && !self.is_packed(ctx))
        {
            bare_field_member
        } else {
            this_field_member
        }
    }
}

impl FieldDescriptorProto {
    pub fn default_value_bin_expr<F: Fn(&FieldDescriptorProto) -> Expr>(
        &self,
        ctx: &mut Context,
        accessor: F,
    ) -> Expr {
        let neq_undefined_check = crate::bin_expr!(
            accessor(self),
            quote_ident!("undefined").into(),
            BinaryOp::NotEqEq
        );

        // for oneof field we have to serialize the value unconditionally
        // even if the value is the default.
        if self.has_oneof_index() {
            return neq_undefined_check;
        }

        let presence_check = if self.is_map(ctx) {
            crate::bin_expr!(
                neq_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(accessor(self), "size"),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else if (self.is_bytes() && ctx.syntax == &Syntax::Proto3) || self.is_repeated() {
            crate::bin_expr!(
                neq_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(accessor(self), "length"),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else {
            neq_undefined_check
        };

        if ctx.syntax == &Syntax::Proto3 {
            let default_expr = self.proto3_default(ctx);
            if let Some(default_expr) = default_expr {
                crate::chain_bin_exprs_and!(
                    presence_check,
                    crate::bin_expr!(accessor(self), default_expr, BinaryOp::NotEqEq)
                )
            } else {
                presence_check
            }
        } else {
            presence_check
        }
    }

    pub fn proto3_default(&self, ctx: &mut Context) -> Option<Expr> {
        if self.is_repeated() || self.has_oneof_index() {
            return None;
        }
        if self.is_string() {
            Some(crate::lit_str!("").into())
        } else if self.is_bigint() {
            Some(crate::lit_bigint!(0.into()).into())
        } else if self.is_number() {
            Some(crate::lit_num!(0).into())
        } else if self.is_booelan() {
            Some(crate::lit_bool!(false).into())
        } else if self.is_enum() {
            Some(crate::lit_num!(ctx.get_leading_enum_member(self.type_name())).into())
        } else {
            None
        }
    }

    pub fn default_value_expr(&self, ctx: &mut Context, include_message: bool) -> Expr {
        if self.has_oneof_index() {
            return Expr::Ident(quote_ident!("undefined"));
        }
        if self.is_map(ctx) {
            crate::new_expr!(Expr::Ident(quote_ident!("Map")))
        } else if self.is_repeated() {
            Expr::Array(ArrayLit {
                elems: vec![],
                span: DUMMY_SP,
            })
        } else if self.is_enum() {
            crate::lit_num!(ctx.get_leading_enum_member(self.type_name())).into()
        } else if self.is_message() && include_message {
            crate::new_expr!(ctx.lazy_type_ref(self.type_name()).into())
        } else if self.is_bytes() {
            crate::new_expr!(quote_ident!("Uint8Array").into())
        } else if self.is_string() {
            quote_str!(self.default_value()).into()
        } else if self.is_bigint() {
            if ctx.options.bigint_as_long {
                if self.has_default_value() {
                    crate::call_expr!(
                        crate::member_expr_bare!(long_expr(ctx), "fromValue"),
                        vec![crate::expr_or_spread!(Expr::Lit(crate::lit_num!(self
                            .default_value()
                            .parse::<f64>()
                            .unwrap())))]
                    )
                } else {
                    crate::member_expr_bare!(long_expr(ctx), "ZERO")
                }
            } else {
                crate::lit_bigint!(self
                    .default_value
                    .clone()
                    .unwrap_or("0".to_string())
                    .parse::<num_bigint::BigInt>()
                    .expect("can not parse the default")
                    .into())
                .into()
            }
        } else if self.is_number() {
            crate::lit_num!(self
                .default_value
                .clone()
                .unwrap_or("0".to_string())
                .parse::<f64>()
                .expect("can not parse the default"))
            .into()
        } else if self.is_booelan() {
            crate::lit_bool!(self
                .default_value
                .clone()
                .unwrap_or("false".to_string())
                .parse::<bool>()
                .expect("can not parse the default"))
            .into()
        } else {
            quote_ident!("undefined").into()
        }
    }

    pub fn type_default_value(&self, ctx: &mut Context) -> Expr {
        if self.is_bigint() && ctx.options.bigint_as_long {
            let long = ctx.get_default_import("long", "Long");
            crate::call_expr!(
                crate::member_expr!(long, "fromValue"),
                vec![crate::expr_or_spread!(crate::lit_num!(0).into())]
            )
        } else if self.is_number() {
            crate::lit_num!(0).into()
        } else if self.is_booelan() {
            crate::lit_bool!(false).into()
        } else if self.is_string() {
            crate::lit_str!("").into()
        } else {
            self.default_value_expr(ctx, false)
        }
    }

    pub fn ts_type(&self, ctx: &Context) -> Option<TsType> {
        let mut ts_type: Option<TsType> = None;

        if let Some(typref) = self.type_ref(ctx) {
            ts_type = Some(TsType::TsTypeRef(typref))
        }

        if let Some(kind) = self.keyword_type_kind() {
            if ctx.options.bigint_as_long && matches!(kind, TsKeywordTypeKind::TsBigIntKeyword) {
                ts_type = Some(TsType::TsTypeRef(long_ref_type(ctx)))
            } else {
                ts_type = Some(TsType::TsKeywordType(TsKeywordType {
                    span: DUMMY_SP,
                    kind,
                }))
            }
        }

        if self.is_repeated() && self.is_map(ctx) {
            let descriptor = ctx
                .get_map_type(self.type_name())
                .expect(format!("can not find the map type {}", self.type_name()).as_str());
            ts_type = Some(TsType::TsTypeRef(TsTypeRef {
                span: DUMMY_SP,
                type_name: TsEntityName::Ident(quote_ident!("Map")),
                type_params: Some(Box::new(TsTypeParamInstantiation {
                    span: DUMMY_SP,
                    params: descriptor
                        .field
                        .into_iter()
                        .map(|x: FieldDescriptorProto| {
                            x.type_annotation(ctx)
                                .expect("expect map fields to have corresponding type")
                                .type_ann
                        })
                        .collect(),
                })),
            }))
        } else if ts_type.is_some() && self.is_repeated() && !self.is_map(ctx) {
            ts_type = Some(TsType::TsArrayType(TsArrayType {
                elem_type: Box::new(ts_type.unwrap()),
                span: DUMMY_SP,
            }))
        }
        ts_type
    }

    pub fn type_annotation(&self, ctx: &Context) -> Option<Box<TsTypeAnn>> {
        Some(Box::new(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::new(self.ts_type(ctx)?),
        }))
    }
    pub fn nullish_type_annotation(&self, ctx: &Context) -> Option<Box<TsTypeAnn>> {
        Some(Box::new(crate::type_union!(
            self.ts_type(ctx)?,
            crate::undefined_type!()
        )))
    }

    pub fn field_header(&self) -> Tag {
        Tag::make(self.number() as _, self.wire_type())
    }

    pub fn wire_type(&self) -> WireType {
        WireType::for_type(self.type_())
    }
}

impl FieldDescriptorProto {
    pub fn interface_type_ann(&self, ctx: &Context) -> Option<Box<TsTypeAnn>> {
        Some(Box::new(TsTypeAnn {
            span: DUMMY_SP,
            type_ann: Box::new(self.interface_ts_type(ctx)?),
        }))
    }

    pub fn interface_nullish_type_ann(&self, ctx: &Context) -> Option<Box<TsTypeAnn>> {
        Some(Box::new(crate::type_union!(
            self.interface_ts_type(ctx)?,
            crate::undefined_type!()
        )))
    }

    pub fn interface_ts_type(&self, ctx: &Context) -> Option<TsType> {
        let mut ts_type: Option<TsType> = None;

        if let Some(typref) = self.interface_type_ref(ctx) {
            ts_type = Some(TsType::TsTypeRef(typref))
        }

        if let Some(kind) = self.keyword_type_kind() {
            if ctx.options.bigint_as_long && matches!(kind, TsKeywordTypeKind::TsBigIntKeyword) {
                ts_type = Some(TsType::TsTypeRef(long_ref_type(ctx)))
            } else {
                ts_type = Some(TsType::TsKeywordType(TsKeywordType {
                    span: DUMMY_SP,
                    kind,
                }))
            }
        }

        if self.is_repeated() && self.is_map(ctx) {
            let descriptor = ctx
                .get_map_type(self.type_name())
                .expect(format!("can not find the map type {}", self.type_name()).as_str());
            ts_type = Some(TsType::TsTypeRef(TsTypeRef {
                span: DUMMY_SP,
                type_name: TsEntityName::Ident(quote_ident!("Map")),
                type_params: Some(Box::new(TsTypeParamInstantiation {
                    span: DUMMY_SP,
                    params: descriptor
                        .field
                        .into_iter()
                        .map(|x: FieldDescriptorProto| {
                            x.interface_type_ann(ctx)
                                .expect("expect map fields to have corresponding type")
                                .type_ann
                        })
                        .collect(),
                })),
            }))
        } else if ts_type.is_some() && self.is_repeated() && !self.is_map(ctx) {
            ts_type = Some(TsType::TsArrayType(TsArrayType {
                elem_type: Box::new(ts_type.unwrap()),
                span: DUMMY_SP,
            }))
        }
        ts_type
    }
}

impl FieldDescriptorProto {
    pub fn has_computed_prop(&self) -> bool {
        self.is_optional() && self.is_primitive()
    }
}
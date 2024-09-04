use std::fmt::{Display, LowerExp};

use crate::context::Syntax;
use crate::descriptor::field_descriptor_proto::Type;
use crate::{context::Context, descriptor::FieldDescriptorProto};

use super::field::FieldAccessorFn;
use convert_case::{Case, Casing};
use swc_common::DUMMY_SP;
use swc_ecma_ast::{BinaryOp, Expr, Stmt, UnaryOp};
use swc_ecma_utils::{quote_ident, quote_str};

pub fn json_key_name_field_member(field: &FieldDescriptorProto) -> Expr {
    crate::member_expr!("json", field.json_key_name())
}

pub fn name_field_member(field: &FieldDescriptorProto) -> Expr {
    crate::member_expr!("json", field.name())
}

impl FieldDescriptorProto {
    fn json_repr_for_well_known_type(&self) -> &str {
        match self.type_name().trim_start_matches(".") {
            "google.protobuf.BoolValue" => "boolean",
            "google.protobuf.BytesValue" => "string",
            "google.protobuf.DoubleValue" => "number",
            "google.protobuf.Duration" => "string",
            "google.protobuf.FieldMask" => "string",
            "google.protobuf.FloatValue" => "number",
            "google.protobuf.Int32Value" => "number",
            "google.protobuf.Int64Value" => "number|string",
            "google.protobuf.ListValue" => "array",
            "google.protobuf.StringValue" => "string",
            "google.protobuf.Timestamp" => "string",
            "google.protobuf.UInt32Value" => "number",
            "google.protobuf.UInt64Value" => "number|string",
            "google.protobuf.Value" => "unknown",
            "google.protobuf.NullValue" => "null",
            _ => "object",
        }
    }
    fn typeof_expr_for_well_known_type(&self, accessor: FieldAccessorFn) -> Expr {
        self.typeof_expr_for_type(accessor, self.json_repr_for_well_known_type())
    }

    fn typeof_expr_for_type(&self, accessor: FieldAccessorFn, typ: &str) -> Expr {
        match typ {
            "unknown" => crate::paren_expr!(crate::chain_bin_exprs_or!(
                crate::typeof_unary_expr!(accessor(self).into(), "number"),
                crate::typeof_unary_expr!(accessor(self).into(), "string"),
                crate::typeof_unary_expr!(accessor(self).into(), "boolean"),
                crate::typeof_unary_expr!(accessor(self).into(), "object"),
                crate::bin_expr!(
                    accessor(self).into(),
                    quote_ident!("null").into(),
                    BinaryOp::EqEqEq
                )
            )),
            "number|string" => crate::paren_expr!(crate::chain_bin_exprs_or!(
                crate::typeof_unary_expr!(accessor(self).into(), "number"),
                crate::typeof_unary_expr!(accessor(self).into(), "string")
            )),
            "array" => crate::call_expr!(
                crate::member_expr!("Array", "isArray"),
                vec![crate::expr_or_spread!(accessor(self).into())]
            ),
            "null" => crate::bin_expr!(
                accessor(self).into(),
                quote_ident!("null").into(),
                BinaryOp::EqEqEq
            ),
            typ => crate::typeof_unary_expr!(accessor(self).into(), typ),
        }
    }

    pub(crate) fn infinity_and_nan_check(&self, accessor: FieldAccessorFn) -> Expr {
        crate::chain_bin_exprs_or!(
            crate::bin_expr!(
                accessor(self),
                crate::lit_str!("NaN").into(),
                BinaryOp::EqEqEq
            ),
            crate::bin_expr!(
                accessor(self),
                crate::lit_str!("Infinity").into(),
                BinaryOp::EqEqEq
            ),
            crate::bin_expr!(
                accessor(self),
                crate::lit_str!("-Infinity").into(),
                BinaryOp::EqEqEq
            )
        )
    }

    pub(crate) fn min_max_check<T>(&self, accessor: FieldAccessorFn, min: T, max: T) -> Expr
    where
        T: Display + LowerExp,
    {
        crate::paren_expr!(crate::chain_bin_exprs_and!(
            crate::bin_expr!(
                accessor(self),
                quote_ident!(format!("{:+e}", min)).into(),
                BinaryOp::GtEq
            ),
            crate::bin_expr!(
                accessor(self),
                quote_ident!(format!("{:+e}", max)).into(),
                BinaryOp::LtEq
            )
        ))
    }

    pub(crate) fn min_max_check_bigint<T>(&self, accessor: FieldAccessorFn, min: T, max: T) -> Expr
    where
        T: Into<num_bigint::BigInt>,
    {
        crate::paren_expr!(crate::chain_bin_exprs_and!(
            crate::bin_expr!(
                accessor(self),
                crate::lit_bigint!(min.into()).into(),
                BinaryOp::GtEq
            ),
            crate::bin_expr!(
                accessor(self),
                crate::lit_bigint!(max.into()).into(),
                BinaryOp::LtEq
            )
        ))
    }

    pub fn default_value_bin_expr_for_json(
        &self,
        ctx: &mut Context,
        accessor: FieldAccessorFn,
    ) -> Expr {
        let neq_undefined_check = crate::bin_expr!(
            accessor(self),
            quote_ident!("undefined").into(),
            BinaryOp::NotEqEq
        );

        let neq_null_check = crate::bin_expr!(
            accessor(self),
            quote_ident!("null").into(),
            BinaryOp::NotEqEq
        );

        let neq_null_or_undefined_check = if self.json_repr_for_well_known_type() == "unknown" {
            neq_undefined_check
        } else {
            crate::chain_bin_exprs_and!(neq_null_check, neq_undefined_check)
        };

        let presence_check = if self.has_oneof_index() {
            // for oneof field we have to serialize the value unconditionally even if the value is the default.
            neq_null_or_undefined_check
        } else if self.is_map(ctx) {
            crate::bin_expr!(
                neq_null_or_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(accessor(self), "size"),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else if (self.is_bytes() && ctx.syntax == &Syntax::Proto3) || self.is_repeated() {
            crate::bin_expr!(
                neq_null_or_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(accessor(self), "length"),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else {
            neq_null_or_undefined_check
        };

        let default_expr = self.proto3_default(ctx);

        if default_expr.is_some() && ctx.syntax == &Syntax::Proto3 && !self.has_oneof_index() {
            crate::bin_expr!(
                presence_check,
                crate::bin_expr!(accessor(self), default_expr.unwrap(), BinaryOp::NotEqEq)
            )
        } else {
            presence_check
        }
    }

    pub fn value_check_stmt(&self, ctx: &Context, accessor: FieldAccessorFn) -> Stmt {
        let min_max_check: Option<Expr> = match self.type_() {
            Type::TYPE_FLOAT => Some(self.min_max_check(accessor, f32::MIN, f32::MAX)),
            Type::TYPE_DOUBLE => Some(self.min_max_check(accessor, f64::MIN, f64::MAX)),
            Type::TYPE_UINT32 | Type::TYPE_FIXED32 => {
                Some(self.min_max_check(accessor, u32::MIN, u32::MAX))
            }
            Type::TYPE_UINT64 | Type::TYPE_FIXED64 => {
                Some(self.min_max_check_bigint(accessor, u64::MIN, u64::MAX))
            }
            Type::TYPE_INT32 | Type::TYPE_SFIXED32 | Type::TYPE_SINT32 => {
                Some(self.min_max_check(accessor, i32::MIN, i32::MAX))
            }
            Type::TYPE_INT64 | Type::TYPE_SFIXED64 | Type::TYPE_SINT64 => {
                Some(self.min_max_check_bigint(accessor, i64::MIN, i64::MAX))
            }
            _ => None,
        };

        let num_check = if min_max_check.is_some() {
            Some(crate::chain_bin_exprs_or!(
                self.infinity_and_nan_check(accessor),
                min_max_check.unwrap()
            ))
        } else {
            None
        };

        let typeof_check = if self.is_well_known_message() {
            self.typeof_expr_for_well_known_type(accessor)
        } else if self.is_string() || self.is_bytes() {
            self.typeof_expr_for_type(accessor, "string")
        } else if self.is_booelan() {
            self.typeof_expr_for_type(accessor, "boolean")
        } else if self.is_message() {
            /* also map */
            self.typeof_expr_for_type(accessor, "object")
        } else if self.is_integer() {
            // integer (non-bigint-float-double) needs special check
            crate::chain_bin_exprs_and!(
                crate::paren_expr!(crate::chain_bin_exprs_or!(
                    self.typeof_expr_for_type(accessor, "number"),
                    crate::chain_bin_exprs_and!(
                        self.typeof_expr_for_type(accessor, "string"),
                        crate::bin_expr!(
                            crate::call_expr!(
                                crate::member_expr_bare!(accessor(self), "indexOf"),
                                vec![crate::expr_or_spread!(crate::lit_str!(" ").into())]
                            ),
                            crate::lit_num!(-1).into(),
                            BinaryOp::EqEqEq
                        )
                    )
                )),
                crate::call_expr!(
                    crate::member_expr!("Number", "isInteger"),
                    vec![crate::expr_or_spread!(crate::unary_expr!(
                        accessor(self).into(),
                        UnaryOp::Plus
                    ))]
                )
            )
        } else if self.is_number() {
            self.typeof_expr_for_type(accessor, "number|string")
        } else if self.is_enum() {
            crate::chain_bin_exprs_or!(
                self.typeof_expr_for_type(accessor, "number"),
                crate::chain_bin_exprs_and!(
                    self.typeof_expr_for_type(accessor, "string"),
                    crate::bin_expr!(
                        accessor(self).into(),
                        ctx.lazy_type_ref(self.type_name()).into(),
                        BinaryOp::In
                    )
                )
            )
        } else {
            self.typeof_expr_for_type(accessor, "never!")
        };

        let check = if num_check.is_some() {
            crate::chain_bin_exprs_and!(typeof_check, crate::paren_expr!(num_check.unwrap()))
        } else {
            typeof_check
        };

        crate::if_stmt!(
            crate::unary_expr!(crate::paren_expr!(check)),
            crate::throw_stmt!(crate::new_expr!(
                quote_ident!("Error").into(),
                vec![crate::expr_or_spread!(crate::lit_str!(format!(
                    "illegal value for {}",
                    self.json_key_name()
                ))
                .into())]
            ))
        )
    }

    pub fn json_key_name(&self) -> String {
        if self.has_json_name() {
            self.json_name().to_string()
        } else {
            self.name().to_case(Case::Camel)
        }
    }

    pub fn into_to_stringified_map_expr(&self, ctx: &mut Context) -> Expr {
        if self.is_string() {
            return Expr::Ident(quote_ident!(self.name()));
        }
        if self.name() == "key" {
            super::field::to_string_normalizer(&quote_ident!(self.name()).into())
        } else {
            self.into_to_json_expr(ctx, super::field::bare_field_member)
        }
    }

    pub fn into_to_json_expr(
        &self,
        ctx: &mut Context,
        accessor_fn: super::field::FieldAccessorFn,
    ) -> Expr {
        let accessor = accessor_fn(self);
        if self.is_enum() {
            crate::bin_expr!(
                crate::member_expr_computed!(ctx.lazy_type_ref(self.type_name()).into(), accessor),
                accessor_fn(self).into(),
                BinaryOp::NullishCoalescing
            )
        } else if self.is_bytes() {
            let base64 = ctx.get_import(ctx.options.base64_package.as_str(), None);
            crate::call_expr!(
                crate::member_expr!(base64, "encode"),
                vec![crate::expr_or_spread!(accessor)]
            )
        } else if self.is_bigint() {
            crate::call_expr!(crate::member_expr_bare!(accessor, "toString"))
        } else if self.is_number() {
            crate::cond_expr!(
                crate::call_expr!(
                    crate::member_expr!("Number", "isFinite"),
                    vec![crate::expr_or_spread!(accessor.clone())]
                ),
                accessor.clone().into(),
                super::field::to_string_normalizer(&accessor)
            )
        } else if self.is_message() && !self.is_map(ctx) {
            crate::call_expr!(crate::member_expr_bare!(accessor, "toJson"))
        } else {
            accessor
        }
    }

    pub fn into_from_json_expr_for_map_key(
        &self,
        ctx: &mut Context,
        accessor_fn: super::field::FieldAccessorFn,
    ) -> Expr {
        let accessor = accessor_fn(self);
        if self.is_booelan() {
            crate::bin_expr!(accessor, quote_str!("true").into(), BinaryOp::EqEqEq)
        } else {
            self.into_from_json_expr(ctx, accessor_fn)
        }
    }

    pub fn into_from_json_expr(
        &self,
        ctx: &mut Context,
        accessor_fn: super::field::FieldAccessorFn,
    ) -> Expr {
        let accessor = accessor_fn(self);
        if self.is_enum() {
            crate::cond_expr!(
                crate::typeof_unary_expr!(accessor_fn(self).into(), "number"),
                accessor_fn(self).into(),
                crate::member_expr_computed!(ctx.lazy_type_ref(self.type_name()).into(), accessor)
            )
        } else if self.is_bytes() {
            let base64 = ctx.get_import(ctx.options.base64_package.as_str(), None);
            crate::call_expr!(
                crate::member_expr!(base64, "decode"),
                vec![crate::expr_or_spread!(accessor)]
            )
        } else if self.is_bigint() {
            crate::call_expr!(
                quote_ident!("BigInt").into(),
                vec![crate::expr_or_spread!(accessor)]
            )
        } else if self.is_number() {
            crate::call_expr!(
                quote_ident!("Number").into(),
                vec![crate::expr_or_spread!(accessor)]
            )
        } else if self.is_message() && !self.is_map(ctx) {
            crate::call_expr!(
                crate::member_expr_bare!(ctx.lazy_type_ref(self.type_name()).into(), "fromJson"),
                vec![crate::expr_or_spread!(accessor)]
            )
        } else {
            accessor
        }
    }
}

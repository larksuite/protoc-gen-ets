use std::collections::HashSet;

use heck::ToLowerCamelCase;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    BindingIdent, BlockStmt, ComputedPropName, Expr, ForHead, ForOfStmt, MemberExpr, MemberProp,
    Pat, Stmt, SwitchCase, SwitchStmt, VarDecl, VarDeclKind, VarDeclarator,
};
use swc_ecma_utils::{quote_ident, quote_str};

use crate::{
    common::field,
    context::Context,
    descriptor::{self, OneofDescriptorProto},
    runtime::lark::protobuf_js::codec::host_field_accessor,
};

use super::{
    codec::{self, field_accesscor, FieldAccessor},
    common::normalize_name,
    LarkPbRuntime,
};

impl LarkPbRuntime {
    pub(super) fn serialize_setup_inner(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        accessor: FieldAccessor,
    ) -> Vec<Stmt> {
        let mut stmts = vec![];
        stmts.push(self.serialize_setup_writer(ctx));

        let mut oneofs = HashSet::new();
        let host_expr = Expr::Ident(quote_ident!("$req"));

        for field in &descriptor.field {
            let field_accessor = if descriptor.options.map_entry() {
                accessor
            } else {
                host_field_accessor
            };

            let field_stmt = self.serialize_field_expr(
                ctx,
                host_expr.clone(),
                descriptor,
                field,
                field_accessor,
                &mut oneofs,
                None,
            );

            if let Some(stmt) = field_stmt {
                stmts.push(crate::if_stmt!(
                    self.default_value_bin_expr(
                        ctx,
                        host_expr.clone(),
                        descriptor,
                        field,
                        accessor
                    ),
                    Stmt::Block(BlockStmt {
                        span: DUMMY_SP,
                        stmts: vec![stmt]
                    })
                ));
            }
        }
        stmts.push(crate::return_stmt!(Expr::Ident(quote_ident!("$writer"))));
        stmts
    }

    pub(super) fn serialize_field_expr(
        &self,
        ctx: &mut Context,
        host_expr: Expr,
        descriptor: &descriptor::DescriptorProto,
        field: &descriptor::FieldDescriptorProto,
        field_accessor: FieldAccessor,
        oneofs: &mut HashSet<i32>,
        computed_prop: Option<bool>,
    ) -> Option<Stmt> {
        let has_computed_prop = computed_prop.unwrap_or(field.has_computed_prop());
        let field_accessor = if field.is_repeated() {
            codec::field_accesscor
        } else {
            field_accessor
        };
        let field_stmt: Stmt = if field.has_oneof_index() {
            if !oneofs.contains(&field.oneof_index()) {
                oneofs.insert(field.oneof_index());
                let oneof = descriptor
                    .oneof_decl
                    .get(field.oneof_index() as usize)
                    .unwrap();
                let mut fields = descriptor.get_oneof_fields(field);
                fields.push(field.clone());
                self.serialize_oneof_field_stmt(ctx, host_expr.clone(), oneof, &fields)
            } else {
                return None;
            }
        } else if field.is_map(ctx) {
            self.serialize_map_field_stmt(ctx, host_expr.clone(), field)
        } else if field.is_message() {
            self.serialize_message_field_stmt(ctx, host_expr.clone(), field, field_accessor, computed_prop)
        } else {
            self.serialize_primitive_field_stmt(ctx, host_expr.clone(), field, field_accessor, None, computed_prop)
        };

        let field_stmt = if field.is_repeated() && !field.is_packed(ctx) {
            Stmt::ForOf(ForOfStmt {
                span: DUMMY_SP,
                is_await: false,
                left: ForHead::VarDecl(Box::new(crate::const_decl_uinit!(normalize_name(
                    field.name()
                )))),
                right: Box::new(host_field_accessor(host_expr.clone(), field, has_computed_prop)),
                body: Box::new(match field_stmt {
                    Stmt::Block(_) => field_stmt,
                    _ => Stmt::Block(BlockStmt {
                        span: DUMMY_SP,
                        stmts: vec![field_stmt],
                    }),
                }),
            })
        } else {
            field_stmt
        };
        Some(field_stmt)
    }

    pub(super) fn serialize_setup_writer(&self, ctx: &mut Context) -> Stmt {
        let writer = ctx.get_import_from(Self::RUNTIME_PACKAGE, Self::PB_WRITER_NAME);
        let call = Expr::Member(MemberExpr {
            span: DUMMY_SP,
            obj: Box::new(writer.into()),
            prop: MemberProp::Ident(quote_ident!("create")),
        });
        let w = Expr::Ident(quote_ident!("$w"));
        let writer_decl_init = crate::bin_expr!(
            w,
            crate::call_expr!(call),
            swc_ecma_ast::BinaryOp::NullishCoalescing
        );
        Stmt::Decl(crate::const_decl!("$writer", writer_decl_init))
    }

    pub(super) fn serialize_oneof_field_stmt(
        &self,
        ctx: &mut Context,
        host: Expr,
        oneof: &OneofDescriptorProto,
        oneofs: &[descriptor::FieldDescriptorProto],
    ) -> Stmt {
        // req.oneof_field_name
        let field_expr = crate::member_expr_bare!(host, normalize_name(oneof.name()));
        let cases: Vec<SwitchCase> = oneofs
            .iter()
            .map(|o| {
                let write_expr = if o.is_message() {
                    self.serialize_message_field_stmt(
                        ctx,
                        field_expr.clone(),
                        o,
                        host_field_accessor,
                        Some(false),
                    )
                } else {
                    self.serialize_primitive_field_stmt(
                        ctx,
                        field_expr.clone(),
                        o,
                        host_field_accessor,
                        None,
                        Some(false),
                    )
                };
                let stmts = vec![
                    write_expr,
                    Stmt::Break(swc_ecma_ast::BreakStmt {
                        span: DUMMY_SP,
                        label: None,
                    }),
                ];
                SwitchCase {
                    span: DUMMY_SP,
                    test: Some(Box::new(crate::lit_str!(normalize_name(o.name())).into())),
                    cons: stmts,
                }
            })
            .collect();
        assert!(
            !cases.is_empty(),
            "no variant for oneof: {}.{}",
            ctx.get_namespace(),
            oneof.name()
        );

        Stmt::Switch(SwitchStmt {
            span: DUMMY_SP,
            discriminant: Box::new(crate::member_expr_bare!(field_expr.clone(), "$oneofKind")),
            cases,
        })
    }

    pub fn serialize_message_field_stmt(
        &self,
        ctx: &mut Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
        field_accessor: FieldAccessor,
        computed_prop: Option<bool>,
    ) -> Stmt {
        let has_computed_prop = computed_prop.unwrap_or(field.has_computed_prop());
        let access_expr = field_accessor(host, field, has_computed_prop);
        let full_name = ctx.lazy_type_ref(field.type_name());
        // $w.uint32(/* (id << 3) |  wireType */)
        let id_write_expr = crate::call_expr!(
            crate::member_expr!("$writer", "uint32"),
            vec![crate::expr_or_spread!(
                crate::lit_num!(field.field_header().value()).into()
            )]
        );
        let writer_fork_expr = crate::call_expr!(crate::member_expr_bare!(id_write_expr, "fork"));
        let field_write_expr = crate::call_expr!(
            crate::member_expr!(full_name, "encode"),
            vec![
                crate::expr_or_spread!(access_expr),
                crate::expr_or_spread!(writer_fork_expr)
            ]
        );
        // package.XXX.encode(req.field_name, $w.uint32(/* */).fork()).ldelim();
        crate::expr_stmt!(crate::call_expr!(crate::member_expr_bare!(
            field_write_expr,
            "ldelim"
        )))
    }

    pub fn serialize_map_field_stmt(
        &self,
        ctx: &mut Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
    ) -> Stmt {
        let descriptor = ctx
            .get_map_type(field.type_name())
            .expect(format!("cannot find the map type {}", field.type_name()).as_str());

        let id_writer_expr = crate::call_expr!(
            crate::member_expr!("$writer", "uint32"),
            vec![crate::expr_or_spread!(
                crate::lit_num!(field.field_header().value()).into()
            )]
        );
        let sub_start_expr = crate::call_expr!(crate::member_expr_bare!(id_writer_expr, "fork"));
        let sub_start_stmt = crate::expr_stmt!(sub_start_expr);
        let mut stmts = vec![sub_start_stmt];
        let mut oneofs = HashSet::new();
        
        let entry_ident = quote_ident!(normalize_name(field.name()));
        let entry_expr = Expr::Ident(entry_ident.clone());
        stmts.push(Stmt::Decl(crate::const_decl!(
            "key",
            Expr::Member(MemberExpr {
                span: DUMMY_SP,
                obj: Box::new(entry_expr.clone()),
                prop: MemberProp::Computed(ComputedPropName {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Lit(crate::lit_num!(0)))
                }),
            })
        )));
        stmts.push(Stmt::Decl(crate::const_decl!(
            "value",
            Expr::Member(MemberExpr {
                span: DUMMY_SP,
                obj: Box::new(entry_expr.clone()),
                prop: MemberProp::Computed(ComputedPropName {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Lit(crate::lit_num!(1)))
                }),
            })
        )));
        for field in &descriptor.field {
            if let Some(stmt) = self.serialize_field_expr(
                ctx,
                host.clone(),
                &descriptor,
                field,
                field_accesscor,
                &mut oneofs,
                Some(false)
            ) {
                stmts.push(stmt);
            }
        }
        stmts.push(crate::expr_stmt!(crate::call_expr!(crate::member_expr!(
            "$writer", "ldelim"
        ))));
        Stmt::Block(BlockStmt {
            span: DUMMY_SP,
            stmts,
        })
    }

    pub fn serialize_primitive_field_stmt(
        &self,
        _ctx: &mut Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
        field_accessor: FieldAccessor,
        access_normalizer: Option<field::AccessNormalizerFn>,
        has_computed_prop: Option<bool>,
    ) -> Stmt {
        let has_computed_prop = has_computed_prop.unwrap_or(field.has_computed_prop());
        let access_expr = field_accessor(host, field, has_computed_prop);
        let access_expr = access_normalizer
            .map(|f| f(&access_expr))
            .unwrap_or(access_expr);
        // $w.uint32(/* id + wireType */)
        let id_write_expr = crate::call_expr!(
            crate::member_expr!("$writer", "uint32"),
            vec![crate::expr_or_spread!(
                crate::lit_num!(field.field_header().value()).into()
            )]
        );
        // $w.uint32(/* */).xxx(/* */)
        crate::expr_stmt!(crate::call_expr!(
            crate::member_expr_bare!(id_write_expr, self.codec_fn_name(field)),
            vec![crate::expr_or_spread!(access_expr)]
        ))
    }
}

use std::{collections::HashMap, iter::once};

use heck::{ToLowerCamelCase, ToUpperCamelCase};
use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    AssignOp, AssignTarget, BinaryOp, BindingIdent, BlockStmt, Decl, Expr, KeyValueProp, ObjectLit,
    ParenExpr, Pat, Prop, PropName, PropOrSpread, SimpleAssignTarget, Stmt, SwitchCase, TsAsExpr,
    TsNonNullExpr, VarDecl, VarDeclKind, VarDeclarator, WhileStmt,
};
use swc_ecma_utils::{quote_ident, quote_str};

use crate::{
    context::Context,
    descriptor::{self, OneofDescriptorProto},
    runtime::lark::protobuf_js::common::ns_name,
};

use super::{
    codec::{field_accesscor, FieldAccessor},
    common::{normalize_name, normalize_type_name},
    LarkPbRuntime,
};

impl LarkPbRuntime {
    pub(super) fn deserialize_setup_inner(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        ns: &mut Vec<String>,
    ) -> Vec<Stmt> {
        let mut stmts = vec![];
        // const $r = data instanceof $protobuf.Reader ? data : $protobuf.Reader.create(data);
        stmts.push(self.deserialize_setup_reader(ctx));
        // let $m = new XXX();
        stmts.extend(self.deserialize_setup_result(ctx, descriptor, ns));
        // let c = l === undefined ? $r.len : $r.pos + l;
        stmts.push(self.deserialize_setup_offset());
        stmts.push(self.deserialize_stmt(
            ctx,
            Expr::Ident(quote_ident!("$m")),
            descriptor,
            field_accesscor,
            &ns,
        ));
        stmts.push(self.deserialize_return_result(ctx, descriptor, ns));

        stmts
    }

    fn deserialize_setup_result(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        ns: &[String],
    ) -> Vec<Stmt> {
        let mut decls = vec![];
        for field in &descriptor.field {
            if !field.has_oneof_index() {
                let field_ident = quote_ident!(normalize_name(field.name()));
                let ty = if field.is_message() {
                    field.interface_nullish_type_ann(ctx)
                } else {
                    field.nullish_type_annotation(ctx)
                };
                decls.push(VarDeclarator {
                    span: DUMMY_SP,
                    name: Pat::Ident(BindingIdent {
                        id: field_ident.clone(),
                        type_ann: ty,
                    }),
                    init: None,
                    definite: false,
                })
            }
        }
        for oneof in &descriptor.oneof_decl {
            let oneof_ident = quote_ident!(normalize_name(oneof.name()));
            let mut ns = ns.to_vec();
            ns.push(descriptor.name().to_string());
            let oneof_type = crate::type_ref!(crate::entity_name_ident!(
                ctx.lazy_type_ref(&ns_name(&ns, &oneof.name().to_upper_camel_case()))
            ));
            let ty = crate::type_union!(oneof_type, crate::undefined_type!());
            decls.push(VarDeclarator {
                span: DUMMY_SP,
                name: Pat::Ident(BindingIdent {
                    id: oneof_ident,
                    type_ann: Some(Box::new(ty)),
                }),
                init: None,
                definite: false,
            });
        }
        decls
            .into_iter()
            .map(|decl| {
                Stmt::Decl(Decl::Var(Box::new(VarDecl {
                    span: DUMMY_SP,
                    kind: VarDeclKind::Let,
                    declare: false,
                    decls: vec![decl],
                })))
            })
            .collect()
    }

    fn deserialize_return_result(
        &self,
        ctx: &mut Context,
        descriptor: &descriptor::DescriptorProto,
        ns: &[String],
    ) -> Stmt {
        let mut init_props = vec![];
        for field in &descriptor.field {
            if !field.has_oneof_index() {
                let field_ident = quote_ident!(normalize_name(field.name()));

                let init_value = if field.is_optional() {
                    Expr::Ident(field_ident.clone())
                } else {
                    Expr::TsNonNull(TsNonNullExpr {
                        span: DUMMY_SP,
                        expr: Box::new(Expr::Ident(field_ident.clone())),
                    })
                };
                init_props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                    key: PropName::Ident(field_ident.clone()),
                    value: Box::new(init_value),
                }))))
            }
        }
        for oneof in &descriptor.oneof_decl {
            let oneof_ident = quote_ident!(normalize_name(oneof.name()));
            init_props.push(PropOrSpread::Prop(Box::new(Prop::Shorthand(oneof_ident))));
        }
        let init_expr = Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props: init_props,
        });
        crate::return_stmt!(init_expr)
    }

    pub(super) fn deserialize_setup_reader(&self, ctx: &mut Context) -> Stmt {
        let reader = ctx.get_import_from(Self::RUNTIME_PACKAGE, Self::PB_READER_NAME);
        let call = crate::member_expr!(reader.clone(), "create");
        let r = Expr::Ident(quote_ident!("$data"));
        let reader_ty_distinct = crate::bin_expr!(
            r.clone(),
            Expr::Ident(reader.clone()),
            swc_ecma_ast::BinaryOp::InstanceOf
        );
        let reader_decl_init = crate::cond_expr!(
            reader_ty_distinct,
            r.clone(),
            crate::call_expr!(call, vec![crate::expr_or_spread!(r.clone())])
        );
        Stmt::Decl(crate::const_decl!("$r", reader_decl_init))
    }

    fn deserialize_setup_offset(&self) -> Stmt {
        let r = Expr::Ident(quote_ident!("$r"));
        let len = Expr::Ident(quote_ident!("$l"));
        let verify_check = crate::bin_expr!(
            len.clone(),
            Expr::Ident(quote_ident!("undefined")),
            BinaryOp::EqEqEq
        );
        Stmt::Decl(crate::const_decl!(
            "$c",
            crate::cond_expr!(
                verify_check,
                crate::member_expr_bare!(r.clone(), "len"),
                crate::bin_expr!(
                    crate::member_expr_bare!(r.clone(), "pos"),
                    len.clone(),
                    BinaryOp::Add
                )
            )
        ))
    }

    fn deserialize_field_setup_stmts(
        &self,
        ctx: &mut Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
        accessor: FieldAccessor,
        deserialize_stmt: Stmt,
        ns: &[String],
    ) -> Vec<Stmt> {
        let mut stmts = vec![];
        if field.is_map(ctx) {
            // ?= {};
            let assign_taget = AssignTarget::Simple(SimpleAssignTarget::Paren(ParenExpr {
                span: DUMMY_SP,
                expr: Box::new(accessor(host.clone(), field, false)),
            }));
            stmts.push(crate::expr_stmt!(crate::assign_expr!(
                assign_taget,
                field.default_value_expr(ctx, false),
                AssignOp::NullishAssign
            )))
        } else if field.is_repeated() {
            // ?= []
            let assign_taget = AssignTarget::Simple(SimpleAssignTarget::Paren(ParenExpr {
                span: DUMMY_SP,
                expr: Box::new(accessor(host.clone(), field, false)),
            }));
            stmts.push(crate::expr_stmt!(crate::assign_expr!(
                assign_taget,
                field.default_value_expr(ctx, false),
                AssignOp::NullishAssign
            )));
        }
        let reader_expr = quote_ident!("$r");
        let end_expr = "$c".to_string();
        let end_ident = quote_ident!(end_expr.as_str());
        if field.is_map(ctx) {
            let descriptor = ctx
                .get_map_type(field.type_name())
                .expect(format!("cannot find map thpe {}", field.type_name()).as_str());
            let key_field = &descriptor.field[0];
            let value_field = &descriptor.field[1];

            stmts.extend(vec![
                Stmt::Decl(crate::let_decl!(
                    end_expr.as_str(),
                    None,
                    crate::bin_expr!(
                        crate::call_expr!(crate::member_expr!(reader_expr.clone(), "uint32")),
                        crate::member_expr!(reader_expr.clone(), "pos"),
                        BinaryOp::Add
                    )
                )),
                Stmt::Decl(crate::let_decl!(
                    "key",
                    key_field.interface_nullish_type_ann(ctx)
                )),
                Stmt::Decl(crate::let_decl!(
                    "value",
                    value_field.interface_nullish_type_ann(ctx)
                )),
                self.deserialize_stmt(ctx, host.clone(), &descriptor, field_accesscor, ns),
                deserialize_stmt,
            ]);
        } else if field.is_packable() {
            stmts.push(crate::if_stmt!(
                crate::bin_expr!(
                    Expr::Paren(swc_ecma_ast::ParenExpr {
                        span: DUMMY_SP,
                        expr: Box::new(crate::bin_expr!(
                            Expr::Ident(quote_ident!("t")),
                            crate::lit_num!(7).into(),
                            BinaryOp::BitAnd
                        ))
                    }),
                    crate::lit_num!(2).into(),
                    BinaryOp::EqEqEq
                ),
                Stmt::Block(BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![
                        Stmt::Decl(crate::let_decl!(
                            end_expr.as_str(),
                            None,
                            crate::bin_expr!(
                                crate::call_expr!(crate::member_expr!(
                                    reader_expr.clone(),
                                    "uint32"
                                )),
                                crate::member_expr!(reader_expr.clone(), "pos"),
                                BinaryOp::Add
                            )
                        )),
                        Stmt::While(WhileStmt {
                            span: DUMMY_SP,
                            test: Box::new(crate::bin_expr!(
                                crate::member_expr!(reader_expr.clone(), "pos"),
                                end_ident.clone().into(),
                                BinaryOp::Lt
                            )),
                            body: Box::new(Stmt::Block(BlockStmt {
                                span: DUMMY_SP,
                                stmts: vec![deserialize_stmt.clone()]
                            }))
                        })
                    ]
                }),
                Stmt::Block(BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![deserialize_stmt.clone()]
                })
            ));
        } else {
            stmts.push(deserialize_stmt);
        }
        stmts
    }

    fn deserialize_message_field_expr(
        &self,
        ctx: &mut Context,
        _host: Expr,
        field: &descriptor::FieldDescriptorProto,
        _accessor: FieldAccessor,
    ) -> Expr {
        let reader_expr = Expr::Ident(quote_ident!("$r"));
        let decoder_full_name = ctx.lazy_type_ref(field.type_name());
        crate::call_expr!(
            crate::member_expr_bare!(
                crate::member_expr_bare!(
                    crate::member_expr!(decoder_full_name, "$meta"),
                    "decoder"
                ),
                "decode"
            ),
            vec![
                crate::expr_or_spread!(reader_expr.clone()),
                crate::expr_or_spread!(crate::call_expr!(crate::member_expr_bare!(
                    reader_expr.clone(),
                    "uint32"
                )))
            ]
        )
    }

    fn deserialize_primitive_field_expr(
        &self,
        ctx: &mut Context,
        _host: Expr,
        field: &descriptor::FieldDescriptorProto,
        _force_unpacked: bool,
    ) -> Expr {
        let field_expr = crate::call_expr!(crate::member_expr!("$r", self.codec_fn_name(field)));
        if field.is_bigint() && ctx.options.bigint_as_long {
            let long = ctx.get_default_import("long", "Long");
            crate::call_expr!(
                crate::member_expr!(long, "fromValue"),
                vec![crate::expr_or_spread!(field_expr)]
            )
        } else {
            field_expr
        }
    }

    fn deserialize_map_field_expr(
        &self,
        _ctx: &Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
        accessor: FieldAccessor,
    ) -> Expr {
        crate::call_expr!(
            crate::member_expr_bare!(accessor(host, field, false), "set"),
            vec![
                crate::expr_or_spread!(Expr::TsNonNull(TsNonNullExpr {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Ident(quote_ident!("key"))),
                })),
                crate::expr_or_spread!(Expr::TsNonNull(TsNonNullExpr {
                    span: DUMMY_SP,
                    expr: Box::new(Expr::Ident(quote_ident!("value")))
                }))
            ]
        )
    }

    fn deserialize_field_expr(
        &self,
        ctx: &mut Context,
        host: Expr,
        field: &descriptor::FieldDescriptorProto,
        accessor: FieldAccessor,
        force_unpacked: bool,
    ) -> Expr {
        if field.is_map(ctx) {
            self.deserialize_map_field_expr(ctx, host, field, accessor)
        } else if field.is_message() {
            self.deserialize_message_field_expr(ctx, host, field, accessor)
        } else {
            self.deserialize_primitive_field_expr(ctx, host, field, force_unpacked)
        }
    }

    fn deserialize_oneof_field_expr(
        &self,
        ctx: &Context,
        descriptor: &descriptor::DescriptorProto,
        field: &descriptor::FieldDescriptorProto,
        field_expr: &Expr,
        ns: &[String],
    ) -> Expr {
        let oneof: &OneofDescriptorProto = &descriptor.oneof_decl[field.oneof_index() as usize];
        let mut ns = ns.to_vec();
        ns.push(descriptor.name().to_string());
        let oneof_type = crate::type_ref!(crate::entity_name_ident!(ctx.lazy_type_ref(&ns_name(
            &ns,
            &format!(
                "${}_{}",
                normalize_type_name(oneof.name()),
                normalize_type_name(field.name())
            )
        ))));
        Expr::TsAs(TsAsExpr {
            span: DUMMY_SP,
            expr: Box::new(Expr::Object(swc_ecma_ast::ObjectLit {
                span: DUMMY_SP,
                props: vec![
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                        key: PropName::Ident(quote_ident!("$oneofKind")),
                        value: Box::new(Expr::Lit(crate::lit_str!(normalize_name(field.name())))),
                    }))),
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(swc_ecma_ast::KeyValueProp {
                        key: PropName::Ident(quote_ident!(normalize_name(field.name()))),
                        value: Box::new(field_expr.clone()),
                    }))),
                ],
            })),
            type_ann: Box::new(oneof_type),
        })
    }

    pub fn deserialize_stmt(
        &self,
        ctx: &mut Context,
        host: Expr,
        descriptor: &descriptor::DescriptorProto,
        accessor: FieldAccessor,
        ns: &[String],
    ) -> Stmt {
        let mut cases: Vec<SwitchCase> = vec![];
        let reader_expr = Expr::Ident(quote_ident!("$r"));
        for field in &descriptor.field {
            let field_expr = self.deserialize_field_expr(ctx, host.clone(), field, accessor, false);
            let field_expr = if field.has_oneof_index() {
                self.deserialize_oneof_field_expr(ctx, descriptor, field, &field_expr, ns)
            } else {
                field_expr
            };
            let read_expr = if field.has_oneof_index() {
                let oneof = &descriptor.oneof_decl[field.oneof_index() as usize];
                crate::assign_expr!(
                    AssignTarget::Simple(SimpleAssignTarget::Ident(BindingIdent {
                        id: quote_ident!(normalize_name(oneof.name())),
                        type_ann: None
                    })),
                    field_expr
                )
            } else if field.is_map(ctx) {
                field_expr
            } else if field.is_repeated() && !field.is_packed(ctx) {
                crate::call_expr!(
                    crate::member_expr_bare!(accessor(host.clone(), field, false), "push"),
                    vec![crate::expr_or_spread!(field_expr)]
                )
            } else {
                crate::assign_expr!(
                    AssignTarget::Simple(SimpleAssignTarget::Paren(ParenExpr {
                        span: DUMMY_SP,
                        expr: Box::new(accessor(host.clone(), field, false))
                    })),
                    field_expr
                )
            };

            let stmts = self.deserialize_field_setup_stmts(
                ctx,
                host.clone(),
                field,
                accessor,
                crate::expr_stmt!(read_expr),
                ns,
            );
            let mut stmts = if stmts.len() > 1 {
                vec![Stmt::Block(BlockStmt {
                    span: DUMMY_SP,
                    stmts,
                })]
            } else {
                stmts
            };
            stmts.push(Stmt::Break(swc_ecma_ast::BreakStmt {
                span: DUMMY_SP,
                label: None,
            }));

            cases.push(SwitchCase {
                span: DUMMY_SP,
                test: Some(Box::new(crate::lit_num!(field.number()).into())),
                cons: stmts,
            })
        }

        cases.push(SwitchCase {
            span: DUMMY_SP,
            test: None,
            cons: vec![
                crate::expr_stmt!(crate::call_expr!(
                    crate::member_expr_bare!(reader_expr.clone(), "skipType"),
                    vec![crate::expr_or_spread!(crate::bin_expr!(
                        Expr::Ident(quote_ident!("t")),
                        crate::lit_num!(7).into(),
                        BinaryOp::BitAnd
                    )),]
                )),
                Stmt::Break(swc_ecma_ast::BreakStmt {
                    span: DUMMY_SP,
                    label: None,
                }),
            ],
        });

        let pos_expr = crate::member_expr_bare!(reader_expr.clone(), "pos");
        let while_test_expr =
            crate::bin_expr!(pos_expr, Expr::Ident(quote_ident!("$c")), BinaryOp::Lt);

        Stmt::While(WhileStmt {
            span: DUMMY_SP,
            test: Box::new(while_test_expr),
            body: Box::new(Stmt::Block(BlockStmt {
                span: DUMMY_SP,
                stmts: vec![
                    Stmt::Decl(Decl::Var(Box::new(VarDecl {
                        span: DUMMY_SP,
                        kind: VarDeclKind::Let,
                        declare: false,
                        decls: vec![VarDeclarator {
                            span: DUMMY_SP,
                            name: swc_ecma_ast::Pat::Ident(BindingIdent {
                                id: quote_ident!("t"),
                                type_ann: None,
                            }),
                            init: Some(Box::new(crate::call_expr!(crate::member_expr_bare!(
                                reader_expr.clone(),
                                "uint32"
                            )))),
                            definite: false,
                        }],
                    }))),
                    Stmt::Switch(swc_ecma_ast::SwitchStmt {
                        span: DUMMY_SP,
                        discriminant: Box::new(crate::bin_expr!(
                            Expr::Ident(quote_ident!("t")),
                            crate::lit_num!(3).into(),
                            BinaryOp::ZeroFillRShift
                        )),
                        cases,
                    }),
                ],
            })),
        })
    }
}

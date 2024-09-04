use std::collections::HashMap;

use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    Accessibility, ArrayLit, AssignTarget, BinaryOp, BindingIdent, BlockStmt, BreakStmt, Class,
    ClassDecl, ClassMember, ClassMethod, ClassProp, Constructor, Decl, ExportDecl, Expr, Function,
    KeyValueProp, MemberExpr, MemberProp, MethodKind, ModuleDecl, ModuleItem, ObjectLit,
    OptChainExpr, Param, ParamOrTsParamProp, Pat, Prop, PropName, PropOrSpread, SimpleAssignTarget,
    Stmt, Str, SwitchCase, SwitchStmt, TsEntityName, TsExprWithTypeArgs, TsInterfaceBody,
    TsInterfaceDecl, TsPropertySignature, TsQualifiedName, TsTupleElement, TsTupleType, TsType,
    TsTypeAnn, TsTypeElement, TsTypeRef,
};
use swc_ecma_utils::quote_ident;

use crate::{
    context::{Context, PropPos},
    descriptor::{self, DescriptorProto, FieldDescriptorProto, OneofDescriptorProto},
    quote_ident_optional,
};

use super::{common::normalize_name, LarkPbRuntime};

impl LarkPbRuntime {
    pub(crate) fn print_decl_interface(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
    ) -> ModuleItem {
        let mut interface_members = vec![];
        let mut oneofs = HashMap::new();
        for member in descriptor.field.clone() {
            if member.has_oneof_index() {
                let _ = oneofs.entry(member.oneof_index()).or_insert_with(|| {
                    let mut fields = descriptor.get_oneof_fields(&member);
                    fields.push(member.clone());
                    fields
                });
            } else {
                interface_members.extend(self.print_prop(ctx, &member, PropPos::Interface));
            }
        }
        let oneof_descs = descriptor.oneof_decl.clone();
        assert_eq!(
            oneof_descs.len(),
            oneofs.len(),
            "Descriptor oneof not all matches found."
        );
        for oneof in oneof_descs.iter() {
            interface_members.push(self.print_oneof_prop(
                ctx,
                descriptor,
                oneof,
                PropPos::Interface,
            ));
        }
        let mut ns: Vec<_> = ctx
            .get_namespace()
            .split('.')
            .map(ToString::to_string)
            .collect();
        ns.pop();
        let interface_ident = quote_ident!(format!("I{}", descriptor.name()));
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::TsInterface(Box::new(TsInterfaceDecl {
                span: DUMMY_SP,
                id: interface_ident.clone(),
                declare: false,
                type_params: None,
                extends: vec![],
                body: TsInterfaceBody {
                    span: DUMMY_SP,
                    body: interface_members
                        .into_iter()
                        .filter_map(|m| {
                            if let ClassMember::ClassProp(ClassProp {
                                key: PropName::Ident(key_name),
                                type_ann,
                                is_optional,
                                ..
                            }) = m
                            {
                                Some(TsTypeElement::TsPropertySignature(TsPropertySignature {
                                    span: DUMMY_SP,
                                    readonly: false,
                                    key: Box::new(Expr::Ident(key_name)),
                                    computed: false,
                                    optional: is_optional,
                                    type_ann,
                                }))
                            } else {
                                None
                            }
                        })
                        .collect(),
                },
            })),
        }))
    }

    pub(crate) fn print_decl_class(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &mut Vec<String>,
    ) -> ModuleItem {
        let ident = quote_ident!(descriptor.name());
        let mut members: Vec<ClassMember> = vec![];
        for member in descriptor.field.clone() {
            if !member.has_oneof_index() {
                members.extend(self.print_prop(ctx, &member, PropPos::Definition));
            }
        }
        let oneof_descs = descriptor.oneof_decl.clone();
        for oneof in oneof_descs.iter() {
            members.push(self.print_oneof_prop(ctx, descriptor, oneof, PropPos::Definition));
        }

        members.push(self.print_meta(ctx, descriptor, ns));

        members.push(self.print_constructor(ctx, descriptor));
        members.push(self.print_create(ctx, descriptor));

        members.extend(self.print_codec_methods(ctx, descriptor));

        let interface_ident = quote_ident!(format!("I{}", descriptor.name()));
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Class(ClassDecl {
                ident: ident.clone(),
                declare: false,
                class: Box::new(Class {
                    span: DUMMY_SP,
                    decorators: vec![],
                    body: members,
                    super_class: None,
                    is_abstract: false,
                    type_params: None,
                    super_type_params: None,
                    implements: vec![TsExprWithTypeArgs {
                        span: DUMMY_SP,
                        expr: Box::new(Expr::Ident(interface_ident.clone())),
                        type_args: None,
                    }],
                }),
            }),
        }))
    }

    pub(crate) fn print_oneof_decls(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
    ) -> Vec<ModuleItem> {
        let mut oneofs = HashMap::new();
        for member in descriptor.field.clone() {
            if member.has_oneof_index() {
                let _ = oneofs.entry(member.oneof_index()).or_insert_with(|| {
                    let mut fields = descriptor.get_oneof_fields(&member);
                    fields.push(member.clone());
                    fields
                });
            }
        }

        descriptor
            .oneof_decl
            .iter()
            .enumerate()
            .flat_map(|(idx, oneof)| {
                self.print_oneof_types(
                    ctx,
                    descriptor,
                    oneof,
                    oneofs.get(&(idx as _)).unwrap(),
                    PropPos::Interface,
                )
            })
            .chain(
                descriptor
                    .oneof_decl
                    .iter()
                    .enumerate()
                    .flat_map(|(idx, oneof)| {
                        self.print_oneof_types(
                            ctx,
                            descriptor,
                            oneof,
                            oneofs.get(&(idx as _)).unwrap(),
                            PropPos::Definition,
                        )
                    }),
            )
            .collect()
    }

    pub(crate) fn print_constructor(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
    ) -> ClassMember {
        let param_ident = if self.is_all_optional(ctx, descriptor) {
            quote_ident_optional!("obj")
        } else {
            quote_ident!("obj")
        };
        let mut assign_stmts = vec![];
        for member in &descriptor.field {
            if !member.has_oneof_index() {
                let has_computed_prop = member.has_computed_prop();
                let field_name = if has_computed_prop {
                    format!("_{}", normalize_name(member.name()))
                } else {
                    normalize_name(member.name())
                };
                let init_value =
                    crate::member_expr!(param_ident.clone(), normalize_name(member.name()));
                let init_value = if !member.is_optional()
                    && !member.is_repeated()
                    && !member.is_map(ctx)
                    && !member.is_primitive()
                    && !member.has_default_value()
                {
                    crate::nonnull_expr!(init_value)
                } else {
                    init_value
                };

                let create_expr = if member.is_map(ctx) || member.is_repeated() {
                    self.print_collection_ctor_mapper(ctx, member, init_value.clone())
                } else {
                    self.print_member_ctor_mapper(ctx, member, init_value.clone())
                };

                let default_value_expr: Expr = if member.is_map(ctx) {
                    crate::new_expr!(Expr::Ident(quote_ident!("Map")))
                } else if member.is_repeated() {
                    Expr::Array(ArrayLit {
                        span: DUMMY_SP,
                        elems: vec![],
                    })
                } else {
                    quote_ident!("undefined").into()
                };

                let init_expr = if member.is_message() {
                    if !member.is_map(ctx) && (member.is_optional() || member.is_repeated()) {
                        crate::cond_expr!(
                            crate::bin_expr!(
                                init_value.clone(),
                                Expr::Ident(quote_ident!("undefined")),
                                swc_ecma_ast::BinaryOp::NotEq
                            ),
                            create_expr,
                            default_value_expr
                        )
                    } else {
                        create_expr
                    }
                } else if member.is_bigint() && ctx.options.bigint_as_long {
                    if member.is_optional() || member.is_repeated() {
                        crate::cond_expr!(
                            crate::bin_expr!(
                                init_value.clone(),
                                Expr::Ident(quote_ident!("undefined")),
                                swc_ecma_ast::BinaryOp::NotEq
                            ),
                            create_expr,
                            default_value_expr
                        )
                    } else {
                        create_expr
                    }
                } else if !member.is_optional() {
                    crate::nonnull_expr!(init_value)
                } else {
                    init_value
                };
                let assign_target = swc_ecma_ast::AssignTarget::Simple(
                    swc_ecma_ast::SimpleAssignTarget::Member(MemberExpr {
                        span: DUMMY_SP,
                        obj: Box::new(Expr::Ident(quote_ident!("this"))),
                        prop: MemberProp::Ident(quote_ident!(field_name)),
                    }),
                );
                assign_stmts.push(crate::expr_stmt!(crate::assign_expr!(
                    assign_target,
                    init_expr,
                    swc_ecma_ast::AssignOp::Assign
                )));
            }
        }
        for (idx, oneof) in descriptor.oneof_decl.iter().enumerate() {
            let variants = descriptor.get_oneof_fields_by_index(idx as _);
            let init_value = crate::member_expr!(param_ident.clone(), normalize_name(oneof.name()));
            let assign_stmt = self.print_oneof_ctor_mapper(
                ctx,
                oneof,
                &variants,
                crate::member_expr!(param_ident.clone(), normalize_name(oneof.name())),
            );
            assign_stmts.push(crate::if_stmt!(
                crate::bin_expr!(
                    init_value.clone(),
                    Expr::Ident(quote_ident!("undefined")),
                    swc_ecma_ast::BinaryOp::NotEq
                ),
                assign_stmt
            ));
        }

        ClassMember::Constructor(Constructor {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!(descriptor.name())),
            params: vec![ParamOrTsParamProp::Param(Param {
                span: DUMMY_SP,
                decorators: vec![],
                pat: crate::pat_ident!(
                    param_ident.clone(),
                    crate::type_annotation!(crate::type_ref!(crate::entity_name_ident!(
                        quote_ident!(format!("I{}", descriptor.name()))
                    )))
                ),
            })],
            body: Some(BlockStmt {
                span: DUMMY_SP,
                stmts: assign_stmts,
            }),
            accessibility: Some(Accessibility::Private),
            is_optional: false,
        })
    }

    pub(crate) fn print_create(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
    ) -> ClassMember {
        let param_ident = if self.is_all_optional(ctx, descriptor) {
            quote_ident_optional!("obj")
        } else {
            quote_ident!("obj")
        };

        let create_expr = crate::new_expr!(
            Expr::Ident(quote_ident!(descriptor.name())),
            vec![crate::expr_or_spread!(Expr::Ident(quote_ident!("obj")))]
        );

        let instance_type = quote_ident!(descriptor.name());
        let interface_type = quote_ident!(format!("I{}", descriptor.name()));
        let input_type = crate::type_union!(
            crate::type_ref!(TsEntityName::Ident(instance_type.clone())),
            crate::type_ref!(TsEntityName::Ident(interface_type.clone()))
        );
        ClassMember::Method(ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("create")),
            function: Box::new(Function {
                params: vec![Param {
                    span: DUMMY_SP,
                    decorators: vec![],
                    pat: crate::pat_ident!(param_ident, input_type),
                }],
                decorators: vec![],
                span: DUMMY_SP,
                body: Some(BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![crate::if_stmt!(
                        crate::bin_expr!(
                            Expr::Ident(quote_ident!("obj")),
                            Expr::Ident(instance_type.clone()),
                            BinaryOp::InstanceOf
                        ),
                        Stmt::Block(BlockStmt {
                            span: DUMMY_SP,
                            stmts: vec![crate::return_stmt!(Expr::Ident(quote_ident!("obj")))]
                        }),
                        Stmt::Block(BlockStmt {
                            span: DUMMY_SP,
                            stmts: vec![crate::return_stmt!(create_expr)],
                        })
                    )],
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: Some(Box::new(crate::type_annotation!(crate::type_ref!(
                    crate::entity_name_ident!(quote_ident!(descriptor.name()))
                )))),
            }),
            kind: MethodKind::Method,
            is_static: true,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        })
    }

    pub(crate) fn print_codec_methods(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
    ) -> Vec<ClassMember> {
        let protobuf = ctx.get_import(Self::RUNTIME_PACKAGE, Some(Self::RUNTIME_PACKAGE_NAME));
        let instance_type = quote_ident!(descriptor.name());
        let _interface_type = quote_ident!(format!("I{}", descriptor.name()));
        let decode_stmts = vec![crate::return_stmt!(crate::new_expr!(crate::call_expr!(
            instance_type.clone().into(),
            vec![crate::expr_or_spread!(crate::call_expr!(
                crate::member_expr_bare!(
                    crate::member_expr_bare!(
                        crate::member_expr!(instance_type.clone(), "$meta"),
                        "decoder"
                    ),
                    "decode"
                ),
                vec![
                    crate::expr_or_spread!(Expr::Ident(quote_ident!("data"))),
                    crate::expr_or_spread!(Expr::Ident(quote_ident!("l"))),
                ]
            ))]
        )))];
        let decode_method = ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("decode")),
            function: Box::new(Function {
                params: vec![
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident!("data"),
                            crate::type_union!(
                                crate::type_ref!("Uint8Array"),
                                TsType::TsTypeRef(TsTypeRef {
                                    span: DUMMY_SP,
                                    type_name: TsEntityName::TsQualifiedName(Box::new(
                                        TsQualifiedName {
                                            left: TsEntityName::Ident(protobuf.clone()),
                                            right: quote_ident!(Self::PB_READER_NAME),
                                        }
                                    )),
                                    type_params: None
                                })
                            )
                        ),
                    },
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident_optional!("l"),
                            crate::type_annotation!(TsType::TsKeywordType(
                                swc_ecma_ast::TsKeywordType {
                                    span: DUMMY_SP,
                                    kind: swc_ecma_ast::TsKeywordTypeKind::TsNumberKeyword
                                }
                            ))
                        ),
                    },
                ],
                decorators: vec![],
                span: DUMMY_SP,
                body: Some(swc_ecma_ast::BlockStmt {
                    span: DUMMY_SP,
                    stmts: decode_stmts,
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: Some(Box::new(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::new(crate::type_ref!(crate::entity_name_ident!(
                        instance_type.clone()
                    ))),
                })),
            }),
            kind: MethodKind::Method,
            is_static: true,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        };

        let writer_type = TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::TsQualifiedName(Box::new(TsQualifiedName {
                left: TsEntityName::Ident(protobuf.clone()),
                right: quote_ident!("Writer"),
            })),
            type_params: None,
        });
        let encode_stmts = vec![crate::return_stmt!(crate::call_expr!(
            crate::member_expr_bare!(
                crate::member_expr_bare!(
                    crate::member_expr!(instance_type.clone(), "$meta"),
                    "encoder"
                ),
                "encode"
            ),
            vec![
                crate::expr_or_spread!(Expr::Ident(quote_ident!("req"))),
                crate::expr_or_spread!(Expr::Ident(quote_ident!("w"))),
            ]
        ))];
        let encode_method = ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("encode")),
            function: Box::new(Function {
                params: vec![
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident!("req"),
                            crate::type_annotation!(crate::type_ref!(instance_type.clone().into()))
                        ),
                    },
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident_optional!("w"),
                            crate::type_annotation!(writer_type.clone())
                        ),
                    },
                ],
                decorators: vec![],
                span: DUMMY_SP,
                body: Some(swc_ecma_ast::BlockStmt {
                    span: DUMMY_SP,
                    stmts: encode_stmts,
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: Some(Box::new(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::new(writer_type.clone()),
                })),
            }),
            kind: MethodKind::Method,
            is_static: true,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        };
        vec![
            ClassMember::Method(decode_method),
            ClassMember::Method(encode_method),
        ]
    }

    pub(crate) fn print_oneof_ctor_mapper(
        &self,
        ctx: &mut Context,
        oneof: &OneofDescriptorProto,
        variants: &[&FieldDescriptorProto],
        init_value: Expr,
    ) -> Stmt {
        let assign_left = MemberExpr {
            span: DUMMY_SP,
            obj: Box::new(Expr::Ident(quote_ident!("this"))),
            prop: MemberProp::Ident(quote_ident!(normalize_name(oneof.name()))),
        };
        let cases = variants
            .iter()
            .map(|field| {
                let init_value =
                    crate::member_expr_bare!(init_value.clone(), normalize_name(field.name()));
                let variant_expr = Expr::Object(ObjectLit {
                    span: DUMMY_SP,
                    props: vec![
                        PropOrSpread::Prop(Box::new(swc_ecma_ast::Prop::KeyValue(KeyValueProp {
                            key: PropName::Ident(quote_ident!("$oneofKind")),
                            value: Box::new(Expr::Lit(swc_ecma_ast::Lit::Str(
                                normalize_name(field.name()).into(),
                            ))),
                        }))),
                        PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: PropName::Ident(quote_ident!(
                                normalize_name(field.name()).as_str()
                            )),
                            value: Box::new(self.print_member_ctor_mapper(
                                ctx,
                                field,
                                init_value.clone(),
                            )),
                        }))),
                    ],
                });
                let assign_target =
                    AssignTarget::Simple(SimpleAssignTarget::Member(assign_left.clone()));
                SwitchCase {
                    span: DUMMY_SP,
                    test: Some(Box::new(Expr::Lit(swc_ecma_ast::Lit::Str(Str {
                        span: DUMMY_SP,
                        value: normalize_name(field.name()).into(),
                        raw: None,
                    })))),
                    cons: vec![
                        crate::expr_stmt!(crate::assign_expr!(
                            assign_target,
                            variant_expr,
                            swc_ecma_ast::AssignOp::Assign
                        )),
                        Stmt::Break(BreakStmt {
                            span: DUMMY_SP,
                            label: None,
                        }),
                    ],
                }
            })
            .collect();
        Stmt::Switch(SwitchStmt {
            span: DUMMY_SP,
            discriminant: Box::new(crate::member_expr_bare!(init_value.clone(), "$oneofKind")),
            cases,
        })
    }

    fn print_bigint_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        if member.is_bigint() && ctx.options.bigint_as_long {
            crate::call_expr!(
                crate::member_expr!("Long", "fromValue"),
                vec![crate::expr_or_spread!(init_value)]
            )
        } else {
            init_value
        }
    }

    fn print_message_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        crate::call_expr!(
            crate::member_expr!(ctx.lazy_type_ref(member.type_name()), "create"),
            vec![crate::expr_or_spread!(init_value)]
        )
    }

    fn print_member_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        if member.is_message() {
            self.print_message_ctor_mapper(ctx, member, init_value)
        } else if member.is_bigint() {
            self.print_bigint_ctor_mapper(ctx, member, init_value)
        } else if member.is_map(ctx) {
            self.print_map_ctor_mapper(ctx, member, init_value)
        } else {
            init_value
        }
    }

    fn print_collection_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        if member.is_map(ctx) {
            self.print_map_ctor_mapper(ctx, member, init_value)
        } else if member.is_repeated() {
            self.print_repeated_ctor_mapper(ctx, member, init_value)
        } else {
            init_value
        }
    }

    fn print_repeated_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        let mapper_value_ident = quote_ident!("$v");
        let item_mapper =
            self.print_member_ctor_mapper(ctx, member, Expr::Ident(mapper_value_ident.clone()));
        crate::call_expr!(
            Expr::OptChain(OptChainExpr {
                span: DUMMY_SP,
                base: Box::new(swc_ecma_ast::OptChainBase::Member(MemberExpr {
                    span: DUMMY_SP,
                    obj: Box::new(init_value),
                    prop: swc_ecma_ast::MemberProp::Ident(quote_ident!("map"))
                })),
                optional: false,
            }),
            vec![crate::expr_or_spread!(crate::arrow_func_short!(
                item_mapper,
                vec![crate::pat_ident!(mapper_value_ident.clone())]
            ))]
        )
    }

    fn print_map_ctor_mapper(
        &self,
        ctx: &mut Context,
        member: &descriptor::FieldDescriptorProto,
        init_value: Expr,
    ) -> Expr {
        let map_desc = ctx
            .get_map_type(member.type_name())
            .expect(&format!("cannot get map type: {}", member.type_name()));

        let key_field = &map_desc.field[0];
        let value_field = &map_desc.field[1];
        // obj?.xx ?? new Map
        let nonnull_val = crate::bin_expr!(
            init_value.clone(),
            crate::new_expr!(quote_ident!("Map").into()),
            BinaryOp::NullishCoalescing
        );
        let key_mapper: Expr = if key_field.is_message() {
            crate::call_expr!(
                crate::member_expr!(ctx.lazy_type_ref(key_field.type_name()), "create"),
                vec![crate::expr_or_spread!(Expr::Ident(quote_ident!("key")))]
            )
        } else {
            Expr::Ident(quote_ident!("key"))
        };
        let value_mapper: Expr = if value_field.is_message() {
            crate::call_expr!(
                crate::member_expr!(ctx.lazy_type_ref(value_field.type_name()), "create"),
                vec![crate::expr_or_spread!(Expr::Ident(quote_ident!("value")))]
            )
        } else {
            Expr::Ident(quote_ident!("value"))
        };
        let entry_ident = quote_ident!("entry");
        let key_init_value =
            crate::member_expr_computed!(entry_ident.clone().into(), crate::lit_num!(0).into());
        let value_init_value =
            crate::member_expr_computed!(entry_ident.clone().into(), crate::lit_num!(1).into());
        let mapper_fn_expr = crate::arrow_func!(
            vec![Pat::Ident(BindingIdent {
                id: entry_ident.clone(),
                type_ann: Some(Box::new(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::new(TsType::TsTupleType(TsTupleType {
                        span: DUMMY_SP,
                        elem_types: vec![
                            TsTupleElement {
                                span: DUMMY_SP,
                                label: None,
                                ty: Box::new(key_field.interface_ts_type(ctx).unwrap(),),
                            },
                            TsTupleElement {
                                span: DUMMY_SP,
                                label: None,
                                ty: Box::new(value_field.interface_ts_type(ctx).unwrap(),),
                            },
                        ],
                    })),
                })),
            })],
            vec![
                Stmt::Decl(crate::const_decl!("key", key_init_value)),
                Stmt::Decl(crate::const_decl!("value", value_init_value)),
                crate::return_stmt!(Expr::Array(ArrayLit {
                    span: DUMMY_SP,
                    elems: vec![
                        Some(crate::expr_or_spread!(key_mapper)),
                        Some(crate::expr_or_spread!(value_mapper))
                    ]
                })),
            ]
        );
        crate::new_expr!(crate::call_expr!(
            Expr::Ident(quote_ident!("Map")),
            vec![crate::expr_or_spread!(crate::call_expr!(
                crate::member_expr!("Array", "from"),
                vec![
                    crate::expr_or_spread!(nonnull_val.into()),
                    crate::expr_or_spread!(mapper_fn_expr)
                ]
            ))]
        ))
    }

    pub(crate) fn is_all_optional(&self, ctx: &mut Context, descriptor: &DescriptorProto) -> bool {
        descriptor
            .field
            .iter()
            .all(|f| f.is_optional() || f.is_map(ctx) || f.is_repeated() || f.has_oneof_index())
    }
}

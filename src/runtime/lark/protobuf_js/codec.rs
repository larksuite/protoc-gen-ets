use std::{iter::once, vec};

use swc_common::DUMMY_SP;
use swc_ecma_ast::{
    Accessibility, ArrayLit, BinaryOp, BindingIdent, BlockStmt, Class, ClassMember, ClassMethod,
    ClassProp, Decl, Decorator, ExportDecl, Expr, ExprStmt, Function, Ident, KeyValueProp,
    MethodKind, ModuleDecl, ModuleItem, ObjectLit, Param, Pat, Prop, PropName, PropOrSpread, Stmt,
    TsEntityName, TsExprWithTypeArgs, TsLit, TsLitType, TsPropertySignature, TsQualifiedName,
    TsType, TsTypeAliasDecl, TsTypeAnn, TsTypeElement, TsTypeParamInstantiation, TsTypeRef,
    TsUnionOrIntersectionType, TsUnionType,
};
use swc_ecma_utils::quote_ident;

use crate::{
    context::{Context, PropPos, Syntax},
    descriptor::{
        self, field_descriptor_proto::Type, DescriptorProto, FieldDescriptorProto,
        OneofDescriptorProto,
    },
    quote_ident_optional,
    runtime::Runtime,
};
use heck::ToUpperCamelCase;

use super::{
    common::{normalize_name, normalize_type_name, ns_name},
    LarkPbRuntime,
};

pub type FieldAccessor = fn(Expr, &FieldDescriptorProto, bool) -> Expr;

fn computed_field_name(name: &str, has_computed_prop: bool) -> String {
    if has_computed_prop {
        format!("_{}", normalize_name(name))
    } else {
        normalize_name(name)
    }
}

pub(super) fn field_accesscor(
    _host: Expr,
    field: &FieldDescriptorProto,
    has_computed_prop: bool,
) -> Expr {
    Expr::Ident(quote_ident!(computed_field_name(
        field.name(),
        has_computed_prop
    )))
}

#[allow(dead_code)]
pub(super) fn bare_field_accessor(
    host: Expr,
    _field: &FieldDescriptorProto,
    _has_computed_prop: bool,
) -> Expr {
    host
}

pub(super) fn host_field_accessor(
    host: Expr,
    field: &FieldDescriptorProto,
    has_computed_prop: bool,
) -> Expr {
    crate::member_expr_bare!(host, computed_field_name(field.name(), has_computed_prop))
}

pub(super) fn serialize_field_accessor(
    _: Expr,
    field: &FieldDescriptorProto,
    has_computed_prop: bool,
) -> Expr {
    crate::member_expr!(
        quote_ident!("$req"),
        computed_field_name(field.name(), has_computed_prop)
    )
}

impl LarkPbRuntime {
    pub(super) fn default_value_bin_expr(
        &self,
        ctx: &mut Context,
        host: Expr,
        descriptor: &DescriptorProto,
        field: &FieldDescriptorProto,
        accessor: FieldAccessor,
    ) -> Expr {
        let has_computed_prop = field.has_computed_prop();
        let field_expr: Expr = if field.has_oneof_index() {
            let oneof = &descriptor.oneof_decl[field.oneof_index() as usize];
            crate::member_expr_bare!(host.clone(), normalize_name(oneof.name()))
        } else {
            accessor(host.clone(), field, has_computed_prop)
        };
        let neq_undefined_check = crate::bin_expr!(
            field_expr,
            quote_ident!("undefined").into(),
            BinaryOp::NotEqEq
        );
        if field.has_oneof_index() {
            return neq_undefined_check;
        }

        if field.is_map(ctx) {
            crate::bin_expr!(
                neq_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(accessor(host, field, has_computed_prop), "size"),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else if (field.is_bytes() && ctx.syntax == &Syntax::Proto3) || field.is_repeated() {
            crate::bin_expr!(
                neq_undefined_check,
                crate::bin_expr!(
                    crate::member_expr_bare!(
                        accessor(host.clone(), field, has_computed_prop),
                        "length"
                    ),
                    Expr::Lit(crate::lit_num!(0)),
                    BinaryOp::NotEqEq
                )
            )
        } else {
            neq_undefined_check
        }
    }

    #[allow(dead_code)]
    pub(super) fn print_unknown_fields(&self, _descriptor: &DescriptorProto) -> ClassMember {
        ClassMember::ClassProp(swc_ecma_ast::ClassProp {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("$unknown_fields")),
            value: Some(Box::new(Expr::Array(ArrayLit {
                span: DUMMY_SP,
                elems: vec![],
            }))),
            type_ann: None,
            is_static: false,
            decorators: vec![],
            accessibility: Some(Accessibility::Private),
            is_abstract: false,
            is_optional: false,
            is_override: false,
            readonly: false,
            declare: false,
            definite: false,
        })
    }

    pub(super) fn codec_fn_name(&self, field: &FieldDescriptorProto) -> String {
        match field.type_() {
            Type::TYPE_BOOL => "bool",
            Type::TYPE_DOUBLE => "double",
            Type::TYPE_FLOAT => "float",
            Type::TYPE_ENUM => "int32",

            Type::TYPE_INT32 => "int32",
            Type::TYPE_INT64 => "int64",
            Type::TYPE_UINT32 => "uint32",
            Type::TYPE_UINT64 => "uint64",
            Type::TYPE_SINT32 => "sint32",
            Type::TYPE_SINT64 => "sint64",

            Type::TYPE_FIXED32 => "fixed32",
            Type::TYPE_FIXED64 => "fixed64",
            Type::TYPE_SFIXED32 => "sfixed32",
            Type::TYPE_SFIXED64 => "sfixed64",

            Type::TYPE_STRING => "string",
            Type::TYPE_BYTES => "bytes",
            ty => unimplemented!("unsupported decode type: {:?}", ty),
        }
        .to_string()
    }

    pub(super) fn print_prop(
        &self,
        ctx: &mut Context<'_>,
        field: &descriptor::FieldDescriptorProto,
        pos: PropPos,
    ) -> Vec<ClassMember> {
        fn prop_name_ident(
            ctx: &Context,
            field_name: &str,
            pos: PropPos,
            field: &descriptor::FieldDescriptorProto,
        ) -> Ident {
            match pos {
                PropPos::Definition => {
                    if field.is_map(ctx) || field.is_repeated() {
                        quote_ident!(field_name)
                    } else if field.is_optional() {
                        quote_ident_optional!(field_name)
                    } else {
                        quote_ident!(field_name)
                    }
                }
                PropPos::Interface => {
                    if field.is_optional()
                        || field.is_map(ctx)
                        || field.is_repeated()
                        || field.has_oneof_index()
                    {
                        crate::quote_ident_optional!(field_name)
                    } else {
                        quote_ident!(field_name)
                    }
                }
            }
        }

        let has_computed_prop = matches!(pos, PropPos::Definition) && field.has_computed_prop();
        let field_name = if has_computed_prop {
            format!("_{}", normalize_name(field.name()))
        } else {
            normalize_name(field.name())
        };
        let value: Expr = if ctx.syntax == &Syntax::Proto3
            || field.is_repeated()
            || field.is_map(ctx)
            || field.has_default_value()
        {
            field.default_value_expr(ctx, false)
        } else if field.is_primitive() {
            field.type_default_value(ctx)
        } else {
            Expr::Ident(quote_ident!("undefined"))
        };

        let type_ann = match pos {
            PropPos::Interface if field.is_message() => field.interface_type_ann(ctx),
            _ => field.type_annotation(ctx),
        };

        let field_ident = prop_name_ident(ctx, &field_name, pos, field);
        let mut members = vec![ClassMember::ClassProp(ClassProp {
            span: DUMMY_SP,
            key: PropName::Ident(field_ident.clone()),
            value: None,
            type_ann: type_ann.clone(),
            is_static: false,
            decorators: vec![],
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
            readonly: false,
            declare: false,
            definite: false,
        })];

        if has_computed_prop {
            let computed_prop_ident = quote_ident!(normalize_name(field.name()));
            let getter = ClassMember::Method(ClassMethod {
                span: DUMMY_SP,
                key: PropName::Ident(computed_prop_ident.clone()),
                function: Box::new(Function {
                    params: vec![],
                    decorators: vec![],
                    span: DUMMY_SP,
                    body: Some(BlockStmt {
                        span: DUMMY_SP,
                        stmts: vec![crate::return_stmt!(crate::bin_expr!(
                            crate::member_expr!("this", field_name.clone()),
                            value.clone(),
                            BinaryOp::NullishCoalescing
                        ))],
                    }),
                    is_generator: false,
                    is_async: false,
                    type_params: None,
                    return_type: type_ann.clone(),
                }),
                kind: MethodKind::Getter,
                is_static: false,
                accessibility: Some(Accessibility::Public),
                is_abstract: false,
                is_optional: false,
                is_override: false,
            });
            let setter = ClassMember::Method(ClassMethod {
                span: DUMMY_SP,
                key: PropName::Ident(computed_prop_ident.clone()),
                function: Box::new(Function {
                    params: vec![Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: Pat::Ident(BindingIdent {
                            id: quote_ident!("v"),
                            type_ann: field.nullish_type_annotation(ctx),
                        }),
                    }],
                    decorators: vec![],
                    span: DUMMY_SP,
                    body: Some(BlockStmt {
                        span: DUMMY_SP,
                        stmts: vec![Stmt::Expr(ExprStmt {
                            span: DUMMY_SP,
                            expr: Box::new(crate::assign_expr!(
                                crate::assign_target!("this", field_name.clone()),
                                quote_ident!("v").into()
                            )),
                        })],
                    }),
                    is_generator: false,
                    is_async: false,
                    type_params: None,
                    return_type: None,
                }),
                kind: MethodKind::Setter,
                is_static: false,
                accessibility: Some(Accessibility::Public),
                is_abstract: false,
                is_optional: false,
                is_override: false,
            });
            members.push(getter);
            members.push(setter);
        }

        members
    }

    pub(super) fn print_oneof_prop(
        &self,
        _ctx: &mut Context,
        descriptor: &DescriptorProto,
        oneof: &OneofDescriptorProto,
        pos: PropPos,
    ) -> ClassMember {
        let ty_ann = crate::type_annotation!(self.oneof_type(descriptor, oneof, pos));
        ClassMember::ClassProp(ClassProp {
            span: DUMMY_SP,
            key: quote_ident_optional!(normalize_name(oneof.name())).into(),
            value: None,
            type_ann: Some(Box::new(ty_ann)),
            is_static: false,
            decorators: vec![],
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
            readonly: false,
            declare: false,
            definite: false,
        })
    }

    pub(super) fn print_oneof_types(
        &self,
        ctx: &Context,
        descriptor: &DescriptorProto,
        oneof: &OneofDescriptorProto,
        fields: &[FieldDescriptorProto],
        pos: PropPos,
    ) -> Vec<ModuleItem> {
        let oneof_variant_decls = fields.iter().map(|field| {
            ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                span: DUMMY_SP,
                decl: Decl::TsInterface(Box::new(swc_ecma_ast::TsInterfaceDecl {
                    span: DUMMY_SP,
                    id: self.oneof_variant_type_name(oneof, field, pos),
                    declare: false,
                    type_params: None,
                    extends: vec![],
                    body: swc_ecma_ast::TsInterfaceBody {
                        span: DUMMY_SP,
                        body: vec![
                            TsTypeElement::TsPropertySignature(TsPropertySignature {
                                span: DUMMY_SP,
                                readonly: false,
                                key: quote_ident!("$oneofKind").into(),
                                computed: false,
                                optional: false,
                                type_ann: Some(Box::new(TsTypeAnn {
                                    span: DUMMY_SP,
                                    type_ann: Box::new(TsType::TsLitType(TsLitType {
                                        span: DUMMY_SP,
                                        lit: TsLit::Str(swc_ecma_ast::Str {
                                            span: DUMMY_SP,
                                            value: normalize_name(field.name()).into(),
                                            raw: None,
                                        }),
                                    })),
                                })),
                            }),
                            TsTypeElement::TsPropertySignature(TsPropertySignature {
                                span: DUMMY_SP,
                                readonly: false,
                                key: quote_ident!(normalize_name(field.name())).into(),
                                computed: false,
                                optional: false,
                                type_ann: match pos {
                                    PropPos::Interface => field.interface_type_ann(ctx),
                                    PropPos::Definition => field.type_annotation(ctx),
                                },
                            }),
                        ],
                    },
                })),
            }))
        });
        let oneof_union_decl = ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::TsTypeAlias(Box::new(TsTypeAliasDecl {
                span: DUMMY_SP,
                declare: false,
                id: self.oneof_type_name(oneof, pos),
                type_params: None,
                type_ann: Box::new(TsType::TsUnionOrIntersectionType(
                    TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                        span: DUMMY_SP,
                        types: fields
                            .iter()
                            .map(|f| {
                                Box::new(self.oneof_variant_type(ctx, descriptor, oneof, f, pos))
                            })
                            .collect(),
                    }),
                )),
            })),
        }));
        oneof_variant_decls
            .chain(std::iter::once(oneof_union_decl))
            .collect()
    }

    pub(crate) fn oneof_type_name(&self, oneof: &OneofDescriptorProto, pos: PropPos) -> Ident {
        quote_ident!(if matches!(pos, PropPos::Interface) {
            format!("I{}", oneof.name().to_upper_camel_case())
        } else {
            oneof.name().to_upper_camel_case()
        })
    }

    pub(crate) fn oneof_variant_type(
        &self,
        ctx: &Context,
        descriptor: &DescriptorProto,
        oneof: &OneofDescriptorProto,
        variant_field: &FieldDescriptorProto,
        pos: PropPos,
    ) -> TsType {
        let ty_name = self.oneof_variant_type_name(oneof, variant_field, pos);
        TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: if ctx.is_codec() {
                TsEntityName::TsQualifiedName(Box::new(TsQualifiedName {
                    left: TsEntityName::Ident(quote_ident!(descriptor.name())),
                    right: ty_name,
                }))
            } else {
                TsEntityName::Ident(ty_name)
            },
            type_params: None,
        })
    }

    pub(crate) fn oneof_variant_type_name(
        &self,
        oneof: &OneofDescriptorProto,
        variant_field: &FieldDescriptorProto,
        pos: PropPos,
    ) -> Ident {
        match pos {
            PropPos::Definition => quote_ident!(format!(
                "${}_{}",
                normalize_type_name(oneof.name()),
                normalize_type_name(variant_field.name())
            )),
            PropPos::Interface => quote_ident!(format!(
                "${}_I{}",
                normalize_type_name(oneof.name()),
                normalize_type_name(variant_field.name())
            )),
        }
    }

    pub(crate) fn oneof_type(
        &self,
        descriptor: &DescriptorProto,
        oneof: &OneofDescriptorProto,
        pos: PropPos,
    ) -> TsType {
        TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::TsQualifiedName(Box::new(TsQualifiedName {
                left: TsEntityName::Ident(quote_ident!(descriptor.name())),
                right: self.oneof_type_name(oneof, pos),
            })),
            type_params: None,
        })
    }

    pub(super) fn print_decoder(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &mut Vec<String>,
    ) -> ModuleItem {
        let lark = ctx.get_import(Self::LARK_PACKAGE, Some(Self::LARK_PACKAGE_NAME));
        let reader = ctx.get_import_from(Self::RUNTIME_PACKAGE, Self::PB_READER_NAME);
        let interface_type =
            ctx.lazy_decl_type_ref(&ns_name(ns, descriptor.name()), PropPos::Interface);
        let implement = TsExprWithTypeArgs {
            span: DUMMY_SP,
            expr: Box::new(crate::member_expr!(lark, "Decoder")),
            type_args: Some(Box::new(swc_ecma_ast::TsTypeParamInstantiation {
                span: DUMMY_SP,
                params: vec![Box::new(TsType::TsTypeRef(TsTypeRef {
                    span: DUMMY_SP,
                    type_name: TsEntityName::Ident(interface_type.clone()),
                    type_params: None,
                }))],
            })),
        };
        let stmts = self.deserialize_setup_inner(ctx, descriptor, ns);
        let decode_method = ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("decode")),
            function: Box::new(swc_ecma_ast::Function {
                params: vec![
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident!("$data"),
                            crate::type_union!(
                                crate::type_ref!("Uint8Array"),
                                TsType::TsTypeRef(TsTypeRef {
                                    span: DUMMY_SP,
                                    type_name: TsEntityName::Ident(reader.clone()),
                                    type_params: None
                                })
                            )
                        ),
                    },
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident_optional!("$l"),
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
                    stmts,
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: Some(Box::new(TsTypeAnn {
                    span: DUMMY_SP,
                    type_ann: Box::new(crate::type_ref!(crate::entity_name_ident!(
                        interface_type.clone()
                    ))),
                })),
            }),
            kind: MethodKind::Method,
            is_static: false,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        };
        let class = Class {
            span: DUMMY_SP,
            decorators: vec![Decorator {
                span: DUMMY_SP,
                expr: Box::new(Expr::Ident(quote_ident!("Sendable"))),
            }],
            body: vec![ClassMember::Method(decode_method)],
            super_class: None,
            is_abstract: false,
            type_params: None,
            super_type_params: None,
            implements: vec![implement],
        };
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Class(swc_ecma_ast::ClassDecl {
                ident: quote_ident!("$Decoder"),
                declare: false,
                class: Box::new(class),
            }),
        }))
    }

    pub(super) fn print_encoder(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &mut Vec<String>,
    ) -> ModuleItem {
        let lark = ctx.get_import(Self::LARK_PACKAGE, Some(Self::LARK_PACKAGE_NAME));
        let writer = ctx.get_import_from(Self::RUNTIME_PACKAGE, Self::PB_WRITER_NAME);
        let instance_type =
            ctx.lazy_decl_type_ref(&ns_name(ns, descriptor.name()), PropPos::Definition);
        let implement = TsExprWithTypeArgs {
            span: DUMMY_SP,
            expr: Box::new(crate::member_expr!(lark, "Encoder")),
            type_args: Some(Box::new(swc_ecma_ast::TsTypeParamInstantiation {
                span: DUMMY_SP,
                params: vec![Box::new(TsType::TsTypeRef(TsTypeRef {
                    span: DUMMY_SP,
                    type_name: TsEntityName::Ident(instance_type.clone()),
                    type_params: None,
                }))],
            })),
        };
        let stmts = self.to_binary(ctx, descriptor);

        let writer_type = TsType::TsTypeRef(TsTypeRef {
            span: DUMMY_SP,
            type_name: TsEntityName::Ident(writer),
            type_params: None,
        });

        let encode_method = ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("encode")),
            function: Box::new(swc_ecma_ast::Function {
                params: vec![
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident!("$req"),
                            crate::type_annotation!(crate::type_ref!(instance_type.clone().into()))
                        ),
                    },
                    Param {
                        span: DUMMY_SP,
                        decorators: vec![],
                        pat: crate::pat_ident!(
                            quote_ident_optional!("$w"),
                            crate::type_annotation!(writer_type.clone())
                        ),
                    },
                ],
                decorators: vec![],
                span: DUMMY_SP,
                body: Some(swc_ecma_ast::BlockStmt {
                    span: DUMMY_SP,
                    stmts,
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
            is_static: false,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        };
        let class = Class {
            span: DUMMY_SP,
            decorators: vec![Decorator {
                span: DUMMY_SP,
                expr: Box::new(Expr::Ident(quote_ident!("Sendable"))),
            }],
            body: vec![ClassMember::Method(encode_method)],
            super_class: None,
            is_abstract: false,
            type_params: None,
            super_type_params: None,
            implements: vec![implement],
        };
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Class(swc_ecma_ast::ClassDecl {
                ident: quote_ident!("$Encoder"),
                declare: false,
                class: Box::new(class),
            }),
        }))
    }

    pub(super) fn print_creator(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &[String],
    ) -> ModuleItem {
        let lark = ctx.get_import(Self::LARK_PACKAGE, Some(Self::LARK_PACKAGE_NAME));
        let interface_type =
            ctx.lazy_decl_type_ref(&ns_name(ns, descriptor.name()), PropPos::Interface);
        let instance_type =
            ctx.lazy_decl_type_ref(&ns_name(ns, descriptor.name()), PropPos::Definition);
        let implement = TsExprWithTypeArgs {
            span: DUMMY_SP,
            expr: Box::new(crate::member_expr!(lark, "Creator")),
            type_args: Some(Box::new(swc_ecma_ast::TsTypeParamInstantiation {
                span: DUMMY_SP,
                params: vec![
                    Box::new(TsType::TsTypeRef(TsTypeRef {
                        span: DUMMY_SP,
                        type_name: TsEntityName::Ident(interface_type.clone()),
                        type_params: None,
                    })),
                    Box::new(TsType::TsTypeRef(TsTypeRef {
                        span: DUMMY_SP,
                        type_name: TsEntityName::Ident(instance_type.clone()),
                        type_params: None,
                    })),
                ],
            })),
        };
        let pat_ident = if self.is_all_optional(ctx, descriptor) {
            quote_ident_optional!("obj")
        } else {
            quote_ident!("obj")
        };
        let create_method = ClassMethod {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("create")),
            function: Box::new(swc_ecma_ast::Function {
                params: vec![Param {
                    span: DUMMY_SP,
                    decorators: vec![],
                    pat: crate::pat_ident!(
                        pat_ident,
                        crate::type_annotation!(crate::type_ref!(interface_type.clone().into()))
                    ),
                }],
                decorators: vec![],
                span: DUMMY_SP,
                body: Some(swc_ecma_ast::BlockStmt {
                    span: DUMMY_SP,
                    stmts: vec![crate::return_stmt!(crate::call_expr!(
                        crate::member_expr!(instance_type.clone(), "create"),
                        vec![crate::expr_or_spread!(Expr::Ident(quote_ident!("obj")))]
                    ))],
                }),
                is_generator: false,
                is_async: false,
                type_params: None,
                return_type: Some(Box::new(crate::type_annotation!(crate::type_ref!(
                    instance_type.clone().into()
                )))),
            }),
            kind: MethodKind::Method,
            is_static: false,
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
        };
        let class = Class {
            span: DUMMY_SP,
            decorators: vec![],
            body: vec![ClassMember::Method(create_method)],
            super_class: None,
            is_abstract: false,
            type_params: None,
            super_type_params: None,
            implements: vec![implement],
        };
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
            span: DUMMY_SP,
            decl: Decl::Class(swc_ecma_ast::ClassDecl {
                ident: quote_ident!("$Creator"),
                declare: false,
                class: Box::new(class),
            }),
        }))
    }

    pub(super) fn print_meta(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &[String],
    ) -> ClassMember {
        let lark = ctx.get_import(Self::LARK_PACKAGE, Some(Self::LARK_PACKAGE_NAME));
        ClassMember::ClassProp(ClassProp {
            span: DUMMY_SP,
            key: PropName::Ident(quote_ident!("$meta")),
            value: Some(Box::new(Expr::Object(ObjectLit {
                span: DUMMY_SP,
                props: vec![
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(quote_ident!("decoder")),
                        value: Box::new(self.print_decoder_instance(ctx, descriptor, ns)),
                    }))),
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(quote_ident!("encoder")),
                        value: Box::new(self.print_encoder_instance(ctx, descriptor, ns)),
                    }))),
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(quote_ident!("creator")),
                        value: Box::new(self.print_creator_instance(ctx, descriptor, ns)),
                    }))),
                ],
            }))),
            type_ann: Some(Box::new(TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(crate::type_ref!(
                    crate::entity_name_qualified!(TsEntityName::Ident(lark), quote_ident!("Meta")),
                    TsTypeParamInstantiation {
                        span: DUMMY_SP,
                        params: vec![
                            Box::new(crate::type_ref!(crate::entity_name_ident!(quote_ident!(
                                format!("I{}", descriptor.name())
                            )))),
                            Box::new(crate::type_ref!(crate::entity_name_ident!(quote_ident!(
                                descriptor.name()
                            ))))
                        ]
                    }
                )),
            })),
            is_static: true,
            decorators: vec![],
            accessibility: Some(Accessibility::Public),
            is_abstract: false,
            is_optional: false,
            is_override: false,
            readonly: false,
            declare: false,
            definite: false,
        })
    }

    pub(super) fn print_encoder_instance(
        &self,
        ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &[String],
    ) -> Expr {
        let name = ns
            .iter()
            .map(|x| format!("${x}"))
            .chain(once(format!("${}", descriptor.name())))
            .collect::<Vec<_>>()
            .join(".");
        let codec_ident = quote_ident!(name);
        crate::new_expr!(crate::member_expr!(codec_ident, "$Encoder"))
    }

    pub(super) fn print_creator_instance(
        &self,
        _ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &[String],
    ) -> Expr {
        let name = ns
            .iter()
            .map(|x| format!("${x}"))
            .chain(once(format!("${}", descriptor.name())))
            .collect::<Vec<_>>()
            .join(".");
        let codec_ident = quote_ident!(name);
        crate::new_expr!(crate::member_expr!(codec_ident, "$Creator"))
    }

    pub(super) fn print_decoder_instance(
        &self,
        _ctx: &mut Context,
        descriptor: &DescriptorProto,
        ns: &[String],
    ) -> Expr {
        let name = ns
            .iter()
            .map(|x| format!("${x}"))
            .chain(once(format!("${}", descriptor.name())))
            .collect::<Vec<_>>()
            .join(".");
        let codec_ident = quote_ident!(name);
        crate::new_expr!(crate::member_expr!(codec_ident, "$Decoder"))
    }
}

use crate::analyze::ListWrapper;
use std::{
    fmt::{self, Write as _},
    fs, io,
    path::Path,
};
use swc_common::{source_map::Pos, SourceFile};
use swc_ecma_ast as ast;
use swc_ecma_parser as parser;

const DOCS_PAGE: &str = "/dev/null";

pub(crate) fn object_extension_for_resolver(path: &Path, out: &mut String, errs: &mut String) {
    let module = match parse_file(path) {
        Ok(module) => module,
        Err(err) => {
            writeln!(errs, "{}", err).ok();
            return;
        }
    };
    let Some(resolver_fn) = module.body.iter().find_map(|item| match item {
        ast::ModuleItem::ModuleDecl(ast::ModuleDecl::ExportDefaultDecl(decl)) => {
            match &decl.decl {
                ast::DefaultDecl::Fn(func) => Some(func),
                _ => todo!(), // todo expected export default fn
            }
        }
        _ => None, // todo handle
    }) else {
        writeln!(errs, "Missing default export").unwrap();
        return;
    };

    let Some(resolver_name) = resolver_name(&resolver_fn) else {
        writeln!(errs, "Could not determine resolver name").unwrap();
        return;
    };
    let resolver_parent = resolver_parent(&resolver_fn);
    let args = resolver_args(&resolver_fn);
    let Some(resolver_return_type) = resolver_return_type(&resolver_fn) else {
        panic!("no return type")
    };

    writeln!(
        out,
        "extend type {resolver_parent} {{ {resolver_name}{args}: {resolver_return_type} }}"
    )
    .unwrap();
}

fn resolver_args(func: &ast::FnExpr) -> InferredArgs<'_> {
    let mut args = InferredArgs { args: Vec::new() };
    let Some(second_arg) = func.function.params.get(1) else {
        return args;
    };
    // TODO: handle args with defaults
    let type_ann = match &second_arg.pat {
        ast::Pat::Ident(binding_ident) => binding_ident.type_ann.as_deref(),
        ast::Pat::Object(obj_pat) => obj_pat.type_ann.as_deref(),
        _ => return args,
    };
    let Some(type_ann) = type_ann else { return args };
    let Some(type_lit) = type_ann.type_ann.as_ts_type_lit() else {
        return args;
    };

    for field in &type_lit.members {
        match field {
            ast::TsTypeElement::TsPropertySignature(property) => {
                let arg_name = match property.key.as_ref() {
                    ast::Expr::Ident(ident) => ident.sym.as_ref(),
                    _ => {
                        // TODO: error
                        continue;
                    }
                };
                let Some(arg_type) = &property.type_ann else { continue };
                let Some(arg_type) = infer_return_type(arg_type.type_ann.as_ref()) else {
                    continue;
                };

                args.args.push((arg_name, arg_type));
            }
            _ => {
                // TODO: error
                continue;
            }
        }
    }

    // let type_annotation = second_arg.decorators.iter().find_map(|decorator| {
    //     match decorator.try_into {

    //         ast::Decorator
    //     }
    // });
    // let ast::Pat::Object(pat) = second_arg.decorators else {};

    // for arg in &func.function.params {
    //     args.push();
    // }

    args
}

fn resolver_name(func: &ast::FnExpr) -> Option<&str> {
    func.ident.as_ref().map(|ident| ident.sym.as_ref())
}

fn resolver_parent(func: &ast::FnExpr) -> String {
    let first_param = func.function.params.first().unwrap();
    match &first_param.pat {
        ast::Pat::Ident(ast::BindingIdent { id: _, type_ann }) => match type_ann {
            Some(ann) => match ann.type_ann.as_ref() {
                ast::TsType::TsKeywordType(_) => todo!(),
                ast::TsType::TsThisType(_) => todo!(),
                ast::TsType::TsFnOrConstructorType(_) => todo!(),
                ast::TsType::TsTypeRef(typeref) => typeref.type_name.as_ident().unwrap().sym.as_ref().to_owned(),
                ast::TsType::TsTypeQuery(_) => todo!(),
                ast::TsType::TsTypeLit(_) => todo!(),
                ast::TsType::TsArrayType(_) => todo!(),
                ast::TsType::TsTupleType(_) => todo!(),
                ast::TsType::TsOptionalType(_) => todo!(),
                ast::TsType::TsRestType(_) => todo!(),
                ast::TsType::TsUnionOrIntersectionType(_) => todo!(),
                ast::TsType::TsConditionalType(_) => todo!(),
                ast::TsType::TsInferType(_) => todo!(),
                ast::TsType::TsParenthesizedType(_) => todo!(),
                ast::TsType::TsTypeOperator(_) => todo!(),
                ast::TsType::TsIndexedAccessType(_) => todo!(),
                ast::TsType::TsMappedType(_) => todo!(),
                ast::TsType::TsLitType(_) => todo!(),
                ast::TsType::TsTypePredicate(_) => todo!(),
                ast::TsType::TsImportType(_) => todo!(),
            },
            None => todo!(),
        },
        _ => todo!(),
    }
}

fn resolver_return_type(func: &ast::FnExpr) -> Option<InferredType<'_>> {
    infer_return_type(func.function.return_type.as_ref()?.type_ann.as_ref())
}

fn parse_file(path: &Path) -> io::Result<ast::Module> {
    let mut recovered_errors = Vec::new(); // not used by us
    let source_file = path_to_swc_source_file(path)?;
    let module = parser::parse_file_as_module(
        &source_file,
        parser::Syntax::Typescript(parser::TsConfig::default()),
        ast::EsVersion::EsNext,
        None,
        &mut recovered_errors,
    )
    .unwrap();
    Ok(module)
}

fn path_to_swc_source_file(path: &Path) -> io::Result<SourceFile> {
    let src = fs::read_to_string(path)?;
    let file_name = swc_common::FileName::Real(path.to_owned());
    Ok(SourceFile::new(
        file_name.clone(),
        false,
        file_name,
        src,
        swc_common::BytePos::from_u32(1),
    ))
}

fn ts_union_to_graphql_type_name(
    ty: &ast::TsUnionOrIntersectionType,
    is_nullable: bool,
    list_wrappers: Vec<ListWrapper>,
) -> Option<InferredType<'_>> {
    if is_nullable {
        return None;
    }

    let ty = ty.as_ts_union_type()?;
    if ty.types.len() != 2 {
        return None;
    }

    let null_idx = ty.types.iter().position(|ty| ts_type_as_str(&ty) == Some("null"));
    let ty_idx = match null_idx {
        Some(0) => 1,
        Some(1) => 0,
        _ => return None,
    };

    infer_return_type_rec(&ty.types[ty_idx], true, list_wrappers)
}

fn ts_type_as_str(ty: &ast::TsType) -> Option<&str> {
    match ty {
        ast::TsType::TsKeywordType(kw) => Some(match kw.kind {
            ast::TsKeywordTypeKind::TsAnyKeyword => "any",
            ast::TsKeywordTypeKind::TsUnknownKeyword => "unknown",
            ast::TsKeywordTypeKind::TsNumberKeyword => "number",
            ast::TsKeywordTypeKind::TsObjectKeyword => "object",
            ast::TsKeywordTypeKind::TsBooleanKeyword => "boolean",
            ast::TsKeywordTypeKind::TsBigIntKeyword => "BigInt",
            ast::TsKeywordTypeKind::TsStringKeyword => "string",
            ast::TsKeywordTypeKind::TsSymbolKeyword => todo!(),
            ast::TsKeywordTypeKind::TsVoidKeyword => "void",
            ast::TsKeywordTypeKind::TsUndefinedKeyword => "undefined",
            ast::TsKeywordTypeKind::TsNullKeyword => "null",
            ast::TsKeywordTypeKind::TsNeverKeyword => todo!(),
            ast::TsKeywordTypeKind::TsIntrinsicKeyword => todo!(),
        }),
        ast::TsType::TsTypeRef(ty) => match &ty.type_name {
            ast::TsEntityName::Ident(name) => Some(name.sym.as_ref()),
            ast::TsEntityName::TsQualifiedName(_) => None,
        },
        _ => None,
    }
}

fn infer_ts_type_ref(
    type_ref: &ast::TsTypeRef,
    is_nullable: bool,
    mut list_wrappers: Vec<ListWrapper>,
) -> Option<InferredType<'_>> {
    let type_name: Option<&str> = type_ref.type_name.as_ident().map(|ident| ident.sym.as_ref());

    match type_name? {
        "Promise" => {
            let param = type_ref.type_params.as_ref()?.params.first()?;
            infer_return_type_rec(param, is_nullable, list_wrappers)
        }
        "Array" => {
            list_wrappers.push(ListWrapper::NonNullList);
            let param = type_ref.type_params.as_ref()?.params.first()?;
            infer_return_type_rec(param, false, list_wrappers)
        }
        name => Some(InferredType {
            is_nullable,
            name,
            list_wrappers,
        }),
    }
}

fn infer_return_type(ts_type: &ast::TsType) -> Option<InferredType<'_>> {
    infer_return_type_rec(ts_type, false, Vec::new())
}

fn infer_return_type_rec(
    ts_type: &ast::TsType,
    is_nullable: bool,
    mut list_wrappers: Vec<ListWrapper>,
) -> Option<InferredType<'_>> {
    match ts_type {
        ast::TsType::TsKeywordType(kw) => {
            let name = match kw.kind {
                ast::TsKeywordTypeKind::TsAnyKeyword => "any",
                ast::TsKeywordTypeKind::TsUnknownKeyword => "unknown",
                ast::TsKeywordTypeKind::TsNumberKeyword => "number",
                ast::TsKeywordTypeKind::TsObjectKeyword => "object",
                ast::TsKeywordTypeKind::TsBooleanKeyword => "boolean",
                ast::TsKeywordTypeKind::TsBigIntKeyword => "BigInt",
                ast::TsKeywordTypeKind::TsStringKeyword => "string",
                ast::TsKeywordTypeKind::TsSymbolKeyword => todo!(),
                ast::TsKeywordTypeKind::TsVoidKeyword => "void",
                ast::TsKeywordTypeKind::TsUndefinedKeyword => "undefined",
                ast::TsKeywordTypeKind::TsNullKeyword => "null",
                ast::TsKeywordTypeKind::TsNeverKeyword => todo!(),
                ast::TsKeywordTypeKind::TsIntrinsicKeyword => todo!(),
            };
            Some(InferredType {
                name,
                is_nullable,
                list_wrappers,
            })
        }
        ast::TsType::TsTypeRef(type_ref) => infer_ts_type_ref(type_ref, is_nullable, list_wrappers),
        ast::TsType::TsArrayType(ty) => {
            list_wrappers.push(ListWrapper::NonNullList);
            infer_return_type_rec(ty.elem_type.as_ref(), false, list_wrappers)
        }
        ast::TsType::TsUnionOrIntersectionType(ty) => ts_union_to_graphql_type_name(ty, is_nullable, list_wrappers),
        ast::TsType::TsParenthesizedType(ty) => infer_return_type_rec(ty.type_ann.as_ref(), is_nullable, list_wrappers),
        other => todo!("{:?}", other),
    }
}

struct InferredType<'a> {
    name: &'a str,
    is_nullable: bool,
    list_wrappers: Vec<ListWrapper>,
}

impl fmt::Display for InferredType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in &self.list_wrappers {
            f.write_str("[")?;
        }

        f.write_str(self.name)?;
        if !self.is_nullable {
            f.write_str("!")?;
        }

        for wrapper in &self.list_wrappers {
            match wrapper {
                ListWrapper::NullableList => f.write_str("]")?,
                ListWrapper::NonNullList => f.write_str("]!")?,
            }
        }

        Ok(())
    }
}

struct InferredArgs<'a> {
    args: Vec<(&'a str, InferredType<'a>)>,
}

impl fmt::Display for InferredArgs<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.args.is_empty() {
            return Ok(());
        }

        f.write_str("(")?;

        for (name, r#type) in &self.args {
            f.write_str(name)?;
            f.write_str(": ")?;
            fmt::Display::fmt(r#type, f)?;
        }

        f.write_str(")")
    }
}

use std::{fmt::Write as _, fs, io, path::Path};
use swc_common::SourceFile;
use swc_ecma_ast as ast;
use swc_ecma_parser as parser;

pub(crate) fn object_extension_for_resolver(path: &Path, out: &mut String, errs: &mut String) {
    let module = match parse_file(path) {
        Ok(module) => module,
        Err(err) => {
            writeln!(errs, "{}", err).ok();
            return;
        }
    };
    let resolver_fn = module
        .body
        .iter()
        .find_map(|item| match item {
            ast::ModuleItem::ModuleDecl(ast::ModuleDecl::ExportDefaultExpr(expr)) => {
                match expr.expr.as_ref() {
                    ast::Expr::Fn(func) => Some(func),
                    _ => todo!(), // todo expected export default fn
                }
            }
            _ => None, // todo handle
        })
        .unwrap(); // todo handle absence of default export

    let resolver_name = resolver_name(&resolver_fn);
    let mut resolver_parent = resolver_parent(&resolver_fn);
    let mut resolver_return_type = resolver_return_type(&resolver_fn);

    writeln!(
        out,
        "extend {resolver_parent} {{ {resolver_name}: {resolver_return_type} }}"
    )
    .unwrap();
}

fn resolver_name(func: &ast::FnExpr) -> String {
    let ident = func.ident.as_ref().expect("function must have a name");
    ident.to_string()
}

fn resolver_parent(func: &ast::FnExpr) -> String {
    let first_param = func.function.params.first().unwrap();
    match &first_param.pat {
        ast::Pat::Ident(ast::BindingIdent { id: _, type_ann }) => match type_ann {
            Some(ann) => match ann.type_ann.as_ref() {
                ast::TsType::TsKeywordType(_) => todo!(),
                ast::TsType::TsThisType(_) => todo!(),
                ast::TsType::TsFnOrConstructorType(_) => todo!(),
                ast::TsType::TsTypeRef(_) => todo!(),
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

fn resolver_return_type(func: &ast::FnExpr) -> String {
    match &func.function.return_type {
        Some(ty) => match ty.type_ann.as_ref() {
            ast::TsType::TsKeywordType(_) => todo!(),
            ast::TsType::TsThisType(_) => todo!(),
            ast::TsType::TsFnOrConstructorType(_) => todo!(),
            ast::TsType::TsTypeRef(_) => todo!(),
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
    }
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
        swc_common::BytePos::DUMMY,
    ))
}

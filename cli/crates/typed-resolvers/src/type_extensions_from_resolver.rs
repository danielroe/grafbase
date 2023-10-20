use crate::analyze::{AnalyzedSchema, ListWrapper};
use miette::{Diagnostic, LabeledSpan, SourceSpan};
use std::{
    fmt::{self, Write as _},
    fs,
    path::Path,
    rc::Rc,
};
use swc_common::{source_map::Pos, SourceFile, Span};
use swc_ecma_ast as ast;
use swc_ecma_parser as parser;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Default)]
#[error("Could not infer GraphQL field definition for a resolver.")]
#[diagnostic(url("https://grafbase.com/docs/edge-gateway/resolvers"))]
struct AnalysisErrors {
    #[related]
    errors: Vec<miette::Error>,
}

impl AnalysisErrors {
    fn push<T: Diagnostic + Send + Sync + 'static>(&mut self, err: T) {
        self.errors.push(err.into())
    }
}

struct AnalysisContext<'a> {
    graphql_schema: &'a AnalyzedSchema<'a>,
    errors: AnalysisErrors,
}

impl AnalysisContext<'_> {
    fn push<T: Diagnostic + Send + Sync + 'static>(&mut self, err: T) {
        self.errors.push(err)
    }
}

type Ctx<'a> = AnalysisContext<'a>;

pub(crate) fn object_extension_for_resolver(
    path: &Path,
    out: &mut String,
    graphql_schema: &AnalyzedSchema<'_>,
) -> miette::Result<()> {
    let (src, module) = parse_file(path)?;
    handle_module(&module, out, graphql_schema).map_err(|err| {
        err.with_source_code(miette::NamedSource::new(
            path.display().to_string(),
            src.as_str().to_owned(),
        ))
    })
}

fn handle_module(module: &ast::Module, out: &mut String, graphql_schema: &AnalyzedSchema<'_>) -> miette::Result<()> {
    let resolver_fn = find_default_export_function(&module)?;

    let mut ctx = AnalysisContext {
        graphql_schema,
        errors: AnalysisErrors::default(),
    };

    let (Some(resolver_name), Some(resolver_parent), Some(resolver_args), Some(resolver_return_type)) = (
        resolver_name(&resolver_fn, &mut ctx),
        resolver_parent(&resolver_fn, &mut ctx),
        resolver_args(&resolver_fn, &mut ctx),
        resolver_return_type(&resolver_fn, &mut ctx),
    ) else {
        return Err(ctx.errors.into());
    };

    writeln!(
        out,
        "extend type {resolver_parent} {{ {resolver_name}{resolver_args}: {resolver_return_type} }}"
    )
    .unwrap();
    Ok(())
}

fn swc_span_to_miette_span(span: Span) -> SourceSpan {
    SourceSpan::new(
        miette::SourceOffset::from(span.lo.0 as usize),
        miette::SourceOffset::from((span.hi.0 - span.lo.0) as usize),
    )
}

fn find_default_export_function(module: &ast::Module) -> miette::Result<&ast::FnExpr> {
    #[derive(Debug, Error, miette::Diagnostic)]
    #[error("The default export must be a function")]
    struct WrongDefaultExport {
        #[label]
        span: SourceSpan,
    }

    for item in &module.body {
        match item {
            ast::ModuleItem::ModuleDecl(ast::ModuleDecl::ExportDefaultDecl(decl)) => match &decl.decl {
                ast::DefaultDecl::Fn(func) => return Ok(func),
                _ => {
                    return Err(WrongDefaultExport {
                        span: swc_span_to_miette_span(decl.span),
                    }
                    .into())
                }
            },
            _ => (),
        }
    }

    Err(miette::miette!("Missing default export"))
}

fn resolver_args<'a>(func: &'a ast::FnExpr, ctx: &mut Ctx<'_>) -> Option<InferredArgs<'a>> {
    #[derive(Debug, Diagnostic, Error)]
    #[error("Not the right shape for arguments.")]
    struct BadArguments(#[label] SourceSpan);

    #[derive(Debug, Diagnostic, Error)]
    #[error("Not a type literal, needs to be a literal object.")]
    #[diagnostic(help(
        "Define the arguments type as object literal type: `args: {{ argOne: string, argTwo: string }}``"
    ))]
    struct TypeIsNotTypeLit(#[label] SourceSpan);

    let mut args = InferredArgs { args: Vec::new() };
    let Some(second_arg) = func.function.params.get(1) else {
        return Some(args); // no arguments is valid
    };

    // TODO: handle args with defaults
    let type_ann = match &second_arg.pat {
        ast::Pat::Ident(binding_ident) => binding_ident.type_ann.as_deref(),
        ast::Pat::Object(obj_pat) => obj_pat.type_ann.as_deref(),
        _ => {
            ctx.push(BadArguments(swc_span_to_miette_span(second_arg.span)));
            return None;
        }
    };
    let Some(type_ann) = type_ann else {
        ctx.push(BadArguments(swc_span_to_miette_span(second_arg.span)));
        return None;
    };
    let Some(type_lit) = type_ann.type_ann.as_ts_type_lit() else {
        ctx.push(TypeIsNotTypeLit(swc_span_to_miette_span(type_ann.span)));
        return None;
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
                let Some(arg_type) = infer_graphql_type(arg_type.type_ann.as_ref()) else {
                    continue;
                };

                args.args.push((arg_name, arg_type));
            }
            _ => {
                ctx.push(TypeIsNotTypeLit(swc_span_to_miette_span(type_lit.span)));
                continue;
            }
        }
    }

    Some(args)
}

fn resolver_name<'a>(func: &'a ast::FnExpr, ctx: &mut Ctx<'_>) -> Option<&'a str> {
    #[derive(Debug, Error, Diagnostic)]
    #[error("The function must have a name.")]
    #[diagnostic(help("Give a name to the function: `export default async function myField(...)`"))]
    struct ResolverFunctionMustHaveAName(#[label] SourceSpan);

    match func.ident.as_ref().map(|ident| ident.sym.as_ref()) {
        Some(name) => Some(name),
        None => {
            ctx.push(ResolverFunctionMustHaveAName(swc_span_to_miette_span(
                func.function.span,
            )));
            None
        }
    }
}

fn resolver_parent(func: &ast::FnExpr, ctx: &mut Ctx<'_>) -> Option<String> {
    let first_param = func.function.params.first().unwrap();
    let type_ann = match &first_param.pat {
        ast::Pat::Ident(ast::BindingIdent { id: _, type_ann }) => type_ann.as_ref(),
        ast::Pat::Object(obj) => obj.type_ann.as_ref(),
        _ => {
            let first_param_span = swc_span_to_miette_span(first_param.span);
            ctx.push(miette::diagnostic!(
                labels = vec![LabeledSpan::at(
                    first_param_span,
                    "Not a valid first argument for a resolver."
                )],
                "Could not infer what type the resolver is attached to."
            ));
            return None;
        }
    };

    let Some(type_ann) = type_ann else {
        let first_param_span = swc_span_to_miette_span(first_param.span);
        ctx.push(miette::diagnostic!(
            labels = vec![LabeledSpan::at(first_param_span, "Missing type annotation.")],
            "Could not infer what type the resolver is attached to."
        ));
        return None;
    };

    match type_ann.type_ann.as_ref() {
        ast::TsType::TsTypeRef(typeref) => Some(typeref.type_name.as_ident().unwrap().sym.as_ref().to_owned()),
        _ => {
            let span = swc_span_to_miette_span(type_ann.span);
            ctx.push(miette::diagnostic!(
                labels = vec![LabeledSpan::at(span, "")],
                "Could not infer what type the resolver is attached to."
            ));
            None
        }
    }
}

fn resolver_return_type<'a>(func: &'a ast::FnExpr, ctx: &mut Ctx<'_>) -> Option<InferredType<'a>> {
    #[derive(Debug, Error, Diagnostic)]
    #[error("The return type must be specified on resolver functions.")]
    #[diagnostic(help("Add a return type: `function (...): string | null`"))]
    struct MissingReturnType(#[label] SourceSpan);

    match func.function.return_type.as_ref() {
        None => {
            ctx.push(MissingReturnType(swc_span_to_miette_span(func.function.span)));
            None
        }
        Some(return_type) => infer_graphql_type(return_type.type_ann.as_ref()),
    }
}

fn parse_file(path: &Path) -> miette::Result<(Rc<String>, ast::Module)> {
    let mut recovered_errors = Vec::new(); // not used by us
    let source_file = path_to_swc_source_file(path)?;
    let src = source_file.src.clone();

    let module = parser::parse_file_as_module(
        &source_file,
        parser::Syntax::Typescript(parser::TsConfig::default()),
        ast::EsVersion::EsNext,
        None,
        &mut recovered_errors,
    )
    .map_err(|_| {
        miette::miette!(
            severity = miette::Severity::Error,
            "The resolver at {} is not valid TypeScript.",
            path.display(),
        )
    })?;
    Ok((src, module))
}

#[derive(Debug, Error, miette::Diagnostic)]
#[error("Could not read the file.")]
struct CouldNotReadFile;

fn path_to_swc_source_file(path: &Path) -> Result<SourceFile, CouldNotReadFile> {
    let Ok(src) = fs::read_to_string(path) else {
        return Err(CouldNotReadFile);
    };
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

    infer_graphql_type_rec(&ty.types[ty_idx], true, list_wrappers)
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
            ast::TsKeywordTypeKind::TsSymbolKeyword => "symbol",
            ast::TsKeywordTypeKind::TsVoidKeyword => "void",
            ast::TsKeywordTypeKind::TsUndefinedKeyword => "undefined",
            ast::TsKeywordTypeKind::TsNullKeyword => "null",
            ast::TsKeywordTypeKind::TsNeverKeyword => "never",
            ast::TsKeywordTypeKind::TsIntrinsicKeyword => "intrinsic",
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
            infer_graphql_type_rec(param, is_nullable, list_wrappers)
        }
        "Array" => {
            list_wrappers.push(ListWrapper::NonNullList);
            let param = type_ref.type_params.as_ref()?.params.first()?;
            infer_graphql_type_rec(param, false, list_wrappers)
        }
        name => Some(InferredType {
            is_nullable,
            name,
            list_wrappers,
        }),
    }
}

fn infer_graphql_type(ts_type: &ast::TsType) -> Option<InferredType<'_>> {
    infer_graphql_type_rec(ts_type, false, Vec::new())
}

fn infer_graphql_type_rec(
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
                ast::TsKeywordTypeKind::TsStringKeyword => "String",
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
            infer_graphql_type_rec(ty.elem_type.as_ref(), false, list_wrappers)
        }
        ast::TsType::TsUnionOrIntersectionType(ty) => ts_union_to_graphql_type_name(ty, is_nullable, list_wrappers),
        ast::TsType::TsParenthesizedType(ty) => {
            infer_graphql_type_rec(ty.type_ann.as_ref(), is_nullable, list_wrappers)
        }
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

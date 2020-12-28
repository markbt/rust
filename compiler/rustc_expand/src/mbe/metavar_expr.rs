use std::fmt::Display;
use std::str::FromStr;

use rustc_ast::token;
use rustc_ast::tokenstream;
use rustc_ast_pretty::pprust;
use rustc_session::parse::ParseSess;

use rustc_span::symbol::Ident;
use rustc_span::Span;

/// A meta-variable expression, for expansions based on properties of meta-variables.
#[derive(Debug, Clone, PartialEq, Encodable, Decodable)]
crate enum MetaVarExpr {
    /// The index of the repetition at a particular depth, where 0 is the inner-most
    /// repetition.
    Index(usize),

    /// The length of the repetition at a particular depth, where 0 is the inner-most
    /// repetition.
    Length(usize),

    /// Ignore a meta-variable for repetition without expansion.
    Ignore(Ident),

    /// The number of repetitions of an identifier, optionally limited to a number
    /// of repetition depths.  If the depth limit is 0 then the depth is unlimited.
    Count(Ident, usize),
}

impl MetaVarExpr {
    /// Attempt to parse a meta-variable expression from a token stream.
    crate fn parse(input: &tokenstream::TokenStream, sess: &ParseSess) -> Option<MetaVarExpr> {
        let mut tts = input.trees();
        match tts.next() {
            Some(tokenstream::TokenTree::Token(token)) if token.is_ident() => {
                let (ident, _) = token.ident().unwrap();
                let parser = match &*ident.as_str() {
                    "index" => Self::parse_index,
                    "length" => Self::parse_length,
                    "ignore" => Self::parse_ignore,
                    "count" => Self::parse_count,
                    _ => {
                        let msg = "unrecognised metavariable expression";
                        sess.span_diagnostic.span_err(ident.span, msg);
                        return None;
                    }
                };
                parser(ident.span, tts, sess)
            }
            Some(tokenstream::TokenTree::Token(token)) => {
                let msg = format!(
                    "expected meta-variable expression, found `{}`",
                    pprust::token_to_string(&token),
                );
                sess.span_diagnostic.span_err(token.span, &msg);
                None
            }
            Some(tokenstream::TokenTree::Delimited(span, ..)) => {
                let msg = "expected meta-variable expression, found delimited span";
                sess.span_diagnostic.span_err(span.entire(), msg);
                None
            }
            None => None,
        }
    }

    /// Parse a meta-variable `index` expression: `index([depth])`
    fn parse_index(
        span: Span,
        input: impl Iterator<Item = tokenstream::TokenTree>,
        sess: &ParseSess,
    ) -> Option<MetaVarExpr> {
        let mut args = parse_args(span, input, sess).into_iter();
        let depth = args.next().and_then(|arg| arg.parse_literal::<usize>(sess));
        if let Some(arg) = args.next() {
            let msg = "unexpected meta-variable expression argument";
            sess.span_diagnostic.span_err(arg.span(), msg);
        }

        Some(MetaVarExpr::Index(depth.unwrap_or(0)))
    }

    /// Parse a meta-variable `length` expression: `length([depth])`
    fn parse_length(
        span: Span,
        input: impl Iterator<Item = tokenstream::TokenTree>,
        sess: &ParseSess,
    ) -> Option<MetaVarExpr> {
        let mut args = parse_args(span, input, sess).into_iter();
        let depth = args.next().and_then(|arg| arg.parse_literal::<usize>(sess));
        if let Some(arg) = args.next() {
            let msg = "unexpected meta-variable expression argument";
            sess.span_diagnostic.span_err(arg.span(), msg);
            return None;
        }

        Some(MetaVarExpr::Length(depth.unwrap_or(0)))
    }

    /// Parse a meta-variable `ignore` expression: `ignore(ident)`
    fn parse_ignore(
        span: Span,
        input: impl Iterator<Item = tokenstream::TokenTree>,
        sess: &ParseSess,
    ) -> Option<MetaVarExpr> {
        let mut args = parse_args(span, input, sess).into_iter();
        let ident = match args.next() {
            Some(arg) => arg.parse_ident(sess)?,
            None => {
                let msg = "ignore requires an identifier";
                sess.span_diagnostic.span_err(span, msg);
                return None;
            }
        };
        if let Some(arg) = args.next() {
            let msg = "unexpected meta-variable expression argument";
            sess.span_diagnostic.span_err(arg.span(), msg);
            return None;
        }

        Some(MetaVarExpr::Ignore(ident))
    }

    /// Parse a meta-variable `count` expression: `count(ident[, depth])`
    fn parse_count(
        span: Span,
        input: impl Iterator<Item = tokenstream::TokenTree>,
        sess: &ParseSess,
    ) -> Option<MetaVarExpr> {
        let mut args = parse_args(span, input, sess).into_iter();
        let ident = match args.next() {
            Some(arg) => arg.parse_ident(sess)?,
            None => {
                let msg = "count requires an identifier to count";
                sess.span_diagnostic.span_err(span, msg);
                return None;
            }
        };
        let depth = args.next().and_then(|arg| arg.parse_literal::<usize>(sess));
        if let Some(arg) = args.next() {
            let msg = "unexpected meta-variable expression argument";
            sess.span_diagnostic.span_err(arg.span(), msg);
            return None;
        }

        Some(MetaVarExpr::Count(ident, depth.unwrap_or(0)))
    }

    /// The ident referred to in this meta-variable expression, if any.
    crate fn ident(&self) -> Option<&Ident> {
        match self {
            MetaVarExpr::Count(ident, _) | MetaVarExpr::Ignore(ident) => Some(&ident),
            MetaVarExpr::Index(..) | MetaVarExpr::Length(..) => None,
        }
    }
}

/// A meta-variable expression argument.
enum Arg {
    /// An identifer argument.
    Ident(Ident),

    /// A literal argument.
    Literal(token::Lit, Span),
}

impl Arg {
    /// The span that contains the argument.
    fn span(&self) -> Span {
        match *self {
            Arg::Ident(ident) => ident.span.clone(),
            Arg::Literal(_, span) => span.clone(),
        }
    }

    /// Attempt to parse the argument as an identifier.
    fn parse_ident(self, sess: &ParseSess) -> Option<Ident> {
        match self {
            Arg::Ident(ident) => Some(ident),
            Arg::Literal(_, span) => {
                let msg = "expected identifier";
                sess.span_diagnostic.span_err(span, msg);
                None
            }
        }
    }

    /// Attempt to parse the identifier as a literal and convert it
    /// to a parsed type.
    fn parse_literal<T: FromStr>(self, sess: &ParseSess) -> Option<T>
    where
        <T as FromStr>::Err: Display,
    {
        match self {
            Arg::Literal(lit, span) => {
                if lit.suffix.is_some() {
                    let msg = "literal suffixes are not supported in meta-variable expressions";
                    sess.span_diagnostic.span_err(span, msg);
                    return None;
                }
                match lit.symbol.as_str().parse::<T>() {
                    Ok(depth) => Some(depth),
                    Err(e) => {
                        let msg =
                            format!("failed to parse meta-variable expression argument: {}", e);
                        sess.span_diagnostic.span_err(span, &msg);
                        return None;
                    }
                }
            }
            Arg::Ident(ident) => {
                let msg = "expected literal";
                sess.span_diagnostic.span_err(ident.span, msg);
                return None;
            }
        }
    }
}

/// Attempt to parse meta-variable expression arguments.
fn parse_args(
    span: Span,
    mut input: impl Iterator<Item = tokenstream::TokenTree>,
    sess: &ParseSess,
) -> Vec<Arg> {
    let mut result = Vec::new();
    let args = match input.next() {
        Some(tokenstream::TokenTree::Delimited(_, token::Paren, args)) => Some(args),
        Some(tree) => {
            let msg = format!("expected metavariable expression arguments");
            sess.span_diagnostic.span_err(tree.span(), &msg);
            None
        }
        None => {
            let msg = format!("expected metavariable expression arguments");
            sess.span_diagnostic.span_err(span, &msg);
            None
        }
    };
    if let Some(args) = args {
        let mut tts = args.trees();
        while let Some(tt) = tts.next() {
            match tt {
                tokenstream::TokenTree::Token(token) if token.is_ident() => {
                    result.push(Arg::Ident(token.ident().unwrap().0))
                }
                tokenstream::TokenTree::Token(token::Token {
                    kind: token::TokenKind::Literal(lit),
                    span,
                }) => result.push(Arg::Literal(lit, span)),
                _ => {
                    let msg = "expected meta-variable expression argument";
                    sess.span_diagnostic.span_err(tt.span(), msg);
                }
            }
            if !eat_comma(&mut tts, sess) {
                break;
            }
        }
    }
    if let Some(tt) = input.next() {
        let msg = "unexpected token after meta-variable expression arguments";
        sess.span_diagnostic.span_err(tt.span(), msg);
    }
    result
}

/// Eat the next comma in the token stream, if there is one.
///
/// Returns `true` if a comma was eaten.  It is an error if there is
/// something other than a comma.
fn eat_comma(input: &mut impl Iterator<Item = tokenstream::TokenTree>, sess: &ParseSess) -> bool {
    match input.next() {
        Some(tokenstream::TokenTree::Token(token::Token { kind: token::Comma, span: _ })) => true,
        Some(tree) => {
            let msg = "expected ','";
            sess.span_diagnostic.span_err(tree.span(), msg);
            false
        }
        None => false,
    }
}

use winnow::combinator::alt;
use winnow::combinator::eof;
use winnow::combinator::not;
use winnow::combinator::peek;
use winnow::combinator::repeat;
use winnow::combinator::repeat_till0;
use winnow::error::ContextError;
use winnow::error::ParseError;
use winnow::error::StrContext;
use winnow::token::any;
use winnow::token::take_till;
use winnow::PResult;
use winnow::Parser;

/// A harlowe expression
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Macro(MacroExpr<'a>),
    Text(&'a str),
    Hook(&'a str),
}

/// A harlowe macro expression
#[derive(Debug, PartialEq)]
pub struct MacroExpr<'a> {
    pub name: &'a str,
    pub content: &'a str,
}

/// Parse a harlowe passage.
pub fn parse(input: &str) -> Result<Vec<Expr>, ParseError<&str, ContextError>> {
    repeat(
        0..,
        alt((
            macro_.map(Expr::Macro).context(StrContext::Label("macro")),
            hook.map(Expr::Hook).context(StrContext::Label("hook")),
            text.map(Expr::Text).context(StrContext::Label("text")),
        )),
    )
    .parse(input)
}

fn macro_<'a>(input: &mut &'a str) -> PResult<MacroExpr<'a>> {
    '('.parse_next(input)?;
    let name = take_till(1.., |c| c == ':').parse_next(input)?;
    ':'.parse_next(input)?;
    let content = take_till(1.., |c| c == ')').parse_next(input)?;
    ')'.parse_next(input)?;

    Ok(MacroExpr { name, content })
}

fn text<'a>(input: &mut &'a str) -> PResult<&'a str> {
    let end_text = peek(alt((macro_.void(), eof.void())));

    peek(not(eof)).parse_next(input)?;

    repeat_till0::<_, (), (), _, _, _, _>(any.void(), end_text)
        .map(|(text, _)| text)
        .recognize()
        .parse_next(input)
}

fn hook<'a>(input: &mut &'a str) -> PResult<&'a str> {
    '['.parse_next(input)?;
    let content = take_till(1.., |c| c == ']').parse_next(input)?;
    ']'.parse_next(input)?;

    Ok(content)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn kitchen_sink() {
        let content = [
            "(print: 54)",
            "(set:$value to 45 )",
            "(set:$value to 0 )(set:$value to 1)",
            "(print: 54) hello there!",
            "[basic hook]",
        ];
        for content in content {
            let parsed = parse(content).expect("failed to parse");
            dbg!(parsed);
        }
    }

    #[test]
    fn not_a_macro() {
        let content = "(not a macro)";
        let parsed = parse(content).expect("failed to parse");
        let expected = vec![Expr::Text("(not a macro)")];
        assert!(parsed == expected);
    }
}

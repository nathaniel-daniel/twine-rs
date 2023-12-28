use winnow::combinator::alt;
use winnow::combinator::repeat;
use winnow::error::ContextError;
use winnow::error::ParseError;
use winnow::token::take_till;
use winnow::PResult;
use winnow::Parser;

/// A harlowe expression
#[derive(Debug)]
pub enum Expr<'a> {
    Macro(MacroExpr<'a>),
    Text(&'a str),
}

/// A harlowe macro expression
#[derive(Debug)]
pub struct MacroExpr<'a> {
    pub name: &'a str,
    pub content: &'a str,
}

/// Parse a harlowe passage.
pub fn parse(input: &str) -> Result<Vec<Expr>, ParseError<&str, ContextError>> {
    repeat(
        0..,
        alt((
            macro_.map(Expr::Macro),
            take_till(1.., |c| c == '(').map(Expr::Text),
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
        ];
        for content in content {
            parse(content).unwrap();
        }
    }
}

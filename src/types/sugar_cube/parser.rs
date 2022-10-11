use nom::branch::alt;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_till;
use nom::bytes::complete::take_until;
use nom::bytes::complete::take_while;
use nom::character::complete::char;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::opt;
use nom::combinator::peek;
use nom::combinator::recognize;
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::sequence::preceded;
use nom::sequence::tuple;
use nom::IResult;

/// A Macro
#[derive(Debug, PartialEq, Eq)]
pub struct Macro<'a> {
    /// The macro name
    pub name: &'a str,

    /// Macro arguments
    pub args: Vec<&'a str>,
}

/// An Expression
#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    /// A string literal
    StringLiteral { value: &'a str },

    /// An integer literal
    IntegerLiteral { value: i64 },

    /// An Ident
    Ident { value: &'a str },

    /// The to operator
    ToOperator,
}

/// Content
#[derive(Debug, PartialEq)]
pub enum Content<'a> {
    Text { value: &'a str },
    Macro { macro_: Macro<'a> },
}

/// Parse a macro name
///
/// Macro Name: [A-Za-z][\\w-]*|[=-]
fn parse_macro_name(i: &str) -> IResult<&str, &str> {
    alt((
        tag("="),
        tag("-"),
        take_while(|c: char| c.is_ascii_alphabetic()),
    ))(i)
}

pub fn parse_macro_args(i: &str) -> IResult<&str, Vec<&str>> {
    let (mut i, _) = tag(" ")(i)?;
    let mut args = Vec::new();

    loop {
        if i.is_empty() {
            break;
        }

        let (arg, local_i, terminate) = 'splitter: loop {
            for (idx, ch) in i.char_indices() {
                match ch {
                    ' ' => {
                        break 'splitter (&i[..idx], &i[(idx + 1)..], false);
                    }
                    '>' => {
                        if i[idx..].starts_with(">>") {
                            break 'splitter (&i[..idx], &i[idx..], true);
                        }
                    }
                    _ => {}
                }
            }

            break (i, "", true);
        };

        i = local_i;

        args.push(arg);

        if terminate {
            break;
        }
    }

    Ok((i, args))
}

/// Parse a macro
pub(crate) fn parse_macro(i: &str) -> IResult<&str, Macro> {
    map(
        delimited(
            context("missing `<<`", tag("<<")),
            tuple((parse_macro_name, opt(parse_macro_args))),
            context("missing `>>`", tag(">>")),
        ),
        |(name, args)| Macro {
            name,
            args: args.unwrap_or_else(Vec::new),
        },
    )(i)
}

/// Parse an expr
fn parse_expr(i: &str) -> IResult<&str, Expr> {
    alt((
        parse_string_literal,
        parse_integer_literal,
        parse_ident,
        parse_to_operator,
    ))(i)
}

/// Parse string literal
fn parse_string_literal(i: &str) -> IResult<&str, Expr> {
    map(
        alt((
            delimited(char('"'), is_not("\""), char('"')),
            delimited(char('\''), is_not("\'"), char('\'')),
        )),
        |value| Expr::StringLiteral { value },
    )(i)
}

/// Parse an ident
fn parse_ident(i: &str) -> IResult<&str, Expr> {
    let (rest, value) = recognize(preceded(
        char('$'),
        take_while(|c: char| c.is_ascii_alphabetic()),
    ))(i)?;

    Ok((rest, Expr::Ident { value }))
}

/// Parse an integer literal
fn parse_integer_literal(i: &str) -> IResult<&str, Expr> {
    let (rest, n_str) = map_res(take_while(|c: char| c.is_ascii_digit()), |v: &str| {
        v.parse()
    })(i)?;
    Ok((rest, Expr::IntegerLiteral { value: n_str }))
}

/// Parse a `to` operator
fn parse_to_operator(i: &str) -> IResult<&str, Expr> {
    let (rest, _) = tag("to")(i)?;
    Ok((rest, Expr::ToOperator))
}

/// Parse content
pub fn parse_content(input: &str) -> Result<Vec<Content>, nom::error::Error<&str>> {
    let mut start = 0;
    let mut iter = input.char_indices();
    let mut content = Vec::new();

    while let Some((i, ch)) = iter.next() {
        if i < start {
            continue;
        }

        match ch {
            '<' => match parse_macro(&input[i..]) {
                Ok((rest, macro_)) => {
                    let text = &input[start..i];
                    if !text.is_empty() {
                        content.push(Content::Text { value: text });
                    }
                    content.push(Content::Macro { macro_ });

                    start = i + (input[i..].len() - rest.len());
                }
                Err(_e) => {}
            },
            _ => {}
        }
    }

    let text = &input[start..];
    if !text.is_empty() {
        content.push(Content::Text { value: text });
    }

    Ok(content)
}

/// An error occured while parsing
#[derive(Debug, thiserror::Error)]
pub enum ParseError<'a> {
    /// Reached the EOF unexpectedly.
    #[error("unexpectedly reached the end of the string")]
    UnexpectedEof,

    /// Expected one value but got another
    #[error("expected `{expected}` but got `{actual:?}`")]
    Unexpected {
        expected: &'static str,
        actual: Option<&'a str>,
    },
}

/// The parser context, where macros should be registered
pub struct ParserContext {}

impl ParserContext {
    pub fn new() -> Self {
        Self {}
    }
}

/// The content parser
#[derive(Clone)]
pub struct Parser<'a, 'b> {
    ctx: &'a ParserContext,
    chars: std::str::CharIndices<'b>,
    input: &'b str,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(ctx: &'a ParserContext, input: &'b str) -> Self {
        Self {
            ctx,
            chars: input.char_indices(),
            input,
        }
    }

    /// Get the current position
    fn get_cur_pos(&self) -> usize {
        self.input.len() - self.chars.as_str().len()
    }

    /// Get the next char
    fn next_ch(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    /// Peek the next char
    fn peek_ch(&mut self) -> Option<(usize, char)> {
        self.chars.clone().next()
    }

    /// Get the next n chars as a str
    fn next_n_str(&mut self, n: usize) -> Option<&'b str> {
        let start = self.get_cur_pos();
        let mut end = start;
        for _ in 0..n {
            end = self.chars.next()?.0;
        }
        Some(&self.input[start..=end])
    }

    /// Get the next chars as a str while the chars pass the func
    fn next_while_str<F>(&mut self, func: F) -> Option<&'b str>
    where
        F: Fn((usize, char)) -> bool,
    {
        let start = self.get_cur_pos();
        let mut end = start;

        while let Some((i, ch)) = self.peek_ch() {
            if !func((i, ch)) {
                end = i;
                break;
            }
        }

        Some(&self.input[start..end])
    }

    /// Advance n chars.
    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            let _ = self.chars.next();
        }
    }

    /// Parse all content
    pub fn parse_all_content(&mut self) -> Result<Vec<Content<'b>>, ParseError<'b>> {
        let mut start = self.get_cur_pos();
        let mut content = Vec::new();
        while let Some((i, ch)) = self.peek_ch() {
            match ch {
                '<' => {
                    if self.chars.as_str().starts_with('<') {
                        let mut self_clone = self.clone();
                        match self_clone.parse_macro() {
                            Ok(macro_) => {
                                // Push the text block we were working on
                                let text = &self.input[start..i];
                                if !text.is_empty() {
                                    content.push(Content::Text { value: text });
                                }

                                // Skip the amount the macro parser ate.
                                self.advance_n(self_clone.get_cur_pos() - self.get_cur_pos());
                                content.push(Content::Macro { macro_ });

                                // Set the start of the next text block.
                                start = self.get_cur_pos();
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        };
                    }
                }
                _ => {
                    self.advance_n(1);
                }
            }
        }

        Ok(content)
    }

    fn parse_macro(&mut self) -> Result<Macro<'b>, ParseError<'b>> {
        // <<
        let lmacro = self.next_n_str(2).ok_or(ParseError::UnexpectedEof)?;
        if lmacro != "<<" {
            return Err(ParseError::Unexpected {
                expected: "<<",
                actual: Some(lmacro),
            });
        }

        // macro_name
        let name = self.parse_macro_name()?;

        // TODO: Conditionally attempt to parse args
        let mut args = Vec::new();
        {
            let peek_buf = self
                .clone()
                .next_n_str(2)
                .ok_or(ParseError::UnexpectedEof)?;
            let peek_buf_bytes = peek_buf.as_bytes();
            match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                (Some(b' '), _) => {
                    self.advance_n(1);
                }
                (Some(b'>'), Some(b'>')) => {
                    self.advance_n(2);
                    return Ok(Macro { name, args });
                }
                _ => {
                    return Err(ParseError::Unexpected {
                        expected: " ",
                        actual: Some(&peek_buf[..1]),
                    });
                }
            }

            'main: loop {
                // Read arg
                let start = self.get_cur_pos();
                loop {
                    let peek_buf = self
                        .clone()
                        .next_n_str(2)
                        .ok_or(ParseError::UnexpectedEof)?;
                    let peek_buf_bytes = peek_buf.as_bytes();

                    match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                        (Some(b' '), _) => {
                            let end = self.get_cur_pos();
                            args.push(&self.input[start..end]);

                            break;
                        }
                        (Some(b'>'), Some(b'>')) => {
                            let end = self.get_cur_pos();
                            args.push(&self.input[start..end]);

                            break 'main;
                        }
                        (Some(_), Some(b' ' | b'>')) => {
                            self.advance_n(1);
                        }
                        (_, _) => {
                            self.advance_n(2);
                        }
                    }
                }

                // Read space
                let space = self.next_n_str(1).ok_or(ParseError::UnexpectedEof)?;
                if space != " " {
                    return Err(ParseError::Unexpected {
                        expected: " ",
                        actual: Some(space),
                    });
                }
            }
        }

        // >>
        let rmacro = self.next_n_str(2).ok_or(ParseError::UnexpectedEof)?;
        if rmacro != ">>" {
            return Err(ParseError::Unexpected {
                expected: ">>",
                actual: Some(rmacro),
            });
        }

        Ok(Macro { name, args })
    }

    fn parse_macro_name(&mut self) -> Result<&'b str, ParseError<'b>> {
        // = or -
        {
            let peek_buf = self
                .clone()
                .next_n_str(2)
                .ok_or(ParseError::UnexpectedEof)?
                .as_bytes();

            match (peek_buf[0], peek_buf[1]) {
                (b'=' | b'-', c) if c == b'>' || c == b' ' => {
                    let ret = &self.chars.as_str()[..1];

                    // Eat the name, not the space
                    self.advance_n(1);

                    return Ok(ret);
                }
                (_, _) => {
                    // Name is not - or =.
                }
            }
        }

        let start = self.get_cur_pos();
        let mut end = start;
        while let Some((i, ch)) = self.peek_ch() {
            if !ch.is_alphabetic() {
                end = i;
                break;
            }

            self.advance_n(1);
        }
        let name = &self.input[start..end];

        Ok(name)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_print_macro() {
        let ctx = ParserContext::new();
        {
            let content = "<<= 'hello world'>>";

            /*
            let (rest, macro_) = parse_macro(content).expect("failed to parse");
            dbg!(&macro_);
            assert!(rest.is_empty());
            assert!(
                macro_
                    == Macro {
                        name: "=",
                        args: vec!["'hello", "world'"],
                    }
            );
            */

            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "=",
                    args: vec!["'hello", "world'"],
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "<<print 'hello world'>>";

            /*
            let (rest, macro_) = parse_macro(content).expect("failed to parse");
            assert!(rest.is_empty());
            assert!(
                macro_
                    == Macro {
                        name: "print",
                        args: vec!["'hello", "world'"]
                    }
            );
            */

            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "print",
                    args: vec!["'hello", "world'"],
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "<<- 'hello world'>>";

            /*
            let (rest, macro_) = parse_macro(content).expect("failed to parse");
            assert!(rest.is_empty());
            assert!(
                macro_
                    == Macro {
                        name: "-",
                        args: vec!["'hello", "world'"]
                    }
            );
            */

            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "-",
                    args: vec!["'hello", "world'"],
                },
            }];
            assert!(content == expected);
        }
    }

    #[test]
    fn parse_set_macro() {
        let ctx = ParserContext::new();

        let content = "<<set $name to 5>>";
        /*
        let (rest, macro_) = parse_macro(content).expect("failed to parse");
        dbg!(&macro_);
        assert!(rest.is_empty());
        assert!(
            macro_
                == Macro {
                    name: "set",
                    args: vec!["$name", "to", "5"]
                }
        );
        */

        let mut parser = Parser::new(&ctx, &content);
        let content = parser
            .parse_all_content()
            .expect("failed to parse all content");
        let expected = vec![Content::Macro {
            macro_: Macro {
                name: "set",
                args: vec!["$name", "to", "5"],
            },
        }];
        assert!(content == expected);
    }

    #[test]
    fn parse_macros() {
        let ctx = ParserContext::new();

        // TODO: This should be a container
        {
            let content = "<<silently>>";

            /*
            let (rest, macro_) = parse_macro(content).expect("failed to parse");
            assert!(rest.is_empty());
            assert!(
                macro_
                    == Macro {
                        name: "silently",
                        args: vec![]
                    }
            );
            */

            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "silently",
                    args: vec![],
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "<<talkr \"https://talkrapp.com/apngdemo/apng/morpheus.png\" en male 0>>";

            /*
            let (rest, macro_) = parse_macro(content).expect("failed to parse");
            assert!(rest.is_empty());
            */

            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "talkr",
                    args: vec![
                        "\"https://talkrapp.com/apngdemo/apng/morpheus.png\"",
                        "en",
                        "male",
                        "0",
                    ],
                },
            }];
            assert!(content == expected);
        }
    }

    #[test]
    fn parse_content_kitchen_sink() {
        let ctx = ParserContext::new();

        {
            let content = "<<set $name to 5>>Hello<<- $name>>";

            /*
            let content = parse_content(content).expect("failed to parse");
            */
            let mut parser = Parser::new(&ctx, &content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            dbg!(&content);
            let expected = vec![
                Content::Macro {
                    macro_: Macro {
                        name: "set",
                        args: vec!["$name", "to", "5"],
                    },
                },
                Content::Text { value: "Hello" },
                Content::Macro {
                    macro_: Macro {
                        name: "-",
                        args: vec!["$name"],
                    },
                },
            ];

            assert!(content == expected);
        }
    }
}

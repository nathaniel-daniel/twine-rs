#![allow(clippy::get_first)]

use std::collections::HashMap;

/// A Macro
#[derive(Debug, PartialEq, Eq)]
pub struct Macro<'a> {
    /// The macro name
    pub name: &'a str,

    /// Macro arguments
    pub args: Vec<&'a str>,
}

/// A closing Macro
#[derive(Debug, PartialEq, Eq)]
pub struct CloseMacro<'a> {
    /// The macro name
    pub name: &'a str,
}

/// Content
#[derive(Debug, PartialEq)]
pub enum Content<'a> {
    Text { value: &'a str },
    Macro { macro_: Macro<'a> },
    CloseMacro { macro_: CloseMacro<'a> },
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

    /// Unknown Macro
    #[error("the macro `{name}` is unknown")]
    UnknownMacro { name: &'a str },
}

/// The parser context, where macros should be registered
#[derive(Debug, Clone)]
pub struct ParserContext {
    macro_defs: HashMap<String, MacroDef>,
}

impl ParserContext {
    /// Make a new parser context, with default macros added.
    pub fn new() -> Self {
        let mut ctx = Self {
            macro_defs: HashMap::new(),
        };
        let print_macro_def = MacroDef { tags: None };
        ctx.add_macro_def("print".into(), print_macro_def.clone());
        ctx.add_macro_def("-".into(), print_macro_def.clone());
        ctx.add_macro_def("=".into(), print_macro_def);

        ctx.add_macro_def("set".into(), MacroDef { tags: None });

        ctx.add_macro_def(
            "silently".into(),
            MacroDef {
                tags: Some(Vec::new()),
            },
        );

        ctx.add_macro_def("remember".into(), MacroDef { tags: None });

        ctx
    }

    /// Add a macro def.
    ///
    /// # Panics
    /// Panics if the macro is already defined.
    pub fn add_macro_def(&mut self, name: String, def: MacroDef) {
        self.macro_defs.insert(name, def);
    }

    /// Get a macro def
    pub fn get_macro_def(&self, name: &str) -> Option<&MacroDef> {
        self.macro_defs.get(name)
    }
}

impl Default for ParserContext {
    fn default() -> Self {
        Self::new()
    }
}

/// The def of a macro
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// An array of internal tags.
    ///
    /// Specifying this will make the macro non self-closing.
    pub tags: Option<Vec<String>>,
}

impl MacroDef {
    /// Returns `true` if this is self-closing
    pub fn is_self_closing(&self) -> bool {
        self.tags.is_none()
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

    /*
    /// Get the next char
    fn next_ch(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }
    */

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

    /*
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
    */

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
                    if self.chars.as_str().starts_with("<</") {
                        let mut self_clone = self.clone();
                        match self_clone.parse_close_macro() {
                            Ok(macro_) => {
                                // Push the text block we were working on
                                let text = &self.input[start..i];
                                if !text.is_empty() {
                                    content.push(Content::Text { value: text });
                                }

                                // Skip the amount the macro parser ate.
                                self.advance_n(self_clone.get_cur_pos() - self.get_cur_pos());
                                content.push(Content::CloseMacro { macro_ });

                                // Set the start of the next text block.
                                start = self.get_cur_pos();
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    }
                    if self.chars.as_str().starts_with("<<") {
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
                    } else {
                        self.advance_n(1);
                    }
                }
                _ => {
                    self.advance_n(1);
                }
            }
        }

        // Push the text block we were working on
        let text = &self.input[start..];
        if !text.is_empty() {
            content.push(Content::Text { value: text });
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

        // macro name
        let name = self.parse_macro_name()?;
        let _macro_def = self
            .ctx
            .get_macro_def(name)
            .ok_or(ParseError::UnknownMacro { name })?;

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

    fn parse_close_macro(&mut self) -> Result<CloseMacro<'b>, ParseError<'b>> {
        // <</
        let lmacro = self.next_n_str(3).ok_or(ParseError::UnexpectedEof)?;
        if lmacro != "<</" {
            return Err(ParseError::Unexpected {
                expected: "<</",
                actual: Some(lmacro),
            });
        }

        let name = self.parse_macro_name()?;

        // >>
        let rmacro = self.next_n_str(2).ok_or(ParseError::UnexpectedEof)?;
        if rmacro != ">>" {
            return Err(ParseError::Unexpected {
                expected: ">>",
                actual: Some(rmacro),
            });
        }

        Ok(CloseMacro { name })
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
        let mut ctx = ParserContext::new();
        ctx.add_macro_def("talkr".into(), MacroDef { tags: None });

        // TODO: This should be a container
        {
            let content = "<<silently>>";

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

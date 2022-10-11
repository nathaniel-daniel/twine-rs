#![allow(clippy::get_first)]

use std::collections::HashMap;

/// A Macro
#[derive(Debug, PartialEq, Eq)]
pub struct Macro<'a> {
    /// The macro name
    pub name: &'a str,

    /// Macro arguments
    pub args: Vec<&'a str>,

    /// Macro content
    pub content: Vec<Content<'a>>,
}

/// A closing Macro
#[derive(Debug, PartialEq, Eq)]
pub struct CloseMacro<'a> {
    /// The macro name
    pub name: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Link<'a> {
    /// Link
    pub link: &'a str,

    /// Text
    pub text: Option<&'a str>,

    /// Setter
    pub setter: Option<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Image<'a> {
    /// The image
    pub image: &'a str,

    /// The link
    pub link: Option<&'a str>,

    /// The setter
    pub setter: Option<&'a str>,

    /// The title
    pub title: Option<&'a str>,
}

/// Content
#[derive(Debug, PartialEq, Eq)]
pub enum Content<'a> {
    Text { value: &'a str },
    Macro { macro_: Macro<'a> },
    Link { link: Link<'a> },
    Image { image: Image<'a> },
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

    /// Read a str, returning an error if the given string does not match
    pub fn expect_next_str(&mut self, expected_str: &'static str) -> Result<(), ParseError<'b>> {
        let actual_str = self
            .next_n_str(expected_str.len())
            .ok_or(ParseError::UnexpectedEof)?;
        if actual_str != expected_str {
            return Err(ParseError::Unexpected {
                expected: expected_str,
                actual: Some(actual_str),
            });
        }

        Ok(())
    }

    /// Parse all content
    pub fn parse_all_content(&mut self) -> Result<Vec<Content<'b>>, ParseError<'b>> {
        let mut start = self.get_cur_pos();
        let mut content = Vec::new();
        while let Some((i, ch)) = self.peek_ch() {
            match ch {
                '<' => {
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
                                while let Some((i, _ch)) = self.peek_ch() {
                                    if i == self_clone.get_cur_pos() {
                                        break;
                                    }
                                    self.advance_n(1);
                                }
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
                '[' => {
                    if self.chars.as_str().starts_with("[img[") {
                        let mut self_clone = self.clone();
                        match self_clone.parse_image() {
                            Ok(image) => {
                                // Push the text block we were working on
                                let text = &self.input[start..i];
                                if !text.is_empty() {
                                    content.push(Content::Text { value: text });
                                }

                                // Skip the amount the image parser ate.
                                while let Some((i, _ch)) = self.peek_ch() {
                                    if i == self_clone.get_cur_pos() {
                                        break;
                                    }
                                    self.advance_n(1);
                                }
                                content.push(Content::Image { image });

                                // Set the start of the next text block.
                                start = self.get_cur_pos();
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    } else if self.chars.as_str().starts_with("[[") {
                        let mut self_clone = self.clone();
                        match self_clone.parse_link() {
                            Ok(link) => {
                                // Push the text block we were working on
                                let text = &self.input[start..i];
                                if !text.is_empty() {
                                    content.push(Content::Text { value: text });
                                }

                                // Skip the amount the link parser ate.
                                while let Some((i, _ch)) = self.peek_ch() {
                                    if i == self_clone.get_cur_pos() {
                                        break;
                                    }
                                    self.advance_n(1);
                                }
                                content.push(Content::Link { link });

                                // Set the start of the next text block.
                                start = self.get_cur_pos();
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
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
        self.expect_next_str("<<")?;

        // macro name
        let name = self.parse_macro_name()?;

        // TODO: If we are missing the macro def,
        // try to guess what it is.
        let _macro_def = self
            .ctx
            .get_macro_def(name)
            .ok_or(ParseError::UnknownMacro { name })?;

        // TODO: Conditionally attempt to parse args?
        let args = self.parse_macro_args()?;

        // >>
        self.expect_next_str(">>")?;

        let mut content = Vec::new();

        // Try to find closing tag
        let closing_tag_index = self.chars.as_str().find(&format!("<</{name}>>"));
        if let Some(closing_tag_index) = closing_tag_index {
            let mut parser = Parser::new(self.ctx, &self.chars.as_str()[..closing_tag_index]);
            content = parser.parse_all_content()?;
            let resume_index = self.get_cur_pos() + closing_tag_index;
            while let Some((i, _ch)) = self.peek_ch() {
                if i == resume_index {
                    break;
                }
                self.advance_n(1);
            }

            // Consume close macro tag
            let _close_macro = self.parse_close_macro()?;
        }

        Ok(Macro {
            name,
            args,
            content,
        })
    }

    fn parse_close_macro(&mut self) -> Result<CloseMacro<'b>, ParseError<'b>> {
        // <</
        self.expect_next_str("<</")?;

        let name = self.parse_macro_name()?;

        // >>
        self.expect_next_str(">>")?;

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

    fn parse_macro_args(&mut self) -> Result<Vec<&'b str>, ParseError<'b>> {
        let mut args = Vec::new();

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
                // self.advance_n(2);
                return Ok(args);
            }
            _ => {
                return Err(ParseError::Unexpected {
                    expected: " ",
                    actual: Some(peek_buf),
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
                    (Some(_), Some(_)) => {
                        self.advance_n(2);
                    }
                    (_, _) => {
                        self.advance_n(1);
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

        Ok(args)
    }

    fn parse_link(&mut self) -> Result<Link<'b>, ParseError<'b>> {
        let mut text: Option<&str> = None;
        let mut setter: Option<&str> = None;

        // [
        self.expect_next_str("[[")?;

        let mut link = self.parse_link_or_image_initial_block_body()?;

        let (ch_i, ch) = self.peek_ch().ok_or(ParseError::UnexpectedEof)?;
        match ch {
            '|' => {
                self.advance_n(1);

                let mut chunk = self.parse_link_or_image_block_body()?;
                std::mem::swap(&mut link, &mut chunk);
                text = Some(chunk);
            }
            ']' => {
                self.advance_n(1);
                let (ch_i, ch) = self.peek_ch().ok_or(ParseError::UnexpectedEof)?;

                match ch {
                    ']' => {
                        self.advance_n(1);
                        return Ok(Link { link, text, setter });
                    }
                    '[' => {
                        self.advance_n(1);

                        let chunk = {
                            let start = self.get_cur_pos();

                            loop {
                                let peek_buf = self
                                    .clone()
                                    .next_n_str(2)
                                    .ok_or(ParseError::UnexpectedEof)?;
                                let peek_buf_bytes = peek_buf.as_bytes();

                                match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                                    (Some(b']'), Some(b']')) => {
                                        let end = self.get_cur_pos();
                                        self.advance_n(2);
                                        break &self.input[start..end];
                                    }
                                    (_, _) => {
                                        self.advance_n(1);
                                    }
                                }
                            }
                        };
                        setter = Some(chunk);

                        return Ok(Link { link, text, setter });
                    }
                    _ => {
                        return Err(ParseError::Unexpected {
                            expected: "]",
                            actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                        });
                    }
                }
            }
            _ => {
                return Err(ParseError::Unexpected {
                    expected: "|",
                    actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                });
            }
        }

        let peek_buf = self
            .clone()
            .next_n_str(2)
            .ok_or(ParseError::UnexpectedEof)?;
        let peek_buf_bytes = peek_buf.as_bytes();
        match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
            (Some(b']'), Some(b']')) => {
                self.advance_n(2);

                Ok(Link { link, text, setter })
            }
            (Some(b']'), Some(b'[')) => {
                self.advance_n(2);

                let chunk = {
                    let start = self.get_cur_pos();

                    loop {
                        let peek_buf = self
                            .clone()
                            .next_n_str(2)
                            .ok_or(ParseError::UnexpectedEof)?;
                        let peek_buf_bytes = peek_buf.as_bytes();

                        match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                            (Some(b']'), Some(b']')) => {
                                let end = self.get_cur_pos();
                                self.advance_n(2);
                                break &self.input[start..end];
                            }
                            (_, _) => {
                                self.advance_n(1);
                            }
                        }
                    }
                };
                setter = Some(chunk);

                Ok(Link { link, text, setter })
            }
            _ => Err(ParseError::Unexpected {
                expected: "]]",
                actual: Some(peek_buf),
            }),
        }
    }

    fn parse_image(&mut self) -> Result<Image<'b>, ParseError<'b>> {
        let mut link = None;
        let mut setter = None;
        let mut title = None;

        // [img[
        self.expect_next_str("[img[")?;

        let mut image = self.parse_link_or_image_initial_block_body()?;

        let (ch_i, ch) = match self.peek_ch().ok_or(ParseError::UnexpectedEof)? {
            (_, '|') => {
                self.advance_n(1);
                let mut chunk = self.parse_link_or_image_block_body()?;
                std::mem::swap(&mut image, &mut chunk);
                title = Some(chunk);

                self.peek_ch().ok_or(ParseError::UnexpectedEof)?
            }
            v => v,
        };

        match ch {
            ']' => {
                self.advance_n(1);
                let (ch_i, ch) = self.peek_ch().ok_or(ParseError::UnexpectedEof)?;

                match ch {
                    ']' => {
                        self.advance_n(1);

                        return Ok(Image {
                            image,
                            link,
                            setter,
                            title,
                        });
                    }
                    '[' => {
                        self.advance_n(1);
                        let chunk = self.parse_link_or_image_block_body()?;
                        link = Some(chunk);
                    }
                    _ => {
                        return Err(ParseError::Unexpected {
                            expected: "]",
                            actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                        });
                    }
                }
            }
            _ => {
                return Err(ParseError::Unexpected {
                    expected: "]",
                    actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                });
            }
        }

        let (ch_i, ch) = self.peek_ch().ok_or(ParseError::UnexpectedEof)?;
        match ch {
            ']' => {
                self.advance_n(1);

                let (ch_i, ch) = self.peek_ch().ok_or(ParseError::UnexpectedEof)?;

                match ch {
                    ']' => {
                        self.advance_n(1);

                        return Ok(Image {
                            image,
                            link,
                            setter,
                            title,
                        });
                    }
                    '[' => {
                        self.advance_n(1);

                        let chunk = self.parse_link_or_image_block_body()?;
                        setter = Some(chunk);
                    }
                    _ => {
                        return Err(ParseError::Unexpected {
                            expected: "]",
                            actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                        });
                    }
                }
            }
            _ => {
                return Err(ParseError::Unexpected {
                    expected: "]",
                    actual: Some(&self.input[ch_i..(ch_i + ch.len_utf8())]),
                });
            }
        }

        self.expect_next_str("]]")?;

        Ok(Image {
            image,
            link,
            setter,
            title,
        })
    }

    /// Parse a text + (']' or '|') and return the text. This will not consume the trailing char.
    fn parse_link_or_image_initial_block_body(&mut self) -> Result<&'b str, ParseError<'b>> {
        let start = self.get_cur_pos();
        let text = loop {
            let peek_buf = self
                .clone()
                .next_n_str(2)
                .ok_or(ParseError::UnexpectedEof)?;
            let peek_buf_bytes = peek_buf.as_bytes();

            match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                (Some(b'|'), _) | (Some(b']'), Some(b']')) | (Some(b']'), Some(b'[')) => {
                    let end = self.get_cur_pos();
                    break &self.input[start..end];
                }
                (_, _) => {
                    self.advance_n(1);
                }
            }
        };

        Ok(text)
    }

    /// Parse a text + (']') and return the text. This will not consume the trailing char.
    pub fn parse_link_or_image_block_body(&mut self) -> Result<&'b str, ParseError<'b>> {
        let start = self.get_cur_pos();
        let text = loop {
            let peek_buf = self
                .clone()
                .next_n_str(2)
                .ok_or(ParseError::UnexpectedEof)?;
            let peek_buf_bytes = peek_buf.as_bytes();

            match (peek_buf_bytes.get(0), peek_buf_bytes.get(1)) {
                (Some(b']'), Some(b']')) | (Some(b']'), Some(b'[')) => {
                    let end = self.get_cur_pos();
                    break &self.input[start..end];
                }
                (_, _) => {
                    self.advance_n(1);
                }
            }
        };

        Ok(text)
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

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "=",
                    args: vec!["'hello", "world'"],
                    content: vec![],
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "<<print 'hello world'>>";

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "print",
                    args: vec!["'hello", "world'"],
                    content: vec![],
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "<<- 'hello world'>>";

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "-",
                    args: vec!["'hello", "world'"],
                    content: vec![],
                },
            }];
            assert!(content == expected);
        }
    }

    #[test]
    fn parse_set_macro() {
        let ctx = ParserContext::new();

        let content = "<<set $name to 5>>";

        let mut parser = Parser::new(&ctx, content);
        let content = parser
            .parse_all_content()
            .expect("failed to parse all content");
        let expected = vec![Content::Macro {
            macro_: Macro {
                name: "set",
                args: vec!["$name", "to", "5"],
                content: vec![],
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
            let content = "<<silently>><</silently>>"; // <</silently>>

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![Content::Macro {
                macro_: Macro {
                    name: "silently",
                    args: vec![],
                    content: vec![],
                },
            }];
            assert!(
                content == expected,
                "(content) {content:#?} != (expected) {expected:#?}"
            );
        }

        {
            let content =
                "<<talkr \"https://talkrapp.com/apngdemo/apng/morpheus.png\" en male 0>><</talkr>>more text";

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");
            let expected = vec![
                Content::Macro {
                    macro_: Macro {
                        name: "talkr",
                        args: vec![
                            "\"https://talkrapp.com/apngdemo/apng/morpheus.png\"",
                            "en",
                            "male",
                            "0",
                        ],
                        content: vec![],
                    },
                },
                Content::Text { value: "more text" },
            ];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }
    }

    #[test]
    fn parse_link() {
        let ctx = ParserContext::new();
        {
            let content = "[[Link]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Link {
                link: Link {
                    link: "Link",
                    text: None,
                    setter: None,
                },
            }];
            assert!(content == expected);
        }

        {
            let content = "[[Text|Link]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Link {
                link: Link {
                    link: "Link",
                    text: Some("Text"),
                    setter: None,
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[[Link][Setter]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Link {
                link: Link {
                    link: "Link",
                    text: None,
                    setter: Some("Setter"),
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[[Text|Link][Setter]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Link {
                link: Link {
                    link: "Link",
                    text: Some("Text"),
                    setter: Some("Setter"),
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[[Regular APNG files|NonTalkr APNG]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Link {
                link: Link {
                    link: "NonTalkr APNG",
                    text: Some("Regular APNG files"),
                    setter: None,
                },
            }];
            assert!(content == expected);
        }
    }

    #[test]
    fn parse_image() {
        let ctx = ParserContext::new();
        {
            let content = "[img[Image]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: None,
                    setter: None,
                    title: None,
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[img[Image][Link]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: Some("Link"),
                    setter: None,
                    title: None,
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[img[Image][Link][Setter]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: Some("Link"),
                    setter: Some("Setter"),
                    title: None,
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[img[Title|Image]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: None,
                    setter: None,
                    title: Some("Title"),
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[img[Title|Image][Link]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: Some("Link"),
                    setter: None,
                    title: Some("Title"),
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }

        {
            let content = "[img[Title|Image][Link][Setter]]";
            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![Content::Image {
                image: Image {
                    image: "Image",
                    link: Some("Link"),
                    setter: Some("Setter"),
                    title: Some("Title"),
                },
            }];
            assert!(content == expected, "{content:#?} != {expected:#?}");
        }
    }

    #[test]
    fn parse_content_kitchen_sink() {
        let ctx = ParserContext::new();

        {
            let content = "<<set $name to 5>>Hello<<- $name>>";

            let mut parser = Parser::new(&ctx, content);
            let content = parser
                .parse_all_content()
                .expect("failed to parse all content");

            let expected = vec![
                Content::Macro {
                    macro_: Macro {
                        name: "set",
                        args: vec!["$name", "to", "5"],
                        content: vec![],
                    },
                },
                Content::Text { value: "Hello" },
                Content::Macro {
                    macro_: Macro {
                        name: "-",
                        args: vec!["$name"],
                        content: vec![],
                    },
                },
            ];

            assert!(content == expected);
        }
    }
}


/// The story format
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum StoryFormat {
    /// Sugar cube
    SugarCube,
}

impl StoryFormat {
    /// Is sugar cube?
    fn is_sugar_cube(self) -> bool {
        matches!(self, StoryFormat::SugarCube)
    }
}

impl std::str::FromStr for StoryFormat {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "SugarCube" => Ok(Self::SugarCube),
            _ => Err(input.into()),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum SugarCubeContentToken<'a> {
    /// Arbitrary text
    Text(&'a str),

    /// `<<`
    MacroOpen,

    /// `>>`
    MacroClose,

    /// `[[`
    LinkOpen,

    /// `]]`
    LinkClose,

    /// `[img[`
    ImgOpen,
}

struct SugarCubeContentTokenizer<'a> {
    text: &'a str,
    iter: std::str::CharIndices<'a>,

    next_char1: Option<(usize, char)>,
    next_char2: Option<(usize, char)>,
}

impl<'a> SugarCubeContentTokenizer<'a> {
    fn new(text: &'a str) -> Self {
        let mut iter = text.char_indices();
        let next_char1 = iter.next();
        let next_char2 = iter.next();

        Self {
            text,
            iter,
            next_char1,
            next_char2,
        }
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        let c = self.next_char1;
        self.next_char1 = self.next_char2;
        self.next_char2 = self.iter.next();

        c
    }

    fn peek_char1(&mut self) -> Option<(usize, char)> {
        self.next_char1
    }

    fn peek_char2(&mut self) -> Option<(usize, char)> {
        self.next_char2
    }
}

impl<'a> Iterator for SugarCubeContentTokenizer<'a> {
    type Item = SugarCubeContentToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.next_char()?;
        let peek_c = self.peek_char1();

        match (c, peek_c) {
            ((_index, '<'), Some((_peek_index, '<'))) => {
                self.next_char()?;
                return Some(SugarCubeContentToken::MacroOpen);
            }
            ((_index, '>'), Some((_peek_index, '>'))) => {
                self.next_char()?;
                return Some(SugarCubeContentToken::MacroClose);
            }
            ((_start_index, '['), Some((_, '['))) => {
                self.next_char()?;
                return Some(SugarCubeContentToken::LinkOpen);
            }
            ((_start_index, ']'), Some((_, ']'))) => {
                self.next_char()?;
                return Some(SugarCubeContentToken::LinkClose);
            }
            ((start_index, '['), _) if self.text[start_index..].starts_with("[img[") => {
                // Lyfe hack for a smaller char buffer
                self.next_char()?;
                self.next_char()?;
                self.next_char()?;
                self.next_char()?;

                return Some(SugarCubeContentToken::ImgOpen);
            }
            ((start_index, _c), None) => {
                return Some(SugarCubeContentToken::Text(&self.text[start_index..]));
            }
            ((start_index, _c), Some((_end_index, _peek_c))) => {
                // Regular text
                loop {
                    let peek_c_1 = self.peek_char1();
                    let peek_c_2 = self.peek_char2();

                    match (peek_c_1, peek_c_2) {
                        (None, None) => {
                            return Some(SugarCubeContentToken::Text(&self.text[start_index..]));
                        }
                        (Some((end_index, '<')), Some((_, '<'))) => {
                            return Some(SugarCubeContentToken::Text(
                                &self.text[start_index..end_index],
                            ));
                        }
                        (Some((end_index, '>')), Some((_, '>'))) => {
                            return Some(SugarCubeContentToken::Text(
                                &self.text[start_index..end_index],
                            ));
                        }
                        (Some((end_index, '[')), Some((_, '['))) => {
                            return Some(SugarCubeContentToken::Text(
                                &self.text[start_index..end_index],
                            ));
                        }
                        (Some((end_index, ']')), Some((_, ']'))) => {
                            return Some(SugarCubeContentToken::Text(
                                &self.text[start_index..end_index],
                            ));
                        }
                        (Some((end_index, '[')), _) => {
                            // Lyfe hack for a smaller char buffer
                            if self.text[end_index..].starts_with("[img[") {
                                return Some(SugarCubeContentToken::Text(
                                    &self.text[start_index..end_index],
                                ));
                            }
                        }
                        _ => {}
                    }

                    self.next_char()
                        .expect("there should be at least 1 char left");
                }
            }
        }
    }
}

fn parse_sugar_cube_content(text: &str) -> anyhow::Result<Vec<SugarCubeContent>> {
    let mut token_iter = SugarCubeContentTokenizer::new(text);
    let mut ret = Vec::with_capacity(16);

    while let Some(token) = token_iter.next() {
        match token {
            SugarCubeContentToken::Text(text) => {
                ret.push(SugarCubeContent::Text(text.into()));
            }
            SugarCubeContentToken::MacroOpen => {
                let token = token_iter.next().context("unexpected EOF")?;
                match token {
                    SugarCubeContentToken::Text(text) => {
                        ret.push(SugarCubeContent::Macro(text.into()));
                    }
                    _ => {
                        anyhow::bail!("unexpected token '{token:?}'");
                    }
                }

                anyhow::ensure!(
                    token_iter.next().context("unexpected EOF")?
                        == SugarCubeContentToken::MacroClose
                );
            }
            SugarCubeContentToken::ImgOpen => {
                let token = token_iter.next().context("unexpected EOF")?;
                let img_text = match token {
                    SugarCubeContentToken::Text(text) => text,
                    _ => {
                        anyhow::bail!("unexpected token '{token:?}'");
                    }
                };

                let mut img_text_iter = img_text.split("][");
                let img = img_text_iter.next().context("missing img url")?.to_string();
                let link = img_text_iter.next().map(|v| v.to_string());
                let setter = img_text_iter.next().map(|v| v.to_string());
                anyhow::ensure!(img_text_iter.next().is_none());

                let token = token_iter.next().context("unexpected EOF")?;
                if token == SugarCubeContentToken::LinkClose {
                    ret.push(SugarCubeContent::Image {
                        img: img.into(),
                        link,
                        setter,
                    });
                } else {
                    anyhow::bail!("unexpected token '{token:?}'");
                }
            }
            SugarCubeContentToken::LinkOpen => {
                let token = token_iter.next().context("unexpected EOF")?;
                let link_text = match token {
                    SugarCubeContentToken::Text(text) => text,
                    _ => {
                        anyhow::bail!("unexpected token '{token:?}'");
                    }
                };

                let mut link_text_iter = link_text.split("][");
                let link_text_1 = link_text_iter.next().context("missing link text")?;
                let link_text_2 = link_text_iter.next();
                anyhow::ensure!(link_text_iter.next().is_none());

                let setter = link_text_2.map(|v| v.to_string());

                let mut link_text_1_iter = link_text_1.split('|');
                let link_text_1_1 = link_text_1_iter.next().context("missing link text 1")?;
                let link_text_1_2 = link_text_1_iter.next();
                anyhow::ensure!(link_text_1_iter.next().is_none());

                let (text, link) = match link_text_1_2 {
                    Some(link) => (Some(link_text_1_1.to_string()), link.to_string()),
                    None => (None, link_text_1_1.to_string()),
                };

                let token = token_iter.next().context("unexpected EOF")?;
                if token == SugarCubeContentToken::LinkClose {
                    ret.push(SugarCubeContent::Link { link, text, setter });
                } else {
                    anyhow::bail!("unexpected token '{token:?}'");
                }
            }
            _ => {
                anyhow::bail!("unexpected token '{token:?}'");
            }
        }
    }

    Ok(ret)
}

#[derive(Debug)]
pub enum SugarCubeContent {
    Text(String),
    Macro(String),
    Image {
        img: String,
        link: Option<String>,
        setter: Option<String>,
    },
    Link {
        link: String,
        text: Option<String>,
        setter: Option<String>,
    },
}
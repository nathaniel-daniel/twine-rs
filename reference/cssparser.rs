
            if let Some(css) = story.story_stylesheet {
                struct StylesheetParser;

                impl<'i> cssparser::DeclarationParser<'i> for StylesheetParser {
                    type Declaration = ();
                    type Error = ();

                    fn parse_value<'t>(
                        &mut self,
                        name: cssparser::CowRcStr<'i>,
                        input: &mut cssparser::Parser<'i, 't>,
                    ) -> Result<(), cssparser::ParseError<'i, ()>> {
                        dbg!(name);
                        Ok(())
                    }
                }

                impl<'i> cssparser::AtRuleParser<'i> for StylesheetParser {
                    type PreludeBlock = ();
                    type PreludeNoBlock = ();
                    type AtRule = ();
                    type Error = ();
                }

                impl<'i> cssparser::QualifiedRuleParser<'i> for StylesheetParser {
                    type Prelude = ();
                    type QualifiedRule = ();
                    type Error = ();

                    fn parse_prelude<'t>(
                        &mut self,
                        input: &mut cssparser::Parser<'i, 't>,
                    ) -> Result<Self::Prelude, cssparser::ParseError<'i, Self::Error>>
                    {
                        while let Ok(token) = input.next_including_whitespace().map(|t| t.clone()) {
                            dbg!(token);
                        }
                        Ok(())
                    }

                    fn parse_block<'t>(
                        &mut self,
                        prelude: Self::Prelude,
                        location: cssparser::SourceLocation,
                        input: &mut cssparser::Parser<'i, 't>,
                    ) -> Result<Self::QualifiedRule, cssparser::ParseError<'i, Self::Error>>
                    {
                        while let Ok(token) = input.next_including_whitespace().map(|t| t.clone()) {
                            dbg!(token);
                        }

                        Ok(())
                    }
                }

                println!("Parsing story stylesheet...");
                let mut input = cssparser::ParserInput::new(&css);
                let mut parser = cssparser::Parser::new(&mut input);
                let list: Vec<_> =
                    cssparser::RuleListParser::new_for_stylesheet(&mut parser, StylesheetParser)
                        .collect();

                dbg!(list);
            }
        }
    }

    Ok(())
}

/// Types
pub mod types;

pub use crate::types::ParsedStoryFormat;
pub use crate::types::Twine2Passage;
pub use crate::types::Twine2Story;

/// Library error type
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid twine2 story")]
    InvalidTwine2Story(#[from] crate::types::story::FromHtmlError),
}

#[cfg(test)]
mod test {
    use super::*;

    /// https://talkrapp.com/apngdemo/index.html
    const TEST_1: &str = include_str!("../test_data/talkrapp_com_apngdemo_index_html.html");

    #[test]
    fn test_1_parse() {
        let story = Twine2Story::from_html(TEST_1).expect("failed to parse TEST_1");
        assert!(story.name == "Talkr SugarCube Demo");
        assert!(story.ifid == "5AFA83C5-4D61-438D-AE9B-23A3EDABBF49");
        assert!(story.format.as_deref() == Some("SugarCube"));
        assert!(story.format_version.as_deref() == Some("2.21.0"));
        assert!(story.startnode.as_deref() == Some("1"));
        assert!(story.zoom.as_deref() == Some("1"));
        assert!(story.creator.as_deref() == Some("Twine"));
        assert!(story.creator_version.as_deref() == Some("2.2.1"));
        assert!(story.passages.len() == 10);

        assert!(matches!(
            story.parse_format(),
            Some(Ok(ParsedStoryFormat::SugarCube))
        ));

        {
            use self::types::sugar_cube::MacroDef;
            use self::types::sugar_cube::Parser;
            use self::types::sugar_cube::ParserContext;

            let mut ctx = ParserContext::new();
            ctx.add_macro_def("talkr".into(), MacroDef { tags: None });

            for passage in story.passages.iter() {
                dbg!(&passage.content);

                let mut parser = Parser::new(&ctx, &passage.content);

                let content = parser.parse_all_content().expect("failed to parse");
                dbg!(content);
            }
        }

        // dbg!(&story);
    }
}

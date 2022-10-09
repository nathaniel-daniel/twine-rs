/// Types
pub mod types;

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
        dbg!(&story);
    }
}

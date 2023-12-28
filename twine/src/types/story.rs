use once_cell::sync::Lazy;
use scraper::Html;
use scraper::Selector;
use std::str::FromStr;

static TW_STORY_DATA_SELECTOR: Lazy<Selector> =
    Lazy::new(|| Selector::parse("tw-storydata").expect("invalid TW_STORY_DATA_SELECTOR"));
static TW_PASSAGE_DATA_SELECTOR: Lazy<Selector> =
    Lazy::new(|| Selector::parse("tw-passagedata").expect("invalid TW_PASSAGE_DATA_SELECTOR"));
static STORY_JAVASCRIPT_SELECTOR: Lazy<Selector> = Lazy::new(|| {
    Selector::parse("script[type=\"text/twine-javascript\"]")
        .expect("invalid STORY_JAVASCRIPT_SELECTOR")
});
static STORY_STYLESHEET_SELECTOR: Lazy<Selector> = Lazy::new(|| {
    Selector::parse("style[type=\"text/twine-css\"]").expect("invalid STORY_JAVASCRIPT_SELECTOR")
});

/// An error that may occur while parsing a Twine2Story from html
#[derive(Debug, thiserror::Error)]
pub enum FromHtmlError {
    #[error("missing twine story data")]
    MissingTwStoryData,

    #[error("missing twine story name")]
    MissingName,

    #[error("missing twine story ifid")]
    MissingIfid,

    #[error("invalid passage")]
    InvalidPassage(#[from] FromElementError),
}

/// See <https://github.com/iftechfoundation/twine-specs/blob/master/twine-2-htmloutput-spec.md>
#[derive(Debug)]
pub struct Twine2Story {
    /// The name of the story
    pub name: String,

    /// The story id
    pub ifid: String,

    /// The story format
    pub format: Option<String>,

    /// The story format version
    pub format_version: Option<String>,

    /// The PID matching a tw-passagedata which should be shown first
    pub startnode: Option<String>,

    /// The zoom
    pub zoom: Option<String>,

    /// The creator program
    pub creator: Option<String>,

    /// The creator program version
    pub creator_version: Option<String>,

    /// Passages
    pub passages: Vec<Twine2Passage>,

    /// Story javascript
    pub story_javascript: Option<String>,

    /// Story stylesheet
    pub story_stylesheet: Option<String>,
}

impl Twine2Story {
    /// Parse a story from a html string.
    pub fn from_html(html: &str) -> Result<Self, FromHtmlError> {
        let html = Html::parse_document(html);

        let tw_story_data_el = html
            .select(&TW_STORY_DATA_SELECTOR)
            .next()
            .ok_or(FromHtmlError::MissingTwStoryData)?;
        let tw_story_data_el_value = tw_story_data_el.value();

        let name = tw_story_data_el_value
            .attr("name")
            .ok_or(FromHtmlError::MissingName)?
            .to_string();

        let ifid = tw_story_data_el_value
            .attr("ifid")
            .ok_or(FromHtmlError::MissingIfid)?
            .to_string();

        let format = tw_story_data_el_value.attr("format").map(|v| v.to_string());

        let format_version = tw_story_data_el_value
            .attr("format-version")
            .map(|v| v.to_string());

        let startnode = tw_story_data_el_value
            .attr("startnode")
            .map(|v| v.to_string());

        let zoom = tw_story_data_el_value.attr("zoom").map(|v| v.to_string());

        let creator = tw_story_data_el_value
            .attr("creator")
            .map(|v| v.to_string());

        let creator_version = tw_story_data_el_value
            .attr("creator-version")
            .map(|v| v.to_string());

        let passages = tw_story_data_el
            .select(&TW_PASSAGE_DATA_SELECTOR)
            .map(Twine2Passage::from_element)
            .collect::<Result<_, _>>()?;

        let story_javascript = html
            .select(&STORY_JAVASCRIPT_SELECTOR)
            .next()
            .and_then(|script| Some(script.text().next()?.to_string()));

        let story_stylesheet = html
            .select(&STORY_STYLESHEET_SELECTOR)
            .next()
            .and_then(|script| Some(script.text().next()?.to_string()));

        Ok(Self {
            name,
            ifid,
            format,
            format_version,
            startnode,
            zoom,
            creator,
            creator_version,
            passages,
            story_javascript,
            story_stylesheet,
        })
    }

    /// Try to parse the story format.
    pub fn parse_format(&self) -> Option<Result<ParsedStoryFormat, FromStrError>> {
        self.format.as_deref().map(ParsedStoryFormat::from_str)
    }
}

/// An error that may occur while parsing a Twine2Passage
#[derive(Debug, thiserror::Error)]
pub enum FromElementError {
    #[error("missing pid")]
    MissingPid,

    #[error("missing name")]
    MissingName,

    #[error("invalid position")]
    InvalidPosition,

    #[error("invalid x position")]
    InvalidXPosition(std::num::ParseIntError),

    #[error("invalid y position")]
    InvalidYPosition(std::num::ParseIntError),

    #[error("invalid size")]
    InvalidSize,

    #[error("invalid x size")]
    InvalidWidthSize(std::num::ParseIntError),

    #[error("invalid y size")]
    InvalidHeightSize(std::num::ParseIntError),
}

/// A twine2 passage
#[derive(Debug)]
pub struct Twine2Passage {
    /// Passage id
    pub pid: String,

    /// Passage name
    pub name: String,

    /// Passage tags
    pub tags: Vec<String>,

    /// The position
    pub position: Option<(u32, u32)>,

    /// The size
    pub size: Option<(u32, u32)>,

    /// The content
    pub content: String,
}

impl Twine2Passage {
    /// Parse a Twine2Passage from an element
    pub(crate) fn from_element(element: scraper::ElementRef) -> Result<Self, FromElementError> {
        let el_value = element.value();

        let pid = el_value
            .attr("pid")
            .ok_or(FromElementError::MissingPid)?
            .to_string();
        let name = el_value
            .attr("name")
            .ok_or(FromElementError::MissingName)?
            .to_string();
        let tags = el_value
            .attr("tags")
            .map(|tags| {
                tags.split(' ')
                    .filter(|v| !v.is_empty())
                    .map(|v| v.to_string())
                    .collect()
            })
            .unwrap_or_default();
        let position = el_value
            .attr("position")
            .map(|position| {
                let (x_str, y_str) = position
                    .split_once(',')
                    .ok_or(FromElementError::InvalidPosition)?;
                Ok((
                    x_str.parse().map_err(FromElementError::InvalidXPosition)?,
                    y_str.parse().map_err(FromElementError::InvalidYPosition)?,
                ))
            })
            .transpose()?;
        let size = el_value
            .attr("size")
            .map(|size| {
                let (w_str, h_str) = size.split_once(',').ok_or(FromElementError::InvalidSize)?;
                Ok((
                    w_str.parse().map_err(FromElementError::InvalidWidthSize)?,
                    h_str.parse().map_err(FromElementError::InvalidHeightSize)?,
                ))
            })
            .transpose()?;

        // If there is no text, the passage is empty.
        let content = element.text().next().unwrap_or("").to_string();

        Ok(Self {
            pid,
            name,
            tags,
            position,
            size,
            content,
        })
    }
}

/// An error that may occur while parsing a story format.
#[derive(Debug)]
pub struct FromStrError(String);

impl std::fmt::Display for FromStrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\" is not a valid story format", self.0)
    }
}

/// A parsed story format
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ParsedStoryFormat {
    /// The Sugar Cube format
    SugarCube,
    
    /// The Harlowe format
    Harlowe,
}

impl FromStr for ParsedStoryFormat {
    type Err = FromStrError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match input {
            "SugarCube" => Ok(Self::SugarCube),
            "Harlowe" => Ok(Self::Harlowe),
            _ => Err(FromStrError(input.into())),
        }
    }
}

use anyhow::anyhow;
use anyhow::Context;
use once_cell::sync::Lazy;
use regex::Regex;
use scraper::Selector;
use std::path::Path;
use std::path::PathBuf;
use tokio::io::AsyncWriteExt;
use twine::types::sugar_cube::Content as SugarCubeContent;
use twine::types::sugar_cube::Parser;
use twine::types::sugar_cube::ParserContext;
use twine::ParsedStoryFormat;
use twine::Twine2Story;
use url::Url;

#[derive(Debug, argh::FromArgs)]
#[argh(description = "a cli to interact with twine projects")]
struct Options {
    #[argh(subcommand)]
    subcommand: Subcommand,
}

#[derive(Debug, argh::FromArgs)]
#[argh(subcommand)]
enum Subcommand {
    DownloadHtml(DownloadHtmlOptions),
}

#[derive(Debug, argh::FromArgs)]
#[argh(
    subcommand,
    name = "download-html",
    description = "download a twine story"
)]
struct DownloadHtmlOptions {
    #[argh(positional, description = "the url to download")]
    url: Url,

    #[argh(
        option,
        long = "out",
        short = 'o',
        description = "the place to download files to",
        default = "PathBuf::from(\"out\")"
    )]
    out_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = argh::from_env();
    let tokio_rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("failed to build tokio runtime")?;
    tokio_rt.block_on(async_main(options))?;

    Ok(())
}

async fn async_main(options: Options) -> anyhow::Result<()> {
    match options.subcommand {
        Subcommand::DownloadHtml(options) => {
            let base_url = {
                let mut url = options.url.clone();
                url.path_segments_mut()
                    .ok()
                    .context("invalid url")?
                    .pop()
                    .push("/");
                url
            };

            println!("Base Url: {}", base_url.as_str());

            let client = reqwest::Client::new();

            println!("Downloading html...");
            let html = async {
                client
                    .get(options.url.as_str())
                    .send()
                    .await
                    .context("failed to send")?
                    .error_for_status()
                    .context("invalid status")?
                    .text()
                    .await
                    .context("failed to get text")
            }
            .await?;

            let html_file_name = options
                .url
                .path_segments()
                .context("missing path")?
                .next_back()
                .context("missing file name")?;

            println!("Saving html...");
            tokio::fs::create_dir_all(&options.out_dir)
                .await
                .context("failed to create out dir")?;
            {
                let path = options.out_dir.join(html_file_name);
                let tmp_path = nd_util::with_push_extension(&path, "part");

                let mut file = tokio::fs::OpenOptions::new()
                    .create_new(true)
                    .write(true)
                    .open(&tmp_path)
                    .await
                    .context("failed to open tmp file")?;
                let mut tmp_path = nd_util::DropRemovePath::new(tmp_path);

                file.write_all(html.as_bytes()).await?;
                file.flush().await?;
                file.sync_all().await?;
                tokio::fs::rename(&tmp_path, path)
                    .await
                    .context("failed to rename path")?;

                tmp_path.persist();
            }

            let story = Twine2Story::from_html(&html).context("failed to parse story")?;
            let mut resources = Vec::with_capacity(16);
            match story.parse_format() {
                Some(Ok(format)) => {
                    println!("Story Format: {:?}", format);

                    match format {
                        ParsedStoryFormat::SugarCube => {
                            println!("Warning: It is impossible to enumerate all resources for this story format.");
                            println!(
                                "Warning: This downloader will perform a best-effort attempt."
                            );

                            for passage in story.passages.iter() {
                                let parser_ctx = ParserContext::new();
                                let mut content_list =
                                    Parser::new(&parser_ctx, passage.content.as_str())
                                        .parse_all_content()
                                        .map_err(|e| anyhow!(e.to_string()))
                                        .context("failed to parse content")?;

                                while let Some(content) = content_list.pop() {
                                    match content {
                                        SugarCubeContent::Image { image } => {
                                            println!("Found Image: \"{}\"", image.image);
                                            resources.push(image.image.to_string());
                                        }
                                        SugarCubeContent::Macro { macro_ } => {
                                            content_list.extend(macro_.content);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                        ParsedStoryFormat::Harlowe => {
                            static IMG_SELECTOR: Lazy<Selector> =
                                Lazy::new(|| Selector::parse("img").unwrap());

                            for passage in story.passages.iter() {
                                let parsed = harlowe::parse(passage.content.as_str())
                                    .map_err(|error| anyhow!(error.to_string()))?;

                                for expr in parsed {
                                    let text = match expr {
                                        harlowe::Expr::Text(text) => text,
                                        harlowe::Expr::Hook(text) => text,
                                        _ => continue,
                                    };

                                    let html = scraper::Html::parse_fragment(text);

                                    for img in html.select(&IMG_SELECTOR) {
                                        let src = match img.attr("src") {
                                            Some(src) => src,
                                            None => continue,
                                        };

                                        // TODO: I don't really know a good way to handle this without rewriting the html directly.
                                        if src.starts_with("https://") {
                                            println!(
                                                "Warning: Skipping absolute url image resource"
                                            );
                                            continue;
                                        }

                                        resources.push(src.to_string());
                                    }
                                }
                            }
                        }
                    }
                }
                Some(Err(story_format)) => {
                    println!("Warning: Unknown story format \"{story_format}\"");
                }
                None => {
                    println!("Warning: Story is missing a format");
                }
            }

            if story.story_javascript.is_some() {
                println!("Warning: Ignoring story javascript...");
            }

            if let Some(css) = story.story_stylesheet.as_deref() {
                static CSS_URL_REGEX: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r#"url\((.*)\);"#).unwrap());
                println!("Warning: Story stylesheet parser only recognizes url() values");
                let captures = CSS_URL_REGEX.captures_iter(css);
                for capture in captures {
                    let url = &capture[1];
                    println!("Found CSS resource: \"{url}\"");
                    resources.push(url.to_string());
                }
            }

            let mut last_error = Ok(());
            for resource in resources.iter() {
                println!("Downloading \"{resource}\"...");

                let url = base_url.join(resource).context("invalid url")?;
                let path = options.out_dir.join(resource);

                match download_resource(&client, url, &path).await {
                    Ok(()) => {}
                    Err(error) => {
                        println!("Error: {error:?}");
                        last_error = Err(error);
                    }
                }
            }
            last_error?;
        }
    }

    Ok(())
}

async fn download_resource(client: &reqwest::Client, url: Url, path: &Path) -> anyhow::Result<()> {
    if let Some(path_parent) = path.parent() {
        tokio::fs::create_dir_all(&path_parent)
            .await
            .with_context(|| format!("failed to create dir \"{}\"", path_parent.display()))?;
    }

    if tokio::fs::try_exists(&path)
        .await
        .context("failed to stat file")?
    {
        println!("  File exists, skipping...");
        return Ok(());
    }

    nd_util::download_to_path(client, url.as_str(), &path).await?;

    Ok(())
}

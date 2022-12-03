# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def YTScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    return findall(soup, url) if everything else current_page(soup, url)


def video_key_from_url(url: str):
    return extract_parameter(url, "v")


def current_page(soup: Soup, url: ParseResult) -> List[Thing]:
    if not url.path.startswith("/watch"):
        raise UserError("wrong page")

    key = video_key_from_url(url.query)

    title = soup.select_one("#title > h1")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title, recursive=True)

    return [
        Thing(
            name=name,
            key=key,
            jsmark=change_color_on(title, "lime", fuzzy_body=True, style="background"),
        )
    ]


def findall(soup: Soup, url: ParseResult) -> List[Thing]:
    things = ignore_user_error(current_page, soup, url)

    others = soup.select("a.yt-simple-endpoint")
    for video in others:
        if "href" not in video.attrs:
            continue
        href = video["href"]
        if not href.startswith("/watch?"):
            continue
        key = video_key_from_url(href.removeprefix("/watch?"))
        name = scrape_text(video, recursive=True, allow_empty=True)
        things.append(
            Thing(
                name=name,
                key=key,
                jsmark=change_color_on(
                    video, "lime", fuzzy_body=True, style="background"
                ),
            )
        )

    return things

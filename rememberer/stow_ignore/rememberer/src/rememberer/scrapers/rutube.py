# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def rutubeScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    return findall(soup, url) if everything else current_page(soup, url)


def video_key_from_url(url: str):
    try:
        test = extract_path(url, 2, 0)
        if test != "video":
            return None
    except ParseException:
        return None
    return extract_path(url, 2, 1)


def current_page(soup: Soup, url: ParseResult) -> List[Thing]:
    if (key := video_key_from_url(url.path)) is None:
        raise UserError("wrong page")

    title = soup.select_one("h1")
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

    others = soup.select("article > a[href]")
    for video in others:
        href = video["href"]

        if (key := video_key_from_url(href)) is None:
            continue

        parent = video.parent
        name = scrape_text(parent, recursive=True)

        things.append(
            Thing(
                name=name,
                key=key,
                jsmark=change_color_on(
                    parent, "lime", fuzzy_body=True, style="background"
                ),
            )
        )

    return things

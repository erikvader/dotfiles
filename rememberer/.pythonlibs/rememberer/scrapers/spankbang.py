# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def SpankbangScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    return findall(soup, url) if everything else current_page(soup, url)


def video_key_on_playlist(video: Soup) -> Optional[str]:
    from numpy import base_repr

    vid = video.attrs.get("data-videoid", None) or video.attrs.get("data-id", None)
    if vid is None:
        return None

    try:
        vid = int(vid)
    except ValueError:
        return None

    return base_repr(vid, 36).lower()


def video_key_from_url(urlpath: str) -> Optional[str]:
    try:
        test = extract_path(urlpath, 3, 1)
        if test != "video":
            return None
    except ParseException:
        return None
    return extract_path(urlpath, 3, 0)


def current_page(soup: Soup, url: ParseResult) -> List[Thing]:
    video = soup.select_one("#video")
    if video is None:
        raise UserError("video is removed or wrong page")

    key = video_key_from_url(url.path) or video_key_on_playlist(video)
    if key is None:
        raise UserError("wrong page")

    title = soup.select_one("#video > div.left > h1[title]")
    if not title:
        raise ParseException("couldn't find video title")
    name = title["title"]
    if not name:
        raise ParseException("title name empty")

    return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]


def findall(soup: Soup, url: ParseResult) -> List[Thing]:
    things = ignore_user_error(current_page, soup, url)

    others = soup.select("div.video-list > div.video-item > a.n")
    for video in others:
        if "href" not in video.attrs:
            continue
        href = video["href"]
        key = video_key_from_url(href) or video_key_on_playlist(video.parent)
        if key is None:
            continue
        name = scrape_text(video, recursive=True, allow_empty=True) or "<no name>"
        things.append(Thing(name=name, key=key, jsmark=change_color_on(video, "lime")))
    return things

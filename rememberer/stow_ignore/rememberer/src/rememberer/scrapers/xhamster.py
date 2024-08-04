# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def XHamsterScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def video_key(urlpath: str) -> Optional[str]:
        try:
            test = extract_path(urlpath, 2, 0)
            if test != "videos":
                return None
        except ParseException:
            return None
        return extract_path(urlpath, 2, 1)

    def current_page() -> List[Thing]:
        if (key := video_key(url.path)) is None:
            raise UserError("wrong page")

        title = soup.select_one("main > div.with-player-container > h1:first-child")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title)
        if not name:
            raise ParseException("title name empty")

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall() -> List[Thing]:
        if video_key(url.path) is not None:
            things = current_page()
        else:
            things = []

        others = soup.select(
            "div.thumb-list > div.video-thumb > div.video-thumb-info > a.video-thumb-info__name"
        )
        for video in others:
            if "href" not in video.attrs:
                continue
            href = urlparse(video["href"]).path
            if (key := video_key(href)) is None:
                continue
            name = scrape_text(video)
            things.append(
                Thing(name=name, key=key, jsmark=change_color_on(video, "lime"))
            )
        return things

    return findall() if everything else current_page()

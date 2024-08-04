# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def ehentaiScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def video_key(urlpath: str) -> Optional[str]:
        try:
            test = extract_path(urlpath, 3, 0)
            if test != "g":
                return None
        except ParseException:
            return None
        return extract_path(urlpath, 3, 1)

    def current_page() -> List[Thing]:
        if (key := video_key(url.path)) is None:
            raise UserError("wrong page")

        title = soup.select_one("h1#gn")
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

        for video in soup.select("div.gl1t > a[href] > div.glname"):
            name = scrape_text(video)

            href = urlparse(video.parent["href"]).path
            if (key := video_key(href)) is None:
                raise ParseException("invalid link in tag")

            things.append(
                Thing(name=name, key=key, jsmark=change_color_on(video, "lime"))
            )
        return things

    return findall() if everything else current_page()

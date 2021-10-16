# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def IwaraScraper(
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

        title = soup.select_one(
            "div.content > div.node-info > div.submitted > h1.title"
        )
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

        places = ["div.node-video.node-sidebar_teaser", "div.node-video.node-teaser"]
        others = [t for p in places for t in soup.select(p)]
        for video in others:
            title = video.select_one("h3.title > a")
            name = scrape_text(title) if title is not None else "N/A"

            if (a := video.select_one("div.field a[href]")) is None:
                # NOTE: there are sometimes invisible ones in "More like this"
                continue

            href = urlparse(a["href"]).path
            if (key := video_key(href)) is None:
                raise ParseException("invalid link in tag")

            to_color = video.select("div.icon-bg > div")
            if title is not None:
                to_color.append(title)

            things.append(
                Thing(name=name, key=key, jsmark=change_colors_on(to_color, "lime"))
            )
        return things

    return findall() if everything else current_page()

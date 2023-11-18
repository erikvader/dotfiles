# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def myhentaigalleryScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def comic_key(urlpath):
        if not urlpath.startswith("/g/"):
            return None
        try:
            return extract_path(urlpath, 2, 1)
        except ParseException:
            return None

    def current_page():
        if (key := comic_key(url.path)) is None:
            raise UserError("wrong page")

        title = soup.select_one("div.comic-description > h1")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title)
        if not name:
            raise ParseException("title name empty")

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall():
        if comic_key(url.path) is not None:
            return current_page()

        things = []
        others = soup.select(
            "div.comic-listing > ul.comics-grid > li.item > div.comic-inner > a"
        )
        for comic in others:
            if "href" not in comic.attrs:
                continue
            href = comic["href"]
            if (key := comic_key(href)) is None:
                continue
            nametag = comic.select_one("div.comic-info > h2.comic-name")
            if nametag is None:
                raise ParseException("comic did not have a comic-name")
            name = scrape_text(nametag)
            things.append(
                Thing(name=name, key=key, jsmark=change_color_on(nametag, "lime"))
            )
        return things

    return findall() if everything else current_page()

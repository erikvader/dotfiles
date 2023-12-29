# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def hanimeScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def viewkey(url: ParseResult) -> Optional[str]:
        try:
            if extract_path(url.path, 3, 0) != "videos":
                return None
            if extract_path(url.path, 3, 1) != "hentai":
                return None
        except ParseException:
            return None
        return extract_path(url.path, 3, 2)

    def current_page():
        if (key := viewkey(url)) is None:
            raise UserError("wrong page")

        title = soup.select_one("h1.tv-title")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title)

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall():
        if viewkey(url) is not None:
            things = current_page()
        else:
            things = []

        for a in soup.select("a[href]"):
            if "href" not in a.attrs:
                raise ParseException("found faulty a-tag? no href")
            href = a["href"]
            key = viewkey(urlparse(href))
            if key is None:
                continue

            name = a.get("title", "unknown")
            things.append(Thing(name=name, key=key, jsmark=change_color_on(a, "lime")))

        return things

    return findall() if everything else current_page()

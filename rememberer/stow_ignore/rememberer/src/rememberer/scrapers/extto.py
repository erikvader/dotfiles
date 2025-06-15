# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def exttoScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    def pagekey(path: str) -> str:
        try:
            return extract_path(path, 1, 0)
        except ParseException:
            return None

    def torrentPage():
        title = select_unique(soup, "h1.card-title")
        if title is None:
            raise ParseException("couldn't find the name")
        name = scrape_text(title)
        key = pagekey(url.path)

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def searchPage():
        results = []
        for a in soup.select(
            "table.search-table td.text-left > div.float-left > a[href]"
        ):
            key = pagekey(a["href"])
            if key is None:
                continue
            name = scrape_text(a, recursive=True)

            results.append(Thing(name=name, key=key, jsmark=change_color_on(a, "lime")))

        return results

    if url.path.startswith("/browse/"):
        return searchPage()
    elif pagekey(url.path) is not None:
        return torrentPage()
    else:
        raise UserError("wrong page")

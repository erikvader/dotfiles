# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def torrentgalaxyScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    def pagekey(path: str) -> str:
        try:
            if extract_path(path, 3, 0) != "torrent":
                return None
        except ParseException:
            return None
        return extract_path(path, 3, 1)

    def torrentPage():
        title = select_unique(
            soup,
            ".torrentpagetable > .tprow:nth-child(1) > .tpcell:nth-child(2) a.textshadow",
        )
        if title is None:
            raise ParseException("couldn't find the name")
        name = scrape_text(title)
        key = pagekey(url.path)

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def searchPage():
        results = []
        for a in soup.select("div.tgxtable a[href][title]"):
            key = pagekey(a["href"])
            if key is None:
                continue
            name = scrape_text(a, recursive=True)

            results.append(Thing(name=name, key=key, jsmark=change_color_on(a, "lime")))

        return results

    if url.path.startswith("/torrent/"):
        return torrentPage()
    elif url.path.startswith("/torrents.php"):
        return searchPage()
    else:
        raise UserError("wrong page")

# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def trettontrettiosjuScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    def path_to_key(path: str) -> str:
        return extract_path(path, 3, 1)

    def torrentPage():
        title = soup.select_one("div.box-info-heading > h1")
        if title is None:
            raise ParseException("couldn't find the name")
        title = scrape_text(title)
        if not title:
            raise ParseException("title name empty")
        key = path_to_key(url.path)

        return [Thing(name=title, key=key)]

    def searchPage():
        if soup.select_one("table.table-list") is None:
            raise ParseException("the HTML is not as expected")

        results = []
        for td in soup.select("table.table-list > tbody > tr > td.name"):
            ajs = list(td.select("a"))
            if len(ajs) != 2:
                raise ParseException("unexpected amount of a-tags")
            main_a = ajs[1]

            name = scrape_text(main_a)
            if not name:
                raise ParseException("title name empty")

            if "href" not in main_a.attrs:
                raise ParseException("the a-tag is missing href")
            key = path_to_key(main_a["href"])

            results.append(Thing(name=name, key=key))

        return results

    if url.path.startswith("/torrent/"):
        return torrentPage()
    elif url.path.startswith("/search/"):
        return searchPage()
    else:
        raise UserError("wrong page")

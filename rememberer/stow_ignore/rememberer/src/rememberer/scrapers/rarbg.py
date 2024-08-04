# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def rarbgScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    def path_to_key(path: str) -> str:
        return extract_path(path, 2, 1)

    def torrentPage():
        title = soup.select_one("td.block h1.black")
        if title is None:
            raise ParseException("couldn't find the name")
        title = scrape_text(title)
        key = path_to_key(url.path)

        return [Thing(name=title, key=key)]

    def searchPage():
        if soup.select_one("table.lista2t") is None:
            raise ParseException("the HTML is not as expected")

        results = []
        for tr in soup.find_all("tr", class_="lista2"):
            tds = list(tr.find_all("td", class_="lista", recursive=False))
            if len(tds) != 8:
                raise ParseException("unexpected amount of td-tags")

            main_a = tds[1].a
            if main_a is None:
                raise ParseException("an a-tag was not found")

            name = scrape_text(main_a)

            if "href" not in main_a.attrs:
                raise ParseException("the a-tag is missing href")
            torrent_url = main_a["href"]
            key = path_to_key(torrent_url)

            results.append(
                Thing(name=name, key=key, jsmark=change_color_on(main_a, "black"))
            )

        return results

    if url.path.startswith("/torrent/"):
        return torrentPage()
    elif url.path.startswith("/torrents.php"):
        return searchPage()
    else:
        raise UserError("wrong page")

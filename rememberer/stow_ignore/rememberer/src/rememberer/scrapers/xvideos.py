# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def XVideosScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    if not url.path.startswith("/video"):
        raise UserError("wrong page")

    key = extract_path(url.path, 2, 0)

    title = soup.select_one("h2.page-title")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title)

    return [Thing(name=name, key=key)]

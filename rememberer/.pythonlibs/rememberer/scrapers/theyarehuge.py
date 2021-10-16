# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def theyAreHugeScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    if not url.path.startswith("/v/"):
        raise UserError("wrong page")

    key = extract_path(url.path, 2, 1)
    title = soup.select_one("h1.cs_headline")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title)
    if not name:
        raise ParseException("title name empty")

    return [Thing(name=name, key=key)]

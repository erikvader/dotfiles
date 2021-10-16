# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def TnaFlixScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    try:
        key = extract_path(url.path, 3, 2)
        if not key.startswith("video"):
            raise UserError("wrong page")
    except ParseException as e:
        raise UserError("wrong page") from e

    title = soup.select_one("div.sectHeader > h1")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title)
    if not name:
        raise ParseException("title name empty")

    return [Thing(name=name, key=key)]

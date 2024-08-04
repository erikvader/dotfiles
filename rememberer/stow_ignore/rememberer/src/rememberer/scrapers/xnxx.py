# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def XnxxScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    if not url.path.startswith("/video-"):
        raise UserError("wrong page")

    key = extract_path(url.path, 2, 0)
    title = soup.select_one("p.video-title")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title)
    if not name:
        raise ParseException("title name empty")

    return [Thing(name=name, key=key)]

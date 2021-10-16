# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def YTScraper(soup: Soup, url: ParseResult, **_kwargs) -> List[Thing]:
    if not url.path.startswith("/watch"):
        raise UserError("wrong page")

    key = extract_parameter(url.query, "v")

    title = soup.select_one("h1.title.ytd-video-primary-info-renderer")
    if not title:
        raise ParseException("couldn't find video title")
    name = scrape_text(title, recursive=True)

    return [Thing(name=name, key=key)]

# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def PHScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def viewkey(url: ParseResult) -> Optional[str]:
        if not url.path.startswith("/view_video.php"):
            return None

        try:
            return extract_parameter(url.query, "viewkey")
        except ParseException:
            return None

    def current_page():
        if (key := viewkey(url)) is None:
            raise UserError("wrong page")

        title = soup.select_one("div.title-container > h1.title > span")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title)

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall():
        if viewkey(url) is not None:
            things = current_page()
        else:
            things = []

        for a in soup.select("div.wrap span.title > a"):
            if "href" not in a.attrs:
                raise ParseException("found faulty a-tag? no href")
            href = a["href"]
            key = viewkey(urlparse(href))
            if key is None:
                continue

            if "title" not in a.attrs:
                raise ParseException("found faulty a-tag? no title")
            name = a["title"]
            things.append(Thing(name=name, key=key, jsmark=change_color_on(a, "lime")))

        return things

    return findall() if everything else current_page()

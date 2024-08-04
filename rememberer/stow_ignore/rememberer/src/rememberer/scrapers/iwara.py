# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def IwaraScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def video_key(urlpath: str) -> Optional[str]:
        for length in [2, 3]:
            try:
                test = extract_path(urlpath, length, 0)
                if test != "video":
                    return None
            except ParseException:
                continue
            return extract_path(urlpath, length, 1)

        return None

    def current_page() -> List[Thing]:
        if (key := video_key(url.path)) is None:
            raise UserError("wrong page")

        title = soup.select_one("section.content div.page-video__details > div.text")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title)
        if not name:
            raise ParseException("title name empty")

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall() -> List[Thing]:
        if video_key(url.path) is not None:
            things = current_page()
        else:
            things = []

        places = [
            "div.page-profile__content div.videoTeaser",
            "div.page-video__sidebar div.videoTeaser",
        ]
        others = [t for p in places for t in soup.select(p)]
        for video in others:
            title = video.select_one("a.videoTeaser__title")
            name = scrape_text(title, recursive=True)

            if "href" not in title.attrs:
                raise ParseException("no href on anchor")

            href = urlparse(title["href"]).path
            if (key := video_key(href)) is None:
                print(href)
                raise ParseException("invalid link in anchor")

            things.append(
                Thing(name=name, key=key, jsmark=change_colors_on(title, "lime"))
            )
        return things

    return findall() if everything else current_page()

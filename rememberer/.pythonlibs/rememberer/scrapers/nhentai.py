# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def get_read_ids():
    import re
    import os

    IDFINDER = re.compile(r"^\[?([0-9]+?)( |\])")
    HENTAIDIR = "/media/NAS/Anime/Manga/nhentai/read"  # TODO: make this configurable
    s = set()

    if os.path.isdir(HENTAIDIR):
        with os.scandir(HENTAIDIR) as it:
            for f in it:
                if match := IDFINDER.match(f.name):
                    s.add(match.group(1))
                else:
                    print(
                        "Unexpected filename: {}".format(f.name),
                        file=sys.stderr,
                    )

    # TODO: cache this
    return s


def marker_factory(everything: bool):
    if not everything:
        return lambda key: "lime"
    else:
        read_ids = get_read_ids()

        def mark_color(key):
            if not read_ids:
                return "red"
            elif key in read_ids:
                return "cyan"
            else:
                return "lime"

        return mark_color


def nhentaiScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    color_fun = marker_factory(everything)

    def video_key(urlpath: str) -> Optional[str]:
        try:
            test = extract_path(urlpath, 2, 0)
            if test != "g":
                return None
        except ParseException:
            return None
        return extract_path(urlpath, 2, 1)

    def current_page() -> List[Thing]:
        if (key := video_key(url.path)) is None:
            raise UserError("wrong page")

        title = soup.select_one("h1.title > span.pretty")
        if not title:
            raise ParseException("couldn't find video title")
        name = scrape_text(title, recursive=True)
        if not name:
            raise ParseException("title name empty")

        return [
            Thing(name=name, key=key, jsmark=change_color_on(title, color_fun(key)))
        ]

    def findall() -> List[Thing]:
        if video_key(url.path) is not None:
            things = current_page()
        else:
            things = []

        for video in soup.select("div.gallery > a"):
            title = video.select_one("div.caption")
            name = scrape_text(title)

            href = urlparse(video["href"]).path
            if (key := video_key(href)) is None:
                raise ParseException("invalid link in tag")

            things.append(
                Thing(name=name, key=key, jsmark=change_color_on(title, color_fun(key)))
            )
        return things

    return findall() if everything else current_page()

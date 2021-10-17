# pylint: disable=wildcard-import, unused-wildcard-import
from .scraping import *


def SpankbangScraper(
    soup: Soup,
    url: ParseResult,
    everything: bool = False,
    **_kwargs,
) -> List[Thing]:
    def video_key(urlpath: str) -> Optional[str]:
        try:
            test = extract_path(urlpath, 3, 1)
            if test != "video":
                return None
        except ParseException:
            return None
        return extract_path(urlpath, 3, 0)

    def current_page() -> List[Thing]:
        if (key := video_key(url.path)) is None:
            raise UserError("wrong page")

        if soup.select_one("div.video_removed_page") is not None:
            raise UserError("video is removed")

        title = soup.select_one("#video > div.left > h1[title]")
        if not title:
            raise ParseException("couldn't find video title")
        name = title["title"]
        if not name:
            raise ParseException("title name empty")

        return [Thing(name=name, key=key, jsmark=change_color_on(title, "lime"))]

    def findall() -> List[Thing]:
        things = ignore_user_error(current_page)

        others = soup.select("div.video-list > div.video-item > a.n")
        for video in others:
            if "href" not in video.attrs:
                continue
            href = video["href"]
            if (key := video_key(href)) is None:
                continue
            name = scrape_text(video, recursive=True)
            things.append(
                Thing(name=name, key=key, jsmark=change_color_on(video, "lime"))
            )
        return things

    return findall() if everything else current_page()

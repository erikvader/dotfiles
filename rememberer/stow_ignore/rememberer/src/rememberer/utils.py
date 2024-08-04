from pathlib import Path
from urllib.parse import ParseResult
from typing import Optional, Iterator, Any, List, Callable
from bs4.element import Tag
from .types import T, SiteName


def create_data_dir() -> Path:
    datadir = Path.home() / ".local" / "share" / "rememberer"
    datadir.mkdir(parents=True, exist_ok=True)
    return datadir


def url_to_site_name(url: ParseResult) -> Optional[SiteName]:
    if not url.hostname:
        return None

    parts: List[str] = url.hostname.lower().split(".")
    if not parts:
        return None
    elif len(parts) <= 2:
        # http://sitename
        # http://sitename.tld
        return parts[0]
    else:
        # http://asd.basd.qwe.sitename.tld
        return parts[-2]


def group(lista: Iterator[T], key: Callable[[T], Any] = lambda x: x) -> List[List[T]]:
    res = []
    cur: List[T] = []
    for i in lista:
        if not cur or key(cur[0]) == key(i):
            cur.append(i)
        else:
            res.append(cur)
            cur = [i]

    if cur:
        res.append(cur)
    return res


def unique_selector(tag: Any, fuzzy_body: bool = False) -> Optional[str]:
    parents = list(tag.parents)
    if len(parents) <= 2:  # tag must be inside body
        return None
    parents = parents[:-2]  # remove html and document (?)
    if parents[-1].name != "body":
        return None
    path = []
    for child, parent in zip([tag] + parents, parents):
        # NOTE: Some sites (youtube) have divs and other tags in <head>, which normally
        # don't belong there. BeautifulSoup 4.11.1 seems to put those tags in <body>
        # instead, making its child cound incorrect. So the direct child of <body> doesn't
        # use nth-child. The returned selector is no longer unique though, but probably,
        # in mose cases.
        if fuzzy_body and parent.name == "body":
            path.append(child.name)
        else:
            children_tags = (c for c in parent.children if isinstance(c, Tag))
            nth_child = next(
                (count + 1 for count, c in enumerate(children_tags) if c is child), None
            )
            if nth_child is None:
                return None
            path.append("{}:nth-child({})".format(child.name, nth_child))
    path.append("body")

    return " > ".join(reversed(path))

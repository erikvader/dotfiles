# pylint: disable=unused-import
from ..types import ParseException, Soup, Thing, UserError
from urllib.parse import parse_qs, ParseResult, urlparse
from beautifulsoupUtils import unique_selector  # type: ignore
from typing import Iterator, Any, List, Optional
import sys


def scrape_text(tag, allow_empty=False, recursive=False) -> str:
    ite = tag.descendants if recursive else tag.children
    s = " ".join(c.strip() for c in ite if isinstance(c, str))
    if not allow_empty and not s:
        raise ParseException("tag doesn't have text")
    return s


def extract_path(path: str, expectedLen: int, want: int) -> str:
    assert want >= 0 and want < expectedLen
    parts = path.strip("/").split("/")
    if len(parts) != expectedLen:
        raise ParseException(
            f"unexpected path in url, expected {expectedLen} parts but got {len(parts)}"
        )
    return parts[want]


def extract_parameter(query: str, param: str) -> str:
    try:
        queries = parse_qs(query, strict_parsing=True)
    except ValueError as e:
        raise ParseException("couldn't parse queries") from e

    if param not in queries:
        raise ParseException(f"'{param}' doesn't exist")

    keys = queries[param]
    if len(keys) != 1:
        raise ParseException(f"there are more than one '{param}'")
    return keys[0]


def change_color_on(tag, color: str) -> str:
    sel = unique_selector(tag)
    if sel is None:
        print("couldn't get selector for {}".format(tag), file=sys.stderr)
        return ""

    return 'document.querySelector("{}").style.color = "{}";\n'.format(sel, color)


def change_colors_on(tags: Iterator[Any], color: str) -> str:
    return ";".join(change_color_on(t, color) for t in tags)

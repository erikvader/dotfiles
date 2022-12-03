# pylint: disable=unused-import
from ..types import ParseException, Soup, Thing, UserError, T
from urllib.parse import parse_qs, ParseResult, urlparse
from beautifulsoupUtils import unique_selector  # type: ignore
from typing import Iterator, Any, List, Optional, Callable
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


def change_color_on(
    tag,
    color: str,
    fuzzy_body: bool = False,
    style: str = "color",
) -> str:
    sel = unique_selector(tag, fuzzy_body=fuzzy_body)
    if sel is None:
        print("couldn't get selector for {}".format(tag), file=sys.stderr)
        return ""

    return 'document.querySelector("{}").style.{} = "{}";\n'.format(sel, style, color)


def change_colors_on(tags: Iterator[Any], color: str) -> str:
    return ";".join(change_color_on(t, color) for t in tags)


def ignore_user_error(f: Callable[..., List[T]], *args, **kwargs) -> List[T]:
    try:
        return f(*args, **kwargs)
    except UserError:
        return []

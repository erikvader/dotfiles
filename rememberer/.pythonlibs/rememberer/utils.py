from pathlib import Path
from urllib.parse import ParseResult
from typing import Optional, Iterator, Any, List, Callable
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

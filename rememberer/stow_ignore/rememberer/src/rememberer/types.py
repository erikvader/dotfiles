from dataclasses import dataclass
from typing import (
    Any,
    List,
    Tuple,
    Callable,
    Optional,
    TypeVar,
)


class ParseException(Exception):
    pass


class UserError(Exception):
    pass


@dataclass(frozen=True)
class Thing:
    name: str
    key: str
    jsmark: Optional[str] = None


Soup = Any
SiteName = str
Scraper = Callable[..., List[Thing]]
T = TypeVar("T")
TaggedThing = Tuple[Thing, bool]

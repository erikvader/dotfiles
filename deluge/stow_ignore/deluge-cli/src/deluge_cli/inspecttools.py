# pyright: strict
from inspect import getdoc
from typing import Any


def shortdoc(obj: Any) -> str:
    """Returns the the whole doc of the given object as a single line."""
    if (doc := getdoc(obj)) is None:
        return ""
    if not (lines := doc.splitlines()):
        return ""
    return " ".join(l for l in lines if l and not l.isspace())

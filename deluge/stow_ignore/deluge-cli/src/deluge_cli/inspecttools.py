# pyright: strict
from inspect import getdoc
from typing import Any, get_origin


def shortdoc(obj: Any) -> str:
    """Returns the the whole doc of the given object as a single line."""
    if (doc := getdoc(obj)) is None:
        return ""
    if not (lines := doc.splitlines()):
        return ""
    return " ".join(l for l in lines if l and not l.isspace())


def base_type(typ: type) -> type:
    return get_origin(typ) or typ

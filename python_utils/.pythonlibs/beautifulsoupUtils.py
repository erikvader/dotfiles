from typing import Any, Optional
from bs4.element import Tag


def unique_selector(tag: Any) -> Optional[str]:
    parents = list(tag.parents)
    if len(parents) <= 2:  # tag must be inside body
        return None
    parents = parents[:-2]  # remove html and document (?)
    if parents[-1].name != "body":
        return None
    path = []
    for child, parent in zip([tag] + parents, parents):
        children_tags = (c for c in parent.children if isinstance(c, Tag))
        nth_child = next(
            (count + 1 for count, c in enumerate(children_tags) if c is child), None
        )
        if nth_child is None:
            return None
        path.append("{}:nth-child({})".format(child.name, nth_child))
    path.append("body")

    return " > ".join(reversed(path))

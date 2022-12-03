from typing import Any, Optional
from bs4.element import Tag


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

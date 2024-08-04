from urllib.parse import urlunparse, ParseResult
from pathlib import Path
from bs4 import BeautifulSoup  # type: ignore
import sys
from .types import UserError, TaggedThing
from .storage import Storage, write_results, write_mark_js
from .utils import url_to_site_name, group
from typing import List, Callable
from .scrapers import get_scraper


def replay(lastoutputfile: Path, prefix: str = "Replay of: ") -> None:
    import shutil

    try:
        # TODO: rb?
        with open(lastoutputfile, "r") as f:
            print(prefix, end="")
            shutil.copyfileobj(f, sys.stdout)
    except FileNotFoundError:
        print("Nothing to replay")


def remember(
    url: ParseResult,
    datadir: Path,
    lastoutputfile: Path,
    lastjsfile: Path,
    quiet: bool = False,
    save: bool = True,
    everything: bool = False,
) -> bool:
    if not url.hostname or not url.scheme:
        raise UserError("url is not fully qualified or is invalid")
    if url.scheme not in ["http", "https"]:
        raise UserError("url must be http or https")

    # TODO: fallback to html5lib somehow if lxml fails? Good if the website is pretty broken
    webpage = BeautifulSoup(sys.stdin, "lxml")

    siteName = url_to_site_name(url)
    if not siteName:
        raise UserError(f"couldn't extract a site name from {url.hostname}")

    scraper = get_scraper(siteName)
    if scraper is None:
        raise UserError("'{}' is not supported".format(url.hostname))

    things = scraper(webpage, url, everything=everything)

    if not things:
        raise Exception("didn't find any things but the scraper didn't error")

    if not all(t.key for t in things) or not all(t.name for t in things):
        raise Exception("some names or keys were empty")

    # TODO: handle if scraper returned duplicate keys
    with Storage(siteName, datadir) as s:
        keys = set(t.key for t in s.read())
        new_things = [t.key not in keys for t in things]
        tagged = list(zip(things, new_things))
        if save:
            s.append(t for t, is_new in tagged if is_new)

    with write_results(lastoutputfile, to_stdout=not quiet) as printer:
        printer(urlunparse(url), fileOnly=True)
        print_new_things(tagged, printer=printer)
    write_mark_js(lastjsfile, iter(tagged))

    return any(new_things)


def print_new_things(
    things: List[TaggedThing],
    printer: Callable[[str], None] = print,
    inverse: bool = False,
) -> None:
    def age_word(say_new: bool) -> str:
        return "new" if say_new else "old"

    def print_all(print_new: bool) -> None:
        new = [elem for elem, isnew in things if isnew == print_new]
        printer("all of the following are " + age_word(print_new))
        for i in new:
            printer(i.name)

    num = len(things)

    if num == 0:
        printer("found nothing")
        return
    elif num == 1:
        if not inverse:
            printer(f"this is {age_word(things[0][1])}")
        else:
            printer("no inverse")
        return

    printer("found a total of {} things".format(num))

    num_new = sum(1 for t, b in things if b)
    only_one_new = num_new == 1
    only_one_old = num - num_new == 1
    if only_one_new or only_one_old:
        target = True if only_one_new else False

        if inverse:
            print_all(not target)
            return

        for t, b in things:
            if b != target:
                continue
            printer(
                "only {} is {}".format(
                    t.name,
                    age_word(target),
                )
            )
            return

    groups = group(iter(things), lambda x: x[1])

    if len(groups) == 1:
        if not inverse:
            printer(f"everything here is {age_word(groups[0][0][1])}")
        else:
            printer("no inverse")
        return

    if len(groups) == 2:
        upper = groups[0]
        lower = groups[1]

        if (len(upper) <= len(lower)) ^ inverse:
            critical = upper[-1]
            where = "above"
        else:
            critical = lower[0]
            where = "below"

        printer(
            "everything {} the following is {}:".format(where, age_word(critical[1]))
        )
        printer(critical[0].name)
        return

    if len(groups) == 3 and len(middle := groups[1]) >= 2:
        if not inverse:
            printer(
                "everything between\n{}\nand\n{}\nis {}".format(
                    middle[0][0].name, middle[-1][0].name, age_word(middle[0][1])
                )
            )
        else:
            print_all(not middle[0][1])
        return

    print_all(True ^ inverse)

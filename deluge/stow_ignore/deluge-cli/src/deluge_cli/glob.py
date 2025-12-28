# pyright: strict
from .inspecttools import shortdoc
import textwrap
from pathlib import PurePath
import fnmatch


def path_match(path: PurePath, glob: str) -> bool:
    """Test whether a path matches a glob ala gitignore.

    The glob is matched against the basename of the path.
    If the glob contains at least one /, then the glob needs to match against
    the entire path. Slashes are special as * does not traverse them, use ** for that.
    The glob can also start with an !, in which case the result
    is inverted. The supported special characters are: *, **, ?, [a-z] and [!a-z].
    A trailing / does NOT check whether the path refers to a directory.
    """
    if not (invert := not glob.startswith("!")):
        glob = glob.removeprefix("!")
    if "/" not in glob:
        glob = "**/" + glob
    return path.full_match(glob) == invert


def str_match(string: str, glob: str) -> bool:
    """Test whether a string matches a glob.

    The glob needs to match the entire string.
    The glob can also start with an !, in which case the result
    is inverted. The supported special characters are: *, ?, [a-z] and [!a-z].
    """
    if not (invert := not glob.startswith("!")):
        glob = glob.removeprefix("!")
    # NOTE: fnmatchcase is two lines: fnmatch.translate to a regex and then re.match that
    # regex on the string.
    return fnmatch.fnmatchcase(string, glob) == invert


def argparse_help() -> str:
    sections = "\n".join(
        ["PATH_GLOB"]
        + textwrap.wrap(
            shortdoc(path_match), initial_indent="  ", subsequent_indent="  "
        )
        + ["", "STRING_GLOB"]
        + textwrap.wrap(
            shortdoc(str_match),
            initial_indent="  ",
            subsequent_indent="  ",
        )
    )
    return "globs:\n" + textwrap.indent(sections, "  ")

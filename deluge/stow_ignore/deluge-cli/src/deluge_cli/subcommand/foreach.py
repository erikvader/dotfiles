# pyright: strict
from dataclasses import dataclass
from functools import cache
from pathlib import Path
import logging
import argparse
import textwrap
from typing import Callable, Any
from .. import parser as P
from .. import glob as G
from ..deluge import Deluge, Torrent, File, State

logger = logging.getLogger(__name__)

subcommand = "foreach"


@dataclass(frozen=True)
class Context:
    deluge: Deluge
    torrent: Torrent


@dataclass(frozen=True)
class FileContext:
    deluge: Deluge
    file: File


def add_bool_algebra(p: P.Parser[Any, Any]):
    def and_func[C](
        ctx: C,
        left: P.Tree[C, bool],
        right: P.Tree[C, bool],
    ) -> bool:
        """The boolean AND-function."""
        return left(ctx) and right(ctx)

    def or_func[C](
        ctx: C,
        left: P.Tree[C, bool],
        right: P.Tree[C, bool],
    ) -> bool:
        """The boolean OR-function."""
        return left(ctx) or right(ctx)

    def not_func[C](
        ctx: C,
        left: P.Tree[C, bool],
    ) -> bool:
        """The boolean NOT-function."""
        return not left(ctx)

    def true_func(
        _ctx: Any,
    ) -> bool:
        """The boolean TRUE."""
        return True

    def false_func(
        _ctx: Any,
    ) -> bool:
        """The boolean FALSE."""
        return False

    p.operator("and", P.Assoc.LEFT, 2, and_func).set_implicit("and")
    p.operator("or", P.Assoc.LEFT, 1, or_func)
    p.unary("not", not_func)
    p.atom("true", true_func)
    p.atom("false", false_func)
    p.set_parens("(", ")")


def add_eachfile(p: P.Parser[Any, Any]):
    fp: P.Parser[FileContext, bool] = P.Parser(
        name="File",
        description=" ".join(
            """
            The expression is evaluated on each file. It is evaluated
            from left to right with short circuit evaluation. Each atom evaluates to
            either true of false.
            """.split()
        ),
    )
    add_bool_algebra(fp)

    def eachfile_func(ctx: Context, sub: P.Tree[FileContext, bool]) -> bool:
        """Run a boolean expression on each file contained in the torrent.

        Each subresult is AND:ed with short circuiting, i.e., `eachfile ( X Y Z )` is the
        logical expression `X AND Y AND Z`. De morgan's law can be used if an OR is
        desired instead, also short circuit. To achieve `X OR Y OR Z` use `not eachfile (
        not X not Y not Z )`.
        """
        return all(
            sub(FileContext(deluge=ctx.deluge, file=f)) for f in ctx.torrent.files
        )

    p.sub("eachfile", fp, eachfile_func)

    def fprint_func(ctx: FileContext) -> bool:
        """Print the current file."""
        print(str(ctx.file))
        return True

    fp.atom("print", fprint_func)


@cache
def term_parser() -> P.Parser[Context, bool]:
    p: P.Parser[Context, bool] = P.Parser(
        name="Torrent",
        description=" ".join(
            """
            The expression is evaluated on each torrent.
            It is evaluated from left to right with short circuit evaluation.
            Each atom evaluates to either true or false.
            """.split()
        ),
    )
    add_bool_algebra(p)
    add_eachfile(p)

    def print_func(ctx: Context) -> bool:
        """Print a short description."""
        print(ctx.torrent)
        return True

    p.atom("print", print_func)

    def printf_func(ctx: Context, fmt: str) -> bool:
        """Print using a python {}-format string with the variable `t` bound to the
        current torrent.
        """
        print(fmt.format(t=ctx.torrent))
        return True

    p.atom("printf", printf_func, P.str1)

    def download_location_func(ctx: Context, glob: str) -> bool:
        """Check if a PATH_GLOB matches the download location"""
        return G.path_match(ctx.torrent.download_location, glob)

    p.atom("download-dir", download_location_func, P.str1)

    def name_func(ctx: Context, glob: str) -> bool:
        """Check if a STRING_GLOB matches the name"""
        return G.str_match(ctx.torrent.name, glob)

    p.atom("name", name_func, P.str1)

    def state_func(ctx: Context, state: State) -> bool:
        """Check for the state"""
        return ctx.torrent.state == state

    def state_mapper(arg: P.Token) -> tuple[State]:
        try:
            return (State[arg.upper()],)
        except ValueError as e:
            e.add_note("Valid values are: " + ", ".join(s.name.lower() for s in State))
            raise

    p.atom("state", state_func, state_mapper)

    def move_func(ctx: Context, new_loc: Path) -> bool:
        """Change where the files are downloaded to"""
        ctx.deluge.move_storage(ctx.torrent.hash, new_loc)
        return True

    p.atom("move", move_func, P.path1)

    def remove_func(ctx: Context) -> bool:
        """Remove without deleting the files"""
        ctx.deluge.remove_torrent(ctx.torrent.hash)
        return True

    p.atom("remove", remove_func)

    def remove_everything_func(ctx: Context) -> bool:
        """Remove and delete the files"""
        ctx.deluge.remove_torrent(ctx.torrent.hash, remove_data=True)
        return True

    p.atom("remove-everything", remove_everything_func)

    def confirm_func(ctx: Context) -> bool:
        """Prompt the user for confirmation"""
        return input(f"{ctx.torrent} (y/N): ") == "y"

    p.atom("confirm", confirm_func)

    return p


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    docs = term_parser().docs()
    epilog = str(docs) + "\n" + G.argparse_help()

    parser = add_parser(
        subcommand,
        description=textwrap.fill("Run a bool expression on each torrent ala find"),
        help="Do something on each torrent",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=epilog,
    )
    parser.add_argument(
        "token",
        nargs="+",
        help=f"An argument for {docs.name}",
        metavar=docs.name + ".TOKEN",
    )


def run(args: argparse.Namespace):
    terms: list[str] = args.token

    logger.info("Running foreach")

    logger.debug("Parsing terms: %s", terms)
    parsed = term_parser().parse(P.Tokens(terms))
    logger.debug("Parsed as: %s", parsed)

    with Deluge() as deluge:
        for tor in deluge.get_torrents():
            ctx = Context(deluge, tor)
            _bool_res: bool = parsed(ctx)

# pyright: strict
from dataclasses import dataclass
from functools import cache
import logging
import argparse
import textwrap
from typing import Callable, Any
from .. import parser as P
from ..deluge import Deluge, Torrent, File

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


@cache
def term_parser() -> P.Parser[Context, bool]:
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

    def add_bool_algebra(p: P.Parser[Any, Any]):
        p.operator("and", P.Assoc.LEFT, 2, and_func).set_implicit("and")
        p.operator("or", P.Assoc.LEFT, 1, or_func)
        p.unary("not", not_func)
        p.atom("true", true_func)
        p.atom("false", false_func)
        p.set_parens("(", ")")

    def print_func(ctx: Context) -> bool:
        """Print a short description."""
        print(ctx.torrent)
        return True

    def printf_func(ctx: Context, fmt: str) -> bool:
        """Print using a python {}-format string with the variable `t` bound to the
        current torrent.
        """
        print(fmt.format(t=ctx.torrent))
        return True

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
    p.atom("print", print_func)
    p.atom("printf", printf_func, P.str1)

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

    def fprint_func(ctx: FileContext) -> bool:
        """Print the current file."""
        print(str(ctx.file))
        return True

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
    fp.atom("print", fprint_func)
    p.sub("eachfile", fp, eachfile_func)

    return p


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    docs = term_parser().docs()

    parser = add_parser(
        subcommand,
        description=textwrap.fill("Run a bool expression on each torrent ala find"),
        help="Do something on each torrent",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=str(docs),
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

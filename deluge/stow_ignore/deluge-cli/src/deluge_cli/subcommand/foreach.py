# pyright: strict
from dataclasses import dataclass
import logging
import argparse
from typing import Callable
from .. import parser as P
from ..deluge import Deluge, Torrent

logger = logging.getLogger(__name__)

subcommand = "foreach"


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    parser = add_parser(
        subcommand,
        description="Run a bool expression on each torrent ala find",
        help="Do something on each torrent",
    )
    parser.add_argument("term", nargs="+", help="A term")


@dataclass(frozen=True)
class Context:
    deluge: Deluge
    torrent: Torrent


# TODO: how to add this to the help?
def term_parser() -> P.Parser[Context, bool]:
    def and_func(
        ctx: Context,
        left: P.Tree[Context, bool],
        right: P.Tree[Context, bool],
    ) -> bool:
        return left(ctx) and right(ctx)

    def or_func(
        ctx: Context,
        left: P.Tree[Context, bool],
        right: P.Tree[Context, bool],
    ) -> bool:
        return left(ctx) or right(ctx)

    def print_func(ctx: Context) -> bool:
        """
        hejsan
        """
        print(ctx.torrent)
        return True

    def printf_func(ctx: Context, fmt: str) -> bool:
        print(fmt.format(t=ctx.torrent))
        return True

    p: P.Parser[Context, bool] = P.Parser()
    p.operator("and", P.Assoc.LEFT, 2, and_func)
    p.operator("or", P.Assoc.LEFT, 1, or_func)
    p.set_parens("(", ")")
    p.atom("print", print_func)
    p.atom("printf", printf_func, P.arg1)
    return p


def run(*, terms: list[str]):
    logger.info("Running foreach")

    logger.debug("Parsing terms: %s", terms)
    parsed = term_parser().parse(P.Tokens(terms))
    logger.debug("Parsed as: %s", parsed)

    with Deluge() as deluge:
        for tor in deluge.get_torrents():
            ctx = Context(deluge, tor)
            _bool_res: bool = parsed(ctx)

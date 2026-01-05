# pyright: strict
from collections import deque
import subprocess as S
from ..threadingtools import fork, CancelToken
from .. import clipboard
from dataclasses import dataclass
from functools import cache
from pathlib import Path
import logging
import argparse
import textwrap
from typing import Callable, Any, Iterable, Sequence
from .. import parser as P
from .. import glob as G
from ..deluge import Deluge, Torrent, File, State, Bytes
from ..timetools import Timestamp, Timeseries, parse_seconds, Seconds

logger = logging.getLogger(__name__)

subcommand = "foreach"


@dataclass(frozen=True)
class FileContext:
    deluge: Deluge
    file: File


@dataclass(frozen=True)
class Context:
    deluge: Deluge
    torrent: Torrent
    other_torrents: Sequence[Torrent]
    is_newly_finished: bool
    downloading_state_since: Timestamp | None
    writing_bytes_since: Timestamp | None
    byte_write_history: Timeseries[Bytes] | None


@dataclass(frozen=True)
class ForeachWorkerArgs:
    loop_secs: Seconds | None
    parsed: P.Tree[Context, bool]
    connection_attempts: int


def add_bool_algebra(p: P.Parser[Any, Any]):
    def and_func[C](
        ctx: C,
        left: P.Tree[C, bool],
        right: P.Tree[C, bool],
    ) -> bool:
        """The boolean AND-function."""
        return left(ctx) and right(ctx)

    def seq_func[C](
        ctx: C,
        left: P.Tree[C, bool],
        right: P.Tree[C, bool],
    ) -> bool:
        """Both sides are evaluated, in order, and the right value is returned."""
        left(ctx)
        return right(ctx)

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
    p.operator(",", P.Assoc.LEFT, 0, seq_func)
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

    def eachfile_apply(ctx: Context, sub: P.Tree[FileContext, bool]) -> Iterable[bool]:
        return (sub(FileContext(deluge=ctx.deluge, file=f)) for f in ctx.torrent.files)

    def eachfile_and_func(ctx: Context, sub: P.Tree[FileContext, bool]) -> bool:
        """Run a boolean expression on each file contained in the torrent.

        Aggregate all sub-results into a final bool using AND, with short-circuiting."""
        return all(eachfile_apply(ctx, sub))

    def eachfile_or_func(ctx: Context, sub: P.Tree[FileContext, bool]) -> bool:
        """Run a boolean expression on each file contained in the torrent.

        Aggregate all sub-results into a final bool using OR, with short-circuiting."""
        return any(eachfile_apply(ctx, sub))

    p.sub("eachfile", fp, eachfile_and_func)

    def any_func(ctx: Context, left: P.Tree[Context, bool]) -> bool:
        """When applied to 'eachfile', aggregate all sub-results with OR instead of AND"""
        assert isinstance(left, P.Sub)
        assert left.func is eachfile_and_func
        left = left.change_callable(eachfile_or_func)
        return left(ctx)

    def any_verifier(tree: P.Tree[Any, Any]):
        match tree:
            case P.Unary() if tree.name() == "any" and tree.inner.name() != "eachfile":
                raise P.ParseError(
                    f"A '{tree.name}' can only be applied to 'eachfile', not {tree.inner.name()}",
                    tree.token,
                )
            case _:
                P.visit_default(any_verifier, tree)

    p.unary("any", any_func)
    p.add_verifier(any_verifier)

    def fprint_func(ctx: FileContext) -> bool:
        """Print the current file relatively to the download dir"""
        print(str(ctx.file))
        return True

    fp.atom("print", fprint_func)

    def path_func(ctx: FileContext, glob: str) -> bool:
        """Check if a PATH_GLOB matches the relative path to this file from the download
        dir."""
        return G.path_match(ctx.file.path, glob)

    fp.atom("path", path_func, P.str1)

    def duplicate_func(ctx: FileContext) -> bool:
        """Check if this file is duplicate of another torrent, i.e., two torrents are
        writing to the same file."""
        # TODO: implement
        return True

    fp.atom("duplicate", duplicate_func)


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
            return (State[arg.name.upper()],)
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

    def send_notification_func(ctx: Context, header: str) -> bool:
        """Send a notification with the given header"""
        try:
            clipboard.send_notification(header, str(ctx.torrent))
        except (OSError, S.CalledProcessError):
            logger.exception("Failed to send a notification")
        return True

    p.atom("send-notification", send_notification_func, P.str1)

    # TODO: atom for just finished
    # TODO: atom for been active for
    # TODO: atom for average download speed, or smth

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
        "--spawn-deluge",
        "-d",
        action="store_true",
        help="Also start deluge and log what it is logging, only makes sense with --loop",
    )
    # TODO: adjust this automatically based on activity. If nothing is happening, then
    # don't loop as often.
    parser.add_argument(
        "--loop",
        "-l",
        type=parse_seconds,
        help="Run the expression over and over this often",
        metavar="SECONDS",
    )
    parser.add_argument(
        "token",
        nargs="+",
        help=f"An argument for {docs.name}",
        metavar=docs.name + ".TOKEN",
    )


class Bookkeeping:
    def __init__(self):
        self._download_stats: dict[
            Torrent, tuple[Timestamp, Timestamp | None, Timeseries[Bytes]]
        ] = {}
        self._unfinished: set[Torrent] = set()

    def download_stats(
        self, t: Torrent
    ) -> tuple[Timestamp, Timestamp | None, Timeseries[Bytes]] | None:
        if t.state == State.DOWNLOADING and t not in self._download_stats:
            ts: Timeseries[Bytes] = Timeseries()
            ts.record(t.total_downloaded)
            self._download_stats[t] = (Timestamp.make(), None, ts)
            logger.debug("Torrent is now tracked for download stats: %s", t)
        elif t.state != State.DOWNLOADING:
            self._download_stats.pop(t, None)
            logger.debug("Torrent is no longer tracked for download stats: %s", t)
        elif t in self._download_stats:
            assert t.state == State.DOWNLOADING
            down_since, written_bytes_since, ts = self._download_stats[t]
            newest = ts.newest()
            assert newest is not None
            now = ts.record(t.total_downloaded)
            if written_bytes_since is None and newest[1] != t.total_downloaded:
                self._download_stats[t] = (down_since, now, ts)
                logger.debug("Torrent is now writing bytes: %s", t)

        return self._download_stats.get(t)

    def is_newly_finished(self, t: Torrent) -> bool:
        if t.is_finished and t in self._unfinished:
            self._unfinished.remove(t)
            return True

        if not t.is_finished:
            self._unfinished.add(t)
        return False


def foreach_worker(c: CancelToken, args: ForeachWorkerArgs):
    book = Bookkeeping()

    with Deluge(connection_attempts=args.connection_attempts) as deluge:
        while True:
            new_torrents: deque[Torrent] = deque(deluge.get_torrents())
            visited_torrents: deque[Torrent] = deque(maxlen=len(new_torrents))

            while new_torrents:
                cur_torrent = new_torrents.popleft()
                other_torrents = [*visited_torrents, *new_torrents]
                download_stats = book.download_stats(cur_torrent)
                ctx = Context(
                    deluge=deluge,
                    other_torrents=other_torrents,
                    torrent=cur_torrent,
                    is_newly_finished=book.is_newly_finished(cur_torrent),
                    downloading_state_since=download_stats and download_stats[0],
                    writing_bytes_since=download_stats and download_stats[1],
                    byte_write_history=download_stats and download_stats[2],
                )

                _bool_res: bool = args.parsed(ctx)

                visited_torrents.append(cur_torrent)

            if args.loop_secs is None or c.is_cancelled_wait(args.loop_secs):
                break


def start_foreach_only(args: ForeachWorkerArgs):
    cancel = CancelToken()
    foreach_worker(cancel, args)


def start_foreach_with_deluge(foreach_args: ForeachWorkerArgs):
    args = ["deluge", "--loglevel", "info"]
    logger.info("Starting deluge with args: %s", args)
    process = S.Popen(
        args,
        text=True,
        stdin=S.DEVNULL,
        stdout=S.PIPE,
        stderr=S.PIPE,
    )

    def log_stdout(c: CancelToken):
        assert process.stdout is not None
        while not c.is_cancelled() and (line := process.stdout.readline()):
            logger.info("Deluge stdout: %s", line.strip())

    def log_stderr(c: CancelToken):
        assert process.stderr is not None
        while not c.is_cancelled() and (line := process.stderr.readline()):
            logger.error("Deluge stderr: %s", line.strip())

    def kill_me():
        logger.error("Killing deluge")
        process.terminate()

    def run_foreach(c: CancelToken):
        foreach_worker(c, foreach_args)

    try:
        with process:
            fork(log_stdout, log_stderr, run_foreach, cancel_callback=kill_me)
    finally:
        retcode = process.returncode
        logger.info("Deluge exited with: %s", retcode)


def run(args: argparse.Namespace):
    terms: list[str] = args.token
    spawn_deluge: bool = args.spawn_deluge
    loop_secs: Seconds | None = args.loop

    if spawn_deluge and loop_secs is None:
        raise RuntimeError(
            "Invalid argument combination, spawning deluge requires loop"
        )

    logger.debug("Parsing terms: %s", terms)
    parsed = term_parser().parse(P.Tokens(terms))
    logger.debug("Parsed as: %s", parsed)

    foreach_args = ForeachWorkerArgs(
        loop_secs=loop_secs,
        parsed=parsed,
        connection_attempts=5 if spawn_deluge else 1,
    )

    if spawn_deluge:
        start_foreach_with_deluge(foreach_args)
    else:
        start_foreach_only(foreach_args)

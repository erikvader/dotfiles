# pyright: strict
from ..spawningpool import Spawnling
from collections import deque
from ..spawningpool import SpawningPoolError
from ..threadingtools import fork, CancelToken, CancelPolicy
from .. import clipboard
from dataclasses import dataclass
from functools import cache
from pathlib import Path
import logging
import argparse
import textwrap
from typing import Callable, Any, Iterable, Sequence, assert_never
from .. import parser as P
from .. import glob as G
from ..deluge import (
    Deluge,
    Torrent,
    File,
    DelugeDisconnectedError,
    State,
    Bytes,
    BytesPerSecond,
    parse_bytes_per_second,
    clean_torrent_files,
)
from ..timetools import Timestamp, Timeseries, parse_seconds, Seconds

logger = logging.getLogger(__name__)

subcommand = "foreach"


@dataclass(frozen=True)
class Context:
    deluge: Deluge
    torrent: Torrent
    other_torrents: Sequence[Torrent]
    is_newly_finished: bool
    downloading_state_since: Timestamp | None
    writing_bytes_since: Timestamp | None
    byte_write_history: Timeseries[Bytes] | None
    is_duplicated: bool


@dataclass(frozen=True)
class FileContext:
    parent: Context
    file: File


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
        return (sub(FileContext(parent=ctx, file=f)) for f in ctx.torrent.files)

    def eachfile_and_func(ctx: Context, sub: P.Tree[FileContext, bool]) -> bool:
        """Run a boolean expression on each file contained in the torrent.

        Aggregate all sub-results into a final bool using AND, with short-circuiting."""
        return all(eachfile_apply(ctx, sub))

    def eachfile_or_func(ctx: Context, sub: P.Tree[FileContext, bool]) -> bool:
        """Run a boolean expression on each file contained in the torrent.

        Aggregate all sub-results into a final bool using OR, with short-circuiting."""
        return any(eachfile_apply(ctx, sub))

    p.sub("foreach-file", fp, eachfile_and_func)

    def any_func(ctx: Context, left: P.Tree[Context, bool]) -> bool:
        """When applied to 'foreach-file', aggregate all sub-results with OR instead of AND"""
        assert isinstance(left, P.Sub)
        assert left.func is eachfile_and_func
        left = left.change_callable(eachfile_or_func)
        return left(ctx)

    def any_verifier(tree: P.Tree[Any, Any]):
        match tree:
            case P.Unary() if (
                tree.name() == "any" and tree.inner.name() != "foreach-file"
            ):
                raise P.ParseError(
                    f"A '{tree.name}' can only be applied to 'foreach-file', not {tree.inner.name()}",
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
        """If this file is written to by multiple torrents, then return True and print
        which ones, else return False"""
        retval = False
        me = ctx.parent.torrent
        me_abs = me.download_location / ctx.file.path
        for other in ctx.parent.other_torrents:
            for file in other.files:
                other_abs = other.download_location / file.path
                if me_abs == other_abs:
                    print(f"{other} - {me_abs}")
                    retval = True
        return retval

    fp.atom("print-duplicate", duplicate_func)


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

    def iname_func(ctx: Context, glob: str) -> bool:
        """Check if a STRING_GLOB matches the name case-insensitively, i.e. the given glob
        as is on the name case folded."""
        return G.str_match(ctx.torrent.name.casefold(), glob)

    p.atom("iname", iname_func, P.str1)

    def icontains_func(ctx: Context, string: str) -> bool:
        """Check if string is a substring of the name, case-insensitively. This is similar
        to "iname *STR*", but without having to add and escape asterisks."""
        return string.casefold() in ctx.torrent.name.casefold()

    p.atom("icontains", icontains_func, P.str1)

    def hash_func(ctx: Context, prefix: str) -> bool:
        """Check if the hash (id) has this prefix"""
        return ctx.torrent.hash.has_prefix(prefix)

    p.atom("hash", hash_func, P.str1)

    def state_func(ctx: Context, state: State) -> bool:
        """Check if the torrent is in this state"""
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
        ctx.deluge.move_storage(ctx.torrent.hash, new_loc.absolute())
        return True

    p.atom("move", move_func, P.path1)

    def queue_bottom_func(ctx: Context) -> bool:
        """Queue this torrent to the bottom"""
        ctx.deluge.queue_bottom(ctx.torrent.hash)
        return True

    p.atom("queue-bottom", queue_bottom_func)

    def remove_func(ctx: Context) -> bool:
        """Remove the torrent from deluge without deleting any files"""
        ctx.deluge.remove_torrent(ctx.torrent.hash)
        return True

    p.atom("remove-keep", remove_func)

    def remove_everything_func(ctx: Context) -> bool:
        """Remove the torrent from deluge and delete all files"""
        ctx.deluge.remove_torrent(ctx.torrent.hash, remove_data=True)
        return True

    p.atom("remove-everything", remove_everything_func)

    def finish_torrent_func(ctx: Context) -> bool:
        """Remove the torrent from deluge and delete skipped files/directories."""
        ctx.deluge.remove_torrent(ctx.torrent.hash)
        clean_torrent_files(ctx.torrent)

        return True

    p.atom("remove", finish_torrent_func)

    def confirm_func(ctx: Context) -> bool:
        """Prompt the user for confirmation"""
        return input(f"{ctx.torrent} (y/N): ") == "y"

    p.atom("confirm", confirm_func)

    def is_duplicated_func(ctx: Context) -> bool:
        """If some of this torrent's files are being written to by other torrents as
        well. This will not trigger on the first torrent, only on the others"""
        return ctx.is_duplicated

    p.atom("is-duplicated", is_duplicated_func)

    def send_notification_func(ctx: Context, header: str) -> bool:
        """Send a notification with the given header"""
        try:
            clipboard.send_notification(header, str(ctx.torrent))
        except SpawningPoolError:
            logger.exception("Failed to send a notification")
        return True

    p.atom("send-notification", send_notification_func, P.str1)

    def just_finished_func(ctx: Context) -> bool:
        """Check whether the torrent got finished since the last loop, which only works
        when looping"""
        return ctx.is_newly_finished

    p.atom("just-finished", just_finished_func)

    def download_slower_func(
        ctx: Context, over: Seconds, below: BytesPerSecond
    ) -> bool:
        """Check whether the average download rate averaged over the first argument
        (seconds) is below the second argument (bytes per second). Only works when
        looping"""
        assert over > 0
        assert below >= 0
        if ctx.byte_write_history is None:
            return True
        return ctx.byte_write_history.slope(over, Seconds(0)) <= below

    def rate_args_mapper(
        arg1: P.Token, arg2: P.Token
    ) -> tuple[Seconds, BytesPerSecond]:
        over = parse_seconds(arg1.name)
        if over <= 0:
            raise ValueError("Interval must be positive")
        b = parse_bytes_per_second(arg2.name)
        if b < 0:
            raise ValueError("Threshold must not be negative")
        return (over, BytesPerSecond(b))

    p.atom("download-rate-le", download_slower_func, rate_args_mapper)

    def active_for_func(ctx: Context, for_atleast: Seconds) -> bool:
        """Check whether this torrent has been active, i.e. been in DOWNLOADING state, for
        at least this long. Only works when looping"""
        assert for_atleast > 0
        if ctx.downloading_state_since is None:
            return False
        return ctx.downloading_state_since.age() >= for_atleast

    def seconds_args_mapper(arg1: P.Token) -> tuple[Seconds]:
        over = parse_seconds(arg1.name)
        if over <= 0:
            raise ValueError("Interval must be positive")
        return (over,)

    p.atom("active-for-ge", active_for_func, seconds_args_mapper)

    def writing_for_func(ctx: Context, for_atleast: Seconds) -> bool:
        """Check whether this torrent has been writing new bytes for at least this long,
        i.e. actually been downloading. Only works when looping"""
        assert for_atleast > 0
        if ctx.writing_bytes_since is None:
            return False
        return ctx.writing_bytes_since.age() >= for_atleast

    p.atom("writing-for-ge", writing_for_func, seconds_args_mapper)

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

        elif t.state != State.DOWNLOADING and t in self._download_stats:
            self._download_stats.pop(t)
            logger.debug("Torrent is no longer tracked for download stats: %s", t)

        elif t.state == State.DOWNLOADING and t in self._download_stats:
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

    def prepare_duplicate(self) -> set[Path]:
        return set()

    def is_duplicated(self, t: Torrent, seen_files: set[Path]) -> bool:
        len_before = len(seen_files)
        seen_files.update((t.download_location / f.path for f in t.files))
        return len_before + len(t.files) != len(seen_files)


def foreach_worker(c: CancelToken, args: ForeachWorkerArgs):
    book = Bookkeeping()

    with Deluge(connection_attempts=args.connection_attempts) as deluge:
        while True:
            new_torrents: deque[Torrent] = deque(deluge.get_torrents())
            visited_torrents: deque[Torrent] = deque(maxlen=len(new_torrents))
            dup_cookie = book.prepare_duplicate()

            while new_torrents:
                cur_torrent = new_torrents.popleft()
                other_torrents = [*visited_torrents, *new_torrents]
                match book.download_stats(cur_torrent):
                    case None:
                        downloading_state_since = None
                        writing_bytes_since = None
                        byte_write_history = None
                    case (d, w, b):
                        downloading_state_since = d
                        writing_bytes_since = w
                        byte_write_history = b
                    case _ as unreachable:
                        assert_never(unreachable)

                ctx = Context(
                    deluge=deluge,
                    other_torrents=other_torrents,
                    torrent=cur_torrent,
                    is_newly_finished=book.is_newly_finished(cur_torrent),
                    downloading_state_since=downloading_state_since,
                    writing_bytes_since=writing_bytes_since,
                    byte_write_history=byte_write_history,
                    is_duplicated=book.is_duplicated(cur_torrent, dup_cookie),
                )

                _bool_res: bool = args.parsed(ctx)

                visited_torrents.append(cur_torrent)

            if args.loop_secs is None or c.is_cancelled_wait(args.loop_secs):
                break


def start_foreach_only(args: ForeachWorkerArgs):
    cancel = CancelToken()
    foreach_worker(cancel, args)


def start_foreach_with_deluge(foreach_args: ForeachWorkerArgs):
    deluge_spawnling = Spawnling.spawn("deluge", "--loglevel", "warning")
    deluged_spawnling = Spawnling.spawn(
        "deluged", "--loglevel", "error", "--do-not-daemonize"
    )

    def wait_on_deluge(_c: CancelToken):
        deluge_spawnling.heavy()

    def wait_on_deluged(c: CancelToken):
        try:
            deluged_spawnling.heavy()
        finally:
            c.cancel()
            deluge_spawnling.damage()

    def wait_on_foreach(c: CancelToken):
        try:
            foreach_worker(c, foreach_args)
        except DelugeDisconnectedError as e:
            logger.warning(
                "Foreach got interrupted by losing the connection to deluge", exc_info=e
            )

    fork(
        wait_on_foreach,
        wait_on_deluge,
        wait_on_deluged,
        # NOTE: if a KeyboardInterrupt is raised, then it is assumed that both deluge and
        # deluged also received a SIGINT, so no need to send SIGTERM to them.
        cancel_policy=CancelPolicy.KBD_INT,
        thread_prefix_name="foreach",
    )


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

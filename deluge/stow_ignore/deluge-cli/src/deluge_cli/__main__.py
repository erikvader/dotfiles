# pyright: strict
from typing import TextIO
import logging
import argparse
import sys
from pathlib import Path
from .subcommand import foreach, add, core, monitor
from .loggingtools import supports_color, AnsiColorFormatter, NoExceptionFilter


logger = logging.getLogger(__name__)


def setup_logging(
    *,
    stream_level: int = logging.WARNING,
    stream: TextIO = sys.stderr,
    colors: str = "auto",
    logfile: Path | None = None,
    file_level: int = logging.DEBUG,
):
    format_str = "%(asctime)s %(name)s [%(levelname)s] %(message)s"
    msec_format = "%s.%03d"

    root = logging.getLogger()
    all_levels = [stream_level] + ([file_level] if logfile is not None else [])
    root.setLevel(min(all_levels))

    handler = logging.StreamHandler(stream)
    handler.setLevel(stream_level)
    if colors == "yes" or colors == "auto" and supports_color(stream):
        formatter = AnsiColorFormatter(format_str)
    else:
        formatter = logging.Formatter(format_str)
    formatter.default_msec_format = msec_format
    handler.setFormatter(formatter)
    handler.addFilter(NoExceptionFilter())
    root.addHandler(handler)

    if logfile is not None:
        fhandler = logging.FileHandler(logfile, mode="a", encoding="utf-8")
        fhandler.setLevel(file_level)
        fformatter = logging.Formatter(format_str)
        fformatter.default_msec_format = msec_format
        fhandler.setFormatter(fformatter)
        root.addHandler(fhandler)

    logging.getLogger("deluge_client.client").setLevel(logging.INFO)

    NoExceptionFilter.install_excepthooks()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="A better CLI for Deluge")
    parser.add_argument(
        "--verbose",
        "-v",
        action="count",
        default=0,
        help="Increase stderr verbosity, can be applied twice",
    )
    parser.add_argument(
        "--logfile", type=Path, help="Open this file and log to it", metavar="path"
    )

    subparsers = parser.add_subparsers(
        required=True,
        dest="subcommand",
        description="The subcommand to run",
        metavar="subcommand",
    )

    foreach.argparse_add_subcommand(subparsers.add_parser)
    add.argparse_add_subcommand(subparsers.add_parser)
    core.argparse_add_subcommand(subparsers.add_parser)
    monitor.argparse_add_subcommand(subparsers.add_parser)

    return parser.parse_args()


def main():
    args = parse_args()

    match args.verbose:
        case 1:
            stream_level = logging.INFO
        case 2:
            stream_level = logging.DEBUG
        case _:
            stream_level = logging.WARNING

    setup_logging(stream_level=stream_level, logfile=args.logfile)

    logger.debug("Parsed args: %s", args)
    match args.subcommand:
        case foreach.subcommand:
            foreach.run(args)
        case monitor.subcommand:
            monitor.run(args)
        # TODO: action to find files overwriting each other
        case add.subcommand:
            add.run(args)
        case core.subcommand:
            core.run(args)
        case _:
            raise ValueError("An invalid subcommand entered somehow")


if __name__ == "__main__":
    main()

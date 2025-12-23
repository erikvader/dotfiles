# pyright: strict
from typing import TextIO, override
import logging
import argparse
import sys
import os
from pathlib import Path
from .subcommand import foreach, add


logger = logging.getLogger(__name__)


class AnsiColorFormatter(logging.Formatter):
    @override
    def format(self, record: logging.LogRecord):
        no_style = "\033[0m"
        bold = "\033[1m"
        grey = "\033[90m"
        yellow = "\033[33m"
        red = "\033[31m"
        red_light = "\033[31m"
        start_style = {
            "DEBUG": grey,
            "INFO": no_style,
            "WARNING": yellow + bold,
            "ERROR": red,
            "CRITICAL": red_light + bold,
        }.get(record.levelname, no_style)
        end_style = no_style
        return f"{start_style}{super().format(record)}{end_style}"


class NoExceptionFilter(logging.Filter):
    """Log the exception that caused this script to crash while preventing the stacktrace
    to be printed twice to stderr.
    """

    @staticmethod
    def log():
        logging.exception("Uncaught exception", extra={"suppress_stream": True})

    @override
    def filter(self, record: logging.LogRecord) -> bool:
        return not (hasattr(record, "suppress_stream") and record.suppress_stream)  # type: ignore


def supports_color(stream: TextIO) -> bool:
    return os.getenv("TERM") != "dumb" and stream.isatty()


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

    _monitor_parser = subparsers.add_parser(
        "monitor",
        description="Monitor events and manage torrents",
        help="Monitor events and manage torrents",
    )

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
    try:
        match args.subcommand:
            case foreach.subcommand:
                foreach.run(args)
            case "monitor":
                # TODO: this should also be able to start deluge and log whatever it is logging
                # run_monitor()
                pass
            # TODO: action to find files overwriting each other
            # TODO: action to list saved download dirs in a core subocmmand or smth
            case add.subcommand:
                add.run(args)
            case _:
                raise ValueError("An invalid subcommand entered somehow")
    except:
        NoExceptionFilter.log()
        raise


if __name__ == "__main__":
    main()

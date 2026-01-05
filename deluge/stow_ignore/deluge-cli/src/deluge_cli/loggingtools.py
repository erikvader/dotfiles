# pyright: strict
from typing import TextIO, override, Type, Any
from types import TracebackType
import threading
import logging
import sys
import os


def supports_color(stream: TextIO) -> bool:
    """Return true if, e.g. stderr, most likely supports ANSI codes."""
    return os.getenv("TERM") != "dumb" and stream.isatty()


class AnsiColorFormatter(logging.Formatter):
    """Make the whole log line a color depending on its level."""

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
    to be printed twice to stderr due to the default exception handling.

    This filter must be added to the stderr StreamHandler for anything to be filtered. It
    also needs to be installed after logging has been initialized.
    """

    @staticmethod
    def _log_exc(
        typ: Type[BaseException],
        value: BaseException,
        tb: TracebackType | None,
    ):
        # NOTE: critical since the program exits
        logging.critical(
            "Uncaught exception",
            exc_info=(typ, value, tb),
            extra={"suppress_stream": True},
        )
        sys.__excepthook__(typ, value, tb)

    @staticmethod
    def _log_exc_thread(args: threading.ExceptHookArgs):
        typ: Type[BaseException] = args.exc_type
        value: BaseException | None = args.exc_value
        tb: TracebackType | None = args.exc_traceback
        thr: threading.Thread | None = args.thread
        # NOTE: error since the programs doesn't exit automatically
        logging.error(
            "Uncaught exception in thread %s",
            thr.name if thr is not None else thr,
            # NOTE: these values can be None in a threading except hook, and the exc_info
            # parameter wants them as non-None, but the logging module should be able to
            # handle that anyways. It just forwards to traceback.print_exception, by
            # default, and that can handle the parameters as None.
            exc_info=(typ, value, tb),  # type: ignore
            extra={"suppress_stream": True},
        )
        # TODO: pyright says this doesn't exist for some reason?
        threading.__excepthook__(args)  # type: ignore

    @staticmethod
    def install_excepthooks():
        sys.excepthook = NoExceptionFilter._log_exc
        threading.excepthook = NoExceptionFilter._log_exc_thread

    @override
    def filter(self, record: logging.LogRecord) -> bool:
        return not (hasattr(record, "suppress_stream") and record.suppress_stream)  # type: ignore


class Ellipses:
    """Wrap a value to limit its string representation to a given max length."""

    def __init__(self, inner: Any, width: int = 30, elips: str = "..."):
        assert width >= len(elips)
        self.inner = inner
        self.width = width
        self.elips = elips

    def trunc(self, string: str) -> str:
        if len(string) > self.width:
            return string[: self.width - len(self.elips)] + self.elips
        return string

    def __repr__(self) -> str:
        return self.trunc(repr(self.inner))

    def __str__(self) -> str:
        return self.trunc(str(self.inner))

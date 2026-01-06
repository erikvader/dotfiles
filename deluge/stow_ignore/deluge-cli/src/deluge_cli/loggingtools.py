# pyright: strict
from typing import TextIO, override, Type, Any
from types import TracebackType
import threading
import logging
import sys
import os
import io
import traceback


def supports_color(stream: TextIO) -> bool:
    """Return true if, e.g. stderr, most likely supports ANSI codes."""
    # TODO: use _colorize.can_colorize(file=stream) when stable
    return os.getenv("TERM") != "dumb" and stream.isatty()


class AnsiColorFormatter(logging.Formatter):
    """Make the whole log line a color depending on its level."""

    # TODO: use _colorize.ANSIColors when stable
    no_style = "\033[0m"
    bold = "\033[1m"
    grey = "\033[90m"
    yellow = "\033[33m"
    red = "\033[31m"

    @override
    def format(self, record: logging.LogRecord):
        start_style = {
            "DEBUG": self.grey,
            "INFO": self.no_style,
            "WARNING": self.yellow + self.bold,
            "ERROR": self.red,
            "CRITICAL": self.red + self.bold,
        }.get(record.levelname, self.no_style)
        end_style = self.no_style
        return f"{start_style}{super().format(record)}{end_style}"

    @override
    def formatException(
        self,
        ei: tuple[
            type[BaseException] | None, BaseException | None, TracebackType | None
        ],
    ) -> str:
        # NOTE: this is basically the same as the default implementation, except that it
        # is cleaned up and a secret (at least on python 3.13) kwarg is supplied to
        # print_exception that forces the standard colorisation to be added.
        # https://github.com/python/cpython/blob/d9c1235db44100b16355a347ca9e304df48411a7/Lib/traceback.py#L136
        with io.StringIO() as sio:
            print(self.no_style, file=sio, end="")
            traceback.print_exception(ei[0], ei[1], ei[2], limit=None, file=sio, colorize=True)  # type: ignore
            return sio.getvalue().removesuffix("\n")


class ExceptionLogHooks:
    """Make uncaught exceptions get logged to the root logger"""

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
        )

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
        )

    @staticmethod
    def install_excepthooks():
        sys.excepthook = ExceptionLogHooks._log_exc
        threading.excepthook = ExceptionLogHooks._log_exc_thread


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

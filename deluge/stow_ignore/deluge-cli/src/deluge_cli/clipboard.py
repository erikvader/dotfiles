# pyright: strict
import subprocess as S
import logging
from typing import Generator, Callable
import time

logger = logging.getLogger(__name__)


def paste() -> str:
    """Return the current clipboard contents.

    If the clipboard doesn't contain anything or it contains something that isn't text,
    like an image, then the empty string is returned.

    """
    logger.debug("Running xclip -out")
    try:
        completed = S.run(
            ["xclip", "-selection", "clipboard", "-out"],
            check=True,
            stdin=S.DEVNULL,
            stdout=S.PIPE,
            stderr=S.PIPE,
            # NOTE: It seems like xclip will always output as UTF-8 based on its '-noutf8'
            # flag.
            encoding="utf-8",
        )
    except OSError as e:
        e.add_note("Is xclip installed?")
        raise
    except S.CalledProcessError as e:
        logger.debug("xclip: %s", e)
        logger.debug("xclip stdout: '%s'", e.stdout)
        logger.debug("xclip stderr: '%s'", e.stderr)

        # NOTE: the clipboard is empty
        if e.stderr == "Error: target STRING not available\n":
            return ""

        raise

    logger.debug("xclip: %s", completed)
    return completed.stdout


# TODO: there is a small race condition in which a new thing could be copied between
# invocations of clipnotify. There are flags to resolve this, but those arent in the
# released version yet. https://github.com/cdown/clipnotify/issues/24. Another problem is
# that clipnotify also listens on the primary clipboard, which I don't care about, and
# that is also solved with an unreleased flag.
def clipnotify():
    logger.debug("Running clipnotify")
    completed = S.run(
        ["clipnotify"], check=True, stdin=S.DEVNULL, stdout=S.DEVNULL, stderr=S.DEVNULL
    )
    logger.debug("clipnotify: %s", completed)


def pause():
    logger.debug("Sleeping a short while")
    time.sleep(1)
    logger.debug("Done sleeping")


def get_waiter() -> Callable[[], None]:
    has_clipnotify = True

    def wait():
        nonlocal has_clipnotify
        if has_clipnotify:
            try:
                clipnotify()
            except OSError as e:
                logger.warning(
                    "Failed to start clipnotify, is it installed? Falling back to polling..."
                )
                logger.debug("Failed to start clipnotify", exc_info=e)
                has_clipnotify = False
        else:
            pause()

    return wait


def clipboard_listener() -> Generator[str]:
    """Endlessly wait for new clipboard contents."""
    waiter = get_waiter()
    old = paste()
    while True:
        waiter()
        new = paste()
        if new != old:
            old = new
            yield new

# pyright: strict
from .spawningpool import Spawnling, FailedToSpawnError
import logging
from typing import Generator, Callable
import time

logger = logging.getLogger(__name__)


def paste() -> str:
    """Return the current clipboard contents.

    If the clipboard doesn't contain anything or it contains something that isn't text,
    like an image, then the empty string is returned.

    """
    corpse = Spawnling.spawn("xclip", "-selection", "clipboard", "-out").quick()
    # NOTE: the clipboard is empty
    if corpse.stderr == "Error: target STRING not available\n":
        return ""

    corpse.check()

    return corpse.stdout


# TODO: there is a small race condition in which a new thing could be copied between
# invocations of clipnotify. There are flags to resolve this, but those arent in the
# released version yet. https://github.com/cdown/clipnotify/issues/24. Another problem is
# that clipnotify also listens on the primary clipboard, which I don't care about, and
# that is also solved with an unreleased flag.
def clipnotify():
    Spawnling.spawn("clipnotify").quick().check()


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
            except FailedToSpawnError as e:
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


# TODO: This doesn't belong in this file
def send_notification(summary: str, body: str = ""):
    Spawnling.spawn("notify-send", summary, body).quick().check()

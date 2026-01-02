# pyright: strict
from typing import Callable, assert_never
import threading
import logging
from dataclasses import dataclass

# TODO: remove?
logger = logging.getLogger(__name__)


class CancelToken:
    def __init__(
        self,
        *,
        parent: "CancelToken | None" = None,
        callback: Callable[[], None] | None = None,
    ):
        """Create a cancel token.

        parent: another token that, if cancelled, will also cancel this.
        callback: a callback run when this token is cancelled. Is only run once but from
                  an arbitrary thread.
        """
        self._own_flag = threading.Event()
        self._parent = parent
        self._callback: Callable[[], None] | Exception | None = callback
        self._callback_lock = threading.Lock()

    def cancel(self):
        self._own_flag.set()
        if self._callback_lock.acquire(blocking=False):
            if self._callback is not None:
                assert callable(self._callback)
                try:
                    self._callback()
                # pylint: disable=broad-exception-caught
                except Exception as e:
                    e.add_note("This happened in a CancelToken callback")
                    self._callback = e

    # TODO: make this also wait on the parent(s) somehow
    def is_cancelled_wait(self, timeout: float | None = None) -> bool:
        self._own_flag.wait(timeout)
        return self.is_cancelled()

    def is_cancelled(self) -> bool:
        if self._own_flag.is_set():
            return True

        if self._parent is not None and self._parent.is_cancelled():
            self.cancel()
            return True

        return False

    def get_callback_exception(self) -> Exception | None:
        if self._callback_lock.locked():
            assert not callable(self._callback)
            return self._callback


@dataclass(frozen=True)
class Success[T]:
    value: T


@dataclass(frozen=True)
class Failure[E]:
    value: E


class Result[T, E]:
    def __init__(self):
        self._value: Success[T] | Failure[E] | None = None
        self._lock = threading.Lock()

    def set_value(self, val: T):
        if not self._lock.acquire(blocking=False):
            raise RuntimeError("Not allowed to set twice")
        self._value = Success(val)

    def set_error(self, err: E):
        if not self._lock.acquire(blocking=False):
            raise RuntimeError("Not allowed to set twice")
        self._value = Failure(err)

    def get(self) -> Success[T] | Failure[E] | None:
        if self._lock.locked():
            return self._value
        return None

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}[{self.get()}]"


# NOTE: pylint gets confused
# pylint: disable=redefined-outer-name
def _worker[T](
    cancel: CancelToken, work: Callable[[CancelToken], T], res: Result[T, Exception]
):
    try:
        r = work(cancel)
    # pylint: disable=broad-exception-caught
    except Exception as e:
        e.add_note("This happened in a fork worker")
        # TODO: The backtrace stored in this exception contains all local variables the
        # thread had when this was raised. Call traceback.clear_frames()?
        res.set_error(e)
        cancel.cancel()
    except:
        cancel.cancel()
        raise
    else:
        res.set_value(r)


def fork[T](
    *fs: Callable[[CancelToken], T],
    parent: CancelToken | None = None,
    cancel_callback: Callable[[], None] | None = None,
) -> list[T]:
    if not fs:
        return []

    cancel = CancelToken(parent=parent, callback=cancel_callback)
    results: list[Result[T, Exception]] = []
    threads: list[threading.Thread] = []

    for f in fs[:-1]:
        res: Result[T, Exception] = Result()
        results.append(res)
        t = threading.Thread(target=_worker, args=(cancel, f, res))
        threads.append(t)
        t.start()

    res_last: Result[T, Exception] = Result()
    results.append(res_last)
    try:
        _worker(cancel, fs[-1], res_last)
        for t in threads:
            t.join()
    # NOTE: keyboardinterrupt is only raised on the main thread, so it is fine to only
    # do something about it here and not also in the threads.
    # NOTE: subprocess.run, from the standard library, also catches everything, kills and
    # waits again, so it's probably fine to do that here as well.
    except:
        # TODO: try catch these again and add as many failures as possible from the
        # results to an exceptiongroup that is the cause of the keybordinterrupt?
        cancel.cancel()
        for t in threads:
            t.join()
        raise

    successes: list[T] = []
    failures: list[Exception] = []
    for r in results:
        match r.get():
            case Success(val):
                successes.append(val)
            case Failure(val):
                failures.append(val)
            case None:
                # NOTE: a thread shouldn't experience KeyboardInterrupt and SystemExit
                raise RuntimeError("A thread failed in an unexpected way")
            case _ as unreachable:
                assert_never(unreachable)

    assert len(successes) + len(failures) == len(fs)

    if (cancel_exc := cancel.get_callback_exception()) is not None:
        failures.append(cancel_exc)

    if failures:
        raise ExceptionGroup("One or more threads raised an exception", failures)

    return successes

# pyright: strict
import sys
from typing import Callable, assert_never
import threading
import logging
from dataclasses import dataclass

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
    """A thread return value, either an exception or a value."""

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
    cancel: CancelToken,
    work: Callable[[CancelToken], T],
    res: Result[T, Exception],
):
    thread_name = threading.current_thread().name
    work_name = work.__qualname__
    logger.debug("Starting worker in thread %s running %s", thread_name, work_name)

    try:
        r = work(cancel)
    # pylint: disable=broad-exception-caught
    except Exception as e:
        state = "error " + type(e).__name__
        e.add_note(f"Worker {work_name} in thread {thread_name}")
        # TODO: The backtrace stored in this exception contains all local variables the
        # thread had when this was raised. Call traceback.clear_frames()?
        res.set_error(e)
        cancel.cancel()
    except:
        e = sys.exception()
        assert e is not None
        state = "serious error " + type(e).__name__
        e.add_note(f"Worker {work_name} in thread {thread_name}")
        cancel.cancel()
        raise
    else:
        state = "success"
        res.set_value(r)
    finally:
        logger.debug(
            "Worker in thread %s exiting, ran %s, state %s",
            thread_name,
            work_name,
            state,
        )


def fork[T](
    *fs: Callable[[CancelToken], T],
    parent: CancelToken | None = None,
    cancel_callback: Callable[[], None] | None = None,
) -> list[T]:
    """Run several functions concurrently in their own threads and return their return values.

    This function runs all functions in fs in their own threads, except for the first
    which is run on the current thread, and collects all their results in a list in order.
    If one or more functions raise an exception, then this function raises an exception
    group with all of them.

    Since the first is run on the current thread, it means it can receive
    KeyboardInterrupts and such, if it is the main thread. When that happens, all other
    threads are cancelled and waited upon before re-raising the interrupt. A second
    interrupt can be sent to interrupt the waiting, but that will leave the threads
    running, and they will be force quit when the interpreter exits since they are daemon
    threads. Except for this case, this function makes sure all threads have exited before
    returning.

    Threads are canceled using a CancelToken if any thread raises an exception. Each
    thread must periodically check this token to see if they should exit. A callable can
    be supplied that is run when the token is canceled to help the threads to exit. A
    parent token can also be supplied, where if that token is canceled the child token is
    also canceled, but not the other way around. Useful when a callable calls another
    fork.

    """
    if not fs:
        return []

    cancel = CancelToken(parent=parent, callback=cancel_callback)
    results: list[Result[T, Exception]] = [Result()]
    threads: list[threading.Thread] = []

    for f in fs[1:]:
        res: Result[T, Exception] = Result()
        results.append(res)
        t = threading.Thread(target=_worker, args=(cancel, f, res), daemon=True)
        threads.append(t)
        t.start()

    try:
        _worker(cancel, fs[0], results[0])
        for t in threads:
            t.join()
    # NOTE: keyboardinterrupt is only raised on the main thread, so it is fine to only
    # do something about it here and not also in the threads.
    except:
        e = sys.exception()
        assert e is not None
        # NOTE: this logs so the user know that an interrupt has been received and we are
        # trying to exit. It is critical since all threads are canceled and their return
        # values and exceptions are thrown away, but not sure if that is the best log
        # level to use.
        logger.critical(
            "Received a %s, canceling all other threads and waiting on them",
            type(e).__name__,
        )
        # TODO: try catch these again and add as many failures as possible from the
        # results to an exceptiongroup that is the cause of the keybordinterrupt?
        cancel.cancel()
        for t in threads:
            t.join()
        raise

    assert all(not t.is_alive() for t in threads)

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

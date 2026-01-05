from pytest import mark, raises
from deluge_cli.threadingtools import fork, CancelToken
from threading import Event


def test_empty_fork() -> None:
    assert fork() == []


def test_one_worker() -> None:
    res = fork(lambda c: 5)
    assert res == [5]

    with raises(ExceptionGroup) as exc:
        fork(lambda c: int("hej"))
    assert len(exc.value.exceptions) == 1
    assert isinstance(exc.value.exceptions[0], ValueError)


def test_two_workers() -> None:
    def worker1(_c: CancelToken) -> int:
        return 1

    def worker2(_c: CancelToken) -> int:
        return 2

    assert fork(worker1, worker2) == [1, 2]


def test_two_cancel() -> None:
    event_before = Event()

    def worker1(_c: CancelToken) -> int:
        event_before.wait()
        raise ValueError("oh noes")

    event_after = Event()

    def worker2(c: CancelToken) -> int:
        event_before.set()
        c.is_cancelled_wait()
        event_after.set()
        return 2

    with raises(ExceptionGroup) as exc:
        fork(worker1, worker2)

    assert len(exc.value.exceptions) == 1
    assert isinstance(exc.value.exceptions[0], ValueError)
    assert event_before.is_set()
    assert event_after.is_set()


def test_two_keyboard_interrupt() -> None:
    event_before = Event()

    def worker1(_c: CancelToken) -> int:
        event_before.wait()
        raise KeyboardInterrupt

    event_after = Event()

    def worker2(c: CancelToken) -> int:
        event_before.set()
        c.is_cancelled_wait()
        event_after.set()
        return 2

    with raises(KeyboardInterrupt):
        fork(worker1, worker2)

    assert event_before.is_set()
    assert event_after.is_set()

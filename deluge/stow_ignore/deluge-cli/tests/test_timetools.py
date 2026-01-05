from pytest import mark, approx, raises
from deluge_cli.timetools import parse_seconds, Seconds, Timeseries, Timestamp


@mark.parametrize(
    "string,expected",
    [
        ("5s", 5),
        ("5.1s", 5.1),
        ("-5.1s", -5.1),
        ("3min", 180),
    ],
)
def test_parse_seconds(string: str, expected: float) -> None:
    assert parse_seconds(string) == approx(expected)


@mark.parametrize(
    "string",
    [
        "5",
        "s",
        "5k",
        "k5",
        "-s",
        "5s 3min",
    ],
)
def test_parse_seconds_invalid(string: str) -> None:
    with raises(ValueError):
        parse_seconds(string)


def test_timeseries_evict() -> None:
    ts = Timeseries()
    assert len(ts) == 0
    assert not ts
    with raises(IndexError):
        _ignore = ts[Seconds(5)]

    ts.append(Timestamp(0), 5)
    assert len(ts) == 1
    ts.evict_older_than(Seconds(0))
    ts.evict_older_than(Seconds(1000))
    assert len(ts) == 1

    ts.append(Timestamp(1), 10)
    assert len(ts) == 2
    ts.evict_older_than(Seconds(0))
    assert len(ts) == 1

    ts.append(Timestamp(2), 10)
    ts.evict_older_than(Seconds(100))
    assert len(ts) == 2

    ts.append(Timestamp(3), 10)
    ts.evict_older_than(Seconds(2.5))
    assert len(ts) == 3

    ts.evict_older_than(Seconds(1.5))
    assert len(ts) == 3

    ts.evict_older_than(Seconds(0.5))
    assert len(ts) == 2


def test_timeseries_get() -> None:
    ts = Timeseries()
    ts.append(Timestamp(0), 0)
    ts.append(Timestamp(2), 1)
    ts.append(Timestamp(3), 2)
    ts.append(Timestamp(5), 3)

    assert ts[Seconds(0)] == approx(3)
    assert ts[Seconds(1)] == approx(2.5)
    assert ts[Seconds(2)] == approx(2)
    assert ts[Seconds(2.2)] == approx(1.8)
    assert ts[Seconds(0.2)] == approx(2.9)
    assert ts[Seconds(6)] == approx(0)

    assert ts.slope(Seconds(1), Seconds(0)) == approx(0.5)

# pyright: strict
import math
from itertools import groupby, pairwise
from collections import deque
import time
from dataclasses import dataclass
from typing import NewType, SupportsFloat

Seconds = NewType("Seconds", float)


@dataclass(frozen=True, eq=False)
class Timestamp:
    ts: float

    @classmethod
    def make(cls):
        return cls(time.monotonic())

    def __sub__(self, other: "Timestamp") -> Seconds:
        return Seconds(self.ts - other.ts)


def parse_seconds(s: str) -> Seconds:
    match ["".join(g) for _, g in groupby(s, str.isalpha)]:
        case [num, "s"]:
            f = float(num)
        case [num, "min"]:
            f = float(num) * 60
        case [num, "hr"]:
            f = float(num) * 3600
        case _:
            raise ValueError(f"Invalid duration string: {s}")
    assert math.isfinite(f)
    return Seconds(f)


class Timeseries[T: SupportsFloat]:
    def __init__(self):
        self._series: deque[tuple[Timestamp, T]] = deque()

    def __bool__(self):
        return bool(self._series)

    def __len__(self):
        return len(self._series)

    def oldest(self) -> tuple[Timestamp, T] | None:
        if not self._series:
            return None
        return self._series[0]

    def newest(self) -> tuple[Timestamp, T] | None:
        if not self._series:
            return None
        return self._series[-1]

    def record(self, value: T) -> Timestamp:
        ts = Timestamp.make()
        self.append(ts, value)
        return ts

    def append(self, ts: Timestamp, value: T):
        if (newest := self.newest()) is not None and ts - newest[0] <= 0:
            # TODO: the monotonic clock CAN return the same value twice, but it is
            # unlikely. Handle?
            raise ValueError("The new element must be strictly newer")
        self._series.append((ts, value))

    def evict_older_than(self, this: Seconds):
        if not math.isfinite(this) or this < 0:
            raise ValueError(f"Invalid argument: {this}")

        if (newest := self.newest()) is None:
            return
        last_popped = None
        while (oldest := self.oldest()) is not None and newest[0] - oldest[0] >= this:
            last_popped = self._series.popleft()

        if last_popped is not None:
            self._series.appendleft(last_popped)

        assert self._series

    def __getitem__(self, key: Seconds) -> float:
        if not math.isfinite(key) or key < 0:
            raise ValueError(f"Invalid key: {key}")

        if (oldest := self.oldest()) is None:
            raise IndexError("Series is empty")

        # TODO: this could probably be a binary search
        traveled = 0
        for (ts_new, val_new), (ts_old, val_old) in pairwise(reversed(self._series)):
            span: Seconds = ts_new - ts_old
            assert span > 0
            if traveled <= key <= traveled + span:
                t = unlerp(traveled, traveled + span, key)
                assert 0 <= t <= 1
                return lerp(float(val_new), float(val_old), t)
            traveled += span

        return float(oldest[1])

    def slope(self, a: Seconds, b: Seconds) -> float:
        if a <= b:
            raise ValueError(f"Invalid range: {a} > {b}")
        return (self[b] - self[a]) / (a - b)


def lerp(a: float, b: float, t: float) -> float:
    return a + t * (b - a)


def unlerp(a: float, b: float, c: float) -> float:
    return (c - a) / (b - a)

# pyright: strict

import logging
from enum import Enum, auto
from pathlib import Path, PurePath
from typing import Self, Any, Type, cast, NewType
from deluge_client import LocalDelugeRPCClient  # type: ignore
from dataclasses import dataclass
from datetime import datetime

logger = logging.getLogger(__name__)

Bytes = NewType("Bytes", int)


class State(Enum):
    PAUSED = auto()
    ERROR = auto()
    QUEUED = auto()
    SEEDING = auto()
    CHECKING = auto()
    DOWNLOADING = auto()


class Priority(Enum):
    HIGH = 7
    NORMAL = 4
    LOW = 1
    SKIP = 0


@dataclass(frozen=True)
class Hash:
    inner: str

    def __str__(self) -> str:
        return f"{self.inner:.7}"


@dataclass(frozen=True)
class File:
    path: PurePath
    progress: float
    priority: Priority

    def __str__(self) -> str:
        return f"{self.path}"


@dataclass(frozen=True)
class Torrent:
    hash: Hash
    download_location: Path
    files: list[File]
    is_finished: bool
    name: str
    paused: bool
    progress: float
    state: State
    time_added: datetime
    total_remaining: Bytes

    def __str__(self) -> str:
        return f"{self.hash} {self.name}"


class DelugeError(Exception):
    pass


def typed_get[T](dic: dict[str, Any], key: str, typ: Type[T]) -> T:
    val = dic[key]
    if isinstance(val, typ):
        return val
    raise DelugeError(f"Value {val} from key {key} is not of type {typ}")


class Ellipses:
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


class Deluge:
    def __init__(self):
        logger.info("Connecting to local deluge client")
        self.client = LocalDelugeRPCClient(automatic_reconnect=False, decode_utf8=True)
        self.client.connect()

    def _call(self, cmd: str, *args: Any) -> Any:
        logger.debug("RPC call: %s %s", cmd, args)
        res = cast(Any, self.client.call(cmd, *args))  # type: ignore
        logger.debug("RPC done: %s %s", cmd, Ellipses(res, 100))
        return res

    def get_torrents(self) -> list[Torrent]:
        filter_dict = {}
        keys = [
            "hash",
            "download_location",
            "files",
            "file_progress",
            "is_finished",
            "name",
            "paused",
            "progress",
            "state",
            "time_added",
            "file_priorities",
            "total_remaining",
        ]
        typed: dict[str, dict[str, Any]] = self._call(
            "core.get_torrents_status", filter_dict, keys
        )

        torrents: list[Torrent] = []
        for data in typed.values():
            try:
                # TODO: how to avoid these casts?
                file_progress = cast(
                    tuple[float, ...], typed_get(data, "file_progress", tuple)
                )
                file_priorities = cast(
                    tuple[int, ...], typed_get(data, "file_priorities", tuple)
                )
                raw_files = cast(tuple[dict[str, Any]], typed_get(data, "files", tuple))
                files: list[File] = []
                for i, fdata in enumerate(raw_files):
                    index = typed_get(fdata, "index", int)
                    if index != i:
                        raise DelugeError(
                            "The files didn't arrive in the same order as their indices would imply"
                        )
                    path = PurePath(typed_get(fdata, "path", str))
                    prio = Priority(file_priorities[i])
                    progress = file_progress[i] * 100.0
                    files.append(File(path=path, progress=progress, priority=prio))

                tor = Torrent(
                    hash=Hash(typed_get(data, "hash", str)),
                    files=files,
                    total_remaining=Bytes(typed_get(data, "total_remaining", int)),
                    download_location=Path(typed_get(data, "download_location", str)),
                    is_finished=typed_get(data, "is_finished", bool),
                    name=typed_get(data, "name", str),
                    paused=typed_get(data, "paused", bool),
                    progress=typed_get(data, "progress", float),
                    state=State[typed_get(data, "state", str).upper()],
                    time_added=datetime.fromtimestamp(
                        typed_get(data, "time_added", int)
                    ),
                )
                torrents.append(tor)
            except Exception as e:
                e.add_note(f"Raw data: {data}")
                raise

        logger.debug("Got %s torrents", len(torrents))
        return torrents

    def get_method_list(self) -> list[str]:
        return self._call("daemon.get_method_list")

    def remove_torrent(self, torrent: Hash, *, remove_data: bool = False):
        remove_data_str = "deleting" if remove_data else "keeping"
        logger.info("Removing %s and %s data", torrent, remove_data_str)
        ret = self._call("core.remove_torrent", torrent.inner, remove_data)
        assert isinstance(ret, bool)
        if not ret:
            raise DelugeError(
                f"Failed to remove torrent {torrent} while {remove_data_str} data"
            )

    def move_storage(self, torrent: Hash, new_dir: Path):
        logger.info("Moving storage of %s to %s", torrent, new_dir)
        assert new_dir.is_dir() or not new_dir.exists()
        assert self._call("core.move_storage", [torrent.inner], str(new_dir)) is None

    def queue_bottom(self, torrent: Hash):
        logger.info("Queuing to bottom: %s", torrent)
        assert self._call("core.queue_bottom", [torrent.inner]) is None

    def add_magnet(
        self,
        magnet: str,
        *,
        download_location: Path | None = None,
        paused: bool = False,
    ) -> Hash:
        logger.info("Adding torrent from magnet link '%s'", magnet)

        # NOTE: https://github.com/deluge-torrent/deluge/blob/6158d7b71c8bb587818a50759f4e7fed655ac72c/deluge/core/torrent.py#L118
        options: dict[str, Any] = {"add_paused": paused}
        if download_location is not None:
            options["download_location"] = str(download_location)
        logger.debug("Options set to: %s", options)

        ret = self._call("core.add_torrent_magnet", magnet, options)
        assert isinstance(ret, str)
        return Hash(ret)

    def resume(self, torrent: Hash):
        logger.info("Resuming %s", torrent)
        assert self._call("core.resume_torrent", torrent.inner) is None

    def pause(self, torrent: Hash):
        logger.info("Pausing %s", torrent)
        assert self._call("core.pause_torrent", torrent.inner) is None

    def get_config_value(self, key: str) -> Any:
        ret = self._call("core.get_config_value", key)
        logger.debug("Got config '%s' = '%s'", key, ret)
        return ret

    def __enter__(self) -> Self:
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> bool:
        logger.info("Disconnecting from local deluge client")
        self.client.disconnect()
        return False

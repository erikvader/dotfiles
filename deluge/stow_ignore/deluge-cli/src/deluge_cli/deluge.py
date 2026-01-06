# pyright: strict
import math
import re
import logging
from itertools import groupby
from .inspecttools import base_type
from .loggingtools import Ellipses
from enum import Enum, auto
from pathlib import Path, PurePath
from typing import Self, Any, Type, cast, NewType
from deluge_client import LocalDelugeRPCClient  # type: ignore
from deluge_client.client import RemoteException  # type: ignore
from dataclasses import dataclass, field
from datetime import datetime
import time

logger = logging.getLogger(__name__)

Bytes = NewType("Bytes", int)
BytesPerSecond = NewType("BytesPerSecond", float)


def parse_bytes_per_second(s: str) -> BytesPerSecond:
    """Actually kibibytes, mebibytes etc"""
    match ["".join(g) for _, g in groupby(s, str.isalpha)]:
        case [num, "b", "/", "s"]:
            f = float(num)
        case [num, "kb", "/", "s"]:
            f = float(num) * 1024
        case [num, "Mb", "/", "s"]:
            f = float(num) * 1024 * 1024
        case _:
            raise ValueError(f"Invalid speed string: {s}")
    assert math.isfinite(f)
    return BytesPerSecond(f)


class State(Enum):
    PAUSED = auto()
    ERROR = auto()
    QUEUED = auto()
    SEEDING = auto()
    CHECKING = auto()
    DOWNLOADING = auto()
    MOVING = auto()


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
    path: PurePath  # relative to the download location
    progress: float
    priority: Priority

    def __str__(self) -> str:
        return f"{self.path}"


@dataclass(frozen=True)
class Torrent:
    hash: Hash
    download_location: Path = field(compare=False)
    files: list[File] = field(compare=False)
    is_finished: bool = field(compare=False)
    name: str = field(compare=False)
    paused: bool = field(compare=False)
    progress: float = field(compare=False)
    state: State = field(compare=False)
    time_added: datetime = field(compare=False)
    total_downloaded: Bytes = field(compare=False)
    total_uploaded: Bytes = field(compare=False)
    queue: int | None = field(compare=False)

    def __str__(self) -> str:
        return f"{self.hash} {self.name}"


class DelugeError(Exception):
    pass


def typed_get[T](dic: dict[str, Any], key: str, typ: Type[T]) -> T:
    val = dic[key]
    # NOTE: this only checks that the 'container' type is correct, not the type
    # parameters.
    if isinstance(val, base_type(typ)):
        return val
    raise DelugeError(f"Value {val} from key {key} is not of type {typ}")


class Deluge:
    already_added_regex = re.compile(
        r"^Torrent already in session \(([a-zA-Z0-9]+)\)\.$", re.MULTILINE
    )

    def __init__(self, *, connection_attempts: int = 1):
        if connection_attempts < 1:
            raise ValueError(
                f"Invalid connection attempts value: {connection_attempts}"
            )
        logger.info(
            "Connecting to local deluge client, trying %s times", connection_attempts
        )
        self.client = LocalDelugeRPCClient(automatic_reconnect=False, decode_utf8=True)

        for i in range(1, connection_attempts + 1):
            try:
                self.client.reconnect()
            except ConnectionRefusedError as e:
                e.add_note(f"Connection attempt {i}")
                if i < connection_attempts:
                    logger.warning("Failed to connect try %s because %s", i, e)
                    time.sleep(5)
                else:
                    raise
            else:
                logger.info("Connected successfully")
                break

    def _call[T](self, cmd: str, expect: Type[T], *args: Any) -> T:
        logger.debug("RPC call: %s %s", cmd, args)
        res = cast(Any, self.client.call(cmd, *args))  # type: ignore
        logger.debug("RPC done: %s %s", cmd, Ellipses(res, 100))
        # NOTE: this only checks that the 'container' type is correct, not the type
        # parameters.
        if not isinstance(res, base_type(expect)):
            raise DelugeError(
                f"Unexpected return value from deluge '{res}' expected type {expect}"
            )
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
            # total_payload_download = bytes downloaded this session
            # all_time_download = bytes downloaded since the start, including protocol data?
            # total_done = corresponds to the progress bar?
            "total_payload_download",
            # total_payload_upload = bytes uploaded this session
            # total_uploaded = bytes uploaded since the start, including protocol data?
            "total_payload_upload",
            "queue",
        ]
        typed = self._call(
            "core.get_torrents_status",
            dict[str, dict[str, Any]],
            filter_dict,
            keys,
        )

        torrents: list[Torrent] = []
        for data in typed.values():
            try:
                file_progress = typed_get(data, "file_progress", tuple[float, ...])
                file_priorities = typed_get(data, "file_priorities", tuple[int, ...])
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

                queue = typed_get(data, "queue", int)
                if queue < 0:
                    queue = None
                else:
                    queue += 1

                tor = Torrent(
                    hash=Hash(typed_get(data, "hash", str)),
                    files=files,
                    total_downloaded=Bytes(
                        typed_get(data, "total_payload_download", int)
                    ),
                    total_uploaded=Bytes(typed_get(data, "total_payload_upload", int)),
                    queue=queue,
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
        assert len(torrents) == len(
            set(torrents)
        ), "There are torrents with the same hash"
        return torrents

    def get_method_list(self) -> list[str]:
        return self._call("daemon.get_method_list", list[str])

    def remove_torrent(self, torrent: Hash, *, remove_data: bool = False):
        remove_data_str = "deleting" if remove_data else "keeping"
        logger.info("Removing %s and %s data", torrent, remove_data_str)
        ret = self._call("core.remove_torrent", bool, torrent.inner, remove_data)
        if not ret:
            raise DelugeError(
                f"Failed to remove torrent {torrent} while {remove_data_str} data"
            )

    def move_storage(self, torrent: Hash, new_dir: Path):
        logger.info("Moving storage of %s to %s", torrent, new_dir)
        if not new_dir.is_absolute():
            raise ValueError(f"Can only move to an absolute path: {new_dir}")
        if not new_dir.is_dir() and new_dir.exists():
            raise ValueError(f"Can't move to an existing non-dir: {new_dir}")
        self._call("core.move_storage", type(None), [torrent.inner], str(new_dir))

    def queue_bottom(self, torrent: Hash):
        logger.info("Queuing to bottom: %s", torrent)
        self._call("core.queue_bottom", type(None), [torrent.inner])

    def add_magnet(
        self,
        magnet: str,
        *,
        download_location: Path | None = None,
        paused: bool = False,
    ) -> Hash | None:
        logger.info("Adding torrent from magnet link '%s'", magnet)

        # NOTE: https://github.com/deluge-torrent/deluge/blob/6158d7b71c8bb587818a50759f4e7fed655ac72c/deluge/core/torrent.py#L118
        options: dict[str, Any] = {"add_paused": paused}
        if download_location is not None:
            if not download_location.is_absolute():
                raise ValueError(
                    f"Download location must be absolute: {download_location}"
                )
            options["download_location"] = str(download_location)
        logger.debug("Options set to: %s", options)

        try:
            ret = self._call("core.add_torrent_magnet", str, magnet, options)
        except RemoteException as e:
            name = type(e).__name__
            msg = str(e)
            if (
                name == "AddTorrentError"
                and (m := Deluge.already_added_regex.match(msg)) is not None
            ):
                logger.warning("The torrent is already added as %s", Hash(m.group(1)))
                return None
            else:
                raise

        h = Hash(ret)
        logger.info("Added with hash: %s", h)
        return h

    def resume(self, torrent: Hash):
        logger.info("Resuming %s", torrent)
        self._call("core.resume_torrent", type(None), torrent.inner)

    def pause(self, torrent: Hash):
        logger.info("Pausing %s", torrent)
        self._call("core.pause_torrent", type(None), torrent.inner)

    def get_config_value(self, key: str) -> Any:
        ret = self._call("core.get_config_value", object, key)
        logger.debug("Got config '%s' = '%s'", key, ret)
        return ret

    def __enter__(self) -> Self:
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> bool:
        logger.info("Disconnecting from local deluge client")
        self.client.disconnect()
        return False

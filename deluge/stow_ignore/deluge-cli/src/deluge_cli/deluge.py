# pyright: strict

import logging
from pathlib import Path
from typing import Self, Any, cast
from deluge_client import LocalDelugeRPCClient  # type: ignore
from dataclasses import dataclass

logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Torrent:
    hash: str
    data: Any
    # download_location: Path
    # files: Any  # TODO: list of file objects
    # is_finished: bool
    # name: str
    # paused: bool
    # progress: float
    # state: Any  # TODO: enum
    # added: Any  # TODO: what time object?


class Deluge:
    def __init__(self):
        logger.info("Connecting to local deluge client")
        self.client = LocalDelugeRPCClient()
        self.client.connect()

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
        ]
        untyped: Any = self.client.call("core.get_torrents_status", filter_dict, keys)  # type: ignore
        typed = cast(dict[str, dict[str, Any]], untyped)

        return [Torrent(hash=data["hash"], data=data) for data in typed.values()]

    def __enter__(self) -> Self:
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> bool:
        logger.info("Disconnecting from local deluge client")
        self.client.disconnect()
        return False

# pyright: strict
from pathlib import Path
import logging
import argparse
from typing import Callable
from ..deluge import Deluge

logger = logging.getLogger(__name__)

subcommand = "add"


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    parser = add_parser(
        subcommand,
        description="Add a new torrent and print its hash",
        help="Add a new torrent",
    )

    def magnet(x: str) -> str:
        if not x.startswith("magnet://"):
            raise ValueError("Magnet link expected")
        return x

    parser.add_argument("magnet", type=magnet, help="Magnet link to add")
    parser.add_argument("--location", "-l", type=Path, help="Download location")
    parser.add_argument("--paused", "-p", action="store_true", help="Add it paused")


def run(*, url: str, download_location: Path | None, paused: bool):
    logger.info("Adding torrent from: %s", url)

    with Deluge() as deluge:
        new_hash = deluge.add_magnet(
            url, download_location=download_location, paused=paused
        )
        print(new_hash)

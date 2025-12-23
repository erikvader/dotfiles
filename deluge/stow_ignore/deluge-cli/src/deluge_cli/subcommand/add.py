# pyright: strict
from pathlib import Path
import logging
import argparse
from typing import Callable
from ..deluge import Deluge

logger = logging.getLogger(__name__)

subcommand = "add"


# TODO: auto download everything that hits clipboard? Use clipnotify if available, poll
# otherwise.
def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    parser = add_parser(
        subcommand,
        description="Add a new torrent and print its hash",
        help="Add a new torrent",
    )

    def magnet(x: str) -> str:
        if not x.startswith("magnet:"):
            raise ValueError("Magnet link expected")
        return x

    parser.add_argument("magnet", type=magnet, help="Magnet link to add")
    parser.add_argument(
        "--download-dir",
        "-d",
        type=Path,
        help="Download location. A default directory is used if this is not specified.",
    )
    parser.add_argument("--paused", "-p", action="store_true", help="Add it paused")


def run(args: argparse.Namespace):
    magnet: str = args.magnet
    download_dir: Path | None = args.download_dir
    paused: bool = args.paused

    logger.info("Adding torrent from: %s", args.magnet)

    with Deluge() as deluge:
        new_hash = deluge.add_magnet(
            magnet, download_location=download_dir, paused=paused
        )
        print(new_hash)

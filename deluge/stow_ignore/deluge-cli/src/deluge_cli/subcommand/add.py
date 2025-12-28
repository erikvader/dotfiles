# pyright: strict
from pathlib import Path
import logging
import argparse
from typing import Callable
from ..deluge import Deluge
from .. import clipboard as C

logger = logging.getLogger(__name__)

subcommand = "add"


def is_magnet_link(x: str) -> bool:
    return x.startswith("magnet:")


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    parser = add_parser(
        subcommand,
        description="Add a new torrent and print its hash",
        help="Add a new torrent",
    )

    def magnet(x: str) -> str:
        if not is_magnet_link(x):
            raise ValueError("Magnet link expected")
        return x

    modify_group = parser.add_argument_group("Torrent attributes")
    modify_group.add_argument(
        "--download-dir",
        "-d",
        type=Path,
        help="Download location. A default directory is used if this is not specified.",
    )
    modify_group.add_argument(
        "--paused", "-p", action="store_true", help="Add it paused"
    )
    modify_group.add_argument(
        "--queue-bottom",
        "-b",
        action="store_true",
        help="Queue the torrent to the bottom",
    )

    magnet_group = parser.add_argument_group("Source selection")
    magnet_exgroup = magnet_group.add_mutually_exclusive_group(required=True)
    magnet_exgroup.add_argument(
        "--magnet", "-m", type=magnet, help="Magnet link to add"
    )
    magnet_exgroup.add_argument(
        "--clipboard-listen",
        "-l",
        action="store_true",
        help="Continuously add magnet links from the clipboard",
    )


def run(args: argparse.Namespace):
    download_dir: Path | None = args.download_dir
    paused: bool = args.paused
    queue_bottom: bool = args.queue_bottom

    def add(m: str):
        assert is_magnet_link(m)
        new_hash = deluge.add_magnet(
            m, download_location=download_dir, paused=paused or queue_bottom
        )
        if queue_bottom and not paused:
            deluge.resume(new_hash)
        print(new_hash)

    magnet: str = args.magnet
    clipboard_listen: bool = args.clipboard_listen

    with Deluge() as deluge:
        if clipboard_listen:
            for link in C.clipboard_listener():
                if is_magnet_link(link):
                    add(link)
                else:
                    logger.warning("Not adding '%s'", link)
        else:
            add(magnet)

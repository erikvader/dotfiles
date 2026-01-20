# pyright: strict
import logging
import argparse
from typing import Callable, assert_never
from ..deluge import Deluge
from enum import Enum

logger = logging.getLogger(__name__)

subcommand = "core"


class ListThing(Enum):
    DOWNLOAD_LOCATIONS = "download_locations"

    def deluge_config_key(self) -> str:
        match self:
            case self.DOWNLOAD_LOCATIONS:
                return "download_location_paths_list"
            case _ as unreachable:
                assert_never(unreachable)

    def __str__(self):
        return self.value


def argparse_add_subcommand(add_parser: Callable[..., argparse.ArgumentParser]):
    parser = add_parser(
        subcommand,
        description="Perform actions on the deluge instance itself",
        help="Actions on the deluge core instance",
    )

    subparsers = parser.add_subparsers(
        required=True,
        dest="action",
        description="Action to run",
        metavar="action",
        title="actions",
    )

    list_parser = subparsers.add_parser(
        "list", description="List stuff", help="List stuff"
    )
    list_parser.add_argument(
        "--zero", "-0", action="store_true", help="Delimit the list with null"
    )
    list_parser.add_argument(
        "what", type=ListThing, choices=list(ListThing), help="The thing to list"
    )

    subparsers.add_parser(
        "shutdown",
        description="Ask the daemon to shutdown itself",
        help="Daemon shutdown",
    )


def run(args: argparse.Namespace):
    action: str = args.action

    match action:
        case "list":
            list_thing(args)
        case "shutdown":
            shutdown(args)
        case _:
            raise ValueError("An invalid action entered somehow")


def list_thing(args: argparse.Namespace):
    zero: bool = args.zero
    what: ListThing = args.what

    with Deluge() as deluge:
        items = deluge.get_config_value(what.deluge_config_key())
        for i in items:
            print(i, end="\0" if zero else "\n")


def shutdown(_args: argparse.Namespace):
    with Deluge() as deluge:
        deluge.daemon_shutdown()

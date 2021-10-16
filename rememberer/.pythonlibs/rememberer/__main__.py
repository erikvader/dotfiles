from .types import UserError
from .utils import create_data_dir
from argparse import ArgumentParser, Namespace
from urllib.parse import urlparse
from .rememberer import replay, remember
import sys


def parse_arguments() -> Namespace:
    parser = ArgumentParser(
        prog="rememberer",
        description="""
        Give the url of a webstite as an argument and it's HTML to stdin and I will
        remember that you have been there.
        """,
    )
    g = parser.add_mutually_exclusive_group(required=True)
    g.add_argument(
        "url", type=urlparse, nargs="?", help="The full URL the page is from"
    )
    g.add_argument(
        "--replay",
        action="store_true",
        help="Show the previous output again",
    )

    parser.add_argument(
        "--no-remember",
        action="store_true",
        help="Don't remember new things",
    )

    parser.add_argument(
        "--everything",
        action="store_true",
        help="find more on the page, not the current",
    )

    parser.add_argument(
        "--test",
        action="store_true",
        help="""
        Set exit status to 0 if there were new things added, 1 if everything is old and 2
        if an error occured
        """,
    )

    parser.add_argument(
        "--quiet", action="store_true", help="Don't print what is new and old"
    )

    return parser.parse_args()


def run(args: Namespace) -> bool:
    datadir = create_data_dir()
    lastoutputfile = datadir / ".last_output"
    lastjsfile = datadir / ".last_js"

    if args.replay:
        replay(lastoutputfile)
        return True
    elif args.url:
        return remember(
            args.url,
            datadir,
            lastoutputfile,
            lastjsfile,
            args.quiet,
            not args.no_remember,
            args.everything,
        )
    else:
        raise Exception("one of these options should have been given")

    return False


def main():
    args = parse_arguments()

    try:
        success = run(args)
        if args.test:
            exitcode = 0 if success else 1
        else:
            exitcode = 0
        exit(exitcode)
    except UserError as ue:
        print(ue, end="", file=sys.stderr)
        exit(2)
    except Exception:  # pylint: disable=broad-except
        import traceback

        traceback.print_exc()
        exit(2)


if __name__ == "__main__":
    main()

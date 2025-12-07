import logging
import sys
import os
from .deluge import Deluge


logger = logging.getLogger(__name__)


class AnsiColorFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord):
        no_style = "\033[0m"
        bold = "\033[1m"
        grey = "\033[90m"
        yellow = "\033[33m"
        red = "\033[31m"
        red_light = "\033[31m"
        start_style = {
            "DEBUG": grey,
            "INFO": no_style,
            "WARNING": yellow + bold,
            "ERROR": red,
            "CRITICAL": red_light + bold,
        }.get(record.levelname, no_style)
        end_style = no_style
        return f"{start_style}{super().format(record)}{end_style}"


def supports_color(stream):
    return os.getenv("TERM") != "dumb" and stream.isatty()


def setup_logging(*, global_level=logging.INFO, stream=sys.stderr, colors="auto"):
    root = logging.getLogger()
    root.setLevel(global_level)

    handler = logging.StreamHandler(stream)

    format_str = "%(name)s [%(levelname)s] %(message)s"
    if colors == "yes" or colors == "auto" and supports_color(stream):
        formatter = AnsiColorFormatter(format_str)
    else:
        formatter = logging.Formatter(format_str)
    handler.setFormatter(formatter)

    root.addHandler(handler)

    logging.getLogger("deluge_client.client").setLevel(logging.INFO)


def main():
    setup_logging(global_level=logging.DEBUG)
    with Deluge() as deluge:
        torrents = deluge.get_torrents()
        import pickle

        with open("data.pickle", "wb") as f:
            pickle.dump(torrents, f)


if __name__ == "__main__":
    main()

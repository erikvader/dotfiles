from contextlib import contextmanager
from io import SEEK_END
from fcntl import flock, LOCK_EX
from pathlib import Path
from .types import Thing, SiteName, TaggedThing
from typing import (
    Optional,
    TextIO,
    Iterator,
    Callable,
)

ENCODING = "utf8"


class Storage:
    sanitize_table = str.maketrans({"\n": r"\n", "\t": r"\t"})

    def __init__(self, site: SiteName, datadir: Path) -> None:
        self.site = site
        self.datadir = datadir
        self.openFile: Optional[TextIO] = None

    def read(self) -> Iterator[Thing]:
        if self.openFile is None:
            return

        self.openFile.seek(0)
        for l in self.openFile:
            fields = l.rstrip().split("\t")
            if len(fields) != 2:
                raise Exception("wrong number of fields")
            yield Thing(name=fields[0], key=fields[1])

    def append(self, things: Iterator[Thing]) -> None:
        if self.openFile is None:
            raise Exception("Storage is not open")

        self.openFile.seek(0, SEEK_END)
        for t in things:
            self.openFile.write(self._sanitize(t.name))
            self.openFile.write("\t")
            self.openFile.write(self._sanitize(t.key))
            self.openFile.write("\n")

    def overwrite(self, things: Iterator[Thing]) -> None:
        raise NotImplementedError()

    def _sanitize(self, string: str) -> str:
        return string.translate(Storage.sanitize_table)

    def _open(self) -> None:
        if self.openFile is not None:
            raise Exception("can't open twice")
        if not self.datadir.is_dir():
            raise Exception(f"'{self.datadir}' doesn't exist or isn't a directory")

        fullpath = self.datadir / self.site
        self.openFile = open(fullpath, "a+", encoding=ENCODING)
        flock(self.openFile, LOCK_EX)

    def _close(self) -> None:
        if self.openFile is not None:
            self.openFile.close()

    def __enter__(self):
        self._open()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self._close()
        return False


def write_mark_js(jsfile: Path, things: Iterator[TaggedThing]):
    with open(jsfile, "w", encoding=ENCODING) as f:
        for t in (t for t, is_new in things if not is_new):
            if t.jsmark is not None:
                f.write(t.jsmark)


@contextmanager
def write_results(
    save_file: Path,
    to_stdout=True,
) -> Iterator[Callable[..., None]]:
    with open(save_file, "a+", encoding=ENCODING) as f:
        flock(f, LOCK_EX)
        f.seek(0)
        f.truncate(0)

        def tee(line: str, fileOnly=False) -> None:
            if to_stdout and not fileOnly:
                print(line)
            print(line, file=f)

        yield tee

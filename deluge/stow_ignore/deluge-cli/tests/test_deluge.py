from pytest import mark
from deluge_cli.deluge import File, Priority
from pathlib import PurePath


@mark.parametrize(
    "src,glob,res",
    [
        ("movie.mkv", "*.mkv", True),
        ("file/movie.mkv", "*.mkv", True),
        ("file/movie.mkv", "/*.mkv", False),
        ("/file/movie.mkv", "/file/*.mkv", True),
        ("file/movie.mkv", "/file/*.mkv", False),
        ("file/movie.mkv", "**/file/*.mkv", True),
        ("file/movie.mkv", "*/file/*.mkv", False),
        ("/file/movie.mkv", "file", False),
        ("/file/movie.mkv", "file/*.mkv", False),
        ("/file/movie.mkv", "**/file/**", False),
        ("", "*.mkv", False),
        ("/", "*.mkv", False),
        ("/", "/", True),
    ],
)
def test_torrent_file_glob(src: str, glob: str, res: bool) -> None:
    f = File(path=PurePath(src), progress=0, priority=Priority.NORMAL)
    assert f.match(glob) == res
    assert f.match("!" + glob) != res

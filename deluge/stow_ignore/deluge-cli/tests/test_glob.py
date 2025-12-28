from pytest import mark
from pathlib import PurePath
from deluge_cli.glob import path_match, str_match


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
def test_path_glob(src: str, glob: str, res: bool) -> None:
    p = PurePath(src)
    assert path_match(p, glob) == res
    assert path_match(p, "!" + glob) != res


@mark.parametrize(
    "src,glob,res",
    [
        ("movie.mkv", "*.mkv", True),
        ("file/movie.mkv", "*.mkv", True),
        ("file/movie.mkv", "/*.mkv", False),
        ("/file/movie.mkv", "/file/*.mkv", True),
        ("file/movie.mkv", "/file/*.mkv", False),
        ("file/movie.mkv", "*/file/*.mkv", False),
        ("file/movie.mkv", "*/file/*.mkv", False),
        ("/file/movie.mkv", "file", False),
        ("/file/movie.mkv", "file/*.mkv", False),
        ("/file/movie.mkv", "*/file/*", True),
        ("", "*.mkv", False),
        ("/", "*.mkv", False),
        ("/", "/", True),
    ],
)
def test_str_glob(src: str, glob: str, res: bool) -> None:
    assert str_match(src, glob) == res
    assert str_match(src, "!" + glob) != res

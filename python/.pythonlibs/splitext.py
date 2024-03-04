import os

def splitext(path):
    """
    The same as os.path.splitext, but handles some double-extensions as one.

    Old behaviour:
    >>> os.path.splitext('hej.tar.gz')
    ('hej.tar', '.gz')

    New behaviour! If the first extension is from a compression utility, then merge them:
    >>> splitext('hej.tar.gz')
    ('hej', '.tar.gz')

    mp4 isn't:
    >>> splitext('hej.mkv.mp4')
    ('hej.mkv', '.mp4')

    Also works if the second extension is not .tar:
    >>> splitext('hej.mkv.gz')
    ('hej', '.mkv.gz')
    """
    filename, ext = os.path.splitext(path)
    if not ext:
        return filename, ext

    filename2, ext2 = os.path.splitext(filename)
    if ext2 and ext in {".gz", ".bz2", ".xz", ".lzma", ".Z", ".zst", ".lz", ".lzo"}:
        return filename2, ext2 + ext

    return filename, ext

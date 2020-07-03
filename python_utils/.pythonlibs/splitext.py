import os

def splitext(path):
    """
    The same as os.path.splitext except this splits filenames like
    hej.tar.gz properly.

    os.path: ('hej.tar', '.gz')
    this: ('hej', '.tar.gz')
    """
    filename, ext = os.path.splitext(path)

    if not ext:
        return filename, ext

    act_filename, act_ext = splitext(filename)
    return act_filename, act_ext + ext

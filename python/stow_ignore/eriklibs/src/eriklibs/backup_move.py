import os
import re
from shutil import move

_leading_number_regex = re.compile(r"[0-9]+_(.+)")

class MoveFileException(Exception):
   pass

def _file_gen(name):
   """
   generates filename candidates on the form num_name
   """
   yield name
   i = 1
   while True:
      yield "{}_{}".format(str(i), name)
      i += 1

def move_file(src, dst):
   """
   Moves the source file/directory src to the folder dst, renaming src
   as necessary to make sure nothing in dst gets overwritten.

   returns the path (not name) of the file moved to dst.

   dstpath = move_file("path/to/file", "path/to/dst")
   """
   if not os.path.exists(src):
      raise MoveFileException("source file doesn't exist")

   if not os.path.isdir(dst):
      raise MoveFileException("dst is not a directory")

   src_basename = os.path.basename(src)

   for d_name in _file_gen(src_basename):
      d_path = os.path.join(dst, d_name)
      # TODO: race condition! use RENAME_NOREPLACE of rename instead
      # https://man7.org/linux/man-pages/man2/rename.2.html. It seems that mv uses that
      # https://github.com/coreutils/gnulib/blob/6c99ec373c1ecd531b0ffedfdfce7979db63a0e8/lib/backupfile.c#L375
      if not os.path.exists(d_path):
         move(src, d_path)
         return d_path

# find all possible duplicates of the file `path` in its current directory
def find_duplicates(path):
   assert(os.path.exists(path))
   file_name = os.path.basename(path)
   dir_name = os.path.dirname(path)

   return [f for f in os.listdir(dir_name) if f != file_name and are_duplicates(f, file_name)]

# are the filenames file_a and file_b possible duplicate copies from
# being moved with move_file?
def are_duplicates(file_a, file_b):
   return _remove_leading_number(file_a) == _remove_leading_number(file_b)

def _remove_leading_number(s):
   m = _leading_number_regex.fullmatch(s)
   if m:
      return m[1]
   else:
      return s

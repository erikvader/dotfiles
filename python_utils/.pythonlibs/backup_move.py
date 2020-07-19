import os
import re
from shutil import move

# TODO: put duplicate number before file extension instead?

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

def move_file(src, dst, src_folder=None):
   """
   Moves the source file/directory src to the folder dst, renaming src
   as necessary to make sure nothing in dst gets overwritten.

   returns the name (not path) of the file moved to dst.

   move_file("path/to/file", "path/to/dst")
   is equivalent to
   move_file("file", "path/to/dst", src_folder="path/to")

   dstname = move_file("path/to/file", "path/to/dst")
   assert(os.path.isfile(os.path.join("path/to/dst", dstname)))
   """
   if not src_folder:
      src_folder = os.path.dirname(src)
      src = os.path.basename(src)

   src_path = os.path.join(src_folder, src)
   if not os.path.exists(src_path):
      raise MoveFileException("source file doesn't exist")

   if not os.path.isdir(dst):
      raise MoveFileException("dst is not a directory")

   for d_name in _file_gen(src):
      d_path = os.path.join(dst, d_name)
      # NOTE: possible race condition
      if not os.path.exists(d_path):
         move(src_path, d_path)
         return d_name

# find all possible duplicates of the file `path` in it's current directory
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

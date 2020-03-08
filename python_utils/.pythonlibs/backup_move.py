import os
import re
from shutil import move

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

def find_duplicates(path):
   assert(os.path.exists(path))
   file_name = os.path.basename(path)
   dir_name = os.path.dirname(path)

   m = re.fullmatch(r"[0-9]+_(.+)", file_name)
   if m:
      without_numbers = m[1]
   else:
      without_numbers = file_name

   m = re.compile(r'([0-9]+_)?' + without_numbers)
   return [f for f in os.listdir(dir_name) if f != file_name and m.fullmatch(f)]

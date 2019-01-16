import os
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
   Moves the source file src to the folder dst, renaming src as
   necessary to make sure nothing in dst gets overwritten.

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
   if not os.path.isfile(src_path):
      raise MoveFileException("source file doesn't exist or is not a file")

   for d_name in _file_gen(src):
      d_path = os.path.join(dst, d_name)
      try:
         # Why this is not a problem:
         # https://stackoverflow.com/questions/2028874/what-happens-to-an-open-file-handle-on-linux-if-the-pointed-file-gets-moved-del
         # This is done to prevent race conditions (at least with itself).
         # Can also use link and unlink because link doesn't overwrite stuff.
         # Using this because move can copy between file systems.
         with open(d_path, 'xb'):
            move(src_path, d_path)
         return d_name
      except FileExistsError:
         continue

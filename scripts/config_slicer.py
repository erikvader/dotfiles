#!/usr/bin/python3

import sys
import re
import os

# config_slicer [tags]
# slices config files, keeping only wanted sections based on the given
# tags

tags = set(sys.argv[1:])

blockre = re.compile(r"^[ \t]*;@@@;(.*)$\n?")
blockendre = re.compile(r"^[ \t]*;/@@@;")
elsere = re.compile(r"^[ \t]*;@@@else;")
envre = re.compile(r";@@@\${(.+?):(.+?)}")

def subfunc(mat):
   return os.environ.get(mat.group(1), mat.group(2))

def shouldKeep(line):
   fields = {f for f in line.split(' ') if f}
   return fields & tags

keeping=True
inblock=False

for l in sys.stdin:
   if not inblock:
      m = blockre.match(l)
      if m:
         inblock = True
         if not shouldKeep(m.group(1)):
            keeping=False
         continue

   if inblock:
      m = elsere.match(l)
      if m:
         keeping = not keeping
         continue

   if inblock:
      m = blockendre.match(l)
      if m:
         inblock = False
         keeping=True
         continue

   if keeping:
      print(envre.sub(subfunc, l), end='')


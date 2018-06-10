#!/usr/bin/python3

import sys

# config_slicer [tags]
# slices config files, keeping only wanted sections based on the given
# tags

tags = set(sys.argv[1:])

def shouldKeep(line):
   fields = set(list(filter(None, line.split(' ')))[1:])
   return len(fields & tags) > 0

keeping=True

for l in sys.stdin:
   if ';@@@;' in l:
      if not shouldKeep(l.rstrip('\n')):
         keeping=False
   elif ';/@@@;' in l:
      keeping=True
   elif keeping:
      print(l,end='')



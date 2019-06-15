#!/bin/python

from itertools import zip_longest
import re

LEFT = 0
RIGHT = 1
IGNORE = 2

def _transpose(fields):
   return [list(x) for x in zip_longest(*fields, fillvalue=None)]

def _repeat(x):
   it = iter(x)
   last = None
   while True:
      try:
         last = next(it)
      except StopIteration:
         pass
      yield last

def _align(c, ls, ml, a):
   def f(cc, l):
      if cc is None:
         return None
      d = ml - l
      if d < 0:
         return cc[:ml]
      elif a == LEFT:
         return cc + " "*d
      elif a == RIGHT:
         return " "*d + cc
      else:
         return cc
   return [f(cc, l) for cc, l in zip(c,ls)]

# TODO: end=None to not join the lines
# tabulates fields into a string.
# fields: is an iterable of iterables align: is a tuple of alignments
#         applied in order. The last one is repeated indefinitely
# separators: is a tuple of string to put between each column, last is
#             repeated
# widths: is a tuple of fixed column widths. A value of 0 means that
#         the column gets as wide as needed
# lenfun: is a function to calculate the length of every field
# end: is a string to join each line with
# end_on_last_line: if True will put an end at the end of the table
def tabulate(fields, align=(LEFT,), separators=("  ",), widths=(0,), lenfun=len, end='\n', end_on_last_line=False):
   fields = [[str(ff) for ff in f] for f in fields]
   cols = _transpose(fields)
   lens = [[lenfun(cc) if cc else 0 for cc in c] for c in cols]
   maxlens = (max(c) if w == 0 else w for c,w in zip(lens, _repeat(widths)))
   cols = (_align(*args) for args in zip(cols, lens, maxlens, _repeat(align)))
   rows = _transpose(cols)
   lines = []
   for r in rows:
      l = []
      for rr,s in zip(r, _repeat(separators)):
         if rr is None:
            break
         l.append(rr)
         l.append(s)
      s = "".join(l[:-1])
      lines.append(s.rstrip(" "))
   return end.join(lines) + (end if end_on_last_line else "")

# calculates len(s) with ANSI escape codes removed
def ansiLength(s):
   return len(re.sub(r"\x1b\[[0-9;:<=>?]*[ !\"#$%&'()*+,\-./]*[a-zA-Z[\]^_`\-~{}|@]", "", s))

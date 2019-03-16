#!/bin/python

from itertools import zip_longest

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
      if a == LEFT:
         return cc + " "*d
      elif a == RIGHT:
         return " "*d + cc
      else:
         return cc
   return [f(cc, l) for cc, l in zip(c,ls)]

# tabulates fields into a string
# align is a tuple of alignments applied in order. The last one is repeated indefinitely
# separators is a tuple of string to put between each column. Last is repeated
def tabulate(fields, align=(LEFT,), separators=("  ",), lenfun=len):
   fields = [[str(ff) for ff in f] for f in fields]
   cols = _transpose(fields)
   lens = [[lenfun(cc) if cc else 0 for cc in c] for c in cols]
   maxlens = (max(c) for c in lens)
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
   return "\n".join(lines)

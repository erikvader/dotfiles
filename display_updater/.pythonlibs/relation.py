#!/bin/python

class Relation:
   def __init__(self):
      self.rel = {}

   def _add_relation(self, a, b, name):
      if a not in self.rel:
         self.rel[a] = {name: {b}}
      elif name not in self.rel[a]:
         self.rel[a][name] = {b}
      else:
         self.rel[a][name].add(b)

   def relate(self, a, b, name, reverse_name=None):
      self._add_relation(a, b, name)
      if a != b:
         self._add_relation(b, a, reverse_name if reverse_name else name)

   def areRelated(self, a, b, name):
      if a not in self.rel:
         return False
      if name not in self.rel[a]:
         return False
      return b in self.rel[a][name]

   def __eq__(self, other):
      if not isinstance(other, Relation):
         return False
      return self.rel == other.rel

   def __ne__(self, other):
      return not self == other

   def __le__(self, other):
      if not isinstance(other, Relation):
         return False
      for a,b,n in self:
         if not other.areRelated(a, b, n):
            return False
      return True

   def __bool__(self):
      return bool(self.rel)

   def filter(self, *names):
      newRel = Relation()
      for a,b,n in self:
         if n in names:
            #pylint: disable=protected-access
            newRel._add_relation(a, b, n)
      return newRel

   def __str__(self):
      return ", ".join(("{}-{}>{}".format(a,n,b) for a,b,n in self))

   def __repr__(self):
      return self.__str__()

   def __len__(self):
      c = 0
      for _,_,_ in self:
         c += 1
      return c

   def __iter__(self):
      for a in self.rel:
         for n in self.rel[a]:
            for b in self.rel[a][n]:
               yield (a,b,n)



#!/bin/python

class Relation:
   def __init__(self):
      self.rel = {}

   def _add_relation(self, a, b, name):
      if a not in self.rel:
         self.rel[a] = {name: b}
      else:
         self.rel[a][name] = b

   def relate(self, a, b, name, reverse_name=None):
      self._add_relation(a, b, name)
      self._add_relation(b, a, reverse_name if reverse_name else name)

   def areRelated(self, a, b, name):
      if a not in self.rel:
         return False
      if name not in self.rel[a]:
         return False
      return self.rel[a][name] == b

   def __eq__(self, other):
      if isinstance(other, Relation):
         return self.rel == other.rel
      return False

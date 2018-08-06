#!/bin/python

from relation import Relation
import unittest as U
import display_updater as D

class RelationTestsBasic(U.TestCase):
   def test_relate(self):
      r = Relation()
      r.relate(1,2,"lt","gt")
      self.assertEqual(r.rel, {1: {"lt": {2}}, 2: {"gt": {1}}})

      r.relate(1, 5, "lt", "gt")
      self.assertEqual(r.rel, {1: {"lt": {2,5}}, 2: {"gt": {1}}, 5: {"gt": {1}}})

      r.relate(1, 1, "eq")
      self.assertEqual(r.rel, {1: {"lt": {2,5}, "eq": {1}}, 2: {"gt": {1}}, 5: {"gt": {1}}})

      r.relate(1, 5, "asd")
      self.assertEqual(r.rel, {1: {"lt": {2,5}, "eq": {1}, "asd": {5}}, 2: {"gt": {1}}, 5: {"gt": {1}, "asd": {1}}})

      r.relate(5, 1, "asd")
      self.assertEqual(r.rel, {1: {"lt": {2,5}, "eq": {1}, "asd": {5}}, 2: {"gt": {1}}, 5: {"gt": {1}, "asd": {1}}})


class RelationTests(U.TestCase):
   def setUp(self):
      self.r = Relation()
      self.r.relate(1, 5, "asd")
      self.r.relate(1, 1, "eq")
      self.r.relate(1, 5, "lt", "gt")
      self.r.relate(1,2,"lt","gt")

   def test_areRelated(self):
      r = self.r
      self.assertTrue(r.areRelated(1,5,"asd"))
      self.assertTrue(r.areRelated(1,1,"eq"))
      self.assertFalse(r.areRelated(1,1,"neq"))
      self.assertFalse(r.areRelated(3,1,"gt"))
      self.assertFalse(r.areRelated(1,10,"lt"))

   def test_iter(self):
      r = self.r
      copy = Relation()
      for a,b,n in r:
         #pylint: disable=protected-access
         copy._add_relation(a,b,n)
      self.assertEqual(copy, r)

   def test_filter(self):
      r = self.r
      s = r.filter("lt", "gt")
      self.assertEqual(s.rel, {1: {"lt": {2,5}}, 2: {"gt": {1}}, 5: {"gt": {1}}})
      self.assertTrue(r != s)
      self.assertFalse(r == s)
      self.assertTrue(s <= r)

json = {
   "modes": {
      "single": {
         "xrandr": ["--output", "scr1", "--auto", "--primary"]
      }
   }
}

class OutputsSingle(U.TestCase):
   def setUp(self):
      self.outputs = D.Outputs()
      def add(o):
         self.outputs.outputs.append(o)
         #pylint: disable=protected-access
         self.outputs._outputs[o.name] = o

      o = D.Output()
      o.name = "scr1"
      o.connected = True
      o.displaying = True
      o.crtc = 69
      o.primary = False
      o.rotation = 5 #?
      o.mode = 5 #?
      o.x = 0
      o.y = 0
      o.w = 1920
      o.h = 1080
      add(o)

   def test_1(self):
      x = D.Mode(json, "single").apply(self.outputs, dry=True)
      self.assertEqual(x, D.Mode.ALREADY_APPLIED)

class ModeTests(U.TestCase):
   def test_parse_json(self):
      conf = {}
      self.assertRaises(D.DisplayException, D.Mode, conf, "asd")
      # kan inte kolla om det blev rätt exception eller int

if __name__ == "__main__":
   U.main()

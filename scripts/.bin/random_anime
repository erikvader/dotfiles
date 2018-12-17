#!/usr/bin/python

import xml.etree.ElementTree as ET
import sys
from random import choice
import gzip

if len(sys.argv) <= 1:
   print("Usage:")
   print(sys.argv[0] + " gzip_file.gz")
   exit(1)

with gzip.open(sys.argv[1]) as f:
   root = ET.fromstring(f.read())

cand=[]

for a in root.iter("anime"):
   if a.find("my_status").text == "Plan to Watch":
      cand.append({"name": a.find("series_title").text, "id": a.find("series_animedb_id").text})

r = choice(cand)

print(r["name"])
print("http://myanimelist.net/anime/" + r["id"])


#!/bin/python

import argparse
import urllib.request as U
from urllib.parse import urlparse
import zipfile as Z
import json
from bs4 import BeautifulSoup
import os

headers = {"User-Agent": 'Mozilla/5.0 (Windows NT 6.1; Win64; x64)'}

def get_fancy_json(url):
   u = U.Request(url, headers=headers)
   with U.urlopen(u) as u:
      soup = BeautifulSoup(u.read(), "html.parser")
      text = soup.find("script", attrs={"data-type": "chapter"})
      if not text:
         raise Exception("Couldn't find the correct script type, are you on the right website?")
      return json.loads(text.text)

def get_zip_path(data, dest):
   long_chapname = [c["name"] for c in data["other_chapters"] if c["id"] == data["chapter_id"]][0]
   parts = long_chapname.split()
   zipname = "Vol.{} Ch.{} - {}.cbz".format(parts[1], parts[3], data["chapter_title"])
   return os.path.join(dest, zipname)

def generate_urls(data):
   base = data["server"] + data["dataurl"] + "/"
   return [base + p for p in data["page_array"]]

def download_to_zip(url, z, i):
   u = U.Request(url, headers=headers)
   with U.urlopen(u) as u:
      ext = os.path.splitext(urlparse(url).path)[1]
      filename = "{:03d}{}".format(i, ext)
      print("downloading {}".format(filename), flush=True)
      z.writestr(filename, u.read())

def download_pictures(data, dest):
   zippath = get_zip_path(data, dest)
   urls = generate_urls(data)

   if os.path.exists(zippath):
      raise Exception("\"{}\" already exists".format(zippath), flush=True)

   print("started download of {}".format(os.path.basename(zippath)), flush=True)
   print("{} pictures in total".format(len(urls)), flush=True)

   with Z.ZipFile(zippath, "w") as z:
      for i,u in enumerate(urls, 1):
         download_to_zip(u, z, i)

   return zippath

def main():
   parser = argparse.ArgumentParser()
   parser.add_argument("url", help="chapter url: http://mangadex.org/chapter/<id>")
   parser.add_argument("-d", "--directory", metavar="DIR", default=".", help="target directory to save chapter")
   parser.add_argument("-s", "--add-to-subdirectory", action="store_true", help="adds chapter to a subfolder named after the series")
   parser.add_argument("-o", "--open", action="store_true", help="use rifle to open the downloaded chapter in a new process")
   args = parser.parse_args()

   dest = args.directory
   if not os.path.isdir(dest):
      raise Exception("\"{}\" is not a directory".format(dest))

   print("fetching chapter data", flush=True)
   data = get_fancy_json(args.url)

   if args.add_to_subdirectory:
      dest = os.path.join(dest, data["manga_title"])
      if not os.path.isdir(dest):
         os.mkdir(dest)

   zippath = download_pictures(data, dest)

   if args.open:
      os.execlp("rifle", "rifle", os.path.abspath(zippath))

if __name__ == "__main__":
   try:
      main()
      #pylint: disable=broad-except
   except Exception as e:
      print(repr(e), flush=True)
      exit(1)

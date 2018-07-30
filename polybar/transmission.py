#!/bin/python

import urllib.parse
import urllib.request
from urllib.error import HTTPError, URLError
import json
from time import sleep

# typedef enum
# {
#     TR_STATUS_STOPPED = 0, /* Torrent is stopped */
#     TR_STATUS_CHECK_WAIT = 1, /* Queued to check files */
#     TR_STATUS_CHECK = 2, /* Checking files */
#     TR_STATUS_DOWNLOAD_WAIT = 3, /* Queued to download */
#     TR_STATUS_DOWNLOAD = 4, /* Downloading */
#     TR_STATUS_SEED_WAIT = 5, /* Queued to seed */
#     TR_STATUS_SEED = 6 /* Seeding */
# }
# tr_torrent_activity;

T_STOPPED = 0
T_CHECK_W = 1
T_CHECK   = 2
T_DOWN_W  = 3
T_DOWN    = 4
T_SEED_W  = 5
T_SEED    = 6

OFF_DELAY=30
ON_DELAY=5

def ping_transmission():
   try:
      query_transmission(None, {"method": "session-get"})
   except HTTPError as he:
      if he.code == 409:
         return he.headers["X-Transmission-Session-Id"]
      else:
         raise he
   except URLError:
      return None

def query_transmission(session_id, request):
   url = "http://localhost:9091/transmission/rpc"
   # values = {"arguments": {"fields": ["id", "isStalled", "percentDone", "peersSendingToUs", "isFinished", "status"]}, "method": "torrent-get"}
   headers = {"Content-type": "application/json",
              "Accept": "text/plain"}
   if session_id:
      headers["X-Transmission-Session-Id"] = session_id
   data = json.dumps(request)
   data = data.encode('utf-8')

   req = urllib.request.Request(url, data, headers)
   with urllib.request.urlopen(req) as response:
      byte_response = response.read()
      return json.loads(byte_response.decode("utf-8"))

def get_stuff(session_id):
   try:
      # "id", "isStalled", "percentDone", "peersSendingToUs",
      torrents = query_transmission(session_id, {"arguments": {"fields": ["isFinished", "status"]}, "method": "torrent-get"})
      if torrents["result"] == "success":
         session = query_transmission(session_id, {"arguments": {"fields": ["alt-speed-enabled"]}, "method": "session-get"})
         if session["result"] == "success":
            return {"torrents": torrents["arguments"]["torrents"], "alt-speed-enabled": session["arguments"]["alt-speed-enabled"]}
      return None
   except (URLError, HTTPError):
      return None

def polybar_color(s, c):
   return "%{{F#{}}}{}%{{F-}}".format(c, s)

def format_polybar(resp):
   seeding = 0
   downloading = 0
   # idle = 0
   paused = 0
   for t in resp["torrents"]:
      if t["isFinished"]:
         continue
      elif t["status"] == T_SEED or t["status"] == T_SEED_W:
         seeding += 1
      elif t["status"] == T_DOWN or t["status"] == T_DOWN_W:
         downloading += 1
      # elif t["status"] != T_STOPPED and t["percentDone"] != 1:
      #    idle += 1
      elif t["status"] == T_STOPPED:
         paused += 1
   res = "{} {}{}{}{}{}{}{}{}".format("" if resp["alt-speed-enabled"] else "",
                                      polybar_color("D", "7fff00"),
                                      downloading,
                                      polybar_color("|", "ffd700"),
                                      polybar_color("U", "ff00ff"),
                                      seeding,
                                      # polybar_color("|", "ffd700"),
                                      # polybar_color("I", "ff6347"),
                                      # idle,
                                      polybar_color("|", "ffd700"),
                                      polybar_color("P", "ff4500"),
                                      paused)
   return "%{{A3:transmission-remote -AS:}}%{{A1:transmission-remote -as:}}{}%{{A}}%{{A}}".format(res)

def main():
   tid = None
   while True:
      if not tid:
         tid = ping_transmission()

      if not tid:
         print("", flush=True)
         sleep(OFF_DELAY)
      else:
         resp = get_stuff(tid)
         if resp:
            print(format_polybar(resp), flush=True)
         else:
            tid = None
         sleep(ON_DELAY)

if __name__ == "__main__":
   main()

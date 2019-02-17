#!/bin/python

from deluge_client import DelugeRPCClient, FailedToReconnectException
from ssl import SSLError
from time import sleep

ACTIVE_SLEEP = 5
INACTIVE_SLEEP = 60
RABBIT = "🐇"
TURTLE = "🐢"
SKULL = "☠"

last_stats = {}

def polybar_color(s, c):
   return "%{{F#{}}}{}%{{F-}}".format(c, s)

# get amount of torrents that are downloading (everything that is not 100%)
def get_downloading(client):
   # state kan vara intressant
   torrents = client.call('core.get_torrents_status', {}, ['is_finished', 'paused'])

   downloading = 0
   for t in torrents.values():
      if not t[b'is_finished']:
         downloading += 1

   return downloading

# is the download speed limited to any value?
def is_throttled(client):
   download_speed = client.call('core.get_config_value', b'max_download_speed')
   return download_speed != -1

# get average speeds in B/s
# first reading of a session returns junk values
def get_avg_speeds(client):
   global last_stats
   stats = client.call('core.get_session_status', ['total_download', 'total_upload'])

   down = stats[b'total_download'] - last_stats.get(b'total_download', 0)
   down /= ACTIVE_SLEEP

   up = stats[b'total_upload'] - last_stats.get(b'total_upload', 0)
   up /= ACTIVE_SLEEP

   last_stats = stats
   return down, up

def print_stats(client):
   down, up = get_avg_speeds(client)
   throttled = is_throttled(client)
   active = get_downloading(client)

   if down <= 1024 and up <= 1024:
      status_icon = SKULL
   elif throttled:
      status_icon = TURTLE
   else:
      status_icon = RABBIT

   down = polybar_color(str(round(down/1024, 1)), "7cfc00")
   up = polybar_color(str(round(up/1024, 1)), "ff4500")
   active = polybar_color(str(active), "ffd700")

   print("({}/{})#{} {}".format(down, up, active, status_icon), flush=True)

def main():
   client = DelugeRPCClient('127.0.0.1', 58846, 'erik', 'abc123')
   while True:
      try:
         client.reconnect()
      except (ConnectionRefusedError, SSLError):
         sleep(INACTIVE_SLEEP)
         continue

      while True:
         try:
            print_stats(client)
            sleep(ACTIVE_SLEEP)
         except FailedToReconnectException:
            print("", flush=True)
            break

if __name__ == "__main__":
   main()

#!/bin/python

from deluge_client import DelugeRPCClient, FailedToReconnectException
from time import sleep

ACTIVE_SLEEP = 15
INACTIVE_SLEEP = 60

def get_downloading(client):
   # state kan vara intressant
   torrents = client.call('core.get_torrents_status', {}, ['is_finished', 'paused'])

   downloading = 0
   for t in torrents.values():
      if not t[b'is_finished']:
         downloading += 1

   return downloading

def is_throttled(client):
   download_speed = client.call('core.get_config_value', b'max_download_speed')
   return download_speed != -1

def print_stats(client):
   print("{} {}".format(get_downloading(client), "🐢" if is_throttled(client) else "🐇"), flush=True)

def main():
   client = DelugeRPCClient('127.0.0.1', 58846, 'erik', 'abc123')
   while True:
      try:
         client.reconnect()
      except ConnectionRefusedError:
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

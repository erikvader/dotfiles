# /// script
# dependencies = [
#   "deluge-client ~= 1.9",
# ]
# ///

from deluge_client import DelugeRPCClient, FailedToReconnectException
from ssl import SSLError
from time import sleep
from notify_send import notify_send

import logging
L = logging.getLogger(__name__)
L.setLevel(logging.INFO)
ch = logging.FileHandler("/tmp/delugepy.log", mode="w")
ch.setLevel(logging.DEBUG)
ch.setFormatter(logging.Formatter('%(asctime)s:%(name)s:%(levelname)s: %(message)s'))
L.addHandler(ch)

ACTIVE_SLEEP = 5
INACTIVE_SLEEP = 60
MAX_INACTIVE = (60 * 5) / ACTIVE_SLEEP

def get_all_torrents(client):
   return client.call('core.get_torrents_status', {}, ['is_finished', 'queue', 'all_time_download', 'paused', 'name', 'state'])

# move torrents that have been dead for MAX_INACTIVE to the bottom of the queue
def manage(client, torrents, stats):
   isQueued = lambda t: not t[b'is_finished'] and t[b'state'] == b'Queued'
   isDownloading = lambda t: not t[b'is_finished'] and not t[b'paused'] and t[b'state'] == b'Downloading'
   if not any(isQueued(t) for t in torrents.values()):
      L.debug("skipped `manage' due to no queued torrents")
      return

   new_dead = {}
   for h,t in torrents.items():
      if isDownloading(t):
         if h in stats['dead_torrents']:
            if stats['dead_torrents'][h]['count'] >= MAX_INACTIVE:
               downloaded = t[b'all_time_download'] - stats['dead_torrents'][h]['all_time_download']
               limit = 10 * 1024 * MAX_INACTIVE * ACTIVE_SLEEP
               if downloaded <= limit:
                  client.call('core.queue_bottom', [h])
                  L.info('moved {} to the bottom'.format(t[b'name'].decode()))
               else:
                  new_dead[h] = {'count': 0, 'all_time_download': t[b'all_time_download']}
                  L.info("{} was not moved to the bottom, it had downloaded {} which is more than {}".format(t[b'name'].decode(), downloaded, limit))
            else:
               new_dead[h] = stats['dead_torrents'][h]
               new_dead[h]['count'] += 1
               L.debug("increased count for {} to {} out of {} in dead_torrents".format(t[b'name'].decode(), new_dead[h]['count'], MAX_INACTIVE))
         else:
            new_dead[h] = {'count': 1, 'all_time_download': t[b'all_time_download']}
            L.debug("{} is new and was added to dead_torrents".format(t[b'name'].decode()))
   stats['dead_torrents'] = new_dead

# send a notification when a torrent finishes
def send_notifications(torrents, stats):
   for h,t in torrents.items():
      if t[b'is_finished'] and h in stats['unfinished_torrents']:
         notify_send("Torrent completed!", t[b'name'].decode())
   stats['unfinished_torrents'] = {h for h,t in torrents.items() if not t[b'is_finished']}

# get amount of torrents that are downloading (everything that is not 100%)
def get_downloading(torrents):
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
def get_avg_speeds(client, last_stats):
   stats = client.call('core.get_session_status', ['total_download', 'total_upload'])

   down = stats[b'total_download'] - last_stats.get(b'total_download', 0)
   down /= ACTIVE_SLEEP

   up = stats[b'total_upload'] - last_stats.get(b'total_upload', 0)
   up /= ACTIVE_SLEEP

   last_stats.clear()
   last_stats.update(stats)
   return down, up

def main():
   client = DelugeRPCClient('127.0.0.1', 58846, 'erik', 'abc123')
   while True:
      try:
         client.reconnect()
      except (ConnectionRefusedError, SSLError):
         sleep(INACTIVE_SLEEP)
         continue

      # session stats
      stats = {
         'last_stats': {},
         'unfinished_torrents': {},
         'dead_torrents': {}
      }
      while True:
         try:
            torrents = get_all_torrents(client)
            send_notifications(torrents, stats)
            manage(client, torrents, stats)
            sleep(ACTIVE_SLEEP)
         except FailedToReconnectException:
            print("", flush=True)
            break

if __name__ == "__main__":
   main()

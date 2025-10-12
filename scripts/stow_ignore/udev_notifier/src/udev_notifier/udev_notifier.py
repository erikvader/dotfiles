#!/bin/python

import pyudev
import subprocess as S
from eriklibs.notify_send import notify_send

def safe_get(device, *names):
   for n in names:
      if n in device:
         return device.properties[n]
   return "n/a"

def handle_usb(action, device):
   if action not in ["add", "remove"]:
      return

   names = ['ID_MODEL_FROM_DATABASE', 'ID_MODEL', 'ID_SERIAL', 'DEVPATH']
   manu = ['ID_VENDOR_FROM_DATABASE', 'ID_VENDOR']
   msg = "{}, {}".format(
      safe_get(device, *names),
      safe_get(device, *manu)
   )
   head = "USB plugged in" if action == "add" else "USB disconnected"
   notify_send(head, msg, icon="input-keyboard")

def handle_block(action, device):
   if action not in ["add", "remove"]:
      return

   msg = "{} on {}".format(
      safe_get(device, 'ID_FS_VERSION'),
      safe_get(device, 'DEVNAME')
   )
   head = "block device added" if action == "add" else "block device removed"
   notify_send(head, msg, icon="media-removable")

def handle_disp(action, _device):
   notify_send("Display", "Something display related happened! ({})".format(action), icon="video-display")

def handle_blue(action, _device):
   head = "Bluetooth " + ("connected" if action == "add" else "disconnected")
   notify_send(head, "")

def print_all(context):
   monitor = pyudev.Monitor.from_netlink(context)
   for device in iter(monitor.poll, None):
      print(list(device.properties.items()))

def main():
   context = pyudev.Context()
   usb_mon = pyudev.Monitor.from_netlink(context)
   usb_mon.filter_by('usb', 'usb_device')
   block_mon = pyudev.Monitor.from_netlink(context)
   block_mon.filter_by('block', 'partition')
   disp_mon = pyudev.Monitor.from_netlink(context)
   disp_mon.filter_by("drm")
   blue_mon = pyudev.Monitor.from_netlink(context)
   blue_mon.filter_by("bluetooth")

   usb_obs = pyudev.MonitorObserver(usb_mon, handle_usb)
   block_obs = pyudev.MonitorObserver(block_mon, handle_block)
   disp_obs = pyudev.MonitorObserver(disp_mon, handle_disp)
   blue_obs = pyudev.MonitorObserver(blue_mon, handle_blue)

   usb_obs.daemon = False
   block_obs.daemon = False
   disp_obs.daemon = False
   blue_obs.daemon = False

   usb_obs.start()
   block_obs.start()
   disp_obs.start()
   blue_obs.start()

   #print_all(context)

if __name__ == "__main__":
   main()

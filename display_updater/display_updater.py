#!/bin/python

from Xlib import display
import os, json, sys, re
import subprocess as S
import argparse as A
from copy import deepcopy
from time import sleep
from traceback import print_exc

from relation import Relation

CONFIG_FILE=os.path.join(os.environ["HOME"], ".display_updaterc")

class DisplayException(Exception):
   pass

class Outputs:
   #pylint: disable=protected-access

   def __init__(self, dis):
      self._d = dis
      self._s = self._d.screen()
      self._r = self._s.root

      self.outputs = []
      self._outputs = {}

      self.primary = self._r.xrandr_get_output_primary()._data["output"]

      resources = self._r.xrandr_get_screen_resources()._data
      timestamp = resources["config_timestamp"]
      for output in resources["outputs"]:
         o = Output(output, self._d, timestamp)
         if output == self.primary:
            o.primary = True
         self.outputs.append(o)
         self._outputs[o.name] = o

   def __iter__(self):
      return self.outputs.__iter__()

   def __str__(self):
      return "\n".join([str(o) for o in self])

   def get(self, name):
      return self._outputs.get(name, None)

   def get_relations(self):
      return self._get_mirrors()

   def _get_mirrors(self):
      rel = Relation()
      ms = self.outputs
      for i in range(len(ms)):
         for j in range(i+1, len(ms)):
            if ms[i].is_mirroring(ms[j]):
               rel.relate(ms[i].name, ms[j].name, "mirror")
      return rel

   def all_connected(self, outs):
      for o in outs:
         if not self.get(o).connected:
            return False
      return True

   def all_only_alive(self, outs):
      for o in outs:
         if not self.get(o).is_alive():
            return False
      if self.get_other_alive(outs):
         return False
      return True

   def get_other_alive(self, outs):
      return [o.name for o in self if o.name not in outs and o.is_alive()]

   def get_all_dead(self):
      return [o.name for o in self.outputs if o.is_dead()]

   # get xrandr arguments to disable all dead outputs and to disable non-wanted outputs.
   # be_on is a list if wanted outputs (their names). If it is None, then return only args for dead outputs.
   def get_comp_xrandr_args(self, be_on):
      all_other = self.get_all_dead()
      all_other += self.get_other_alive(be_on) if be_on is not None else []
      xrandr_args = []
      for d in all_other:
         xrandr_args += ["--output", d, "--off"]
      return xrandr_args

class Output:
   def __init__(self, output, d, timestamp):
      #pylint: disable=protected-access
      info = d.xrandr_get_output_info(output, timestamp)._data
      self.name = info["name"]
      self.connected = info["connection"] == 0
      self.displaying = info["crtc"] in info["crtcs"]
      self.crtc = info["crtc"]
      self.primary = False

      if self.displaying:
         info2 = d.xrandr_get_crtc_info(self.crtc, timestamp)._data
         self.rotation = info2["rotation"]
         self.mode = info2["mode"]
         self.x = info2["x"]
         self.y = info2["y"]
         self.w = info2["width"]
         self.h = info2["height"]
      else:
         self.rotation = self.mode = self.x = self.y = self.w = self.h = None

   def is_mirroring(self, output2):
      if not self.displaying or not output2.displaying:
         return False
      return self.x == output2.x and self.y == output2.y and self.w == output2.w and self.h == output2.h

   def is_dead(self):
      return self.displaying and not self.connected

   def is_alive(self):
      return self.displaying and self.connected

   def __str__(self):
      def bool_to_is(b):
         return "is" if b else "is not"
      return "{} {} connected and {} displaying".format(self.name, bool_to_is(self.connected), bool_to_is(self.displaying))

class Mode:
   APPLY_SUCCESS = 0
   CANT_APPLY = 1
   ALREADY_APPLIED = 2

   def __init__(self, conf, mode):
      self.default = None
      self.outputs = []
      self.xrandr = []
      self.no_xrandr = False
      self.programs = []
      self.delay = conf.get("programs_delay", 0)
      self.relations = Relation()

      if "modes" not in conf:
         raise DisplayException("missing required field 'modes'")
      rest = self._parse_mode(conf, mode)

      if not self.no_xrandr:
         self._parse_xrandr(self.xrandr)
         if not self.outputs:
            raise DisplayException("xrandr list didn't produce any outputs")

      self._parse_programs(conf, rest)

   def _parse_xrandr(self, xrandr):
      flag_re = re.compile(r"^--")
      def is_flag(s):
         return flag_re.match(s)

      waiting_out = False
      waiting_same = False
      for x in xrandr:
         if waiting_out:
            waiting_out = False
            if is_flag(x):
               raise DisplayException("xrandr parse error: didn't expect a flag after --output")
            self.outputs.append(x)
         elif waiting_same:
            waiting_same = False
            if is_flag(x):
               raise DisplayException("xrandr parse error: didn't expect a flag after --same-as")
            if not self.outputs:
               raise DisplayException("xrandr parse error: --same-as on nothing?")
            self.relations.relate(self.outputs[-1], x, "mirror")
         # elif not is_flag(x):
         #    raise DisplayException("xrandr parse error: is expecting a flag")
         elif x == "--output":
            waiting_out = True
         elif x == "--off":
            raise DisplayException("xrandr parse error: why use --off? it's not needed")
         elif x == "--same-as":
            waiting_same = True

      if waiting_out or waiting_same:
         raise DisplayException("xrandr parse error: list ended with trailing --output or --same-as")

   def _parse_programs(self, conf, rest):
      if "run_programs" not in rest:
         return
      global_progs = deepcopy(conf.get("programs", {}))
      rp = rest["run_programs"]
      if type(rp) == list:
         global_progs = {k:v for k,v in global_progs.items() if k in rp}
      if "programs" in rest:
         for k in rest["programs"]:
            if k not in global_progs:
               raise DisplayException("trying to update {} but it doesn't exist or is not supposed to run".format(k))
            global_progs[k].update(rest["programs"][k])
      for v in global_progs.values():
         self.programs.append(v)

   def _parse_mode(self, conf, mode):
      def fix_inherit(mode, visited):
         if mode not in conf["modes"]:
            raise DisplayException("'{}' not a valid mode".format(mode))
         m = deepcopy(conf["modes"][mode])
         if "inherit" in m:
            if mode in visited:
               raise DisplayException("recursive inheritance found")
            visited.append(mode)
            inh = fix_inherit(m["inherit"], visited)
            inh.update(m)
            del inh["inherit"]
            return inh
         return m

      m = fix_inherit(mode, [])
      self.default   = m.get("default")
      self.outputs   = m.get("outputs", [])
      self.xrandr    = m.get("xrandr", [])
      self.no_xrandr = not bool(self.xrandr)
      self.mirror    = m.get("mirror", [])
      return m

   def _is_already_applied(self, outputs):
      if self.no_xrandr:
         return False
      if not outputs.all_only_alive(self.outputs):
         return False
      rel = outputs.get_relations()
      if self.relations.filter("mirror") != rel.filter("mirror"):
         return False
      # works because we know at this point that all_only_alive is true
      # and this is needed because outputs.get_relations() returns more relations than self.relations has
      # if not self.relations.filter("right", "left", "osv") <= rel.filter("right", "left", "osv"):
      #    return False
      return True

   def _is_cant_apply(self, outputs):
      if self.no_xrandr:
         return False
      return not outputs.all_connected(self.outputs)

   def apply(self, outputs, force=False, dry=False):
      if self._is_cant_apply(outputs):
         return Mode.CANT_APPLY
      if not force and self._is_already_applied(outputs):
         return Mode.ALREADY_APPLIED

      if not self.no_xrandr or force:
         xrandr_args = outputs.get_comp_xrandr_args(self.outputs)
         start_internal("xrandr", self.xrandr + xrandr_args, dry=dry, hate_non_zero=True)

      if self.delay and not self.no_xrandr:
         sleep(self.delay)

      for p in self.programs:
         if not dry:
            pkill(p["name"])
         c = 0
         while not dry and pgrep(p["name"]):
            c += 1
            if c > 15:
               asd = "{} didn't die".format(p["name"])
               notify_send(asd)
               print(asd, flush=True, file=sys.stderr)
               break
            sleep(0.3)
         else:
            if "file" not in p:
               raise DisplayException("some program is missing required field \"file\"")
            start_external(p["file"], p.get("args"), p.get("env"), dry)

   def __str__(self):
      return "default: {}, outputs: {}, xrandr: {}, no_xrandr: {}, programs: {}, delay: {}".format(self.default, self.outputs, self.xrandr, self.no_xrandr, self.programs, self.delay)

def pkill(name):
   S.run(["pkill", "-x", name], stdin=S.DEVNULL, stderr=S.DEVNULL, stdout=S.DEVNULL)

def pgrep(name):
   r = S.run(["pgrep", "-x", name], stdin=S.DEVNULL, stderr=S.DEVNULL, stdout=S.DEVNULL)
   return r.returncode == 0

def start_external(file, args=None, env=None, dry=False):
   if dry:
      print("{} {} {}".format(file, args, env))
      return
   pid = os.fork()
   if pid != 0:
      return
   os.setsid()
   with open(os.devnull, "r") as r, open(os.devnull, "w") as w:
      os.dup2(r.fileno(), sys.stdin.fileno())
      os.dup2(w.fileno(), sys.stdout.fileno())
      os.dup2(w.fileno(), sys.stderr.fileno())
   eu = os.path.expanduser(file)
   args = args if args else []
   env = {**os.environ, **env} if env else os.environ
   os.execvpe(eu, [eu] + args, env)

def start_internal(file, args=None, dry=False, hate_non_zero=False):
   if dry:
      print("{} {}".format(file, args))
      return
   args = args if args else []
   res = S.run([file] + args)
   if hate_non_zero and res.returncode != 0:
      raise DisplayException("{} exited with exit status {}".format(file, res.returncode))
   return res.returncode

def notify_send(*msg):
   start_internal("notify-send", list(msg))

def read_config():
   if not os.path.isfile(CONFIG_FILE):
      print("couldn't find config file at \"{}\"".format(CONFIG_FILE), file=sys.stderr)
      exit(1)
   try:
      with open(CONFIG_FILE, "r") as f:
         comm = re.compile(r"^[ \t]*//")
         lines = f.readlines()
         lines = [l for l in lines if not comm.match(l)]
         return json.loads("".join(lines))
   except json.JSONDecodeError as e:
      raise DisplayException("config parse error: " + str(e))

def main():
   parser = A.ArgumentParser()
   group = parser.add_mutually_exclusive_group(required=True)
   group.add_argument("-l", "--list", action="store_true", help="list all modes")
   group.add_argument("-c", "--cleanup", action="store_true", help="only kill dead outputs")
   group.add_argument("mode", nargs="?", help="what mode to apply")
   parser.add_argument("-f", "--force", action="store_true", help="always apply mode regardless of state. BE CAREFUL!")
   parser.add_argument("-d", "--dry-run", action="store_true", help="do everything except actually spawning programs and running xrandr")
   parser.add_argument("-k", "--keep-dead", action="store_true", help="don't disable dead outputs if mode can't be applied or is already applied (default is to kill)")
   args = parser.parse_args()

   conf = read_config()

   if args.list:
      if "modes" not in conf:
         raise DisplayException("missing modes")
      for k in conf["modes"]:
         print(k, flush=True)
      return
   else:
      outs = Outputs(display.Display())
      def kill_dead():
         xrandr_dead = outs.get_comp_xrandr_args(None)
         if xrandr_dead:
            start_internal("xrandr", xrandr_dead, dry=args.dry_run, hate_non_zero=False)
            notify_send("killed dead outputs")
      def run_mode(mode):
         mod = Mode(conf, mode)
         res = mod.apply(outs, args.force, args.dry_run)
         if res == Mode.APPLY_SUCCESS:
            notify_send("success")
         elif res == Mode.ALREADY_APPLIED:
            notify_send("nothing to do")
         elif res == Mode.CANT_APPLY:
            if mod.default:
               run_mode(mod.default)
               return
            else:
               notify_send("I literally can't even")

         if res != Mode.APPLY_SUCCESS and not args.keep_dead:
            kill_dead()

      if args.cleanup:
         kill_dead()
      else:
         run_mode(args.mode)

if __name__ == "__main__":
   #pylint: disable=broad-except
   try:
      main()
   except DisplayException as e:
      print(str(e), flush=True, file=sys.stderr)
      notify_send("display updater exception:", str(e))
      exit(1)
   except Exception as e:
      print_exc()
      notify_send("an unexpected exception happened", repr(e))
      exit(1)
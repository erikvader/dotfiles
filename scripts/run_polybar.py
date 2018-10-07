#!/bin/python

import argparse as A
import shlex
import subprocess as S
import sys
import os

# def splitEmpty(seq, delim):
#    return [x for x in seq.split(delim) if x]

def addTempEnv(info):
   for hw in os.scandir("/sys/class/hwmon"):
      if hw.name.startswith("hwmon"):
         with open(os.path.join(hw.path, "name"), "r") as f:
            if f.read().rstrip("\n") == info[1]:
               os.environ[info[0]] = os.path.join(hw.path, info[2])
               break

def getPolybarOutputs():
   res = S.run(["polybar", "-m"], stdout=S.PIPE, text=True)
   return {l.split(":")[0] for l in res.stdout.split("\n") if l}

def startPolybar(args, monitors):
   args = shlex.split(args)
   for m in monitors:
      S.Popen(["polybar", *args], env={"MONITOR": m, **os.environ})

def main():
   parser = A.ArgumentParser(description="helper script for starting polybar on multiple screens")
   parser.add_argument("primary", nargs="+", help="primary outputs to start polybar on")
   parser.add_argument("-p", "--primary-args", help="string of to-be-split arguments given to the primary polybars", required=True)
   parser.add_argument("-s", "--secondary-args", help="string of to-be-split arguments given to the secondary polybars")
   parser.add_argument("-t", "--sensor", default=[], nargs=3, action="append", metavar=("env", "sensor", "input_file"), help="sets env to be the path to input_file of sensor sensor, can be supplied more than once")
   parser.add_argument("-m", "--secondaries", action="store_true", help="whether to start secondary polybars on all other outputs")

   args = parser.parse_args()

   if args.secondaries and not args.secondary_args:
      parser.error("--secondary-args required if --secondaries")

   availableOutputs = getPolybarOutputs()
   primaries = set(args.primary)

   for t in args.sensor:
      addTempEnv(t)

   startPolybar(args.primary_args, availableOutputs & primaries)

   if args.secondaries:
      startPolybar(args.secondary_args, availableOutputs - primaries)

if __name__ == "__main__":
   main()



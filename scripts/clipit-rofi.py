#!/usr/bin/env python

# rofi clipboard selector for Clipit with the extra feature of having
# static clipboard items

# needs to have save history enabled for accesing the history

# TODO: add config file for statics

import struct, os
import subprocess

statics_ = [
    ("shrug", r"¯\_(ツ)_/¯"),
    ("swast", r"卐"),
    ("lenny", r"( ͡° ͜ʖ ͡°)"),
    ("flip", r"(ノಠ益ಠ)ノ彡┻━┻"),
    ("hug", r"(づ｡◕‿‿◕｡)づ"),
    ("flat", r"ಠ_ಠ"),
    ("glitter", r"(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧"),
    ("fight", r"ლ(ಠ益ಠლ)")
]

def readHist():
    homedir  = os.environ['HOME']
    histfile = homedir + '/.local/share/clipit/history'

    if not os.path.isfile(histfile):
        return []

    items = []
    with open(histfile,'rb') as f:
        f.read(68)
        size,_ = struct.unpack('2i',f.read(8))
        while (size > 0):
            item = f.read(size)
            items.append(item)
            _,_,_,size,_ = struct.unpack('5i',f.read(20))
    return items

def runRofi(items, statics):
    longest_key = len(max(statics, key=lambda x: len(x[0]))[0])
    strings = ["{{:<{}}} {{}}".format(longest_key).format(k+":", v.decode("utf-8")) for k,v in statics]
    strings.append("-"*20)
    strings += [x.decode("utf-8").replace("\n", r"\n") for x in items]
    inputline = "\n".join(strings)

    slen = len(statics)

    res = subprocess.run(["rofi", "-dmenu", "-i", "-format", "i", "-no-custom", "-p", "clipboard", "-selected-row", str(slen+1)], universal_newlines=True, stdout=subprocess.PIPE, input=inputline)

    raw = res.stdout.rstrip("\n")
    if not raw:
        return None

    ind = int(raw)

    # selected the separator
    if ind == slen:
        return None
    elif ind < slen:
        return statics[ind][1]
    else:
        return items[ind-slen-1]


def main():

    items = readHist()
    statics = [(k, v.encode()) for k,v in statics_]

    selected = runRofi(items, statics)

    if selected:
        subprocess.run(["xclip", "-i", "-selection", "clipboard"], input=selected)

if __name__ == "__main__":
    main()

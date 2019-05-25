#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# was here already
sysresources=/etc/X11/xinit/.Xresources
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

find "$HOME" -maxdepth 1 -mindepth 1 -xtype f -name '*.Xresources' -print0 | xargs -0 -n1 -r xrdb -merge

# was in here already
sysmodmap=/etc/X11/xinit/.Xmodmap
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

# was here already
# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

exec xmonad


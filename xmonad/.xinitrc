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

# load all xresources in home
for f in "$HOME"/.*.Xresources; do
    [ -f "$f" ] && xrdb -merge "$f"
done

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


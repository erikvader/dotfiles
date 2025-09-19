#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

exec >/tmp/xinit.stdout 2>/tmp/xinit.stderr

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

# 'Copy' of /etc/X11/xinit/xinitrc.d/50-systemd-user.sh, which is shipped with
# systemd, but modified to import ALL environment variables into the systemd
# user daemon. This assumes that all environment variables have been set by the
# login shell before running startx, which is the case for zsh if they are
# defined in .zshenv. Systemd says this usage is deprecated because it can add
# variables that are specific to interactive use and can be confusing if
# imported into the daemon. But when imported here, with only profile sourced,
# not too many extras are imported, so it's probably fine.
dbus-update-activation-environment --all --systemd

exec xmonad


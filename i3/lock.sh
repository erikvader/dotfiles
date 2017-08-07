#!/bin/bash

SCREEN=/tmp/screen.png
LOCK=~/.config/i3/lock.png

scrot $SCREEN
convert $SCREEN -scale 10% -scale 1000% $SCREEN
convert $SCREEN $LOCK -gravity center -composite -matte $SCREEN
#dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop
i3lock -i $SCREEN
rm $SCREEN


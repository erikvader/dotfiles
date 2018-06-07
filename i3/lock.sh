#!/bin/bash

# lock with blurred screenshot ################################################
# SCREEN='/tmp/screen.png'
# LOCK=~/.i3/lock.png

# scrot --delay 0 --quality 1 "$SCREEN"
# convert "$SCREEN" -scale 10% -scale 1000% "$SCREEN"
# convert "$SCREEN" "$LOCK" -gravity center -composite -matte "$SCREEN"
# i3lock -i "$SCREEN"
# rm "$SCREEN"

i3lock -t -i "$HOME/.lock"

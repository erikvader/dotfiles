#!/bin/sh

# if [[ "$1" -eq "up" ]]; then
#     xbflag="-inc"
# elif [[ "$1" -eq "down" ]]; then
#     xbflag="-dec"
# fi;

# cur="$(xbacklight -get)"

# if [[ "$cur<10" | bc -l ]]; then

xbacklight -steps 1 "$@"
notify-send.sh --icon=display-brightness-symbolic.symbolic --replace-file=/tmp/i3brightnessnotifyreplaceid 'Brightness' "$(xbacklight -get)"

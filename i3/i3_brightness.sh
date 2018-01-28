#!/bin/sh

# if [[ "$1" -eq "up" ]]; then
#     xbflag="-inc"
# elif [[ "$1" -eq "down" ]]; then
#     xbflag="-dec"
# fi;

# cur="$(xbacklight -get)"

# if [[ "$cur<10" | bc -l ]]; then

xbacklight $@
# notify-send 'Brightness' "$(xbacklight -get)"

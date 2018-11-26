#!/bin/bash

# saves a screenshot in some directory

DIRECTORY="$HOME/Pictures/screenshots"
filename="$DIRECTORY/$(date '+%F_%T_%N').png"

if maim "$@" "$filename"; then
    notify-send 'screenshot' "saved in $filename"
    exit 0
else
    notify-send 'screenshot' 'failed or was aborted'
    exit 1
fi

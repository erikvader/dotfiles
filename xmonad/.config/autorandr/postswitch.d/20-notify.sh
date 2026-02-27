#!/bin/sh

if [ -n "$AUTORANDR_INIT" ]; then
    # NOTE: dunst hasn't started at this point, so notify-send hangs with a long timeout
    echo Skipped "$0"
    exit
fi

notify-send -i display 'Autorandr profile changed' "$AUTORANDR_CURRENT_PROFILE"

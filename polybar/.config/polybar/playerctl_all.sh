#!/bin/bash

while true; do
    if playerctl -l 2>/dev/null | grep spotify &>/dev/null; then
        if [[ "$(playerctl --player=spotify status)" = Playing ]]; then
            echo -n ' '
        else
            echo -n '  '
        fi
        playerctl --player=spotify metadata artist
        echo -n ' - '
        playerctl --player=spotify metadata title
        echo
        sleep 5
    else
        echo ""
        sleep 60
    fi;
done

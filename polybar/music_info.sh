#!/bin/bash

if [[ "$(playerctl --player=spotify status)" == "Playing" ]]; then
    echo "$(playerctl --player=spotify metadata artist) - $(playerctl --player=spotify metadata title)"
else
    echo ""
fi;

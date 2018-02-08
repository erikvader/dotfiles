#!/bin/bash

if playerctl -l 2>/dev/null | grep spotify &>/dev/null; then
    echo "$(playerctl --player=spotify metadata artist) - $(playerctl --player=spotify metadata title)"
else
    echo ""
fi;

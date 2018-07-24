#!/bin/bash

# open a pdf from downloads with xdg-open

root="$HOME/Downloads"

choice=$(find -H "$root" -name '*.pdf' -printf '%T@ %P\n' 2>/dev/null | LC_ALL=C sort --key=1nr | cut -d ' ' -f 2- | rofi -dmenu -no-custom -p Open -i)

if [[ -n "$choice" ]]; then
    xdg-open "$root/$choice" &
fi


#!/bin/bash

# open a pdf from downloads with xdg-open

root="$HOME/Downloads"

choice=$(find -L "$root" -name '*.pdf' -printf '%T@ %P\n' 2>/dev/null | sort -n | cut -d ' ' -f 2- | rofi -dmenu -no-custom -p Open -i)

if [[ -n "$choice" ]]; then
    xdg-open "$root/$choice" &!
fi


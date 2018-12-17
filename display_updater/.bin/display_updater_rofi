#!/bin/bash

choice="$( display_updater --list | rofi -dmenu -i -p "display" -no-custom )"

if [[ "$choice" ]]; then
    display_updater "$choice"
fi


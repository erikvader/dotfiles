#!/bin/bash

choice="$( display_updater --options | rofi -dmenu -i -p "display" -no-custom )"

if [[ "$choice" ]]; then
    display_updater "$choice"
fi


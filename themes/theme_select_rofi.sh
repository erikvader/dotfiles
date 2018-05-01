#!/bin/bash

choice="$( echo "$(theme_select -l)" | rofi -dmenu -i -p "theme" -no-custom )"

if [[ "$choice" ]]; then
    theme_select "$choice"
fi

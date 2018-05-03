#!/bin/bash

choice="$( (echo "random"; echo "$(theme_select -l)") | rofi -dmenu -i -p "theme" -no-custom )"

if [[ "$choice" = "random" ]]; then
    theme_select -r
elif [[ "$choice" ]]; then
    theme_select "$choice"
fi

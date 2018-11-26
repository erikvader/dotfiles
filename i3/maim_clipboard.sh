#!/bin/bash

tmp='/tmp/screenshot.png'
trap 'rm -f "$tmp"' EXIT

if ! maim "$@" "$tmp"; then
    notify-send 'maim failed'
    exit 1
fi

type=$(file -b --mime-type "$tmp")

if xclip -selection clipboard -t "$type" -i "$tmp"; then
    notify-send 'maim_clipboard' 'Screenshot now in clipboard'
else
    notify-send 'maim_clipboard' 'Screenshot failed!'
fi


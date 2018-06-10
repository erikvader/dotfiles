#!/bin/sh

tmp='/tmp/scrot_to_clipboard.png'

if echo "$@" | grep -- '-s' >/dev/null; then
   notify-send 'scrot_clipboard' 'Select an area with the mouse'
fi;

if scrot "$@" "$tmp" && xclip -selection clipboard -t image/png -i "$tmp"; then
    notify-send 'scrot_clipboard' 'Screenshot now in clipboard'
else
    notify-send 'scrot_clipboard' 'Screenshot failed!'
fi

rm -f "$tmp"

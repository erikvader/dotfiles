#!/bin/sh

tmp='/tmp/scrot_to_clipboard.png'

if echo "$@" | grep '-s' - >/dev/null; then
   notify-send 'scrot_clipboard' 'Select an area with the mouse'
fi;
scrot $@ "$tmp" &&
xclip -selection clipboard -t image/png -i "$tmp" &&
notify-send 'scrot_clipboard' 'Screenshot now in clipboard' ||
notify-send 'scrot_clipboard' 'Screenshot failed!'

rm -f "$tmp"

#!/bin/sh

tmp='/tmp/scrot_to_clipboard.png'

scrot $@ "$tmp" &&
xclip -selection clipboard -t image/png -i "$tmp" &&
notify-send 'scrot_clipboard' 'Screenshot now in clipboard' ||
notify-send 'scrot_clipboard' 'Screenshot failed!'

rm -f "$tmp"

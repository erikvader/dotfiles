#!/bin/bash

tmp='/tmp/screenshot.jpg'
trap 'rm -f "$tmp"' EXIT

if echo "$@" | grep -- '-s' >/dev/null; then
   notify-send 'scrot_clipboard' 'Select an area with the mouse'
fi

if ! scrot "$@" "$tmp"; then
    notify-send 'scrot failed'
    exit 1
fi

# greenclip only likes images that are less than 500kb
if [[ $(identify -format '%B' "$tmp") -lt 512000 ]]; then
    # default jpg quality in scrot is 75
    convert "$tmp" -define jpeg:extent=500kb -quality 75 "$tmp"
fi

if xclip -selection clipboard -t image/jpeg -i "$tmp"; then
    notify-send 'scrot_clipboard' 'Screenshot now in clipboard'
else
    notify-send 'scrot_clipboard' 'Screenshot failed!'
fi


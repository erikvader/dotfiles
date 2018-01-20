#!/bin/sh

out1='eDP1'
out2='HDMI1'
connected="$(xrandr | grep ' connected ' | sed -E 's/^(.+?) connected.*$/\1/' | xargs -d'\n' echo)"

# if echo "$connected" | grep "$out1" &>/dev/null; then
#     out1c="1"
# fi

if echo "$connected" | grep "$out2" &>/dev/null; then
    out2c="1"
fi

notify-send "$connected"
if [[ "$out2c" ]]; then
    xrandr --output "$out1" --auto --primary --output "$out2" --auto --right-of "$out1" && ~/.config/polybar/run_polybar.sh "$out1" "$out2"
else
    xrandr --output "$out1" --auto --primary --output "$out2" --off && ~/.config/polybar/run_polybar.sh "$out1"
fi
~/.fehbg


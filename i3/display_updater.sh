#!/bin/sh

out1='eDP1'
out2='HDMI1'
# connected="$(xrandr | grep ' connected ' | sed -E 's/^(.+?) connected.*$/\1/' | xargs -d'\n' echo)"

# if echo "$connected" | grep "$out1" &>/dev/null; then
#     out1c="1"
# fi

# if echo "$connected" | grep "$out2" &>/dev/null; then
#     out2c="1"
# fi

if xrandr | grep -E "^$out2 connected"; then
    xrandr --output "$out1" --auto --primary --output "$out2" --auto --right-of "$out1" && ~/.config/polybar/run_polybar.sh "$out1" "$out2"
    notify-send "$out2 activated"
else
    xrandr --output "$out1" --auto --primary --output "$out2" --off && ~/.config/polybar/run_polybar.sh "$out1"
    notify-send "$out2 deactivated"
fi
~/.fehbg

pkill conky
while pgrep -x conky >/dev/null; do sleep 1; done
$HOME/.config/conky/start &

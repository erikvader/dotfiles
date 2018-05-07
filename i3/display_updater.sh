#!/bin/bash

out1='eDP1'
out2='HDMI1'
# connected="$(xrandr | grep ' connected ' | sed -E 's/^(.+?) connected.*$/\1/' | xargs -d'\n' echo)"

# if echo "$connected" | grep "$out1" &>/dev/null; then
#     out1c="1"
# fi

# if echo "$connected" | grep "$out2" &>/dev/null; then
#     out2c="1"
# fi

out2connected="$(xrandr | grep -qE "^$out2 connected"; echo $?)"
out2displaying="$(getCRTC "$out2" &>/dev/null; echo $?)"

if [[ "$out2connected" -eq 0 && "$out2displaying" -ne 0 ]]; then
    xrandr --output "$out1" --auto --primary --output "$out2" --auto --right-of "$out1"
    notify-send "$out2 activated"
elif [[ "$out2connected" -ne 0 && "$out2displaying" -eq 0 ]]; then
    xrandr --output "$out1" --auto --primary --output "$out2" --off
    notify-send "$out2 deactivated"
else
    notify-send "nothing to do"
fi

if [[ "$out2connected" -eq 0 ]]; then
    ~/.config/polybar/run_polybar.sh "$out1" "$out2"
else
    ~/.config/polybar/run_polybar.sh "$out1"
fi

"$HOME/.start_feh" &!

pkill conky
while pgrep -x conky >/dev/null; do sleep 1; done
"$HOME/.start_conky" &!

# eftersom en extern monitor blir svart om inte compton startas om
pkill compton
while pgrep -x compton >/dev/null; do sleep 1; done
compton -b

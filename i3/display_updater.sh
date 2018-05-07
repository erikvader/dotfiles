#!/bin/bash

# display_updater action
# action can be: startup - restart/start everything and update
#                update  - check for monitor change and fix everything
#                feh     - restart/start feh
#                conky   - restart/start feh
#                polybar - restart/start polybar
#                compton - restart/start compton
#
# everything uses my custom theme thingy

feh=
conky=
polybar=
compton=
screens=

case "$1" in
    startup)
        feh=true
        conky=true
        polybar=true
        compton=true
        screens=true
        ;;
    feh)
        feh=true
        ;;
    conky)
        conky=true
        ;;
    polybar)
        polybar=true
        ;;
    compton)
        compton=true
        ;;
    update)
        screens=true
        compton=true
        feh=true
        polybar=true
        ;;
    *)
        echo "give me an argument :(" 1>&2
        exit 1
        ;;
esac

out1='eDP1'
out2='HDMI1'

out2connected="$(xrandr | grep -qE "^$out2 connected"; echo $?)"
out2displaying="$(getCRTC "$out2" &>/dev/null; echo $?)"

if [[ "$screens" ]]; then
    if [[ "$out2connected" -eq 0 && "$out2displaying" -ne 0 ]]; then
        xrandr --output "$out1" --auto --primary --output "$out2" --auto --right-of "$out1"
        notify-send "$out2 activated"
    elif [[ "$out2connected" -ne 0 && "$out2displaying" -eq 0 ]]; then
        xrandr --output "$out1" --auto --primary --output "$out2" --off
        notify-send "$out2 deactivated"
    else
        notify-send "nothing to do"
        compton=
        feh=
        polybar=
    fi
fi

if [[ "$polybar" ]]; then
    if [[ "$out2connected" -eq 0 ]]; then
        run_polybar "$out1" "$out2"
    else
        run_polybar "$out1"
    fi
fi

if [[ "$feh" ]]; then
    "$HOME/.start_feh" &!
fi

if [[ "$conky" ]]; then
    pkill conky
    while pgrep -x conky >/dev/null; do sleep 1; done
    "$HOME/.start_conky" &!
fi

# eftersom en extern monitor blir svart om inte compton startas om
if [[ "$compton" ]]; then
    pkill compton
    while pgrep -x compton >/dev/null; do sleep 1; done
    compton -b
fi


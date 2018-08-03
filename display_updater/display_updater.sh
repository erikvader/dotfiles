#!/bin/bash

# display_updater [actions]
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
multihead=
update=
external=
primary=

if [[ $# -lt 1 ]]; then
    echo "Give me some arguments :(" 1>&2
    exit 1
fi

while [[ "$1" ]]; do
    case "$1" in
        --options)
            echo -e 'update\nexternal\nprimary\ncompton\npolybar\nconky\nfeh\nstartup'
            exit 0
            ;;
        startup)
            feh=true
            conky=true
            polybar=true
            compton=true
            multihead=true
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
            update=true
            multihead=true
            compton=true
            feh=true
            # polybar=true
            ;;
        external)
            external=true
            feh=true
            compton=true
            # polybar=true
            ;;
        primary)
            primary=true
            feh=true
            compton=true
            # polybar=true
            ;;
        *)
            echo "invalid argument \"$1\"" 1>&2
            exit 1
            ;;
    esac
    shift
done

out1='eDP1'
out2='HDMI1'

function getScreenInfo {
    out2connected="$(xrandr | grep -qE "^$out2 connected"; echo $?)"
    out2displaying="$(getCRTC "$out2" &>/dev/null; echo $?)"
    out1displaying="$(getCRTC "$out1" &>/dev/null; echo $?)"
}

getScreenInfo

if [[ "$multihead" ]]; then
    if [[ "$out2connected" -eq 0 && ("$out2displaying" -ne 0 || "$out1displaying" -ne 0) ]]; then
        xrandr --output "$out1" --auto --primary --output "$out2" --auto --right-of "$out1"
        notify-send "$out1 activated, $out2 activated"
        getScreenInfo
        sleep 3
    elif [[ ("$out2connected" -ne 0 && "$out2displaying" -eq 0) || "$out1displaying" -ne 0 ]]; then
        xrandr --output "$out1" --auto --primary --output "$out2" --off
        getScreenInfo
        notify-send "$out1 activated, $out2 deactivated"
        sleep 3
    else
        notify-send "nothing to do"
        if [[ "$update" ]]; then
            compton=
            feh=
            # polybar=
        fi
    fi
elif [[ "$external" ]]; then
    if [[ "$out2connected" -eq 0 ]]; then
        if [[ "$out1displaying" -eq 0 || "$out2displaying" -ne 0 ]]; then
            xrandr --output "$out1" --off --output "$out2" --auto --primary
            getScreenInfo
            notify-send "external mode activated"
            sleep 3
        else
            notify-send "external already activated, nothing to do"
            feh=
            compton=
            # polybar=
        fi
    else
        notify-send "$out2 not even connected..."
        feh=
        compton=
        # polybar=
    fi
elif [[ "$primary" ]]; then
    if [[ "$out2displaying" -eq 0 || "$out1displaying" -ne 0 ]]; then
        xrandr --output "$out1" --auto --primary --output "$out2" --off
        getScreenInfo
        notify-send "primary mode activated"
        sleep 3
    else
        notify-send "primary already activated, nothing to do"
        feh=
        compton=
        # polybar=
    fi
fi

function run_polybar {
    pkill polybar

    # Wait until the processes have been shut down
    while pgrep -x polybar >/dev/null; do sleep 1; done

    # Launch bar1 and bar2
    MONITOR=$1 setsid polybar -r example &>/dev/null </dev/null &
    if [[ -n "$2" ]]; then
        MONITOR=$2 setsid polybar -r nonprimary &>/dev/null </dev/null &
    fi
}

if [[ "$polybar" ]]; then
    if [[ "$out2displaying" -eq 0 && "$out1displaying" -ne 0 ]]; then
        run_polybar "$out2"
    else
        run_polybar "$out1"
    fi
fi

if [[ "$feh" ]]; then
    pkill feh_loop
    while pgrep feh_loop >/dev/null; do sleep 1; done
    setsid "$HOME/.start_feh" &>/dev/null </dev/null &
fi

if [[ "$conky" ]]; then
    pkill conky
    while pgrep -x conky >/dev/null; do sleep 1; done
    setsid "$HOME/.start_conky" &>/dev/null </dev/null &
fi

# eftersom en extern monitor blir svart om inte compton startas om
if [[ "$compton" ]]; then
    pkill compton
    while pgrep -x compton >/dev/null; do sleep 1; done
    setsid compton &>/dev/null </dev/null &
fi


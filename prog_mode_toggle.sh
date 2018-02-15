#!/bin/bash

status_file=~/.program_mode
file_contents="$(cat $status_file)"

function on {
    setxkbmap er nodeadkeys &&
    sleep 0.5 &&
    xmodmap ~/.xmodmap_prog &&
    sleep 0.5 &&
    xcape -e 'Control_R=Escape' &&
    xset r rate 300 35 && # keyboard repeat speed
    echo "on" > "$status_file" &&
    notify-send 'Programming keyboard mode' 'on'

    if [[ "$?" -ne 0 ]]; then
        notify-send 'Programming keyboard mode' 'error :('
        exit 1
    fi
}

function sweon {
    xmodmap -e 'keycode 47 = odiaeresis Odiaeresis' \
            -e 'keycode 48 = adiaeresis Adiaeresis' \
            -e 'keycode 34 = aring Aring' &&
    echo "swe" > "$status_file" &&
    notify-send 'åäö mode' 'on'
}

function sweoff {
    xmodmap -e 'keycode 47 = bracketleft braceleft odiaeresis Odiaeresis odiaeresis Odiaeresis' \
            -e 'keycode 48 = bracketright braceright adiaeresis Adiaeresis adiaeresis Adiaeresis' \
            -e 'keycode 34 = at backslash aring Aring aring Aring' &&
    echo "on" > "$status_file" &&
    notify-send 'åäö mode' 'off'
}

function off {
    setxkbmap se
    killall xcape 2>/dev/null
    echo "off" > "$status_file" &&
    notify-send 'Programming keyboard mode' 'off'
}

if [[ "$1" == "off" ]]; then
    off
elif [[ "$1" == "on" ]]; then
    on
elif [[ "$1" == "swetoggle" ]]; then
    if [[ "$file_contents" == "swe" ]]; then
        sweoff
    elif [[ "$file_contents" == "on" ]]; then
        sweon
    else
        notify-send 'åäö mode' "program mode is not enabled, can't toggle"
        exit 1
    fi
elif [[ -z "$1" ]]; then
    if [[ "$file_contents" == "on" ]] || [[ "$file_contents" == "swe" ]]; then
        off
    elif [[ "$file_contents" == "off" ]]; then
        on
    else
        echo "something is wrong with your file" >&2
        exit 1
    fi
else
    echo "your argument is wrong" >&2
    exit 1
fi

# update polybar module
pkill --signal SIGUSR1 progmode

exit 0

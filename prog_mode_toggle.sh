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
    fi
}

function off {
    setxkbmap se
    killall xcape 2>/dev/null
    echo "off" > "$status_file"
    notify-send 'Programming keyboard mode' 'off'
}

if [[ $1 == "off" ]]; then
    off
elif [[ $1 == "on" ]]; then
    on
elif [[ -z $1 ]]; then
    if [[ "$file_contents" == "on" ]]; then
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


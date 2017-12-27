#!/bin/bash

status_file=~/.program_mode
file_contents="$(cat $status_file)"

function on {
    setxkbmap er nodeadkeys && sleep 0.5 && xmodmap ~/.xmodmap_prog && sleep 0.5 && xcape -e "Control_L=Escape" && echo "on" > "$status_file"
}

function off {
    setxkbmap se && killall xcape && echo "off" > "$status_file"
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


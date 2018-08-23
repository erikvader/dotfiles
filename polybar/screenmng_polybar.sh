#!/bin/bash

active=false

function update {
    screenmng ison
    r=$?
    active=false
    if [[ $r -eq 1 ]]; then
        echo "PM"
    elif [[ $r -eq 0 ]]; then
        idle=$(xprintidle)
        if [[ $idle -le 60000 ]]; then
            echo ""
        else
            echo "$((idle / 60000))m"
            active=true
        fi
    else
        echo '????'
    fi
}

trap 'update' USR1

while true; do
    update
    if $active; then
        sleep 10 &
    else
        sleep 60 &
    fi
    wait $! &>/dev/null
done


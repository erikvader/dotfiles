#!/bin/bash

function update {
    screenmng ison
    r=$?
    if [[ $r -eq 1 ]]; then
        echo "PM"
    elif [[ $r -eq 0 ]]; then
        echo ""
    else
        echo '????'
    fi
}

trap 'update' USR1

while true; do
    update
    sleep 60 &
    wait $! &>/dev/null
done


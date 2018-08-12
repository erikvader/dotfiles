#!/bin/bash

function update {
    if [[ $(screenmng ison) = yes ]]; then
        echo ""
    else
        echo "PM"
    fi
}

update
trap 'update' USR1

sleep infinity &
spid=$!
trap 'kill $spid' EXIT

while ps -p $spid &>/dev/null; do
    wait $spid &>/dev/null
done


#!/bin/bash

ison=false

function toggle {
    if $ison; then
        redshift -x &>/dev/null
        ison=false
    else
        redshift -PO 3200 &>/dev/null
        ison=true
    fi
}

trap 'toggle' USR1
trap 'kill $spid' EXIT

sleep infinity &
spid=$!
while ps -p $spid &>/dev/null; do
    wait $spid &>/dev/null
done

#!/bin/bash

function on {
    local t=420
    xset s $t 0 s blank dpms $t $t $t
}

function off {
    xset s 0 0 dpms 0 0 0 -dpms
}

function xss {
    setsid xss-lock -- lock </dev/null >/dev/null &
}

function lock {
    xset dpms force off
}

function is_on {
    state=$(xset q)
    grep -qE 'timeout: +0 +cycle: +0' <<< "$state"
    grep1=$?
    grep -qE 'Standby: +0 +Suspend: +0 +Off: +0' <<< "$state"
    grep2=$?

    if [[ $grep1 -eq 0 && $grep2 -eq 0 ]]; then
        return 1
    elif [[ $grep2 -ne 0 && $grep2 -ne 0 ]]; then
        return 0
    else
        return 2
    fi
}

case "$1" in
    lock)
        lock
        ;;
    toggle)
        if is_on; then
            off
        else
            on
        fi
        pkill -f -USR1 screenmng_polybar.sh
        ;;
    startup)
        on
        xss
        ;;
    ison)
        is_on
        exit $?
        ;;
esac


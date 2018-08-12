#!/bin/bash

function on {
    xset s noblank
    xset s 420 60
}

function off {
    xset s 0 0
}

function xss {
    setsid xss-lock -l -n 'notify-send lock \#soon' -- screenmng lock </dev/null >/dev/null &
}

function lock_suspend {
    theme_lock {XSS_SLEEP_LOCK_FD}<&-
    exec {XSS_SLEEP_LOCK_FD}<&-
}

function lock_saver {
    if pgrep -x i3lock; then
        return
    fi
    theme_lock
    xset dpms force off
}

function lock {
    if [[ -e /dev/fd/${XSS_SLEEP_LOCK_FD:--1} ]]; then
        lock_suspend
    else
        lock_saver
    fi
}

function is_on {
    ! grep -qE 'timeout: +0 +cycle: +0' < <(xset q)
    return $?
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
        if is_on; then
            echo yes
        else
            echo no
        fi
        ;;
esac


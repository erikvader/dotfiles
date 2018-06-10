#!/bin/bash

status_file="$HOME/.program_mode"
file_contents="$(cat "$status_file")"

function on {
    if (set -e
        setxkbmap er nodeadkeys
        sleep 0.5
        xmodmap "$HOME/.xmodmap_prog"
        sleep 0.5
        xcape -e 'Control_R=Escape'
        xset r rate 300 35 # keyboard repeat speed
        echo "on" > "$status_file"
       )
    then
        notify-send 'Programming keyboard mode' 'on'
    else
        notify-send 'Programming keyboard mode' 'error turning on :('
        exit 1
    fi
}

function sweon {
    xmodmap -e 'keycode 47 = odiaeresis Odiaeresis' \
            -e 'keycode 48 = adiaeresis Adiaeresis' \
            -e 'keycode 34 = aring Aring' &&
    echo "swe" > "$status_file"
    # notify-send 'åäö mode' 'on'
}

function sweoff {
    xmodmap -e 'keycode 47 = bracketleft braceleft odiaeresis Odiaeresis odiaeresis Odiaeresis' \
            -e 'keycode 48 = bracketright braceright adiaeresis Adiaeresis adiaeresis Adiaeresis' \
            -e 'keycode 34 = at backslash aring Aring aring Aring' &&
    echo "on" > "$status_file"
    # notify-send 'åäö mode' 'off'
}

function off {
    if (set -e
        setxkbmap se
        pkill xcape 2>/dev/null
        echo "off" > "$status_file"
       )
    then
        notify-send 'Programming keyboard mode' 'off'
    else
        notify-send 'Programming keyboard mode' 'error turning off :('
        exit 1
    fi
}

case "$1" in
    off)
        off
        ;;
    on)
        on
        ;;
    swetoggle)
        case "$file_contents" in
            swe)
                sweoff
                ;;
            on)
                sweon
                ;;
            *)
                notify-send 'åäö mode' "program mode is not enabled, can't toggle"
                exit 1
                ;;
        esac
        ;;
    "")
        case "$file_contents" in
            on | swe)
                off
                ;;
            off)
                on
                ;;
            *)
                echo "something is wrong with your file" >&2
                exit 1
                ;;
        esac
        ;;
    *)
        echo "your argument is wrong" >&2
        exit 1
        ;;
esac


# update polybar module
pkill --signal SIGUSR1 progmode

exit 0

#!/bin/bash

# getCRTC "HDMI1"

found=
while IFS='' read -r line || [[ -n "$line" ]]; do
    if echo "$line" | grep -qE "^$1 "; then
        found="true"
        continue
    fi

    if [[ "$found" ]] && ! echo "$line" | grep -qE '^[^ \t]'; then
        exit 1
    fi

    if [[ "$found" ]] && echo "$line" | grep -q "CRTC:"; then
        echo "$line" | cut -d':' -f2 | tr -d '[:space:]'
        echo ""
        exit 0
    fi
done <<< "$(xrandr -q --verbose)"

exit 1

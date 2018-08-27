#!/bin/bash

# wrapper for polybar
# spawns one bar with automatic MONITORBACKUP

polybar_args=()

while [[ -n "$1" ]]; do
    if [[ "$1" = "--" ]]; then
        shift
        break
    else
        polybar_args+=("$1")
        shift
    fi
done

while IFS='' read -r line || [[ -n "$line" ]]; do
    MONITOR="$line" polybar "${polybar_args[@]}" &
done < <(comm -12 <(polybar -m | cut -d':' -f1 | sort) <(printf '%s\n' "$@" | sort))


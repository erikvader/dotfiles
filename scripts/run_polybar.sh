#!/bin/bash

# wrapper for polybar
# run_polybar [args..] -- [outputs..]
# runs polybar on all outputs with args.
# all outputs that doesn't exist (doesn't appear in polybar -m) will be ignored

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


#!/bin/bash

# feh_loop.sh delay imgs_args..
# feh_loop.sh 12 "--bg-max pic1" "--bg-fill pic2"

set -e
# shopt -s lastpipe

imgs=()
ind=0
delay=

while [[ "$1" ]]; do
    if [[ ind -eq 0 ]]; then
        delay="$1"
    else
        imgs+=("$1")
    fi
    ind=$((ind+1))
    shift
done

if [[ ind -le 1 ]]; then
    echo "too few arguments!" >&2
    exit 1
fi

# trap 'kill -- -$$' EXIT

length=$((ind-1))
ind=$length

while true; do
    if [[ $ind -ge $length ]]; then
        # (IFS=$'\n' imgs=( $(shuf -e -- "${imgs[@]}") ))
        readarray -t imgs < <(shuf -e -- "${imgs[@]}")
        ind=0
    fi
    eval "feh ""${imgs[$ind]}"
    ind=$((ind+1))
    sleep "$delay" &
    wait $!
done


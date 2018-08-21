#!/bin/bash

# m4_start CONF CMD [args] CMD-flag-for-config -- [tags]
# m4_start /conky/config conky -a bl -c -- black bg-white

conf=
cmd=
cmdflags=()
tags=()
current=

i=0
while [[ "$1" ]]; do
    if [[ $i -eq 0 ]]; then
        conf=$1
    elif [[ $i -eq 1 ]]; then
        cmd=$1
    else
        if [[ "$1" = "--" ]]; then
            current=tags
            shift
            continue
        fi
        if [[ -n "$current" ]]; then
            tags+=( "$1" )
        else
            cmdflags+=( "$1" )
        fi
    fi
    shift
    : $((i++))
done

echo "$conf"
echo "$cmd"
echo "${cmdflags[@]}"
echo "${tags[@]}"

if [[ -z "$conf" || -z "$cmd" ]]; then
    echo "conf or cmd wasn't set" >&2
    exit 1
fi

trap 'rm -f "$tempfile"' EXIT
trap '' TERM

set -e

tempfile=$(mktemp --tmpdir 'm4_start.XXXXXXXXX')

m4 -D "${tags[@]}" > "$tempfile" < "$conf"

$cmd "${cmdflags[@]}" "$tempfile"

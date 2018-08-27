#!/bin/bash

# runs one bar with $1 on env MONITOR
# and runs $2 on all the rest

prim=$1
seco=$2
shift
shift

while IFS='' read -r line || [[ -n "$line" ]]; do
    MONITOR="$line" polybar "$seco" &
done < <(comm -23 <(polybar -m | cut -d':' -f1 | sort) <(printf '%s\n' "$@" | sort))

exec run_polybar "$prim" -- "$@"


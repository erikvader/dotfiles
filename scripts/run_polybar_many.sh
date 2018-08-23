#!/bin/bash

# runs one bar with $1 on env MONITOR
# and runs $2 on all the rest

OTHERS=$(polybar -m | sed -nE -e "/^$MONITOR:.*\$/d" -e 's/^(.+?):.*$/\1/p')
for o in "${OTHERS[@]}"; do
    MONITOR="$o" run_polybar "$2" &
done
exec run_polybar "$1"


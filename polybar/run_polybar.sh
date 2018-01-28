#!/usr/bin/env sh

# Terminate already running bar instances
pkill polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# echo $1
# echo $2
# Launch bar1 and bar2
MONITOR=$1 polybar example &
if [ -n "$2" ]; then
    MONITOR=$2 polybar nonprimary &
fi

echo "Bars launched..."

#!/bin/bash

# wrapper for polybar
# spawns one bar with automatic MONITORBACKUP

MONITORBACKUP=$(polybar -m | head -n1 | cut -d':' -f1)
export MONITORBACKUP
exec polybar "$@"

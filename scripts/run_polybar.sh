#!/bin/bash

# wrapper for polybar

MONITORBACKUP=$(polybar -m | head -n1 | cut -d':' -f1)
export MONITORBACKUP
polybar "$@"

#!/bin/sh

if [ -n "$AUTORANDR_INIT" ]; then
    # NOTE: this is handled by the services at startup
    echo Skipped "$0"
    exit
fi

systemctl --user restart monitors-changed.target

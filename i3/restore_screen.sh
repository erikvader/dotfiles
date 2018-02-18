#!/bin/bash

~/.fehbg
if ! pgrep -x conky &>/dev/null; then
    conky &
fi


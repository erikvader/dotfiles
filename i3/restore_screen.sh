#!/bin/bash

~/.fehbg
if ! pgrep -x conky &>/dev/null; then
    $HOME/.config/conky/start &
fi


#!/bin/bash

# runs vlc while blackening the screen. 

~/.i3/blacken_screen.sh &>/dev/null

vlc "$@" &

wait $!

~/.i3/restore_screen.sh &>/dev/null


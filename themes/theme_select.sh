#!/bin/bash

theme_dir="$HOME/themes"
cur="$theme_dir/$1"

case "$1" in
    -l)
        find "$theme_dir" -mindepth 1 -maxdepth 1 -type d -printf '%f\n'
        exit 0
        ;;
    "")
        echo "please give argument"
        ;;
    *)
        if ! [[ -d "$cur" ]]; then
            echo "$1 is not a directory"
            exit 1
        fi;
        ;;
esac;

# change bg
"$cur/feh"

# change lock
ln -sf "$cur/lock.png" "$HOME/.lock"

# change conky
ln -sf "$cur/conky" "$HOME/.start_conky"
pkill conky
while pgrep -x conky >/dev/null; do sleep 1; done
"$HOME/.start_conky" &>/dev/null &!

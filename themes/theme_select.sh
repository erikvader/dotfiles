#!/bin/bash

set -e

theme_dir="$HOME/themes"
cur="$theme_dir/$1"
cur_theme_file="/tmp/theme_select_cur"

if [[ ! -d "$theme_dir" ]]; then
    echo "$theme_dir doesn't exist!" >&2
    exit 1
fi

if [[ -f "$cur_theme_file" ]]; then
    cur_theme="$(cat "$cur_theme_file")"
fi

case "$1" in
    -s)
        "$0" "safe"
        exit
        ;;
    -r)
        "$0" "$(echo "$("$0" -l)" | sed "/^$cur_theme$/d" | shuf -n1)"
        exit
        ;;
    -l)
        find -L "$theme_dir" -mindepth 1 -maxdepth 1 -type d -printf '%f\n'
        exit 0
        ;;
    "")
        echo "please give argument"
        exit 1
        ;;
    *)
        if ! [[ -d "$cur" ]]; then
            echo "$1 is not a directory"
            exit 1
        fi
        ;;
esac

echo "$1" > "$cur_theme_file"

# change bg
ln -sf "$cur/feh" "$HOME/.start_feh"
# "$HOME/.start_feh" &!

# change lock
ln -sf "$cur/lock.png" "$HOME/.lock"

# change conky
ln -sf "$cur/conky" "$HOME/.start_conky"
# pkill conky
# while pgrep -x conky >/dev/null; do sleep 1; done
# "$HOME/.start_conky" &>/dev/null &!

display_updater feh conky


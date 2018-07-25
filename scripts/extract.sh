#!/bin/bash

if [[ -f "$1" ]]; then
    case "$1" in
        *.tar.bz2|*.tar.gz|*.tar|*.tbz2|*.tgz)
            tar -axvf "$1" ;;
        *.bz2)
            bunzip2 "$1" ;;
        *.rar|*.cbr)
            unrar x "$1" ;;
        *.gz)
            gunzip "$1" ;;
        *.zip|*.cbz)
            unzip "$1" ;;
        *.Z)
            uncompress "$1" ;;
        *.7z)
            7z x "$1" ;;
        *) echo "unknown archive" ;;
    esac
else
    echo "'$1' is not a valid file"
fi

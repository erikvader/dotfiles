#!/bin/bash

set -e

fold=

function print_help {
    echo "usage:" >&2
    echo "  extract -h" >&2
    echo "  extract [-d] [--] archive" >&2
    echo "-d extract archive to a folder named after the archive. In the case of .gz, the original file should be kept instead of a new folder" >&2
}

while [[ "$1" ]]; do
    case "$1" in
        -d) fold=true ;;
        -h) print_help; exit 0 ;;
        --) shift; break ;;
        *) break ;;
    esac
    shift
done

function maybeMkdir {
    if [[ -n "$fold" ]]; then
        mkdir "$1"
    fi
}

targ=$1
fn=${targ%.*}

if [[ -f $targ ]]; then
    case "$targ" in
        *.tar.xz|*.tar.bz2|*.tar.gz|*.tar|*.tbz2|*.tgz)
            case "$targ" in
                *.tar.xz|*.tar.gz|*.tar.bz2) fn=${fn%.*} ;;
            esac
            maybeMkdir "$fn"
            tar -axvf "$targ" ${fold:+-C "$fn"} ;;
        *.bz2)
            # TODO: implement directory flag
            bunzip2 "$targ" ;;
        *.rar|*.cbr)
            maybeMkdir "$fn"
            unrar x "$targ" ${fold:+"$fn"} ;;
        *.gz)
            gunzip ${fold:+-k} "$targ" ;;
        *.zip|*.cbz)
            maybeMkdir "$fn"
            unzip "$targ" ${fold:+-d "$fn"} ;;
        *.Z)
            # TODO: implement directory flag
            uncompress "$targ" ;;
        *.7z)
            # TODO: implement directory flag
            7z x "$targ" ;;
        *) echo "unknown archive" ;;
    esac
else
    echo "'$targ' is not a valid file"
fi

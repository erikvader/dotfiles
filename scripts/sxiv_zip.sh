#!/bin/bash

set -e

fold=

function print_help {
    echo "usage:" >&2
    echo "  sxiv_zip -h" >&2
    echo "  sxiv_zip [--] zipfile" >&2
    echo "  sxiv_zip -d [--] dir" >&2
}

function open_zip {
    if [[ $# -ne 1 ]]; then
        echo "I want exactly one zip file ty" >&2
        print_help
        exit 0
    fi

    targ=$1
    if [[ ! -f $targ ]]; then
        echo "\"$targ\" does not exist or is not a file ?? What are you even doing?" >&2
        exit 1
    fi

    temp=$(mktemp -d --tmpdir 'sxiv_zip.XXXXXXXXXX')
    trap 'rm -rf "$temp"' EXIT

    ext=${targ##*.}
    case ${ext,,} in
        zip|cbz)
            if ! unzip "$targ" -d "$temp" &>/dev/null; then
                echo "unzip failed, is this really a zip file?" >&2
                exit 1
            fi
            ;;
        *)
            echo "\"$targ\" doesn't seem to be a zip/cbz file, hmmm.." >&2
            exit 1
            ;;
    esac

    sxiv -- "$temp"
}

function open_fold {
    if [[ $# -ne 1 ]]; then
        echo "expected exactly one directory" >&2
        print_help
        exit 0
    fi

    if [[ ! -d $1 ]]; then
        echo "\"$1\" doesn't exist or is not a directory" >&2
        exit 1
    fi

    temp=$(mktemp -d --tmpdir 'sxiv_zip.XXXXXXXXXX')
    trap 'rm -rf "$temp"' EXIT

    while IFS='' read -r line || [[ -n "$line" ]]; do
        if file -b --mime-type "$line" | grep -q '^image/'; then
            cp -t "$temp" "$line"
        fi
    done <<< "$(find "$1" -mindepth 1 -maxdepth 1)"

    sxiv -- "$temp"
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

if [[ "$fold" ]]; then
    open_fold "$@"
else
    open_zip "$@"
fi


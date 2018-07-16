#!/bin/bash

set -e

if [[ $# -ne 1 ]]; then
    echo "usage: sxiv_zip zipfile" >&2
    exit 1
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

sxiv "$temp"


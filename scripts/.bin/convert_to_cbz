#!/bin/bash

# converts a folder of of png:s and/or jpg:s to cbz-files
# all of these folders are contained in a directory, which is the first
# argument to this script

set -e

if [[ $# -le 0 ]]; then
    echo "me wants some arguments :("
    exit 1
fi

folder="$1"

fs="$(find "$folder" -mindepth 1 -maxdepth 1 -type d)"

if [[ -z "$fs" ]]; then
    echo "$folder is empty" >&2
    exit 1
fi

while IFS='' read -r line || [[ -n "$line" ]]; do
    if [[ -e "$line.cbz" ]]; then
        echo "skipping \"$line\""
        continue
    fi
    zip -jmr "$line.cbz" "$line" -i "*.png" -i "*.jpg"
    rm -d "$line"
    echo "created \"$line.cbz\""
done <<< "$fs"

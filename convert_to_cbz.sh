#!/bin/bash

# converts a folder of of png:s and/or jpg:s to cbz-files
# all of these folder are contained in a directory, which is the first
# argument to this script

if [[ $# -le 0 ]]; then
    echo "me wants some arguments :("
    exit 1
fi

folder="$1"

fs="$(find "$folder" -mindepth 1 -maxdepth 1 -type d)"

while IFS='' read -r line && [[ -n "$line" ]]; do
    if [[ -f "$line.cbz" ]]; then
        echo "skipping \"$line\""
        continue
    fi
    zip -jmr "$line.cbz" "$line" -i "*.png" -i "*.jpg"
    rm -d "$line"
    echo "created \"$line.cbz\""
done <<< "$fs"

#!/bin/bash

# config_slicer [tags]
# slices config files, keeping only wanted sections based on the given
# tags

# if [[ $# < 1 ]]; then
#     echo "inga argument :(" >&2
#     exit 1
# fi

tags=( "$@" )

function shouldKeep {
    read -ra words
    for w in "${words[@]}"; do
        if [[ ! "$w" = ';@@@;' ]]; then
            for t in "${tags[@]}"; do
                if [[ "$w" = "$t" ]]; then
                    return 0
                fi
            done
        fi
    done
    return 1
}

keeping=true

while IFS='' read -r line || [[ -n "$line" ]]; do
    if echo "$line" | grep -q ';@@@;'; then
        if ! shouldKeep <<< "$line"; then
            keeping=
        fi
    elif echo "$line" | grep -q ';/@@@;'; then
        keeping=true
    elif [[ "$keeping" ]]; then
        echo "$line"
    fi
done



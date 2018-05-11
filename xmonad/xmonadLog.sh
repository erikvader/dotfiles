#!/bin/bash

pipe="/tmp/XMonadLog"

if [[ ! -p "$pipe" ]]; then
    echo "pipe doesn't exist" >&2
    exit 1
fi

while IFS='' read -r line || [[ -n "$line" ]]; do
    echo "$line"
done < "$pipe"


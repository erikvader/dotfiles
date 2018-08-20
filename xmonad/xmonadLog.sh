#!/bin/bash

pipe="/tmp/XMonadLog"

if [[ ! -p "$pipe" ]]; then
    echo "pipe doesn't exist" >&2
    exit 1
fi

cat < "$pipe"


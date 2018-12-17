#!/bin/bash

set -e

if [[ $# -lt 1 ]]; then
    echo "Usage: flac_to_mp3 file.flac[,file.flac...]"
    exit 1
fi

for f in "$@"; do
    ffmpeg -i "$f" -codec:a libmp3lame -q:a 0 -c:v copy -map_metadata 0 -id3v2_version 3 "${f/%.*/.mp3}"
done

#!/bin/bash

# create-lock-img.sh bg-image lock-icon save-location

convert "$1" -resize 1920x1080\! "$3"
convert "$3" -blur 5x8 "$3"
# convert "$3" -scale 10% -scale 1000% "$3"
convert "$3" "$2" -gravity center -composite -matte "$3"

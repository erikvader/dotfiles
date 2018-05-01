#!/bin/bash

# mouse_dance.sh $sleep

if ! [[ $1 ]]; then
    sleep=0.04
else
    sleep=$1
fi


# get info of focused window
res=$(xdotool getactivewindow getwindowgeometry --shell)
if [[ $? -ne 0 ]]; then
    exit $?
fi
eval $res
curwindow="$WINDOW"
curx="$X"
cury="$Y"
curw="$WIDTH"
curh="$HEIGHT"

# move to middle
function jumpToMiddle {
    xdotool mousemove --clearmodifiers $(($curx + $curw / 2)) $(($cury + $curh / 2))
}

jumpToMiddle

# test_dist x1 y1 x2 y2 d
function testDist {
    [[ $(( ($1 - $3)**2 + ($2 - $4)**2 )) -le $(( $5**2 )) ]]
    return $?
}

# move randomly
minx=$(bc <<< "($curx + 0.1 * $curw) / 1")
maxx=$(bc <<< "($curx + 0.9 * $curw) / 1")
miny=$(bc <<< "($cury + 0.1 * $curh) / 1")
maxy=$(bc <<< "($cury + 0.9 * $curh) / 1")

rx=-1
ry=-1

# getNext old_x old_y
function moveNext {
    local i=0
    while [[ $i < 10 ]]; do
        i=$(($i+1))
        rx=$(shuf -i $minx-$maxx -n 1)
        ry=$(shuf -i $miny-$maxy -n 1)
        if testDist $1 $2 $rx $ry 100; then
            break
        fi
    done
}

while true; do
    # move to random within window
    eval $(xdotool getmouselocation --shell)
    if [[ $curwindow != $WINDOW ]]; then
        break
    fi

    # moved during sleep? then exit
    if [[ $rx -ne -1 && $ry -ne -1 && ($rx -ne $X || $ry -ne $Y) ]]; then
        jumpToMiddle
        break
    fi

    moveNext $X $Y
    xdotool mousemove --clearmodifiers --sync $rx $ry

    # sleep
    sleep $sleep

done

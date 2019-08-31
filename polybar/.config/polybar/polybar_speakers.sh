#!/bin/bash

function isActive {
    speakers status
}

trap isActive SIGUSR1

while true; do
    isActive
    sleep 60 &
    wait $!
done


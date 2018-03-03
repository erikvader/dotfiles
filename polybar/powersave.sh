#!/bin/bash

function isActive {
    ps="/sys/module/snd_hda_intel/parameters/power_save"
    psc="/sys/module/snd_hda_intel/parameters/power_save_controller"

    if grep '1' "$ps" || grep 'Y' "$psc"; then
        # return 0
        echo ""
    else
        # return 1
        echo "  "
    fi
}

trap isActive SIGUSR1

while true; do
    isActive
    sleep 60 &
    wait "$!"
done


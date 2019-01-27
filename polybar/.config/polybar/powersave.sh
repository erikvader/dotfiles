#!/bin/bash

function isActive {
    ps="/sys/module/snd_hda_intel/parameters/power_save"
    psc="/sys/module/snd_hda_intel/parameters/power_save_controller"

    if grep -q '1' "$ps" || grep -q 'Y' "$psc"; then
        # return 0
        echo ""
    else
        # return 1
        echo "🙉"
    fi
}

trap isActive SIGUSR1
# trap 'trap "" TERM EXIT; kill 0; trap - INT TERM; kill -INT $$' INT
# trap "kill 0" EXIT

while true; do
    isActive
    sleep 60 &
    wait "$!"
done


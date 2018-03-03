#!/bin/bash

# toggle audio module powersave feature

ps="/sys/module/snd_hda_intel/parameters/power_save"
psc="/sys/module/snd_hda_intel/parameters/power_save_controller"

if grep 1 "$ps" >/dev/null; then
    echo 0 > "$psc"
    echo 0 > "$ps"
    notify-send 'powersave off'
else
    echo 'Y' > "$psc"
    echo '1' > "$ps"
    notify-send 'powersave on'
fi

pkill --signal SIGUSR1 -x 'powersave.sh'

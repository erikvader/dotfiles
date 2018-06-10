#!/bin/bash

# if [[ "$(playerctl --player=spotify status)" == "Playing" ]]; then
#     echo "$(playerctl --player=spotify metadata artist) - $(playerctl --player=spotify metadata title)"
# else
#     echo ""
# fi;

zscroll -b "" -u true -M "playerctl --player=spotify status" -m "Playing" "-b '> '" -m "Paused" "-s 0 -b '|| '" -m "Not available" "-b ''" "$HOME/.config/polybar/playerctl_all.sh" &

wait

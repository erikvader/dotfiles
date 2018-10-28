#!/bin/bash

declare -A scripts

scripts["Clipboard history"]="rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"
scripts["Invert colors"]="xrandr-invert-colors"
scripts["Toggle redshift"]="pkill -USR1 '^redshift$'"
scripts["Audio powersave toggle"]="gksudo toggle_audio_powersave"
scripts["Manjaro i3 menu"]="morc_menu"
scripts["Manjaro Settings Manager"]="manjaro-settings-manager"
scripts["PulseAudio Volume Control"]="pavucontrol"
scripts["bmenu"]="terminal -e bmenu"
scripts["Kill compton"]="pkill compton"
scripts["Start/restart compton"]="display_updater compton"
scripts["Screenshot whole screen"]="i3-scrot -d 1"
scripts["Screenshot window"]="i3-scrot -w 1"
scripts["Screenshot selection"]="i3-scrot -s"
scripts["Screenshot clipboard whole screen"]="scrot_clipboard -d 1"
scripts["Screenshot clipboard window"]="scrot_clipboard -u -d 1"
scripts["Screenshot clipboard selection"]="scrot_clipboard -s"
scripts["Set transparency to 99%"]="transset-df -a 0.99"
scripts["Set transparency to 100%"]="transset-df -a 1"
scripts["Program mode toggle"]="prog_mode_toggle"
scripts["Program mode swedish toggle"]="prog_mode_toggle swetoggle"
scripts["Show xtitle"]='notify-send "$(xtitle)"'
scripts["Change theme"]="theme_select_rofi"
scripts["Lock and turn off screen"]="screenmng lock"
scripts["Lock"]='lock'
scripts["Refresh Polybar (SIGUSR1)"]="pkill -USR1 -x polybar"
scripts["Display Updater"]="display_updater_rofi"
scripts["Open Downloaded PDF"]='open_downloaded_pdf'
scripts["Suspend"]='systemctl suspend'
scripts['Presentation Mode toggle']='screenmng toggle'
scripts['Org Agenda']='emacsclient -n -c -e '"'"'(same-buffer (org-agenda))'"'"
scripts['Org quick todo']="rofi -dmenu -p 'TODO' | sed -E 's/^/\\n* TODO /' >> $HOME/Dropbox/org/keep.org"
scripts['Org open keep.org']="emacsclient -c -n $HOME/Dropbox/org/keep.org"

choice="$( printf '%s\n' "${!scripts[@]}" | sort | rofi -dmenu -i -p "exec" -no-custom )"

if [[ "$choice" ]]; then
    echo "du valde $choice"
    eval "${scripts[$choice]}" &
else
    echo "du valde inget"
fi

#!/bin/bash

scripts=("clipit-rofi"
         "xrandr-invert-colors"
         "pkill -USR1 '^redshift$'"
         "gksudo toggle_audio_powersave"
         "morc_menu"
         "manjaro-settings-manager"
         "pavucontrol"
         "terminal -e bmenu"
         "pkill compton"
         "display_updater compton"
         "i3-scrot -d 1"
         "i3-scrot -w 1"
         "i3-scrot -s"
         "scrot_clipboard -d 1"
         "scrot_clipboard -u -d 1"
         "scrot_clipboard -s"
         "transset-df -a 0.99"
         "transset-df -a 1"
         "prog_mode_toggle"
         "prog_mode_toggle swetoggle"
         'notify-send "$(xtitle)"'
         "theme_select_rofi"
         "theme_lock"
         "pkill -USR1 -x polybar"
         "display_updater_rofi"
         'open_downloaded_pdf'
        )

names=("Clipit history"
       "Invert colors"
       "Toggle redshift"
       "Audio powersave toggle"
       "Manjaro i3 menu"
       "Manjaro Settings Manager"
       "PulseAudio Volume Control"
       "bmenu"
       "Kill compton"
       "Start/restart compton"
       "Screenshot whole screen"
       "Screenshot window"
       "Screenshot selection"
       "Screenshot clipboard whole screen"
       "Screenshot clipboard window"
       "Screenshot clipboard selection"
       "Set transparency to 99%"
       "Set transparency to 100%"
       "Program mode toggle"
       "Program mode swedish toggle"
       "Show xtitle"
       "Change theme"
       "Lock"
       "Refresh Polybar (SIGUSR1)"
       "Display Updater"
       "Open Downloaded PDF"
      )

# eller printf '%s\n' "${names[@]}"
choice="$( IFS=$'\n'; echo -e "${names[*]}" | rofi -dmenu -i -p "exec" -no-custom -format i )"

if [[ "$choice" ]]; then
    echo "du valde ${names[$choice]}"
    eval "${scripts[$choice]}" &
else
    echo "du valde inget"
fi

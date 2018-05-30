#!/bin/bash

scripts=("xrandr-invert-colors"
         "pkill -USR1 '^redshift$'"
         "display_updater startup"
         "display_updater update"
         "gksudo ""$HOME/.toggle_audio_powersave.sh"
         "morc_menu"
         "pavucontrol"
         "terminal -e bmenu"
         "pkill compton"
         "display_updater compton"
         "i3-scrot -d 1"
         "i3-scrot -w 1"
         "i3-scrot -s"
         "$HOME/.i3/scrot_clipboard.sh -d 1"
         "$HOME/.i3/scrot_clipboard.sh -u -d 1"
         "$HOME/.i3/scrot_clipboard.sh -s"
         "transset-df -a 0.99"
         "transset-df -a 1"
         "$HOME/prog_mode_toggle.sh"
         "$HOME/prog_mode_toggle.sh swetoggle"
         'notify-send "$(xtitle)"'
         "$HOME/dotfiles/themes/theme_select_rofi.sh"
         "blurlock"
         "pkill -USR1 -x polybar"
        )

names=("Invert colors"
       "Toggle redshift"
       "Display updater: startup"
       "Display updater: update"
       "Audio powersave toggle"
       "Manjaro i3 menu"
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
      )

# eller printf '%s\n' "${names[@]}"
choice="$( IFS=$'\n'; echo -e "${names[*]}" | rofi -dmenu -i -p "exec" -no-custom -format i )"

if [[ "$choice" ]]; then
    echo "du valde ${names[$choice]}"
    eval "${scripts[$choice]}" &
else
    echo "du valde inget"
fi

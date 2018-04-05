#!/bin/bash

scripts=("xrandr-invert-colors"
         "$HOME/.i3/blacken_screen.sh"
         "$HOME/.i3/restore_screen.sh"
         "pkill -USR1 '^redshift$'"
         "$HOME/.i3/display_updater.sh"
         "gksudo ""$HOME/.toggle_audio_powersave.sh"
         "morc_menu"
         "pavucontrol"
         "terminal -e bmenu"
         "pkill compton"
         "compton -b"
         "i3-scrot"
         "i3-scrot -w"
         "i3-scrot -s"
         "$HOME/.i3/scrot_clipboard.sh"
         "$HOME/.i3/scrot_clipboard.sh -u"
         "$HOME/.i3/scrot_clipboard.sh -s"
         "transset-df -a 0.99"
         "$HOME/prog_mode_toggle.sh"
         "$HOME/prog_mode_toggle.sh swetoggle"
         'notify-send "$(xtitle)"'
        )

names=("Invert colors"
       "Blacken screen"
       "Restore screen"
       "Toggle redshift"
       "Display updater"
       "Audio powersave toggle"
       "Manjaro i3 menu"
       "PulseAudio Volume Control"
       "bmenu"
       "Kill compton"
       "Start compton"
       "Screenshot whole screen"
       "Screenshot window"
       "Screenshot selection"
       "Screenshot clipboard whole screen"
       "Screenshot clipboard window"
       "Screenshot clipboard selection"
       "Set transparency to 99%"
       "Program mode toggle"
       "Program mode swedish toggle"
       "Show xtitle"
      )

# eller printf '%s\n' "${names[@]}"
choice="$( IFS=$'\n'; echo -e "${names[*]}" | rofi -dmenu -i -p "exec" -no-custom -format i )"

if [[ "$choice" ]]; then
    echo "du valde ${names[$choice]}"
    eval "${scripts[$choice]}" &
else
    echo "du valde inget"
fi

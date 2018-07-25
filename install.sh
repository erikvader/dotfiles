#!/bin/bash

# places all scripts in a bin folder in home
# this install script relies on $PWD to be the directory that this
# script is stored in (the root of the dotfiles folder)

force=

if [[ $1 = -f ]]; then
    force=true
fi

function colorize {
    tput bold
    tput setaf "$1"
    echo -n "$2"
    tput sgr0
}

# wrapper for ln
function place {
    if [[ -L "$2" && "$(readlink "$2")" = "$1" ]]; then
        colorize 2 "EXISTS "
    elif [[ -z "$force" && -L "$2" ]]; then
        colorize 1 "EXISTS "
    else
        if ln -sTi "$1" "$2"; then
            colorize 2 "OK "
        else
            colorize 1 "FAILED "
        fi
    fi
    printf "%s l-> %s\\n" "$1" "$2"
}

# only for files
function placecp {
    if [[ -f "$2" && "$(cat "$1")" = "$(cat "$2")" ]]; then
        colorize 2 "EXISTS "
    elif [[ -z "$force" && -f "$2" ]]; then
        echo "File exists: $2" >&2
        colorize 1 "EXISTS "
    else
        if cp -iT "$1" "$2"; then
            colorize 2 "OK "
        else
            colorize 1 "FAILED "
        fi
    fi
    printf "%s c-> %s\\n" "$1" "$2"
}

declares="$(declare -f colorize; declare -f place; declare -f placecp); force=$force"

bin_location="$HOME/.bin"
scripts=(
    "xmonad/xmonadLog.sh"
    "themes/theme_select.sh"
    "themes/theme_select_rofi.sh"
    "themes/feh_loop.sh"
    "themes/init_theme.sh"
    "conky/conky_start"
    "i3/scrot_clipboard.sh"
    "i3/lock.sh"
    "i3/display_updater.sh"
    "i3/i3_brightness.sh"
    "i3/create-lock-img.sh"
    "i3/display_updater_rofi.sh"
    "i3/rofi_script_selector.sh"
    # "scripts/convert_to_cbz.sh"
    "scripts/flasher.sh"
    "scripts/prog_mode_toggle.sh"
    "scripts/toggle_audio_powersave.sh"
    "scripts/config_slicer.py"
    # "scripts/config_slicer.sh"
    "scripts/cowfortune"
    "scripts/mouse_dance.sh"
    "scripts/getCRTC.sh"
    "scripts/els"
    "scripts/open_downloaded_pdf.sh"
    "scripts/sxiv_zip.sh"
    "scripts/clipit-rofi.py"
    "scripts/flac_to_mp3.sh"
    "scripts/extract.sh"
)

for s in "${scripts[@]}"; do
    ss=${s%.*}
    ss=${ss##*/}
    place "$PWD/$s" "$bin_location/$ss"
done

# some more links in .config
# ln -sf "$PWD/i3" "$HOME/.i3"
place "$PWD/conky"         "$HOME/.config/conky"
place "$PWD/polybar"       "$HOME/.config/polybar"
place "$PWD/mpv"           "$HOME/.config/mpv"
place "$PWD/zathura"       "$HOME/.config/zathura"
place "$PWD/sxiv/sxiv"     "$HOME/.config/sxiv"
place "$PWD/.compton.conf" "$HOME/.config/compton.conf"
place "$PWD/redshift.conf" "$HOME/.config/redshift.conf"
place "$PWD/zsh/custom"    "$HOME/.my-oh-my-zsh-custom"
place "$PWD/.profile"      "$HOME/.profile"
place "$PWD/.xmodmap_prog" "$HOME/.xmodmap_prog"
place "$PWD/zsh/.zshrc"    "$HOME/.zshrc"
place "$PWD/makefiles"     "$HOME/makefiles"
place "$PWD/.Xresources"   "$HOME/.Xresources"

# thing in urxvt that is overridden after each update
if [[ -f "/usr/lib/urxvt/perl/eval" ]]; then
    # urxvt is installed
    sudo bash -c "$declares; placecp \"$PWD/urxvt/eval\" \"/usr/lib/urxvt/perl/eval\""
fi


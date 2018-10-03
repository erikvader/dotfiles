#!/bin/bash

# places all scripts in a bin folder in home
# this install script relies on $PWD to be the directory that this
# script is stored in (the root of the dotfiles folder)

# random saker att tänka på:
# i3-scrot screenshot dir (~/.config/i3-scrot.conf)

force=
tag=

while [[ "$1" ]]; do
    case "$1" in
        -f) force=true ;;
        *) tag=$1 ;;
    esac
    shift
done

function colorize {
    tput bold
    tput setaf "$1"
    echo -n "$2"
    tput sgr0
}

# wrapper for ln
# place src dstdir dstname tag
function place {
    if [[ ! "$4" = "$tag" && ! "$tag" = all ]]; then
        return
    fi
    mkdir -p "$2"
    if [[ -z "$3" ]]; then
        dst=$2/$(basename "$1")
    else
        dst=$2/$3
    fi
    if [[ -L "$dst" && "$(readlink "$dst")" = "$1" ]]; then
        colorize 2 "EXISTS "
    elif [[ -z "$force" && -L "$dst" ]]; then
        colorize 1 "EXISTS "
    else
        if ln -sTi "$1" "$dst"; then
            colorize 2 "OK "
        else
            colorize 1 "FAILED "
        fi
    fi
    printf "%s l-> %s\\n" "$1" "$dst"
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
    "scripts/m4_start.sh"
    "i3/scrot_clipboard.sh"
    "i3/lock.sh"
    "i3/i3_brightness.sh"
    "i3/create-lock-img.sh"
    "i3/rofi_script_selector.sh"
    "display_updater/display_updater.py"
    "display_updater/display_updater_rofi.sh"
    # "scripts/convert_to_cbz.sh"
    "scripts/flasher.sh"
    "scripts/prog_mode_toggle.sh"
    "scripts/toggle_audio_powersave.sh"
    "scripts/config_slicer.py"
    # "scripts/config_slicer.sh"
    "scripts/cowfortune"
    "scripts/mouse_dance.sh"
    "scripts/els"
    "scripts/open_downloaded_pdf.sh"
    "scripts/sxiv_zip.sh"
    "scripts/flac_to_mp3.sh"
    "scripts/extract.sh"
    "scripts/mangadex_downloader.py"
    "scripts/run_polybar.sh"
    "scripts/run_polybar_many.sh"
    "power/screenmng.sh"
    "redshift/redshift-manual-daemon.sh"
)

for s in "${scripts[@]}"; do
    ss=${s%.*}
    ss=${ss##*/}
    place "$PWD/$s" "$bin_location" "$ss" scripts
done

# some more links in .config
place "$PWD/conky"                       "$HOME/.config"          ""                     conky
place "$PWD/polybar"                     "$HOME/.config"          ""                     polybar
place "$PWD/mpv"                         "$HOME/.config"          ""                     mpv
place "$PWD/zathura"                     "$HOME/.config"          ""                     zathura
place "$PWD/sxiv/sxiv"                   "$HOME/.config"          ""                     sxiv
place "$PWD/.compton.conf"               "$HOME/.config"          "compton.conf"         compton
place "$PWD/zsh/custom"                  "$HOME"                  ".my-oh-my-zsh-custom" zsh
place "$PWD/zsh/.zshrc"                  "$HOME"                  ""                     zsh
place "$PWD/.profile"                    "$HOME"                  ""                     profile
place "$PWD/.xmodmap_prog"               "$HOME"                  ""                     xmodmap
place "$PWD/makefiles"                   "$HOME"                  ""                     makefiles
place "$PWD/.Xresources"                 "$HOME"                  ""                     xresources
place "$PWD/ranger/commands.py"          "$HOME/.config/ranger"   ""                     ranger
place "$PWD/ranger/rc.conf"              "$HOME/.config/ranger"   ""                     ranger
place "$PWD/ranger/rifle.conf"           "$HOME/.config/ranger"   ""                     ranger
place "$PWD/ranger/scope.sh"             "$HOME/.config/ranger"   ""                     ranger
place "$PWD/ranger/r.shell"              "$bin_location"          ""                     ranger
place "$PWD/dunstrc"                     "$HOME/.config/dunst"    ""                     dunst

place "$PWD/xmonad/xmonad.hs"            "$HOME/.xmonad"          ""                     xmonad

place "$PWD/xmonad/CompactWorkspaces.hs" "$HOME/.xmonad/lib/Erik" ""                     xmonad
place "$PWD/xmonad/MyLimitWindows.hs"    "$HOME/.xmonad/lib/Erik" ""                     xmonad
place "$PWD/xmonad/MyStuff.hs"           "$HOME/.xmonad/lib/Erik" ""                     xmonad
place "$PWD/xmonad/ThreeColP.hs"         "$HOME/.xmonad/lib/Erik" ""                     xmonad
place "$PWD/xmonad/IndiPP.hs"            "$HOME/.xmonad/lib/Erik" ""                     xmonad

place "$PWD/rofi"                        "$HOME/.config"          ""                     rofi

# thing in urxvt that is overridden after each update
# if [[ -f "/usr/lib/urxvt/perl/eval" ]]; then
    # urxvt is installed
    # sudo bash -c "$declares; placecp \"$PWD/urxvt/eval\" \"/usr/lib/urxvt/perl/eval\""
# fi


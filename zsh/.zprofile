#
# ~/.zprofile
#
#

export QT_QPA_PLATFORMTHEME=gtk2

export PATH="$HOME/.bin:$HOME/.local/bin:$PATH"
export PYTHONPATH="$HOME/.pythonlibs:$PYTHONPATH"

# java doesn't like xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# no more __pycache__
export PYTHONDONTWRITEBYTECODE=1

export EDITOR='/usr/bin/vim'

[[ -f $HOME/.zprofile.local ]] && . "$HOME/.zprofile.local"

# Auto startx depending on the tty
# https://wiki.gentoo.org/wiki/X_without_Display_Manager
[[ -z $DISPLAY ]] && (( $EUID != 0 )) && [[ ${TTY/tty} != $TTY ]] && (( ${TTY:8:1} == 1 )) && startx

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

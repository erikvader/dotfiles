#
# ~/.zprofile
#
#

# NOTE: can also use typeset -U path PATH and typeset -T PATH path :
[[ ":$PATH:"       != *":$HOME/.bin:"* ]]        && export PATH="$HOME/.bin:$PATH"
[[ ":$PATH:"       != *":$HOME/.local/bin:"* ]]  && export PATH="$HOME/.local/bin:$PATH"
[[ ":$PYTHONPATH:" != *":$HOME/.pythonlibs:"* ]] && export PYTHONPATH="$HOME/.pythonlibs${PYTHONPATH:+:}$PYTHONPATH"

# java doesn't like xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# move all __pycache__
export PYTHONPYCACHEPREFIX=$HOME/.cache/pycache

export EDITOR='/usr/bin/vim'

export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

[[ -f $HOME/.zprofile.local ]] && . "$HOME/.zprofile.local"

# Auto startx depending on the tty
# https://wiki.gentoo.org/wiki/X_without_Display_Manager
[[ -z $DISPLAY ]] && (( $EUID != 0 )) && [[ ${TTY/tty} != $TTY ]] && (( ${TTY:8:1} == 1 )) && startx


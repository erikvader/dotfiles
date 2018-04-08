#
# ~/.profile
#
#

[[ "$XDG_CURRENT_DESKTOP" == "KDE" ]] || export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR=/usr/bin/nano
export PATH="$HOME/.bin:$PATH"

export RANGER_LOAD_DEFAULT_RC="FALSE"

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

[[ -f ~/.extend.profile ]] && . ~/.extend.profile

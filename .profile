#
# ~/.profile
#
#

#[[ "$XDG_CURRENT_DESKTOP" == "KDE" ]] || export QT_QPA_PLATFORMTHEME="qt5ct"

export PATH="$HOME/.bin:$PATH"

# java doesn't like xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# [[ -f ~/.extend.profile ]] && . ~/.extend.profile

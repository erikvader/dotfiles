# NOTE: can also use typeset -U path PATH and typeset -T PATH path :
typeset -U path PATH
path=($HOME/.bin $HOME/.local/bin $path)
export PATH

typeset -UT PYTHONPATH pythonpath :
pythonpath=($HOME/.pythonlibs $pythonpath)
export PYTHONPATH

export EDITOR=/usr/bin/vim
export VISUAL=$EDITOR
export PAGER=/usr/bin/less

# java doesn't like xmonad
export _JAVA_AWT_WM_NONREPARENTING=1

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# move all __pycache__
export PYTHONPYCACHEPREFIX=$HOME/.cache/pycache

# tell ssh-add where ssh-agent.service is listening
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

# smartcase in less
export LESS=-Ri

# disables prompt mangling in virtual_env/bin/activate
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Commands for various keybinds in ranger and zsh
export FZF_BASE_COMMAND='find -L . -mindepth 1 %s \\( -fstype dev -o -fstype proc %s \\) -prune -o \\( -name .git -o -name dosdevices \\) -prune -printf "%%P\\n" -o %s -printf "%%P\\n" 2>/dev/null'
export FZF_DEFAULT_COMMAND=$(printf "$FZF_BASE_COMMAND" "" "" "")
export FZF_CTRL_T_COMMAND=$(printf "$FZF_BASE_COMMAND" "-maxdepth 1 " "" "")

# ranger wont load default conf, only user
export RANGER_LOAD_DEFAULT_RC=FALSE

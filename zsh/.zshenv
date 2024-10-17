# Flag the already linked variable `path` to only contain unique elements
typeset -U path PATH
path=($HOME/.bin $HOME/.local/bin $path)

export EDITOR=/usr/bin/vim
export VISUAL=$EDITOR
export PAGER=/usr/bin/less

# weird HiDPI scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0

# tell ssh-add where ssh-agent.service is listening
export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

# smartcase in less
export LESS=-Ri

# Commands for various keybinds in ranger and zsh
export FZF_BASE_COMMAND='find -L . -mindepth 1 %s \\( -fstype dev -o -fstype proc %s \\) -prune -o \\( -name .git -o -name dosdevices \\) -prune -printf "%%P\\n" -o %s -printf "%%P\\n" 2>/dev/null'
export FZF_DEFAULT_COMMAND=$(printf "$FZF_BASE_COMMAND" "" "" "")
export FZF_CTRL_T_COMMAND=$(printf "$FZF_BASE_COMMAND" "-maxdepth 1 " "" "")

# Load the rest
for f in "$HOME"/.zshenv.d/*.zsh; do
    source "$f"
done

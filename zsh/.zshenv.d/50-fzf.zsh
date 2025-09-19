# Commands for various keybinds in ranger and zsh
export FZF_BASE_COMMAND='find -L . -mindepth 1 %s \\( -fstype dev -o -fstype proc %s \\) -prune -o \\( -name .git -o -name dosdevices \\) -prune -printf "%%P\\n" -o %s -printf "%%P\\n" 2>/dev/null'
export FZF_DEFAULT_COMMAND=$(printf "$FZF_BASE_COMMAND" "" "" "")

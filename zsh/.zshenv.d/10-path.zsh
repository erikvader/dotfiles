# Flag the already linked variable `path` to only contain unique elements
typeset -U path PATH
path=($HOME/.bin $HOME/.local/bin $path)


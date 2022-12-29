# run a command and enter normal shell after it exits instead of exiting the whole shell.
# Good for starting a "normal" shell with ranger started, for example.
# http://www.zsh.org/mla/users/2005/msg00599.html
# zsh -is eval cmd...
if [[ $1 == eval ]]; then
    "$@"
    set --
fi

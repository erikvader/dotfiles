# Emacs tramp fix
if [[ $TERM = dumb ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
fi

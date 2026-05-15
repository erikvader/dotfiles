if [[ $intelligence == dumb ]]; then
    return
fi

# loads the associative arrays fg, fg_bold, bg, etc.
autoload -U colors && colors

# sets LS_COLORS
eval "$(dircolors -b)"

alias diff='diff --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'

function man {
    LESS_TERMCAP_md=$fg_bold[magenta] \
    LESS_TERMCAP_me=$reset_color \
    LESS_TERMCAP_so=$bg[magenta]$fg[black] \
    LESS_TERMCAP_se=$reset_color \
    LESS_TERMCAP_us=$fg_bold[blue] \
    LESS_TERMCAP_ue=$reset_color \
    GROFF_NO_SGR=1 \
    command man "$@"
}

# loads the associative arrays fg, fg_bold, bg, etc.
autoload -U colors && colors

# sets LS_COLORS
eval "$(dircolors -b)"

# enable variable substitutions and stuff in the prompt
setopt prompt_subst

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
    zle && zle -R
}

alias diff='diff --color=auto'
alias ls='ls --color=auto'
alias grep='grep --color=auto'

function man {
    LESS_TERMCAP_md=$'\e[01;35m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[45;30m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;34m' \
    command man "$@"
}

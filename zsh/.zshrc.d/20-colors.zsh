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
    LESS_TERMCAP_md=$(tput bold; tput setaf 5) \
    LESS_TERMCAP_me=$(tput sgr0) \
    LESS_TERMCAP_so=$(tput setab 5; tput setaf 0) \
    LESS_TERMCAP_se=$(tput sgr0) \
    LESS_TERMCAP_us=$(tput bold; tput setaf 4) \
    LESS_TERMCAP_ue=$(tput sgr0) \
    GROFF_NO_SGR=1 \
    command man "$@"
}

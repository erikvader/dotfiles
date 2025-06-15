# Show more things when autocompleting git
GIT_COMPLETION_SHOW_ALL=1
GIT_COMPLETION_SHOW_ALL_COMMANDS=1

alias g=git
alias gg='g -c delta.side-by-side=true'
alias gup='cd "$(git rev-parse --show-cdup)"'
alias gsuper='cd "$(git rev-parse --show-superproject-working-tree)"'

# TODO: en cd som bara autocompletar submoduler
